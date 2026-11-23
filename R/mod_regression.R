# Shiny module: regression --------------------------------------------------

mod_regression_ui <- function(id) {
  ns <- shiny::NS(id)
  rm <- regression_models()
  models <- stats::setNames(rm$model, rm$label)
  sidebar <- shiny::tagList(
    mod_data_input_ui(ns("data")),
    shiny::uiOutput(ns("vars")),
    shiny::selectizeInput(ns("models"), "Modelo(s)", models, selected = c("linear", "quadratic"), multiple = TRUE,
                          options = list(plugins = list("remove_button"))),
    shiny::selectInput(ns("band"), "Bandas", c("Confianca (resposta media)" = "confidence", "Predicao (nova observacao)" = "prediction", "Nenhuma" = "none")),
    shiny::selectInput(ns("conf"), "Nivel de confianca", c("95%" = 0.95, "90%" = 0.90, "99%" = 0.99, "Personalizado" = "custom")),
    shiny::conditionalPanel(sprintf("input['%s'] == 'custom'", ns("conf")), shiny::numericInput(ns("conf_custom"), "Nivel (0,5 a 0,999)", 0.95, 0.5, 0.999, 0.005)),
    bslib::accordion(open = FALSE,
      bslib::accordion_panel("Opcoes avancadas",
        shiny::selectInput(ns("band_method"), "Bandas em modelos nao lineares", c("Metodo delta" = "delta", "Bootstrap de residuos" = "bootstrap")),
        shiny::numericInput(ns("nboot"), "Reamostragens (bootstrap)", 500, 100, 5000, 100),
        shiny::numericInput(ns("seed"), "Semente", 123),
        shiny::selectInput(ns("aggregate"), "Ajustar sobre", c("Observacoes individuais (recomendado)" = "none", "Medias por dose (legado)" = "mean", "Medianas por dose (legado)" = "median")),
        shiny::numericInput(ns("price"), "Razao de precos insumo/produto (dose economica)", NA, 0),
        shiny::textInput(ns("at"), "Resposta marginal em x = (separar por ;)", ""))),
    run_button(ns)
  )
  analysis <- shiny::tagList(shiny::uiOutput(ns("check")), shiny::uiOutput(ns("cards")), shiny::uiOutput(ns("notes")),
                             bslib::card(bslib::card_header("Comparacao de modelos e indicadores de ajuste"), DT::DTOutput(ns("t_stats"))),
                             bslib::card(bslib::card_header("Equacoes ajustadas"), DT::DTOutput(ns("t_eq"))),
                             bslib::card(bslib::card_header("Coeficientes"), DT::DTOutput(ns("t_coef"))),
                             shiny::uiOutput(ns("extra")))
  diagnostics <- shiny::tagList(
    bslib::layout_columns(col_widths = c(6, 6), bslib::card(shiny::plotOutput(ns("d_res"), height = 340)), bslib::card(shiny::plotOutput(ns("d_qq"), height = 340))),
    bslib::card(bslib::card_header("Convergencia e falhas de ajuste"), DT::DTOutput(ns("t_conv"))),
    bslib::card(bslib::card_header("Medias observadas por nivel de x"), DT::DTOutput(ns("t_means")))
  )
  analysis_layout(ns, sidebar, analysis, diagnostics, mod_plot_editor_ui(ns("plot"), "600px"), mod_export_ui(ns("export")), "Regressao")
}

mod_regression_server <- function(id, sci_mode = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dat <- mod_data_server("data")
    output$vars <- shiny::renderUI({
      d <- dat$data(); shiny::req(d)
      numc <- col_choices(d, numeric_only = TRUE)
      gx <- intersect(c("dose", "x", "Tempo", "tempo", "N"), numc)[1] %||% numc[1]
      shiny::tagList(
        shiny::selectInput(ns("x"), "Variavel independente (x)", numc, selected = gx),
        shiny::selectInput(ns("y"), "Variavel-resposta (y)", numc, selected = utils::tail(setdiff(numc, gx), 1)),
        shiny::selectInput(ns("group"), "Agrupamento (curvas por grupo)", c("(nenhum)", names(d))),
        shiny::selectInput(ns("trials"), "Numero de ensaios (binomial)", c("(nenhum)", numc))
      )
    })
    conf <- shiny::reactive(if (identical(input$conf, "custom")) input$conf_custom else as.numeric(input$conf))
    res <- shiny::eventReactive(input$run, {
      d <- dat$data(); shiny::req(d, input$x, input$y, length(input$models) > 0)
      grp <- none_or(input$group); if (identical(grp, input$x) || identical(grp, input$y)) grp <- NULL
      out <- list(check = check_data(d, input$y, x = input$x, design = "regression"))
      shiny::withProgress(message = "Ajustando modelos...", {
        out$reg <- safe_run(fit_regression(d, input$x, input$y, model = input$models, group = grp, trials = none_or(input$trials),
                                           conf_level = conf(), band = input$band, band_method = input$band_method, nboot = input$nboot,
                                           seed = input$seed, aggregate = input$aggregate, exclude = dat$exclude()))
        if (!is.null(out$reg)) {
          at <- suppressWarnings(as.numeric(parse_list(gsub(",", ".", input$at))))
          at <- at[!is.na(at)]
          out$opt <- tryCatch(optimum_dose(out$reg, price_ratio = if (is.na(input$price)) NULL else input$price, at = if (length(at)) at),
                              error = function(e) NULL)
        }
      })
      out
    })
    reg <- shiny::reactive({ r <- res(); shiny::req(r$reg); r$reg })
    output$check <- shiny::renderUI(check_ui(res()$check))
    output$cards <- shiny::renderUI({
      r <- reg(); fs <- r$tables$comparison %||% r$tables$fit_stats
      b <- fs[1, ]
      stat_row(stat_card("Melhor modelo (AICc)", regression_models()$label[match(b$model, regression_models()$model)], if (!is.null(r$settings$group)) b$group),
               stat_card(if (b$r2_type == "classico") r2_lab() else paste0(r2_lab("pseudo-"), " (", b$r2_type, ")"), fmt_num(b$r2, 3)),
               if (!is.na(b$rmse)) stat_card("RMSE", formatC(b$rmse, format = "g", digits = 4)),
               stat_card("AIC", fmt_num(b$AIC, 2)), stat_card("Observacoes", b$n))
    })
    output$notes <- shiny::renderUI(notes_ui(reg(), !isTRUE(sci_mode())))
    output$t_stats <- DT::renderDT(dt_table(reg()$tables$comparison %||% reg()$tables$fit_stats))
    output$t_eq <- DT::renderDT(dt_table(reg()$tables$equations))
    output$t_coef <- DT::renderDT(dt_table(reg()$tables$coefficients))
    output$extra <- shiny::renderUI({
      r <- res()
      shiny::tagList(
        if (!is.null(r$reg$tables$curve_test)) bslib::card(bslib::card_header("Teste de igualdade de curvas entre grupos"), DT::DTOutput(ns("t_curve"))),
        if (!is.null(r$opt) && nrow(r$opt$tables$agronomic)) bslib::card(bslib::card_header("Grandezas agronomicas"), DT::DTOutput(ns("t_opt")), notes_ui(r$opt, FALSE))
      )
    })
    output$t_curve <- DT::renderDT(dt_table(res()$reg$tables$curve_test))
    output$t_opt <- DT::renderDT(dt_table(res()$opt$tables$agronomic))
    output$d_res <- shiny::renderPlot(plot(reg(), "residuals"), res = 96)
    output$d_qq <- shiny::renderPlot(plot(reg(), "qq"), res = 96)
    output$t_conv <- DT::renderDT({
      r <- reg(); fs <- r$tables$fit_stats[, c("model", "group", "converged", "iterations", "n", "k")]
      if (!is.null(r$tables$failures)) fs <- merge(fs, r$tables$failures, all = TRUE)
      dt_table(fs)
    })
    output$t_means <- DT::renderDT(dt_table(reg()$tables$means))
    ed <- mod_plot_editor_server("plot", reg)
    mod_export_server("export", shiny::reactive({ r <- res(); list(r$reg, r$opt) }), dat$name, ed$style)
  })
}
