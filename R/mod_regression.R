# Shiny module: regression --------------------------------------------------

reg_model_choices <- function() {
  rm <- regression_models()
  stats::setNames(rm$model, rm$label)
}

mod_regression_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar <- shiny::tagList(
    step_title(1, "Dados"),
    mod_data_input_ui(ns("data"), "regression"),
    step_title(2, "Vari\u00e1veis"),
    shiny::uiOutput(ns("vars")),
    step_title(3, "Modelos"),
    shiny::selectizeInput(ns("models"), NULL, reg_model_choices(), selected = c("linear", "quadratic"), multiple = TRUE, width = "100%",
                          options = list(plugins = list("remove_button"))),
    details_panel("Op\u00e7\u00f5es avan\u00e7adas",
      shiny::selectInput(ns("band"), "Faixa no gr\u00e1fico", c("Intervalo de confian\u00e7a da m\u00e9dia" = "confidence", "Intervalo de predi\u00e7\u00e3o" = "prediction", "Nenhuma" = "none"), width = "100%"),
      shiny::selectInput(ns("conf"), "N\u00edvel de confian\u00e7a", c("95%" = "0.95", "90%" = "0.90", "99%" = "0.99"), width = "100%"),
      shiny::selectInput(ns("band_method"), "Faixa em modelos n\u00e3o lineares", c("M\u00e9todo delta" = "delta", "Bootstrap de res\u00edduos" = "bootstrap"), width = "100%"),
      shiny::numericInput(ns("nboot"), "Reamostragens (bootstrap)", 500, 100, 5000, 100, width = "100%"),
      shiny::numericInput(ns("seed"), "Semente", 123, width = "100%"),
      shiny::selectInput(ns("aggregate"), "Ajustar sobre", c("Observa\u00e7\u00f5es individuais (recomendado)" = "none", "M\u00e9dias por dose (vers\u00e3o antiga)" = "mean"), width = "100%"),
      shiny::numericInput(ns("price"), "Pre\u00e7o do insumo / pre\u00e7o do produto (dose econ\u00f4mica)", NA, 0, width = "100%")),
    run_button(ns)
  )
  analysis <- shiny::tagList(shiny::uiOutput(ns("check")), shiny::uiOutput(ns("results")))
  diagnostics <- shiny::uiOutput(ns("diag"))
  analysis_layout(ns, sidebar, analysis, diagnostics, mod_plot_editor_ui(ns("plot"), "580px"), mod_export_ui(ns("export")))
}

mod_regression_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dat <- mod_data_server("data", "regression")
    shiny::observeEvent(dat$tick(), {
      ex <- dat$example()
      if (!is.null(ex)) { apply_example_inputs(session, ex$inputs); bslib::nav_select("tabs", "dados") }
    }, ignoreInit = TRUE)
    output$vars <- shiny::renderUI({
      d <- dat$data()
      if (is.null(d)) return(shiny::p(class = "caagro-muted", "Carregue os dados primeiro."))
      pre <- dat$example()$vars %||% list()
      numc <- col_choices(d, numeric_only = TRUE)
      gx <- pick(pre, "x", intersect(c("dose", "x", "Dose", "Tempo", "tempo"), numc)[1] %||% numc[1])
      shiny::tagList(
        shiny::selectInput(ns("x"), "Vari\u00e1vel independente (x)", numc, selected = gx, width = "100%"),
        shiny::selectInput(ns("y"), "Vari\u00e1vel dependente (y)", numc, selected = pick(pre, "y", utils::tail(setdiff(numc, gx), 1)), width = "100%"),
        shiny::selectInput(ns("group"), "Uma curva por grupo (opcional)", c("(nenhum)", names(d)), selected = pick(pre, "group", "(nenhum)"), width = "100%"),
        shiny::selectInput(ns("trials"), "Total de ensaios (s\u00f3 binomial)", c("(nenhum)", numc), selected = pick(pre, "trials", "(nenhum)"), width = "100%")
      )
    })
    shiny::outputOptions(output, "vars", suspendWhenHidden = FALSE)
    res_val <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$run, res_val((function() {
      d <- dat$data()
      if (is.null(d)) { shiny::showNotification("Carregue um exemplo ou importe uma planilha.", type = "warning"); return(NULL) }
      shiny::req(input$x, input$y)
      if (!length(input$models)) { shiny::showNotification("Escolha ao menos um modelo.", type = "warning"); return(NULL) }
      grp <- none_or(input$group); if (identical(grp, input$x) || identical(grp, input$y)) grp <- NULL
      out <- list(check = check_data(d, input$y, x = input$x, design = "regression"))
      shiny::withProgress(message = "Ajustando modelos...", {
        out$reg <- safe_run(fit_regression(d, input$x, input$y, model = input$models, group = grp, trials = none_or(input$trials),
                                           conf_level = as.numeric(input$conf %||% "0.95"), band = input$band %||% "confidence",
                                           band_method = input$band_method %||% "delta", nboot = input$nboot %||% 500,
                                           seed = input$seed %||% 123, aggregate = input$aggregate %||% "none", exclude = dat$exclude()))
        if (!is.null(out$reg)) {
          out$opt <- tryCatch(optimum_dose(out$reg, model = out$reg$tables$comparison$model[1] %||% NULL,
                                           price_ratio = if (is.null(input$price) || is.na(input$price)) NULL else input$price),
                              error = function(e) NULL)
        }
      })
      bslib::nav_select("tabs", "an\u00e1lise")
      out
    })()))
    res <- shiny::reactive(res_val())
    reg <- shiny::reactive({ r <- res(); shiny::req(r$reg); r$reg })
    output$check <- shiny::renderUI({ r <- res(); if (is.null(r)) return(NULL); check_ui(r$check) })
    output$results <- shiny::renderUI({
      r <- res()
      if (is.null(r) || is.null(r$reg)) return(empty_state())
      g <- r$reg
      fs <- g$tables$comparison %||% g$tables$fit_stats
      b <- fs[1, ]
      lab <- names(reg_model_choices())[match(b$model, reg_model_choices())]
      r2lab <- if (b$r2_type == "cl\u00e1ssico") r2_lab() else paste0(r2_lab("pseudo-"), if (b$r2_type == "McFadden") " (McFadden)" else "")
      sentence <- if (length(g$settings$model) > 1) sprintf("Melhor compromisso entre ajuste e simplicidade (menor AICc): %s%s.", lab,
                                                             if (!is.null(g$settings$group)) paste0(" em ", b$group) else "") else
        sprintf("Modelo ajustado: %s.", lab)
      shiny::tagList(
        summary_strip(stats::setNames(list(lab, fmt_br(b$r2, 3), if (is.na(b$rmse)) NULL else fmt_br(b$rmse, 3), fmt_br(b$AIC, 1), b$n),
                                      c("Modelo", r2lab, "RMSE", "AIC", "Observa\u00e7\u00f5es")), sentence),
        bslib::card(bslib::card_header("Equa\u00e7\u00f5es ajustadas"), dt_out(ns("t_eq"))),
        bslib::card(bslib::card_header(if (length(g$settings$model) > 1) "Compara\u00e7\u00e3o de modelos" else "Indicadores de ajuste"), dt_out(ns("t_stats")),
                    shiny::div(class = "caagro-table-foot", "Menor AICc indica melhor equil\u00edbrio entre ajuste e n\u00famero de par\u00e2metros. N\u00e3o escolha o modelo apenas pelo maior R\u00b2.")),
        if (!is.null(g$tables$curve_test)) bslib::card(bslib::card_header("As curvas dos grupos diferem?"), dt_out(ns("t_curve"))),
        if (!is.null(r$opt) && nrow(r$opt$tables$agronomic)) bslib::card(bslib::card_header("Grandezas agron\u00f4micas"), dt_out(ns("t_opt"))),
        details_panel("Coeficientes com erro-padr\u00e3o e intervalos", dt_out(ns("t_coef"))),
        warnings_ui(c(g$warnings, r$opt$warnings)),
        notes_ui(g, show_warnings = FALSE)
      )
    })
    output$t_stats <- DT::renderDT({
      g <- reg(); fs <- g$tables$comparison %||% g$tables$fit_stats
      keep <- intersect(c("model", "group", "n", "r2", "r2_type", "r2_adj", "rmse", "AICc", "delta_AICc"), names(fs))
      fs <- fs[, keep, drop = FALSE]
      if (is.null(g$settings$group)) fs$group <- NULL
      dt_table(fs)
    })
    output$t_eq <- DT::renderDT({ g <- reg(); e <- g$tables$equations; if (is.null(g$settings$group)) e$group <- NULL; dt_table(e) })
    output$t_coef <- DT::renderDT(dt_table(reg()$tables$coefficients))
    output$t_curve <- DT::renderDT(dt_table(reg()$tables$curve_test))
    output$t_opt <- DT::renderDT(dt_table(res()$opt$tables$agronomic))
    output$diag <- shiny::renderUI({
      r <- res()
      if (is.null(r) || is.null(r$reg)) return(empty_state("Os diagn\u00f3sticos aparecem ap\u00f3s ajustar um modelo."))
      shiny::tagList(
        bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = 6),
          bslib::card(bslib::card_header("Res\u00edduos versus valores ajustados"), shiny::plotOutput(ns("d_res"), height = 320)),
          bslib::card(bslib::card_header("Gr\u00e1fico Q-Q dos res\u00edduos"), shiny::plotOutput(ns("d_qq"), height = 320))),
        details_panel("Converg\u00eancia dos ajustes", dt_out(ns("t_conv"))),
        details_panel("M\u00e9dias observadas por n\u00edvel de x", dt_out(ns("t_means"))))
    })
    nt <- caagro_style(show_title = FALSE)
    output$d_res <- shiny::renderPlot(plot(reg(), "residuals", style = nt), res = 96)
    output$d_qq <- shiny::renderPlot(plot(reg(), "qq", style = nt), res = 96)
    output$t_conv <- DT::renderDT({
      g <- reg(); fs <- g$tables$fit_stats[, c("model", "group", "converged", "iterations", "n", "k")]
      if (!is.null(g$tables$failures)) fs <- merge(fs, g$tables$failures, all = TRUE)
      dt_table(fs)
    })
    output$t_means <- DT::renderDT(dt_table(reg()$tables$means))
    ed <- mod_plot_editor_server("plot", shiny::reactive({ r <- res(); r$reg }))
    mod_export_server("export", shiny::reactive({ r <- res(); if (is.null(r)) list() else list(r$reg, r$opt) }), dat$name, ed$style)
  })
}
