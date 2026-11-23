# Shiny module: parametric analyses (ANOVA DIC/DBC/fatorial, t, Welch) ----------

analysis_layout <- function(ns, sidebar, analysis, diagnostics = NULL, graphics, export, title = NULL) {
  tabs <- list(
    bslib::nav_panel("Dados", icon = shiny::icon("table"), mod_data_tab_ui(ns("data"))),
    bslib::nav_panel("Analise", icon = shiny::icon("calculator"), analysis)
  )
  if (!is.null(diagnostics)) tabs[[length(tabs) + 1]] <- bslib::nav_panel("Diagnosticos", icon = shiny::icon("stethoscope"), diagnostics)
  tabs[[length(tabs) + 1]] <- bslib::nav_panel("Graficos", icon = shiny::icon("chart-column"), graphics)
  tabs[[length(tabs) + 1]] <- bslib::nav_panel("Exportacao", icon = shiny::icon("file-export"), export)
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(width = 320, title = title, open = "desktop", sidebar),
    do.call(bslib::navset_card_underline, c(list(id = ns("tabs")), tabs))
  )
}

run_button <- function(ns) shiny::actionButton(ns("run"), "Executar analise", icon = shiny::icon("play"), class = "btn-primary w-100 mt-2")

mod_anova_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar <- shiny::tagList(
    mod_data_input_ui(ns("data")),
    shiny::selectInput(ns("kind"), "Analise", c("ANOVA (DIC, DBC, fatorial)" = "anova", "Teste t (2 grupos)" = "ttest", "ANOVA de Welch (variancias desiguais)" = "welch")),
    shiny::uiOutput(ns("vars")),
    shiny::conditionalPanel(sprintf("input['%s'] == 'anova'", ns("kind")),
      shiny::selectInput(ns("test"), "Comparacao de medias", c("Tukey (HSD / Tukey-Kramer)" = "tukey", "Scott-Knott" = "scott_knott",
                                                              "SNK (Student-Newman-Keuls)" = "snk", "LSD de Fisher" = "lsd", "Dunnett (vs controle)" = "dunnett",
                                                              "t pareado - Bonferroni" = "bonferroni", "t pareado - Holm" = "holm", "Games-Howell (DIC)" = "games_howell",
                                                              "Nenhuma" = "none")),
      shiny::uiOutput(ns("test_opts")),
      shiny::selectInput(ns("ss_type"), "Soma de quadrados", c("Tipo II (padrao)" = "II", "Tipo I (sequencial)" = "I", "Tipo III" = "III"))),
    shiny::conditionalPanel(sprintf("input['%s'] == 'ttest'", ns("kind")),
      shiny::selectInput(ns("ttype"), "Tipo de teste t", c("Welch (variancias desiguais)" = "welch", "Student (variancias iguais)" = "independent", "Pareado" = "paired"))),
    shiny::numericInput(ns("alpha"), "Nivel de significancia (alfa)", 0.05, 0.001, 0.2, 0.01),
    run_button(ns)
  )
  analysis <- shiny::tagList(shiny::uiOutput(ns("check")), shiny::uiOutput(ns("cards")), shiny::uiOutput(ns("notes")),
                             shiny::uiOutput(ns("tables")))
  diagnostics <- shiny::tagList(
    shiny::uiOutput(ns("diag_notes")),
    bslib::layout_columns(col_widths = c(6, 6, 6, 6),
                          bslib::card(shiny::plotOutput(ns("d_qq"), height = 300)), bslib::card(shiny::plotOutput(ns("d_rf"), height = 300)),
                          bslib::card(shiny::plotOutput(ns("d_sl"), height = 300)), bslib::card(shiny::plotOutput(ns("d_ck"), height = 300))),
    bslib::card(bslib::card_header("Testes de pressupostos"), DT::DTOutput(ns("d_tests"))),
    bslib::card(bslib::card_header("Observacoes potencialmente influentes (nada foi removido)"), DT::DTOutput(ns("d_infl")))
  )
  analysis_layout(ns, sidebar, analysis, diagnostics, mod_plot_editor_ui(ns("plot")), mod_export_ui(ns("export")), "Analise parametrica")
}

mod_anova_server <- function(id, sci_mode = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dat <- mod_data_server("data")
    output$vars <- shiny::renderUI({
      d <- dat$data(); shiny::req(d)
      nm <- names(d); numc <- col_choices(d, numeric_only = TRUE)
      fac <- nm
      guess_trt <- intersect(c("tratamento", "trat", "group", "cultivar", "Trat", "N", "tension"), nm)[1] %||% nm[1]
      guess_blk <- intersect(c("bloco", "block", "Bloco", "rep", "Rep"), nm)[1]
      shiny::tagList(
        shiny::selectInput(ns("response"), "Variavel-resposta", numc, selected = setdiff(numc, guess_trt)[length(setdiff(numc, guess_trt))]),
        shiny::selectInput(ns("treatment"), "Tratamento (fator)", fac, selected = guess_trt),
        shiny::conditionalPanel(sprintf("input['%s'] == 'anova'", ns("kind")),
          shiny::selectInput(ns("block"), "Bloco (DBC)", c("(nenhum)", fac), selected = if (!is.na(guess_blk %||% NA)) guess_blk else "(nenhum)"),
          shiny::selectizeInput(ns("factors"), "Fatores adicionais (fatorial)", fac, multiple = TRUE)),
        shiny::conditionalPanel(sprintf("input['%s'] == 'ttest' && input['%s'] == 'paired'", ns("kind"), ns("ttype")),
          shiny::selectInput(ns("id"), "Identificador do par", fac))
      )
    })
    output$test_opts <- shiny::renderUI({
      d <- dat$data(); shiny::req(d, input$treatment)
      tl <- unique(as.character(d[[input$treatment]]))
      shiny::tagList(
        if (identical(input$test, "dunnett")) shiny::selectInput(ns("control"), "Controle", tl),
        if (identical(input$test, "lsd")) shiny::tagList(shiny::checkboxInput(ns("protected"), "LSD protegido (exige F significativo)", TRUE),
                                                         shiny::selectInput(ns("p_adjust"), "Ajuste de p", c("nenhum" = "none", "holm", "bonferroni"))),
        if (length(input$factors)) shiny::selectInput(ns("by"), "Comparar dentro de (efeitos simples)", c("(medias marginais)", input$factors))
      )
    })
    res <- shiny::eventReactive(input$run, {
      d <- dat$data(); shiny::req(d, input$response, input$treatment)
      ex <- dat$exclude()
      out <- list()
      shiny::withProgress(message = "Calculando...", {
        if (input$kind == "anova") {
          blk <- none_or(input$block); fct <- input$factors
          if (!is.null(blk) && blk %in% c(input$treatment, input$response)) blk <- NULL
          fct <- setdiff(fct, c(input$treatment, input$response, blk))
          out$check <- check_data(d, input$response, input$treatment, blk, design = if (length(fct)) "none" else if (is.null(blk)) "DIC" else "DBC")
          out$anova <- safe_run(anova_exp(d, input$response, input$treatment, block = blk, factors = if (length(fct)) fct else NULL,
                                          ss_type = input$ss_type, conf_level = 1 - input$alpha, exclude = ex))
          if (!is.null(out$anova)) {
            out$diag <- safe_run(check_assumptions(out$anova, alpha = input$alpha))
            if (input$test != "none") {
              out$mc <- safe_run(compare_means(out$anova, input$test, by = none_or(if (identical(input$by, "(medias marginais)")) NULL else input$by),
                                               alpha = input$alpha, control = input$control, protected = isTRUE(input$protected %||% TRUE),
                                               p_adjust = input$p_adjust %||% "none"))
            }
          }
        } else if (input$kind == "ttest") {
          out$check <- check_data(d, input$response, input$treatment)
          out$test <- safe_run(mean_test(d, input$response, input$treatment, type = input$ttype, id = if (input$ttype == "paired") input$id, conf_level = 1 - input$alpha))
        } else {
          out$check <- check_data(d, input$response, input$treatment, design = "DIC")
          out$test <- safe_run(welch_anova(d, input$response, input$treatment, exclude = ex))
          a <- safe_run(anova_exp(d, input$response, input$treatment, exclude = ex))
          if (!is.null(a)) out$mc <- safe_run(compare_means(a, "games_howell", alpha = input$alpha))
        }
      })
      out
    })
    output$check <- shiny::renderUI({ r <- res(); check_ui(r$check) })
    output$cards <- shiny::renderUI({
      r <- res()
      if (!is.null(r$anova)) {
        a <- r$anova$tables$anova; pr <- r$anova$tables$precision
        trt <- r$anova$settings$treatment
        p <- a$p[a$term == trt]
        stat_row(stat_card("F (tratamentos)", fmt_num(a$F[a$term == trt], 3), paste0("gl = ", a$df[a$term == trt], "; ", pr$df_error)),
                 stat_card("Valor de p", fmt_p(p), if (isTRUE(p < input$alpha)) "significativo" else "nao significativo", if (isTRUE(p < input$alpha)) "good" else "default"),
                 stat_card("CV experimental", paste0(fmt_num(pr$cv_percent, 2), "%")),
                 stat_card("QM do residuo", formatC(pr$mse, format = "g", digits = 4)),
                 stat_card(paste(r2_lab(), "do modelo"), fmt_num(pr$r_squared, 3)),
                 stat_card("Observacoes", pr$n, r$anova$design))
      } else if (!is.null(r$test)) {
        t <- r$test$tables$test
        stat_row(stat_card("Estatistica", fmt_num(t$statistic, 3)),
                 stat_card("Graus de liberdade", fmt_num(t$df %||% t$df2, 2)),
                 stat_card("Valor de p", fmt_p(t$p), tone = if (isTRUE(t$p < input$alpha)) "good" else "default"),
                 if (!is.null(t$hedges_g)) stat_card("g de Hedges", fmt_num(t$hedges_g, 3)))
      }
    })
    output$notes <- shiny::renderUI({
      r <- res()
      shiny::tagList(notes_ui(r$anova %||% r$test, !isTRUE(sci_mode())), if (!is.null(r$mc)) notes_ui(r$mc, FALSE))
    })
    output$tables <- shiny::renderUI({
      r <- res()
      cards <- list()
      add <- function(title, id) cards[[length(cards) + 1]] <<- bslib::card(bslib::card_header(title), DT::DTOutput(ns(id)))
      if (!is.null(r$anova)) { add("Quadro da analise de variancia", "t_anova"); add("Precisao experimental", "t_prec") }
      if (!is.null(r$mc)) { add(r$mc$method, "t_means"); add("Comparacoes par a par", "t_pairs") }
      else if (!is.null(r$anova)) add("Medias por tratamento", "t_means")
      if (!is.null(r$anova)) add("Tamanho de efeito", "t_eff")
      if (!is.null(r$test)) { add(r$test$method, "t_test"); if (!is.null(r$test$tables$descriptives)) add("Estatisticas por grupo", "t_desc") }
      shiny::tagList(cards)
    })
    output$t_anova <- DT::renderDT(dt_table(res()$anova$tables$anova))
    output$t_prec <- DT::renderDT(dt_table(res()$anova$tables$precision))
    output$t_means <- DT::renderDT({ r <- res(); dt_table(if (!is.null(r$mc)) r$mc$tables$means else r$anova$tables$means) })
    output$t_pairs <- DT::renderDT(dt_table(res()$mc$tables$pairs))
    output$t_eff <- DT::renderDT(dt_table(res()$anova$tables$effect_size))
    output$t_test <- DT::renderDT(dt_table(res()$test$tables$test))
    output$t_desc <- DT::renderDT(dt_table(res()$test$tables$descriptives))
    diag <- shiny::reactive({ r <- res(); shiny::req(r$diag); r$diag })
    output$diag_notes <- shiny::renderUI(notes_ui(diag(), !isTRUE(sci_mode())))
    output$d_qq <- shiny::renderPlot(plot(diag(), "qq"), res = 96)
    output$d_rf <- shiny::renderPlot(plot(diag(), "resid_fitted"), res = 96)
    output$d_sl <- shiny::renderPlot(plot(diag(), "scale_location"), res = 96)
    output$d_ck <- shiny::renderPlot(plot(diag(), "cooks"), res = 96)
    output$d_tests <- DT::renderDT(dt_table(diag()$tables$tests))
    output$d_infl <- DT::renderDT({ i <- diag()$tables$influence; dt_table(i[i$flag, , drop = FALSE]) })
    plot_res <- shiny::reactive({ r <- res(); r$mc %||% r$test %||% r$anova })
    ed <- mod_plot_editor_server("plot", plot_res)
    mod_export_server("export", shiny::reactive({ r <- res(); list(r$anova, r$mc, r$diag, r$test) }), dat$name, ed$style)
  })
}
