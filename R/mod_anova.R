# Shiny module: parametric analysis (Fisher ANOVA for DIC, DBC and factorial) ---

analysis_layout <- function(ns, sidebar, analysis, diagnostics = NULL, graphics, export, title = NULL) {
  tabs <- list(
    bslib::nav_panel("Dados", value = "dados", icon = shiny::icon("table"), mod_data_tab_ui(ns("data"))),
    bslib::nav_panel("An\u00e1lise", value = "an\u00e1lise", icon = shiny::icon("calculator"), analysis)
  )
  if (!is.null(diagnostics)) tabs[[length(tabs) + 1]] <- bslib::nav_panel("Diagn\u00f3sticos", value = "diagn\u00f3sticos", icon = shiny::icon("stethoscope"), diagnostics)
  tabs[[length(tabs) + 1]] <- bslib::nav_panel("Gr\u00e1ficos", value = "gr\u00e1ficos", icon = shiny::icon("chart-column"), graphics)
  tabs[[length(tabs) + 1]] <- bslib::nav_panel("Exporta\u00e7\u00e3o", value = "exporta\u00e7\u00e3o", icon = shiny::icon("file-export"), export)
  bslib::layout_sidebar(
    fillable = FALSE, class = "caagro-layout",
    sidebar = bslib::sidebar(width = 300, title = title, open = sidebar_open(), sidebar),
    do.call(bslib::navset_card_underline, c(list(id = ns("tabs"), selected = "dados"), tabs))
  )
}

# Desktop: collapsible sidebar; mobile: controls stacked above the results
sidebar_open <- function() {
  if (utils::packageVersion("bslib") >= "0.7.0") list(desktop = "open", mobile = "always-above") else "always"
}

run_button <- function(ns) {
  # on small screens, scroll to the results after running
  js <- sprintf("if (window.innerWidth < 768) setTimeout(function(){ var e = document.getElementById('%s'); if (e) e.scrollIntoView({behavior: 'smooth'}); }, 900);", ns("tabs"))
  shiny::actionButton(ns("run"), "Executar an\u00e1lise", icon = shiny::icon("play"), class = "btn-primary w-100 mt-3", onclick = js)
}

alpha_choices <- c("5%" = "0.05", "1%" = "0.01", "10%" = "0.10")

mod_anova_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar <- shiny::tagList(
    step_title(1, "Dados"),
    mod_data_input_ui(ns("data"), "anova"),
    step_title(2, "Vari\u00e1veis"),
    shiny::uiOutput(ns("vars")),
    step_title(3, "Compara\u00e7\u00e3o de m\u00e9dias"),
    shiny::selectInput(ns("test"), NULL, c("Tukey" = "tukey", "Scott-Knott" = "scott_knott", "SNK (Student-Newman-Keuls)" = "snk",
                                          "LSD de Fisher (DMS)" = "lsd", "Dunnett (versus controle)" = "dunnett",
                                          "Bonferroni" = "bonferroni", "Holm" = "holm", "Nenhuma" = "none"), width = "100%"),
    shiny::selectInput(ns("alpha"), "N\u00edvel de signific\u00e2ncia", alpha_choices, width = "100%"),
    shiny::uiOutput(ns("test_opts")),
    details_panel("Op\u00e7\u00f5es avan\u00e7adas",
      shiny::selectInput(ns("kind"), "M\u00e9todo", c("ANOVA convencional (Fisher)" = "anova", "ANOVA de Welch (vari\u00e2ncias diferentes, DIC)" = "welch",
                                                 "Teste t (dois grupos)" = "ttest"), width = "100%"),
      shiny::conditionalPanel(sprintf("input['%s'] == 'anova'", ns("kind")),
        shiny::selectInput(ns("ss_type"), "Soma de quadrados", c("Tipo I - sequencial (padr\u00e3o)" = "I", "Tipo II" = "II", "Tipo III" = "III"), width = "100%"),
        shiny::p(class = "caagro-muted", "Com dados balanceados os tr\u00eas tipos coincidem.")),
      shiny::conditionalPanel(sprintf("input['%s'] == 'ttest'", ns("kind")),
        shiny::selectInput(ns("ttype"), "Tipo de teste t", c("Welch (vari\u00e2ncias diferentes)" = "welch", "Student (vari\u00e2ncias iguais)" = "independent", "Pareado" = "paired"), width = "100%"),
        shiny::uiOutput(ns("id_ui")))),
    run_button(ns)
  )
  analysis <- shiny::tagList(shiny::uiOutput(ns("check")), shiny::uiOutput(ns("results")))
  diagnostics <- shiny::uiOutput(ns("diag"))
  analysis_layout(ns, sidebar, analysis, diagnostics, mod_plot_editor_ui(ns("plot")), mod_export_ui(ns("export")), NULL)
}

mod_anova_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dat <- mod_data_server("data", "anova")
    shiny::observeEvent(dat$tick(), {
      ex <- dat$example()
      if (!is.null(ex)) { apply_example_inputs(session, c(ex$inputs, list(kind = "anova"))); bslib::nav_select("tabs", "dados") }
    }, ignoreInit = TRUE)
    output$vars <- shiny::renderUI({
      d <- dat$data()
      if (is.null(d)) return(shiny::p(class = "caagro-muted", "Carregue os dados primeiro."))
      pre <- dat$example()$vars %||% list()
      nm <- names(d); numc <- col_choices(d, numeric_only = TRUE)
      trt <- pick(pre, "treatment", guess_factor(d))
      shiny::tagList(
        shiny::selectInput(ns("response"), "Vari\u00e1vel-resposta", numc, selected = pick(pre, "response", utils::tail(setdiff(numc, trt), 1)), width = "100%"),
        shiny::selectInput(ns("treatment"), "Tratamentos", nm, selected = trt, width = "100%"),
        shiny::selectInput(ns("block"), "Blocos (se DBC)", c("(nenhum)", nm), selected = pick(pre, "block", "(nenhum)"), width = "100%"),
        shiny::selectizeInput(ns("factors"), "Outros fatores (se fatorial)", nm, selected = pick(pre, "factors", character()), multiple = TRUE, width = "100%",
                              options = list(placeholder = "(nenhum)"))
      )
    })
    shiny::outputOptions(output, "vars", suspendWhenHidden = FALSE)
    output$id_ui <- shiny::renderUI({
      d <- dat$data(); shiny::req(d)
      if (identical(input$ttype, "paired")) shiny::selectInput(ns("id"), "Coluna que identifica os pares", names(d), width = "100%")
    })
    output$test_opts <- shiny::renderUI({
      d <- dat$data(); shiny::req(d, input$treatment, input$treatment %in% names(d))
      tl <- unique(as.character(d[[input$treatment]]))
      shiny::tagList(
        if (identical(input$test, "dunnett")) shiny::selectInput(ns("control"), "Tratamento controle", tl, width = "100%"),
        if (identical(input$test, "lsd")) shiny::checkboxInput(ns("protected"), "Aplicar somente se o teste F for significativo (LSD protegido)", TRUE)
      )
    })
    shiny::outputOptions(output, "test_opts", suspendWhenHidden = FALSE)

    res_val <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$run, res_val((function() {
      d <- dat$data()
      if (is.null(d)) { shiny::showNotification("Carregue um exemplo ou importe uma planilha.", type = "warning"); return(NULL) }
      shiny::req(input$response, input$treatment)
      if (identical(input$response, input$treatment)) { shiny::showNotification("Resposta e tratamentos devem ser colunas diferentes.", type = "error"); return(NULL) }
      ex <- dat$exclude(); alpha <- as.numeric(input$alpha)
      kind <- input$kind %||% "anova"
      out <- list(kind = kind, alpha = alpha)
      blk <- none_or(input$block); fct <- input$factors
      if (!is.null(blk) && blk %in% c(input$treatment, input$response)) blk <- NULL
      fct <- setdiff(fct, c(input$treatment, input$response, blk))
      shiny::withProgress(message = "Calculando...", {
        if (kind == "anova") {
          out$check <- check_data(d, input$response, input$treatment, blk, design = if (length(fct)) "none" else if (is.null(blk)) "DIC" else "DBC")
          out$anova <- safe_run(anova_exp(d, input$response, input$treatment, block = blk, factors = if (length(fct)) fct else NULL,
                                          ss_type = input$ss_type %||% "I", conf_level = 1 - alpha, exclude = ex))
          if (!is.null(out$anova)) {
            out$diag <- safe_run(check_assumptions(out$anova, alpha = alpha))
            if (input$test != "none") {
              by <- NULL
              if (length(fct)) {
                at <- out$anova$tables$anova
                int <- at$term[grepl(":", at$term) & grepl(input$treatment, at$term, fixed = TRUE)]
                if (length(int) && any(at$p[at$term %in% int] < alpha, na.rm = TRUE)) { by <- fct[1]; out$sliced <- fct[1] }
              }
              out$mc <- safe_run(compare_means(out$anova, input$test, by = by, alpha = alpha, control = input$control,
                                               protected = isTRUE(input$protected %||% TRUE)))
            }
          }
        } else if (kind == "welch") {
          out$check <- check_data(d, input$response, input$treatment, design = "DIC")
          out$test <- safe_run(anova_exp(d, input$response, input$treatment, method = "welch", exclude = ex))
          a <- safe_run(anova_exp(d, input$response, input$treatment, exclude = ex))
          if (!is.null(a)) out$mc <- safe_run(compare_means(a, "games_howell", alpha = alpha))
        } else {
          out$check <- check_data(d, input$response, input$treatment)
          out$test <- safe_run(mean_test(d, input$response, input$treatment, type = input$ttype, id = if (identical(input$ttype, "paired")) input$id, conf_level = 1 - alpha))
        }
      })
      bslib::nav_select("tabs", "an\u00e1lise")
      out
    })()))
    res <- shiny::reactive(res_val())

    output$check <- shiny::renderUI({ r <- res(); if (is.null(r)) return(NULL); check_ui(r$check) })

    output$results <- shiny::renderUI({
      r <- res()
      if (is.null(r) || (is.null(r$anova) && is.null(r$test))) return(empty_state())
      if (!is.null(r$anova)) {
        a <- r$anova; at <- a$tables$anova; pr <- a$tables$precision; trt <- a$settings$treatment
        p <- at$p[at$term == trt]
        sig <- isTRUE(p < r$alpha)
        sentence <- if (sig) sprintf("Houve efeito significativo de %s pelo teste F (p %s).", trt, if (p < 0.001) "< 0,001" else paste("=", fmt_p_br(p))) else
          sprintf("N\u00e3o houve efeito significativo de %s pelo teste F (p = %s). Isso n\u00e3o comprova que os tratamentos sejam iguais.", trt, fmt_p_br(p))
        if (!is.null(r$sliced)) sentence <- paste(sentence, sprintf("A intera\u00e7\u00e3o foi significativa: as m\u00e9dias de %s s\u00e3o comparadas dentro de cada n\u00edvel de %s.", trt, r$sliced))
        mc_title <- if (!is.null(r$mc)) sprintf("Compara\u00e7\u00e3o de m\u00e9dias - %s (%s)", c(tukey = "Tukey", scott_knott = "Scott-Knott", snk = "SNK", lsd = "LSD de Fisher",
                                                                                   dunnett = "Dunnett", bonferroni = "Bonferroni", holm = "Holm")[[r$mc$settings$method]],
                                                     paste0(100 * r$alpha, "%")) else NULL
        shiny::tagList(
          summary_strip(list(F = fmt_br(at$F[at$term == trt], 2), `Valor-p` = fmt_p_br(p), `CV (%)` = fmt_br(pr$cv_percent, 2),
                             "M\u00e9dia geral" = fmt_br(pr$grand_mean, 2), "Observa\u00e7\u00f5es" = pr$n), sentence, if (sig) "good" else "default"),
          bslib::card(bslib::card_header("Quadro da an\u00e1lise de vari\u00e2ncia"),
                      dt_out(ns("t_anova")),
                      shiny::div(class = "caagro-table-foot",
                                 sprintf("%s | GL do res\u00edduo: %s | QM do res\u00edduo: %s | Soma de quadrados tipo %s%s",
                                         a$design, pr$df_error, fmt_br(pr$mse, 4), pr$ss_type, if (isTRUE(pr$balanced)) "" else " | dados desbalanceados"))),
          if (!is.null(r$mc)) bslib::card(bslib::card_header(mc_title),
                                          dt_out(ns("t_means")),
                                          shiny::uiOutput(ns("mc_foot")),
                                          details_panel("Compara\u00e7\u00f5es par a par e detalhes das m\u00e9dias", dt_out(ns("t_pairs")), dt_out(ns("t_means_full")))),
          bslib::card(bslib::card_header("Pressupostos da ANOVA"), dt_out(ns("t_assump")),
                      if (!is.null(r$diag$tables$tests)) shiny::tags$ul(class = "caagro-assump-list", lapply(r$diag$tables$tests$interpretation, shiny::tags$li)),
                      shiny::div(class = "caagro-table-foot", "Gr\u00e1ficos de res\u00edduos e testes alternativos na aba Diagn\u00f3sticos.")),
          warnings_ui(c(a$warnings, r$mc$warnings, r$diag$warnings)),
          notes_ui(a, collapsed = TRUE, show_warnings = FALSE)
        )
      } else {
        t <- r$test; tb <- t$tables$test
        sig <- isTRUE(tb$p < r$alpha)
        shiny::tagList(
          summary_strip(list("Estat\u00edstica" = fmt_br(tb$statistic, 3), GL = fmt_br(tb$df %||% tb$df2, 2), `Valor-p` = fmt_p_br(tb$p)),
                        t$notes$Resultado, if (sig) "good" else "default"),
          bslib::card(bslib::card_header(t$method), dt_out(ns("t_test"))),
          if (!is.null(t$tables$descriptives)) bslib::card(bslib::card_header("Estat\u00edsticas por grupo"), dt_out(ns("t_desc"))),
          if (!is.null(r$mc)) bslib::card(bslib::card_header(sprintf("Compara\u00e7\u00e3o de m\u00e9dias - Games-Howell (%s%%)", 100 * r$alpha)), dt_out(ns("t_means"))),
          notes_ui(t)
        )
      }
    })
    mc_simple <- shiny::reactive({
      r <- res(); shiny::req(r$mc)
      m <- r$mc$tables$means; f <- r$mc$settings$factor; by <- r$mc$settings$by
      cols <- c(by, f, "emmean", if (r$mc$settings$method == "dunnett") "vs_controle" else "letras")
      out <- m[, cols, drop = FALSE]
      names(out)[names(out) == f] <- "Tratamento"
      out
    })
    output$t_anova <- DT::renderDT({ r <- res(); shiny::req(r$anova); dt_table(r$anova$tables$anova, digits = 4, drop = c("signif", "partial_eta_sq")) })
    output$t_means <- DT::renderDT(dt_table(mc_simple(), digits = 2, page = 30))
    output$mc_foot <- shiny::renderUI({
      r <- res(); shiny::req(r$mc)
      txt <- if (r$mc$settings$method == "dunnett") "Cada tratamento \u00e9 comparado apenas com o controle." else "M\u00e9dias seguidas pela mesma letra n\u00e3o diferem entre si."
      if (!is.null(r$mc$tables$dms) && !is.na(r$mc$tables$dms$dms)) txt <- paste0(txt, " DMS = ", fmt_br(r$mc$tables$dms$dms, 3), ".")
      shiny::div(class = "caagro-table-foot", txt)
    })
    output$t_pairs <- DT::renderDT({ r <- res(); shiny::req(r$mc); dt_table(r$mc$tables$pairs, drop = c("level1", "level2", "range_p")) })
    output$t_means_full <- DT::renderDT({ r <- res(); shiny::req(r$mc); dt_table(r$mc$tables$means) })
    output$t_assump <- DT::renderDT({ r <- res(); shiny::req(r$diag); dt_table(r$diag$tables$tests, drop = c("assumption", "interpretation"), digits = 4) })
    output$t_test <- DT::renderDT({ r <- res(); shiny::req(r$test); dt_table(r$test$tables$test) })
    output$t_desc <- DT::renderDT({ r <- res(); shiny::req(r$test); dt_table(r$test$tables$descriptives) })

    output$diag <- shiny::renderUI({
      r <- res()
      if (is.null(r) || is.null(r$diag)) return(empty_state("Os diagn\u00f3sticos aparecem ap\u00f3s executar uma ANOVA."))
      shiny::tagList(
        bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = 6),
          bslib::card(bslib::card_header("Gr\u00e1fico Q-Q dos res\u00edduos"), shiny::plotOutput(ns("d_qq"), height = 320)),
          bslib::card(bslib::card_header("Res\u00edduos versus valores ajustados"), shiny::plotOutput(ns("d_rf"), height = 320))),
        details_panel("Testes robustos de homogeneidade (alternativas ao Bartlett)", dt_out(ns("d_robust"))),
        details_panel("Observa\u00e7\u00f5es potencialmente influentes", dt_out(ns("d_infl"))),
        details_panel("Outros gr\u00e1ficos", bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = 6),
          shiny::plotOutput(ns("d_sl"), height = 300), shiny::plotOutput(ns("d_ck"), height = 300))),
        shiny::p(class = "caagro-muted", "A independ\u00eancia dos erros depende da casualiza\u00e7\u00e3o e do planejamento; nenhum teste a garante.")
      )
    })
    diag <- shiny::reactive({ r <- res(); shiny::req(r$diag); r$diag })
    nt <- caagro_style(show_title = FALSE)
    output$d_qq <- shiny::renderPlot(plot(diag(), "qq", style = nt), res = 96, bg = "transparent")
    output$d_rf <- shiny::renderPlot(plot(diag(), "resid_fitted", style = nt), res = 96, bg = "transparent")
    output$d_sl <- shiny::renderPlot(plot(diag(), "scale_location", style = nt), res = 96, bg = "transparent")
    output$d_ck <- shiny::renderPlot(plot(diag(), "cooks", style = nt), res = 96, bg = "transparent")
    output$d_robust <- DT::renderDT(dt_table(diag()$tables$robust))
    output$d_infl <- DT::renderDT({ i <- diag()$tables$influence; dt_table(i[i$flag, , drop = FALSE], drop = "flag") })
    plot_res <- shiny::reactive({ r <- res(); if (is.null(r)) return(NULL); r$mc %||% r$test %||% r$anova })
    ed <- mod_plot_editor_server("plot", plot_res)
    mod_export_server("export", shiny::reactive({ r <- res(); if (is.null(r)) list() else list(r$anova, r$mc, r$diag, r$test) }), dat$name, ed$style)
  })
}
