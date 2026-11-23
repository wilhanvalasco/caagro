# Shiny modules: non-parametric, confidence intervals, multivariate, exploratory

result_tables_ui <- function(ns, x, main = NULL, hide = character()) {
  tn <- setdiff(names(x$tables), hide)
  main <- intersect(main %||% tn[1], tn)
  rest <- setdiff(tn, main)
  shiny::tagList(
    lapply(main, function(t) bslib::card(bslib::card_header(tlabel(t)), dt_out(ns(paste0("t_", t))))),
    if (length(rest)) details_panel("Outras tabelas", lapply(rest, function(t) shiny::tagList(shiny::h6(tlabel(t)), dt_out(ns(paste0("t_", t))))))
  )
}

bind_result_tables <- function(output, x, hide = character()) {
  for (tn in setdiff(names(x$tables), hide)) local({
    t2 <- tn
    output[[paste0("t_", t2)]] <- DT::renderDT(dt_table(x$tables[[t2]], page = 10))
  })
}

# Non-parametric ---------------------------------------------------------------
mod_nonparametric_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar <- shiny::tagList(
    step_title(1, "Dados"),
    mod_data_input_ui(ns("data"), "nonparametric"),
    step_title(2, "Teste"),
    shiny::selectInput(ns("test"), NULL, c("Kruskal-Wallis (grupos independentes)" = "kruskal", "Friedman (blocos)" = "friedman",
                                          "Teste da mediana de Mood" = "mood", "Mann-Whitney (2 grupos)" = "mann_whitney",
                                          "Wilcoxon pareado" = "wilcoxon_paired", "Teste dos sinais" = "sign", "Permuta\u00e7\u00e3o" = "permutation"), width = "100%"),
    step_title(3, "Vari\u00e1veis"),
    shiny::uiOutput(ns("vars")),
    details_panel("Op\u00e7\u00f5es avan\u00e7adas",
      shiny::selectInput(ns("posthoc"), "Compara\u00e7\u00f5es (Kruskal-Wallis/Friedman)", c("Dunn" = "dunn", "Conover" = "conover", "Wilcoxon par a par" = "wilcoxon", "Nenhuma" = "none"), width = "100%"),
      shiny::selectInput(ns("p_adjust"), "Corre\u00e7\u00e3o para m\u00faltiplas compara\u00e7\u00f5es", c("Holm" = "holm", "Bonferroni" = "bonferroni", "Benjamini-Hochberg" = "BH", "Nenhuma" = "none"), width = "100%"),
      shiny::selectInput(ns("alpha"), "N\u00edvel de signific\u00e2ncia", alpha_choices, width = "100%"),
      shiny::numericInput(ns("R"), "Permuta\u00e7\u00f5es", 4999, 999, 1e5, 1000, width = "100%"),
      shiny::numericInput(ns("seed"), "Semente", 123, width = "100%")),
    run_button(ns)
  )
  analysis <- shiny::uiOutput(ns("results"))
  analysis_layout(ns, sidebar, analysis, NULL, mod_plot_editor_ui(ns("plot")), mod_export_ui(ns("export")))
}

mod_nonparametric_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dat <- mod_data_server("data", "nonparametric")
    shiny::observeEvent(dat$tick(), {
      ex <- dat$example()
      if (!is.null(ex)) { apply_example_inputs(session, ex$inputs); bslib::nav_select("tabs", "dados") }
    }, ignoreInit = TRUE)
    output$vars <- shiny::renderUI({
      d <- dat$data()
      if (is.null(d)) return(shiny::p(class = "caagro-muted", "Carregue os dados primeiro."))
      pre <- dat$example()$vars %||% list()
      numc <- col_choices(d, numeric_only = TRUE)
      g <- pick(pre, "group", guess_factor(d))
      shiny::tagList(
        shiny::selectInput(ns("response"), "Vari\u00e1vel-resposta", numc, selected = pick(pre, "response", utils::tail(setdiff(numc, g), 1)), width = "100%"),
        shiny::selectInput(ns("group"), "Tratamentos", names(d), selected = g, width = "100%"),
        if (identical(input$test, "friedman")) shiny::selectInput(ns("block"), "Blocos", names(d), selected = guess_factor(d, g), width = "100%"),
        if (input$test %in% c("wilcoxon_paired", "sign")) shiny::selectInput(ns("id"), "Coluna que identifica os pares", names(d), width = "100%")
      )
    })
    shiny::outputOptions(output, "vars", suspendWhenHidden = FALSE)
    res_val <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$run, res_val((function() {
      d <- dat$data()
      if (is.null(d)) { shiny::showNotification("Carregue um exemplo ou importe uma planilha.", type = "warning"); return(NULL) }
      shiny::req(input$response, input$group)
      if (identical(input$response, input$group)) { shiny::showNotification("Resposta e tratamentos devem ser colunas diferentes.", type = "error"); return(NULL) }
      t <- safe_run(np_test(d, input$response, input$group, input$test, block = if (input$test == "friedman") input$block,
                            id = if (input$test %in% c("wilcoxon_paired", "sign")) input$id, R = input$R %||% 4999,
                            seed = input$seed %||% 123, exclude = dat$exclude()))
      if (!is.null(t) && input$test %in% c("kruskal", "friedman") && input$posthoc != "none") {
        ph <- if (input$test == "friedman" && input$posthoc == "dunn") "conover" else input$posthoc
        t <- safe_run(np_posthoc(t, ph, p_adjust = input$p_adjust, alpha = as.numeric(input$alpha))) %||% t
      }
      bslib::nav_select("tabs", "an\u00e1lise")
      t
    })()))
    res <- shiny::reactive(res_val())
    output$results <- shiny::renderUI({
      x <- res()
      if (is.null(x)) return(empty_state())
      tb <- x$tables$test
      alpha <- as.numeric(input$alpha)
      sig <- isTRUE(tb$p < alpha)
      eff <- if (!is.null(tb$epsilon_sq)) list("\u00c9psilon\u00b2 (efeito)" = fmt_br(tb$epsilon_sq, 3)) else if (!is.null(tb$kendall_w)) list(`W de Kendall` = fmt_br(tb$kendall_w, 3)) else list()
      sentence <- if (sig) "H\u00e1 evid\u00eancia de diferen\u00e7a entre ao menos dois tratamentos." else "N\u00e3o houve evid\u00eancia de diferen\u00e7a entre os tratamentos. Isso n\u00e3o comprova igualdade."
      shiny::tagList(
        summary_strip(c(list("Estat\u00edstica" = fmt_br(tb$statistic, 3), `Valor-p` = fmt_p_br(tb$p)), eff), paste(tb$test, "-", sentence), if (sig) "good" else "default"),
        bslib::card(bslib::card_header("Medianas e postos por tratamento"), dt_out(ns("t_desc_simple")),
                    if (!is.null(x$tables$pairs)) shiny::div(class = "caagro-table-foot", "Tratamentos seguidos pela mesma letra n\u00e3o diferem entre si (valores-p ajustados).")),
        if (!is.null(x$tables$pairs)) details_panel("Compara\u00e7\u00f5es par a par", dt_out(ns("t_pairs"))),
        details_panel("Resultado completo do teste", dt_out(ns("t_test")), if (!is.null(x$tables$contingency)) dt_out(ns("t_cont"))),
        warnings_ui(x$warnings),
        notes_ui(x, show_warnings = FALSE)
      )
    })
    output$t_desc_simple <- DT::renderDT({
      x <- res(); shiny::req(x); d <- x$tables$descriptives
      keep <- intersect(c(x$settings$group, "n", "median", "q1", "q3", "mean_rank", "letras"), names(d))
      dt_table(d[order(-d$mean_rank), keep, drop = FALSE], digits = 2)
    })
    output$t_test <- DT::renderDT({ x <- res(); shiny::req(x); dt_table(x$tables$test) })
    output$t_pairs <- DT::renderDT({ x <- res(); shiny::req(x$tables$pairs); dt_table(x$tables$pairs, drop = c("level1", "level2")) })
    output$t_cont <- DT::renderDT({ x <- res(); shiny::req(x$tables$contingency); dt_table(x$tables$contingency) })
    ed <- mod_plot_editor_server("plot", res)
    mod_export_server("export", shiny::reactive(list(res())), dat$name, ed$style)
  })
}

# Confidence intervals ------------------------------------------------------------
mod_confidence_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar <- shiny::tagList(
    step_title(1, "Dados"),
    mod_data_input_ui(ns("data"), "confidence"),
    step_title(2, "Intervalo"),
    shiny::selectInput(ns("mode"), NULL, list(
      "M\u00e9dias" = c("M\u00e9dia de cada tratamento" = "group_means", "M\u00e9dia (uma amostra)" = "mean"),
      "Diferen\u00e7as" = c("Diferen\u00e7a entre 2 m\u00e9dias (Welch, vari\u00e2ncias diferentes)" = "diff_means_welch",
                       "Diferen\u00e7a entre 2 m\u00e9dias (vari\u00e2ncias iguais)" = "diff_means", "Diferen\u00e7a pareada" = "paired_diff"),
      "Dispers\u00e3o" = c("Vari\u00e2ncia" = "variance", "Desvio-padr\u00e3o" = "sd", "Raz\u00e3o entre 2 vari\u00e2ncias" = "var_ratio"),
      "Outros" = c("Propor\u00e7\u00e3o" = "proportion", "Mediana (bootstrap)" = "median")), width = "100%"),
    step_title(3, "Vari\u00e1veis"),
    shiny::uiOutput(ns("vars")),
    shiny::selectInput(ns("conf"), "N\u00edvel de confian\u00e7a", c("95%" = "0.95", "90%" = "0.90", "99%" = "0.99"), width = "100%"),
    details_panel("Op\u00e7\u00f5es avan\u00e7adas",
      shiny::selectInput(ns("variance"), "Vari\u00e2ncia (m\u00e9dias por tratamento)", c("Combinada - QM do res\u00edduo (vari\u00e2ncias iguais)" = "pooled", "Separada por tratamento" = "separate"), width = "100%"),
      shiny::selectInput(ns("simultaneous"), "Cobertura", c("Individual" = "none", "Simult\u00e2nea (Bonferroni)" = "bonferroni"), width = "100%"),
      shiny::selectInput(ns("prop_method"), "M\u00e9todo para propor\u00e7\u00e3o", c("Wilson" = "wilson", "Exato (Clopper-Pearson)" = "exact", "Wald" = "wald"), width = "100%"),
      shiny::checkboxInput(ns("boot"), "Usar bootstrap (m\u00e9dia, desvio, diferen\u00e7as)", FALSE),
      shiny::numericInput(ns("R"), "Reamostragens", 2000, 200, 1e5, 500, width = "100%"),
      shiny::numericInput(ns("seed"), "Semente", 123, width = "100%")),
    run_button(ns)
  )
  analysis_layout(ns, sidebar, shiny::uiOutput(ns("results")), NULL, mod_plot_editor_ui(ns("plot")), mod_export_ui(ns("export")))
}

mod_confidence_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dat <- mod_data_server("data", "confidence")
    shiny::observeEvent(dat$tick(), {
      ex <- dat$example()
      if (!is.null(ex)) { apply_example_inputs(session, ex$inputs); bslib::nav_select("tabs", "dados") }
    }, ignoreInit = TRUE)
    output$vars <- shiny::renderUI({
      d <- dat$data()
      if (is.null(d)) return(shiny::p(class = "caagro-muted", "Carregue os dados primeiro."))
      pre <- dat$example()$vars %||% list()
      numc <- col_choices(d, numeric_only = TRUE)
      m <- input$mode %||% "group_means"
      g <- pick(pre, "group", guess_factor(d))
      shiny::tagList(
        shiny::selectInput(ns("response"), "Vari\u00e1vel-resposta", if (m == "proportion") names(d) else numc,
                           selected = pick(pre, "response", utils::tail(setdiff(numc, g), 1)), width = "100%"),
        if (!m %in% c("mean", "variance", "sd", "proportion", "median"))
          shiny::selectInput(ns("group"), if (m == "group_means") "Tratamentos" else "Grupos (exatamente 2)", names(d), selected = g, width = "100%"),
        if (m == "group_means") shiny::selectInput(ns("block"), "Blocos (opcional)", c("(nenhum)", names(d)), selected = pick(pre, "block", "(nenhum)"), width = "100%"),
        if (m == "paired_diff") shiny::selectInput(ns("id"), "Coluna que identifica os pares", names(d), width = "100%"),
        if (m == "proportion") shiny::textInput(ns("success"), "Valor considerado sucesso (opcional)", "", width = "100%")
      )
    })
    shiny::outputOptions(output, "vars", suspendWhenHidden = FALSE)
    res_val <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$run, res_val((function() {
      d <- dat$data()
      if (is.null(d)) { shiny::showNotification("Carregue um exemplo ou importe uma planilha.", type = "warning"); return(NULL) }
      shiny::req(input$response)
      m <- input$mode
      if (!m %in% c("mean", "variance", "sd", "proportion", "median") && identical(input$response, input$group)) {
        shiny::showNotification("Resposta e grupo devem ser colunas diferentes.", type = "error"); return(NULL)
      }
      boot <- isTRUE(input$boot) || m == "median"
      r <- safe_run(conf_int(d, input$response, m, group = if (!m %in% c("mean", "variance", "sd", "proportion", "median")) input$group,
                             id = if (m == "paired_diff") input$id, conf_level = as.numeric(input$conf), success = blank_null(input$success),
                             prop_method = input$prop_method %||% "wilson", method = if (boot && m != "group_means") "bootstrap" else "classic",
                             block = none_or(input$block), variance = input$variance %||% "pooled",
                             simultaneous = input$simultaneous %||% "none", exclude = dat$exclude(), R = input$R %||% 2000, seed = input$seed %||% 123))
      bslib::nav_select("tabs", "an\u00e1lise")
      r
    })()))
    res <- shiny::reactive(res_val())
    output$results <- shiny::renderUI({
      x <- res()
      if (is.null(x)) return(empty_state())
      bind_result_tables(output, x)
      if (inherits(x, "caagro_ci_groups")) {
        it <- x$tables$intervals
        strip <- summary_strip(list(Tratamentos = nrow(it), "N\u00edvel" = paste0(100 * x$settings$conf_level, "%"),
                                    "Vari\u00e2ncia" = if (x$settings$variance == "pooled") "combinada" else "separada"),
                               "Cada intervalo indica a precis\u00e3o da m\u00e9dia do tratamento. A sobreposi\u00e7\u00e3o de intervalos n\u00e3o \u00e9 um teste de diferen\u00e7a.")
      } else {
        it <- x$tables$interval
        strip <- summary_strip(list(Estimativa = fmt_br(it$estimate, 4), `Limite inferior` = fmt_br(it$lower, 4), `Limite superior` = fmt_br(it$upper, 4)),
                               x$notes$Interpretacao)
      }
      shiny::tagList(strip, result_tables_ui(ns, x), warnings_ui(x$warnings), notes_ui(x, show_warnings = FALSE))
    })
    ed <- mod_plot_editor_server("plot", res)
    mod_export_server("export", shiny::reactive(list(res())), dat$name, ed$style)
  })
}

# Multivariate ---------------------------------------------------------------------
mod_multivariate_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar <- shiny::tagList(
    step_title(1, "Dados"),
    mod_data_input_ui(ns("data"), "multivariate"),
    step_title(2, "An\u00e1lise"),
    shiny::selectInput(ns("method"), NULL, c("Componentes principais (PCA)" = "pca", "Agrupamento hier\u00e1rquico" = "hclust",
                                            "Agrupamento k-means" = "kmeans", "Matriz de correla\u00e7\u00e3o" = "cor"), width = "100%"),
    step_title(3, "Vari\u00e1veis"),
    shiny::uiOutput(ns("vars")),
    shiny::conditionalPanel(sprintf("['hclust','kmeans'].indexOf(input['%s']) >= 0", ns("method")),
                            shiny::numericInput(ns("k"), "N\u00famero de grupos", 3, 2, 30, width = "100%")),
    details_panel("Op\u00e7\u00f5es avan\u00e7adas",
      shiny::checkboxInput(ns("scale"), "Padronizar vari\u00e1veis (recomendado quando as unidades diferem)", TRUE),
      shiny::selectInput(ns("distance"), "Dist\u00e2ncia (hier\u00e1rquico)", c("Euclidiana" = "euclidean", "Manhattan" = "manhattan", "M\u00e1xima" = "maximum", "Canberra" = "canberra"), width = "100%"),
      shiny::selectInput(ns("linkage"), "Liga\u00e7\u00e3o (hier\u00e1rquico)", c("Ward.D2" = "ward.D2", "Completa" = "complete", "M\u00e9dia (UPGMA)" = "average", "Simples" = "single"), width = "100%"),
      shiny::selectInput(ns("cor_method"), "Coeficiente de correla\u00e7\u00e3o", c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall"), width = "100%"),
      shiny::selectInput(ns("p_adjust"), "Corre\u00e7\u00e3o (correla\u00e7\u00e3o)", c("Holm" = "holm", "Bonferroni" = "bonferroni", "Benjamini-Hochberg" = "BH", "Nenhuma" = "none"), width = "100%"),
      shiny::numericInput(ns("nstart"), "Inicializa\u00e7\u00f5es (k-means)", 25, 1, 500, width = "100%"),
      shiny::numericInput(ns("seed"), "Semente (k-means)", 123, width = "100%")),
    run_button(ns)
  )
  analysis_layout(ns, sidebar, shiny::uiOutput(ns("results")), NULL, mod_plot_editor_ui(ns("plot"), "600px"), mod_export_ui(ns("export")))
}

mod_multivariate_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dat <- mod_data_server("data", "multivariate")
    shiny::observeEvent(dat$tick(), {
      ex <- dat$example()
      if (!is.null(ex)) { apply_example_inputs(session, ex$inputs); bslib::nav_select("tabs", "dados") }
    }, ignoreInit = TRUE)
    output$vars <- shiny::renderUI({
      d <- dat$data()
      if (is.null(d)) return(shiny::p(class = "caagro-muted", "Carregue os dados primeiro."))
      pre <- dat$example()$vars %||% list()
      numc <- col_choices(d, numeric_only = TRUE)
      shiny::tagList(
        shiny::selectizeInput(ns("variables"), "Vari\u00e1veis num\u00e9ricas", numc, selected = numc, multiple = TRUE, width = "100%", options = list(plugins = list("remove_button"))),
        shiny::selectInput(ns("group"), "Grupo (cores no gr\u00e1fico, opcional)", c("(nenhum)", names(d)), selected = pick(pre, "group", "(nenhum)"), width = "100%"),
        shiny::selectInput(ns("label"), "Nome dos indiv\u00edduos (opcional)", c("(nenhum)", names(d)), selected = pick(pre, "label", "(nenhum)"), width = "100%")
      )
    })
    shiny::outputOptions(output, "vars", suspendWhenHidden = FALSE)
    res_val <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$run, res_val((function() {
      d <- dat$data()
      if (is.null(d)) { shiny::showNotification("Carregue um exemplo ou importe uma planilha.", type = "warning"); return(NULL) }
      if (length(input$variables) < 2) { shiny::showNotification("Escolha ao menos duas vari\u00e1veis num\u00e9ricas.", type = "warning"); return(NULL) }
      if (!is.null(dat$exclude())) d <- d[-dat$exclude(), , drop = FALSE]
      g <- none_or(input$group); l <- none_or(input$label)
      r <- switch(input$method,
        pca = safe_run(pca_exp(d, input$variables, group = g, label = l, scale = input$scale)),
        hclust = safe_run(cluster_exp(d, input$variables, label = l, distance = input$distance, method = input$linkage, scale = input$scale, k = input$k, group = g)),
        kmeans = safe_run(cluster_exp(d, input$variables, method = "kmeans", k = input$k, label = l, scale = input$scale, nstart = input$nstart, seed = input$seed, group = g)),
        cor = safe_run(cor_matrix(d, input$variables, method = input$cor_method, p_adjust = input$p_adjust)))
      bslib::nav_select("tabs", "an\u00e1lise")
      r
    })()))
    res <- shiny::reactive(res_val())
    output$results <- shiny::renderUI({
      x <- res()
      if (is.null(x)) return(empty_state())
      bind_result_tables(output, x)
      strip <- if (inherits(x, "caagro_pca")) { e <- x$tables$eigenvalues
        summary_strip(list(PC1 = paste0(fmt_br(e$variance[1], 1), "%"), PC2 = paste0(fmt_br(e$variance[2], 1), "%"),
                           `PC1 + PC2` = paste0(fmt_br(e$cumulative[2], 1), "%")), x$notes$Resultado)
      } else if (inherits(x, "caagro_cluster")) summary_strip(list("Correla\u00e7\u00e3o cofen\u00e9tica" = fmt_br(x$tables$quality$cophenetic_cor, 3), "Indiv\u00edduos" = x$tables$quality$n), x$notes$Interpretacao)
      else if (inherits(x, "caagro_kmeans")) summary_strip(list(`SQ entre / SQ total` = paste0(fmt_br(100 * x$tables$fit$between_over_total, 1), "%"),
                                                                  "Silhueta m\u00e9dia" = fmt_br(x$tables$fit$mean_silhouette, 3)), x$notes$Resultado)
      else NULL
      main <- switch(class(x)[1], caagro_pca = c("eigenvalues", "correlations"), caagro_cluster = "clusters",
                     caagro_kmeans = c("summary", "clusters"), caagro_cor = "pairs")
      shiny::tagList(strip, result_tables_ui(ns, x, main), warnings_ui(x$warnings), notes_ui(x, show_warnings = FALSE))
    })
    ed <- mod_plot_editor_server("plot", res)
    mod_export_server("export", shiny::reactive(list(res())), dat$name, ed$style)
  })
}

# Exploratory ------------------------------------------------------------------------
mod_exploratory_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar <- shiny::tagList(
    step_title(1, "Dados"),
    mod_data_input_ui(ns("data"), "exploratory"),
    step_title(2, "Vari\u00e1veis"),
    shiny::uiOutput(ns("vars")),
    shiny::p(class = "caagro-muted", "Os resultados s\u00e3o atualizados automaticamente. Nenhum teste de hip\u00f3tese \u00e9 feito aqui.")
  )
  analysis <- shiny::tagList(
    shiny::uiOutput(ns("intro")),
    bslib::card(bslib::card_header("Estat\u00edstica descritiva"), dt_out(ns("t_stats"))),
    bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = 6),
      bslib::card(bslib::card_header("Dispers\u00e3o entre duas vari\u00e1veis"), shiny::uiOutput(ns("scatter_ui")), shiny::plotOutput(ns("scatter"), height = 320)),
      bslib::card(bslib::card_header("Correla\u00e7\u00f5es (Pearson)"), shiny::plotOutput(ns("cormat"), height = 360))),
    details_panel("Frequ\u00eancias das vari\u00e1veis qualitativas", dt_out(ns("t_freq"))),
    details_panel("Como ler estas estat\u00edsticas",
      shiny::tags$ul(
        shiny::tags$li("M\u00e9dia e mediana muito diferentes indicam assimetria."),
        shiny::tags$li("CV (%) compara a variabilidade relativa; s\u00f3 faz sentido para vari\u00e1veis positivas."),
        shiny::tags$li("A amplitude interquartil (Q3 - Q1) \u00e9 pouco afetada por valores extremos."),
        shiny::tags$li("Assimetria positiva: cauda longa \u00e0 direita. Curtose positiva: caudas mais pesadas que a normal.")))
  )
  analysis_layout(ns, sidebar, analysis, NULL, mod_plot_editor_ui(ns("plot")), mod_export_ui(ns("export")))
}

mod_exploratory_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dat <- mod_data_server("data", "exploratory")
    shiny::observeEvent(dat$tick(), bslib::nav_select("tabs", "an\u00e1lise"), ignoreInit = TRUE)
    output$vars <- shiny::renderUI({
      d <- dat$data()
      if (is.null(d)) return(shiny::p(class = "caagro-muted", "Carregue os dados primeiro."))
      pre <- dat$example()$vars %||% list()
      numc <- col_choices(d, numeric_only = TRUE)
      shiny::tagList(shiny::selectizeInput(ns("variables"), "Vari\u00e1veis num\u00e9ricas", numc, selected = pick(pre, "variables", numc), multiple = TRUE, width = "100%",
                                           options = list(plugins = list("remove_button"))),
                     shiny::selectInput(ns("group"), "Agrupar por (opcional)", c("(nenhum)", names(d)), selected = pick(pre, "group", "(nenhum)"), width = "100%"))
    })
    shiny::outputOptions(output, "vars", suspendWhenHidden = FALSE)
    r <- shiny::reactive({
      d <- dat$data(); shiny::req(d, length(input$variables) >= 1, all(input$variables %in% names(d)))
      if (!is.null(dat$exclude())) d <- d[-dat$exclude(), , drop = FALSE]
      safe_run(describe_data(d, input$variables, group = none_or(input$group)))
    })
    output$intro <- shiny::renderUI(if (is.null(dat$data())) empty_state("Carregue um exemplo ou importe uma planilha para ver as estat\u00edsticas."))
    output$t_stats <- DT::renderDT({
      st <- r()$tables$statistics
      dt_table(st[, setdiff(names(st), c("variance", "range", "q1", "q3")), drop = FALSE], digits = 2, filter = nrow(st) > 15)
    })
    output$t_freq <- DT::renderDT(dt_table(r()$tables$frequencies, digits = 1))
    output$scatter_ui <- shiny::renderUI({
      v <- input$variables; shiny::req(length(v) >= 1)
      bslib::layout_columns(col_widths = c(6, 6), shiny::selectInput(ns("sx"), "Eixo x", v, v[1]), shiny::selectInput(ns("sy"), "Eixo y", v, v[min(2, length(v))]))
    })
    output$scatter <- shiny::renderPlot({
      d <- dat$data(); shiny::req(d, input$sx, input$sy, input$sx %in% names(d), input$sy %in% names(d))
      g <- none_or(input$group)
      p <- ggplot2::ggplot(d, ggplot2::aes(.data[[input$sx]], .data[[input$sy]], fill = if (is.null(g)) "a" else as.character(.data[[g]]))) +
        ggplot2::geom_point(shape = 21, colour = "white", size = 2.6, alpha = 0.9)
      apply_style(finish_plot(p, groups = if (is.null(g)) "a" else unique(as.character(d[[g]])),
                              defaults = list(title = NULL, x = input$sx, y = input$sy, legend = g)),
                  caagro_style(legend = legend_spec(show = !is.null(g), position = "bottom")))
    }, res = 96)
    output$cormat <- shiny::renderPlot({
      d <- dat$data(); shiny::req(d, length(input$variables) >= 2, all(input$variables %in% names(d)))
      plot(cor_matrix(d, input$variables), style = caagro_style(show_title = FALSE))
    }, res = 96)
    ed <- mod_plot_editor_server("plot", r)
    mod_export_server("export", shiny::reactive(list(r())), dat$name, ed$style)
  })
}
