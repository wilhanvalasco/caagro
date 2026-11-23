# Shiny modules: non-parametric, confidence intervals, multivariate, exploratory

# Non-parametric ---------------------------------------------------------------
mod_nonparametric_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar <- shiny::tagList(
    mod_data_input_ui(ns("data")),
    shiny::selectInput(ns("test"), "Teste", c("Kruskal-Wallis (k grupos independentes)" = "kruskal", "Friedman (blocos completos)" = "friedman",
                                              "Mediana de Mood" = "mood", "Mann-Whitney (2 grupos)" = "mann_whitney",
                                              "Wilcoxon pareado" = "wilcoxon_paired", "Teste dos sinais" = "sign", "Permutacao" = "permutation")),
    shiny::uiOutput(ns("vars")),
    shiny::conditionalPanel(sprintf("['kruskal','friedman'].indexOf(input['%s']) >= 0", ns("test")),
      shiny::selectInput(ns("posthoc"), "Post-hoc", c("Dunn" = "dunn", "Conover" = "conover", "Wilcoxon par a par" = "wilcoxon", "Nenhum" = "none")),
      shiny::selectInput(ns("p_adjust"), "Correcao de multiplicidade", c("Holm" = "holm", "Bonferroni" = "bonferroni", "Benjamini-Hochberg (FDR)" = "BH", "Nenhuma" = "none"))),
    shiny::conditionalPanel(sprintf("input['%s'] == 'permutation'", ns("test")),
      shiny::numericInput(ns("R"), "Permutacoes", 4999, 999, 1e5, 1000), shiny::numericInput(ns("seed"), "Semente", 123)),
    shiny::numericInput(ns("alpha"), "Alfa", 0.05, 0.001, 0.2, 0.01),
    run_button(ns)
  )
  analysis <- shiny::tagList(shiny::uiOutput(ns("cards")), shiny::uiOutput(ns("notes")),
                             bslib::card(bslib::card_header("Resultado do teste"), DT::DTOutput(ns("t_test"))),
                             bslib::card(bslib::card_header("Medianas, quartis e postos"), DT::DTOutput(ns("t_desc"))),
                             shiny::uiOutput(ns("pairs_ui")))
  analysis_layout(ns, sidebar, analysis, NULL, mod_plot_editor_ui(ns("plot")), mod_export_ui(ns("export")), "Nao parametrica")
}

mod_nonparametric_server <- function(id, sci_mode = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dat <- mod_data_server("data")
    output$vars <- shiny::renderUI({
      d <- dat$data(); shiny::req(d)
      numc <- col_choices(d, numeric_only = TRUE)
      shiny::tagList(
        shiny::selectInput(ns("response"), "Resposta", numc, selected = utils::tail(numc, 1)),
        shiny::selectInput(ns("group"), "Grupo / tratamento", names(d), selected = guess_factor(d)),
        shiny::conditionalPanel(sprintf("input['%s'] == 'friedman'", ns("test")), shiny::selectInput(ns("block"), "Bloco", names(d), selected = guess_factor(d, guess_factor(d)))),
        shiny::conditionalPanel(sprintf("['wilcoxon_paired','sign'].indexOf(input['%s']) >= 0", ns("test")), shiny::selectInput(ns("id"), "Identificador do par", names(d)))
      )
    })
    res <- shiny::eventReactive(input$run, {
      d <- dat$data(); shiny::req(d, input$response, input$group)
      if (identical(input$response, input$group)) { shiny::showNotification("Resposta e grupo devem ser colunas diferentes.", type = "error"); return(NULL) }
      t <- safe_run(np_test(d, input$response, input$group, input$test, block = if (input$test == "friedman") input$block,
                            id = if (input$test %in% c("wilcoxon_paired", "sign")) input$id, R = input$R %||% 4999,
                            seed = input$seed %||% 123, exclude = dat$exclude()))
      if (!is.null(t) && input$test %in% c("kruskal", "friedman") && input$posthoc != "none") {
        ph <- if (input$test == "friedman" && input$posthoc == "dunn") "conover" else input$posthoc
        t <- safe_run(np_posthoc(t, ph, p_adjust = input$p_adjust, alpha = input$alpha)) %||% t
      }
      t
    })
    r <- shiny::reactive({ shiny::req(res()); res() })
    output$cards <- shiny::renderUI({
      tb <- r()$tables$test
      stat_row(stat_card("Teste", tb$test), stat_card("Estatistica", fmt_num(tb$statistic, 3)),
               if (!is.na(tb$df %||% NA)) stat_card("gl", fmt_num(tb$df, 0)),
               stat_card("Valor de p", fmt_p(tb$p), tone = if (tb$p < input$alpha) "good" else "default"),
               if (!is.null(tb$epsilon_sq)) stat_card("Epsilon2 (efeito)", fmt_num(tb$epsilon_sq, 3)),
               if (!is.null(tb$kendall_w)) stat_card("W de Kendall", fmt_num(tb$kendall_w, 3)),
               if (!is.null(tb$rank_biserial)) stat_card("Correlacao rank-bisserial", fmt_num(tb$rank_biserial, 3)))
    })
    output$notes <- shiny::renderUI(notes_ui(r(), !isTRUE(sci_mode())))
    output$t_test <- DT::renderDT(dt_table(r()$tables$test))
    output$t_desc <- DT::renderDT(dt_table(r()$tables$descriptives))
    output$pairs_ui <- shiny::renderUI(if (!is.null(r()$tables$pairs)) bslib::card(bslib::card_header("Comparacoes post-hoc"), DT::DTOutput(ns("t_pairs"))) else
      if (!is.null(r()$tables$contingency)) bslib::card(bslib::card_header("Tabela de contingencia"), DT::DTOutput(ns("t_cont"))))
    output$t_pairs <- DT::renderDT(dt_table(r()$tables$pairs))
    output$t_cont <- DT::renderDT(dt_table(r()$tables$contingency))
    ed <- mod_plot_editor_server("plot", r)
    mod_export_server("export", shiny::reactive(list(res())), dat$name, ed$style)
  })
}

# Confidence intervals ------------------------------------------------------------
mod_confidence_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar <- shiny::tagList(
    mod_data_input_ui(ns("data")),
    shiny::selectInput(ns("mode"), "Tipo de intervalo", c(
      "Medias por tratamento" = "groups", "Media (uma amostra)" = "mean", "Variancia" = "variance", "Desvio-padrao" = "sd",
      "Proporcao" = "proportion", "Diferenca de medias (Welch)" = "diff_means_welch", "Diferenca de medias (variancias iguais)" = "diff_means",
      "Razao de variancias" = "var_ratio", "Diferenca pareada" = "paired_diff", "Bootstrap" = "boot")),
    shiny::uiOutput(ns("vars")),
    shiny::numericInput(ns("conf"), "Nivel de confianca", 0.95, 0.5, 0.999, 0.01),
    shiny::conditionalPanel(sprintf("input['%s'] == 'groups'", ns("mode")),
      shiny::selectInput(ns("variance"), "Variancia", c("Combinada (QM do residuo)" = "pooled", "Separada por grupo" = "separate")),
      shiny::selectInput(ns("simultaneous"), "Cobertura", c("Individual" = "none", "Simultanea (Bonferroni)" = "bonferroni"))),
    shiny::conditionalPanel(sprintf("input['%s'] == 'proportion'", ns("mode")),
      shiny::selectInput(ns("prop_method"), "Metodo", c("Wilson" = "wilson", "Exato (Clopper-Pearson)" = "exact", "Wald" = "wald")),
      shiny::textInput(ns("success"), "Valor considerado sucesso (opcional)", "")),
    shiny::conditionalPanel(sprintf("input['%s'] == 'boot'", ns("mode")),
      shiny::selectInput(ns("bstat"), "Estatistica", c("Media" = "mean", "Mediana" = "median", "Desvio-padrao" = "sd", "Diferenca de medias" = "diff_means", "Diferenca de medianas" = "diff_medians")),
      shiny::selectInput(ns("btype"), "Intervalo", c("BCa" = "bca", "Percentil" = "percentile")),
      shiny::numericInput(ns("R"), "Reamostragens", 2000, 200, 1e5, 500), shiny::numericInput(ns("seed"), "Semente", 123)),
    run_button(ns)
  )
  analysis <- shiny::tagList(shiny::uiOutput(ns("cards")), shiny::uiOutput(ns("notes")), shiny::uiOutput(ns("tables")))
  analysis_layout(ns, sidebar, analysis, NULL, mod_plot_editor_ui(ns("plot")), mod_export_ui(ns("export")), "Intervalos de confianca")
}

mod_confidence_server <- function(id, sci_mode = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dat <- mod_data_server("data")
    output$vars <- shiny::renderUI({
      d <- dat$data(); shiny::req(d)
      numc <- col_choices(d, numeric_only = TRUE)
      shiny::tagList(
        shiny::selectInput(ns("response"), "Resposta", if (identical(input$mode, "proportion")) names(d) else numc, selected = utils::tail(numc, 1)),
        shiny::conditionalPanel(sprintf("['mean','variance','sd','proportion'].indexOf(input['%s']) < 0", ns("mode")),
                                shiny::selectInput(ns("group"), "Grupo / tratamento", names(d), selected = guess_factor(d))),
        shiny::conditionalPanel(sprintf("input['%s'] == 'groups'", ns("mode")), shiny::selectInput(ns("block"), "Bloco (opcional)", c("(nenhum)", names(d)))),
        shiny::conditionalPanel(sprintf("input['%s'] == 'paired_diff'", ns("mode")), shiny::selectInput(ns("id"), "Identificador do par", names(d)))
      )
    })
    res <- shiny::eventReactive(input$run, {
      d <- dat$data(); shiny::req(d, input$response)
      m <- input$mode
      if (!m %in% c("mean", "variance", "sd", "proportion") && identical(input$response, input$group)) {
        shiny::showNotification("Resposta e grupo devem ser colunas diferentes.", type = "error"); return(NULL)
      }
      if (m == "groups") return(safe_run(conf_int_groups(d, input$response, input$group, block = none_or(input$block), variance = input$variance,
                                                         simultaneous = input$simultaneous, conf_level = input$conf, exclude = dat$exclude())))
      if (m == "boot") return(safe_run(conf_int_boot(d, input$response, input$bstat, group = if (grepl("diff", input$bstat)) input$group,
                                                     R = input$R, type = input$btype, conf_level = input$conf, seed = input$seed)))
      safe_run(conf_int(d, input$response, m, group = if (!m %in% c("mean", "variance", "sd", "proportion")) input$group,
                        id = if (m == "paired_diff") input$id, conf_level = input$conf,
                        success = blank_null(input$success), prop_method = input$prop_method))
    })
    r <- shiny::reactive({ shiny::req(res()); res() })
    output$cards <- shiny::renderUI({
      x <- r()
      if (inherits(x, "caagro_ci_groups")) {
        it <- x$tables$intervals
        return(stat_row(stat_card("Tratamentos", nrow(it)), stat_card("Nivel", paste0(100 * x$settings$conf_level, "%")),
                        if (!is.null(x$tables$precision)) stat_card("CV experimental", paste0(fmt_num(x$tables$precision$cv_percent, 2), "%")),
                        if (!is.null(x$tables$anova)) stat_card("p (ANOVA)", fmt_p(x$tables$anova$p[x$tables$anova$term == x$settings$group]))))
      }
      it <- x$tables$interval
      stat_row(stat_card("Estimativa", formatC(it$estimate, format = "g", digits = 5)),
               stat_card("Limite inferior", formatC(it$lower, format = "g", digits = 5)),
               stat_card("Limite superior", formatC(it$upper, format = "g", digits = 5)))
    })
    output$notes <- shiny::renderUI(notes_ui(r(), !isTRUE(sci_mode())))
    output$tables <- shiny::renderUI({
      x <- r()
      shiny::tagList(lapply(names(x$tables), function(tn) bslib::card(bslib::card_header(tlabel(tn)), DT::DTOutput(ns(paste0("t_", tn))))))
    })
    shiny::observe({
      x <- r()
      for (tn in names(x$tables)) local({ t2 <- tn; output[[paste0("t_", t2)]] <- DT::renderDT(dt_table(x$tables[[t2]])) })
    })
    ed <- mod_plot_editor_server("plot", r)
    mod_export_server("export", shiny::reactive(list(res())), dat$name, ed$style)
  })
}

# Multivariate ---------------------------------------------------------------------
mod_multivariate_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar <- shiny::tagList(
    mod_data_input_ui(ns("data")),
    shiny::selectInput(ns("method"), "Analise", c("Componentes principais (PCA)" = "pca", "Agrupamento hierarquico" = "hclust", "K-means" = "kmeans", "Matriz de correlacao" = "cor")),
    shiny::uiOutput(ns("vars")),
    shiny::conditionalPanel(sprintf("['pca','hclust','kmeans'].indexOf(input['%s']) >= 0", ns("method")),
                            shiny::checkboxInput(ns("scale"), "Padronizar variaveis (recomendado com unidades diferentes)", TRUE)),
    shiny::conditionalPanel(sprintf("input['%s'] == 'hclust'", ns("method")),
      shiny::selectInput(ns("distance"), "Distancia", c("Euclidiana" = "euclidean", "Manhattan" = "manhattan", "Maxima" = "maximum", "Canberra" = "canberra")),
      shiny::selectInput(ns("linkage"), "Ligacao", c("Ward.D2" = "ward.D2", "Completa" = "complete", "Media (UPGMA)" = "average", "Simples" = "single")),
      shiny::numericInput(ns("k_h"), "Numero de grupos (corte)", 3, 2, 30)),
    shiny::conditionalPanel(sprintf("input['%s'] == 'kmeans'", ns("method")),
      shiny::numericInput(ns("k"), "k (grupos)", 3, 2, 30), shiny::numericInput(ns("nstart"), "Inicializacoes", 25, 1, 500),
      shiny::numericInput(ns("seed"), "Semente", 123)),
    shiny::conditionalPanel(sprintf("input['%s'] == 'cor'", ns("method")),
      shiny::selectInput(ns("cor_method"), "Coeficiente", c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")),
      shiny::selectInput(ns("p_adjust"), "Correcao", c("Holm" = "holm", "Bonferroni" = "bonferroni", "BH (FDR)" = "BH", "Nenhuma" = "none"))),
    run_button(ns)
  )
  analysis <- shiny::tagList(shiny::uiOutput(ns("cards")), shiny::uiOutput(ns("notes")), shiny::uiOutput(ns("tables")))
  analysis_layout(ns, sidebar, analysis, NULL, mod_plot_editor_ui(ns("plot"), "620px"), mod_export_ui(ns("export")), "Multivariada")
}

mod_multivariate_server <- function(id, sci_mode = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dat <- mod_data_server("data")
    output$vars <- shiny::renderUI({
      d <- dat$data(); shiny::req(d)
      numc <- col_choices(d, numeric_only = TRUE)
      shiny::tagList(
        shiny::selectizeInput(ns("variables"), "Variaveis quantitativas", numc, selected = numc, multiple = TRUE, options = list(plugins = list("remove_button"))),
        shiny::selectInput(ns("group"), "Grupo (cores/elipses)", c("(nenhum)", names(d))),
        shiny::selectInput(ns("label"), "Rotulo dos individuos", c("(nenhum)", names(d)))
      )
    })
    res <- shiny::eventReactive(input$run, {
      d <- dat$data(); shiny::req(d, length(input$variables) >= 2)
      if (!is.null(dat$exclude())) d <- d[-dat$exclude(), , drop = FALSE]
      g <- none_or(input$group); l <- none_or(input$label)
      switch(input$method,
        pca = safe_run(pca_exp(d, input$variables, group = g, label = l, scale = input$scale)),
        hclust = safe_run(cluster_exp(d, input$variables, label = l, distance = input$distance, method = input$linkage, scale = input$scale, k = input$k_h, group = g)),
        kmeans = safe_run(kmeans_exp(d, input$variables, k = input$k, label = l, scale = input$scale, nstart = input$nstart, seed = input$seed, group = g)),
        cor = safe_run(cor_matrix(d, input$variables, method = input$cor_method, p_adjust = input$p_adjust)))
    })
    r <- shiny::reactive({ shiny::req(res()); res() })
    output$cards <- shiny::renderUI({
      x <- r()
      if (inherits(x, "caagro_pca")) { e <- x$tables$eigenvalues
        stat_row(stat_card("PC1", paste0(fmt_num(e$variance[1], 1), "%")), stat_card("PC2", paste0(fmt_num(e$variance[2], 1), "%")),
                 stat_card("Acumulado (PC1+PC2)", paste0(fmt_num(e$cumulative[2], 1), "%")), stat_card("Autovalores > 1", sum(e$eigenvalue > 1)))
      } else if (inherits(x, "caagro_cluster")) stat_row(stat_card("Correlacao cofenetica", fmt_num(x$tables$quality$cophenetic_cor, 3)), stat_card("Individuos", x$tables$quality$n))
      else if (inherits(x, "caagro_kmeans")) stat_row(stat_card("SQ entre / total", paste0(fmt_num(100 * x$tables$fit$between_over_total, 1), "%")),
                                                       stat_card("Silhueta media", fmt_num(x$tables$fit$mean_silhouette, 3)), stat_card("Semente", x$settings$seed))
      else NULL
    })
    output$notes <- shiny::renderUI(notes_ui(r(), !isTRUE(sci_mode())))
    output$tables <- shiny::renderUI(shiny::tagList(lapply(names(r()$tables), function(tn) bslib::card(bslib::card_header(tlabel(tn)), DT::DTOutput(ns(paste0("t_", tn)))))))
    shiny::observe({ x <- r(); for (tn in names(x$tables)) local({ t2 <- tn; output[[paste0("t_", t2)]] <- DT::renderDT(dt_table(x$tables[[t2]], page = 10)) }) })
    ed <- mod_plot_editor_server("plot", r)
    mod_export_server("export", shiny::reactive(list(res())), dat$name, ed$style)
  })
}

# Exploratory ------------------------------------------------------------------------
mod_exploratory_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebar <- shiny::tagList(mod_data_input_ui(ns("data")), shiny::uiOutput(ns("vars")),
                            shiny::p(class = "caagro-muted", "A exploracao e atualizada automaticamente; nenhum teste de hipotese e realizado aqui."))
  analysis <- shiny::tagList(shiny::uiOutput(ns("notes")),
                             bslib::card(bslib::card_header("Estatistica descritiva"), DT::DTOutput(ns("t_stats"))),
                             bslib::card(bslib::card_header("Frequencias (variaveis qualitativas)"), DT::DTOutput(ns("t_freq"))),
                             bslib::layout_columns(col_widths = c(6, 6),
                                                   bslib::card(bslib::card_header("Dispersao"), shiny::uiOutput(ns("scatter_ui")), shiny::plotOutput(ns("scatter"), height = 330)),
                                                   bslib::card(bslib::card_header("Matriz de correlacao (Pearson)"), shiny::plotOutput(ns("cormat"), height = 380))),
                             shiny::div(class = "caagro-help", shiny::tags$b("Como ler: "),
                                        "Media e mediana muito diferentes indicam assimetria. O CV% compara a variabilidade relativa (so para variaveis positivas em escala de razao). ",
                                        "O IQR (Q3 - Q1) e robusto a valores extremos. Assimetria positiva = cauda a direita; curtose positiva = caudas mais pesadas que a normal."))
  analysis_layout(ns, sidebar, analysis, NULL, mod_plot_editor_ui(ns("plot")), mod_export_ui(ns("export")), "Exploratoria")
}

mod_exploratory_server <- function(id, sci_mode = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dat <- mod_data_server("data")
    output$vars <- shiny::renderUI({
      d <- dat$data(); shiny::req(d)
      numc <- col_choices(d, numeric_only = TRUE)
      shiny::tagList(shiny::selectizeInput(ns("variables"), "Variaveis", numc, selected = numc, multiple = TRUE, options = list(plugins = list("remove_button"))),
                     shiny::selectInput(ns("group"), "Agrupar por", c("(nenhum)", setdiff(names(d), numc), numc)))
    })
    r <- shiny::reactive({
      d <- dat$data(); shiny::req(d, length(input$variables) >= 1)
      if (!is.null(dat$exclude())) d <- d[-dat$exclude(), , drop = FALSE]
      safe_run(describe_data(d, input$variables, group = none_or(input$group)))
    })
    output$notes <- shiny::renderUI(notes_ui(r(), !isTRUE(sci_mode())))
    output$t_stats <- DT::renderDT(dt_table(r()$tables$statistics, digits = 3, filter = TRUE))
    output$t_freq <- DT::renderDT(dt_table(r()$tables$frequencies, digits = 1))
    output$scatter_ui <- shiny::renderUI({
      v <- input$variables; shiny::req(length(v) >= 1)
      shiny::fluidRow(shiny::column(6, shiny::selectInput(ns("sx"), "x", v, v[1])), shiny::column(6, shiny::selectInput(ns("sy"), "y", v, v[min(2, length(v))])))
    })
    output$scatter <- shiny::renderPlot({
      d <- dat$data(); shiny::req(d, input$sx, input$sy)
      g <- none_or(input$group)
      p <- ggplot2::ggplot(d, ggplot2::aes(.data[[input$sx]], .data[[input$sy]], fill = if (is.null(g)) "a" else as.character(.data[[g]]))) +
        ggplot2::geom_point(shape = 21, colour = "white", size = 2.6, alpha = 0.9)
      apply_style(finish_plot(p, groups = if (is.null(g)) "a" else unique(as.character(d[[g]])),
                              defaults = list(title = NULL, x = input$sx, y = input$sy, legend = g)),
                  caagro_style(legend = legend_spec(show = !is.null(g))))
    }, res = 96)
    output$cormat <- shiny::renderPlot({
      d <- dat$data(); shiny::req(d, length(input$variables) >= 2)
      plot(cor_matrix(d, input$variables), style = caagro_style(show_title = FALSE))
    }, res = 96)
    ed <- mod_plot_editor_server("plot", r)
    mod_export_server("export", shiny::reactive(list(r())), dat$name, ed$style)
  })
}
