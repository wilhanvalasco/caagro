# Shiny module: data import, examples, preview and data tools -------------------

# Examples of each module. `inputs` are applied to the module controls when
# the example is loaded; `vars` pre-select the variable fields.
app_examples <- function(module) {
  ex <- list(
    anova = list(
      soja = list(label = "Produtividade de soja (DIC)", data = function() get_example("sim_soja"),
                  title = "Exemplo: produtividade de soja",
                  objective = "Comparar a produtividade de quatro tratamentos.",
                  variables = c("Vari\u00e1vel-resposta" = "produtividade (sc/ha)", "Tratamento" = "tratamento"),
                  config = "Delineamento inteiramente casualizado (DIC), 5 repeti\u00e7\u00f5es. ANOVA convencional e Tukey a 5%.",
                  expected = "Espera-se efeito significativo de tratamento; o Produto B tende a apresentar a maior m\u00e9dia.",
                  vars = list(response = "produtividade", treatment = "tratamento", block = "(nenhum)", factors = character()),
                  inputs = list(test = "tukey", alpha = "0.05")),
      dbc = list(label = "Seis tratamentos em blocos (DBC)", data = function() get_example("sim_dbc"),
                 title = "Exemplo: ensaio em blocos casualizados",
                 objective = "Comparar seis tratamentos controlando a varia\u00e7\u00e3o entre blocos.",
                 variables = c("Vari\u00e1vel-resposta" = "produtividade (t/ha)", "Tratamento" = "tratamento", "Bloco" = "bloco"),
                 config = "DBC com 4 blocos. ANOVA convencional e Scott-Knott a 5%.",
                 expected = "Tratamentos e blocos significativos; Scott-Knott separa os tratamentos em grupos sem sobreposi\u00e7\u00e3o.",
                 vars = list(response = "produtividade", treatment = "tratamento", block = "bloco", factors = character()),
                 inputs = list(test = "scott_knott", alpha = "0.05")),
      fatorial = list(label = "Fatorial 3 x 3 em blocos", data = function() get_example("sim_fatorial"),
                      title = "Exemplo: cultivar x aduba\u00e7\u00e3o",
                      objective = "Avaliar o efeito de cultivares, doses de adubo e da intera\u00e7\u00e3o entre eles.",
                      variables = c("Vari\u00e1vel-resposta" = "altura (cm)", "Fator 1" = "cultivar", "Fator 2" = "adubacao", "Bloco" = "bloco"),
                      config = "Fatorial 3 x 3 em DBC. Como a intera\u00e7\u00e3o \u00e9 significativa, as cultivares s\u00e3o comparadas dentro de cada aduba\u00e7\u00e3o (Tukey a 5%).",
                      expected = "Intera\u00e7\u00e3o significativa: a melhor cultivar depende da aduba\u00e7\u00e3o.",
                      vars = list(response = "altura", treatment = "cultivar", block = "bloco", factors = "adubacao"),
                      inputs = list(test = "tukey", alpha = "0.05"))
    ),
    regression = list(
      dose = list(label = "Resposta da produtividade \u00e0 dose", data = function() get_example("sim_dose"),
                  title = "Exemplo: resposta da produtividade \u00e0 dose",
                  objective = "Avaliar a resposta da produtividade a doses de nitrog\u00eanio em duas cultivares.",
                  variables = c("Vari\u00e1vel independente" = "dose (kg/ha)", "Vari\u00e1vel dependente" = "produtividade (t/ha)", "Agrupamento" = "cultivar"),
                  config = "Modelos linear e quadr\u00e1tico; banda de confian\u00e7a de 95%.",
                  expected = "O modelo quadr\u00e1tico descreve melhor a resposta (menor AICc), com m\u00e1ximo pr\u00f3ximo de 135-145 kg/ha.",
                  vars = list(x = "dose", y = "produtividade", group = "cultivar", trials = "(nenhum)"),
                  inputs = list(models = c("linear", "quadratic"), band = "confidence")),
      germ = list(label = "Germina\u00e7\u00e3o x dose (binomial)", data = function() { d <- get_example("sim_germinacao"); d[d$dose > 0, ] },
                  title = "Exemplo: germina\u00e7\u00e3o sob doses de herbicida",
                  objective = "Estimar a dose que reduz a germina\u00e7\u00e3o a 50% (ED50) para dois produtos.",
                  variables = c("Dose" = "dose", "Sementes germinadas" = "germinadas", "Total de sementes" = "total", "Produto" = "produto"),
                  config = "Regress\u00e3o binomial (logit) em fun\u00e7\u00e3o do logaritmo da dose.",
                  expected = "ED50 pr\u00f3xima de 2 para P1 e de 4 a 5 para P2.",
                  vars = list(x = "dose", y = "germinadas", group = "produto", trials = "total"),
                  inputs = list(models = "binomial_logdose", band = "confidence"))
    ),
    nonparametric = list(
      notas = list(label = "Notas de severidade", data = function() get_example("sim_notas"),
                   title = "Exemplo: notas de severidade",
                   objective = "Comparar tratamentos avaliados por notas visuais de severidade (escala de 1 a 9).",
                   variables = c("Resposta" = "nota (escala ordinal)", "Tratamento" = "tratamento"),
                   config = "Kruskal-Wallis com compara\u00e7\u00f5es de Dunn (ajuste de Holm).",
                   expected = "Notas s\u00e3o ordinais: a compara\u00e7\u00e3o de postos \u00e9 apropriada. Espera-se diferen\u00e7a entre T1 e T4.",
                   vars = list(response = "nota", group = "tratamento"),
                   inputs = list(test = "kruskal", posthoc = "dunn", p_adjust = "holm"))
    ),
    confidence = list(
      plant = list(label = "Peso de plantas (dados reais)", data = function() get_example("PlantGrowth"),
                   title = "Exemplo: peso seco de plantas",
                   objective = "Estimar a m\u00e9dia de cada tratamento com intervalo de confian\u00e7a de 95%.",
                   variables = c("Resposta" = "weight (g)", "Tratamento" = "group (controle e dois tratamentos)"),
                   config = "Intervalos por tratamento com vari\u00e2ncia combinada (QM do res\u00edduo).",
                   expected = "trt2 apresenta a maior m\u00e9dia. A sobreposi\u00e7\u00e3o de intervalos n\u00e3o \u00e9 um teste de diferen\u00e7a.",
                   vars = list(response = "weight", group = "group", block = "(nenhum)"),
                   inputs = list(mode = "group_means", variance = "pooled"))
    ),
    multivariate = list(
      gen = list(label = "Gen\u00f3tipos de milho (6 caracteres)", data = function() get_example("sim_multivariado"),
                 title = "Exemplo: caracteriza\u00e7\u00e3o de gen\u00f3tipos",
                 objective = "Resumir seis caracteres agron\u00f4micos e visualizar a semelhan\u00e7a entre 36 gen\u00f3tipos.",
                 variables = c("Vari\u00e1veis" = "altura, di\u00e2metro do colmo, massa de 100 gr\u00e3os, fileiras, ciclo, produtividade", "Grupo" = "grupo", "R\u00f3tulo" = "genotipo"),
                 config = "PCA com vari\u00e1veis padronizadas (matriz de correla\u00e7\u00e3o).",
                 expected = "PC1 explica cerca de 70% da varia\u00e7\u00e3o e separa o grupo G3 dos demais.",
                 vars = list(group = "grupo", label = "genotipo"),
                 inputs = list(method = "pca"))
    ),
    exploratory = list(
      fat = list(label = "Altura de plantas", data = function() get_example("sim_fatorial"),
                 title = "Exemplo: altura de plantas",
                 objective = "Conhecer a distribui\u00e7\u00e3o da altura por cultivar antes da an\u00e1lise.",
                 variables = c("Vari\u00e1vel" = "altura (cm)", "Agrupar por" = "cultivar"),
                 config = "Estat\u00edsticas descritivas por cultivar.",
                 expected = "Verifique m\u00e9dias, dispers\u00e3o (CV) e poss\u00edveis valores extremos.",
                 vars = list(variables = "altura", group = "cultivar"),
                 inputs = list())
    )
  )
  ex[[module]]
}

mod_data_input_ui <- function(id, module) {
  ns <- shiny::NS(id)
  exs <- app_examples(module)
  shiny::tagList(
    shiny::div(class = "caagro-example-box",
      if (length(exs) > 1) shiny::selectInput(ns("example_id"), NULL, stats::setNames(names(exs), vapply(exs, `[[`, "", "label")), width = "100%"),
      shiny::actionButton(ns("load_example"), "Carregar exemplo", icon = shiny::icon("flask"), class = "btn-outline-primary btn-sm w-100")),
    shiny::div(class = "caagro-or", "ou"),
    shiny::fileInput(ns("file"), NULL, accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv"), width = "100%",
                     buttonLabel = "Importar...", placeholder = "Planilha .xlsx, .csv ou .txt"),
    shiny::uiOutput(ns("file_opts")),
    shiny::uiOutput(ns("data_badge"))
  )
}

mod_data_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("example_info")),
    bslib::card(bslib::card_header("Dados carregados"),
                shiny::p(class = "caagro-muted", "Para excluir observa\u00e7\u00f5es, selecione as linhas e clique em Excluir. Nada \u00e9 removido automaticamente."),
                dt_out(ns("preview")),
                shiny::div(class = "caagro-btn-row",
                           shiny::actionButton(ns("exclude_rows"), "Excluir selecionadas", class = "btn-sm btn-outline-warning"),
                           shiny::actionButton(ns("restore_rows"), "Restaurar todas", class = "btn-sm btn-outline-secondary"),
                           shiny::downloadButton(ns("download_data"), "Baixar dados (.csv)", class = "btn-sm btn-outline-secondary")),
                shiny::uiOutput(ns("excluded_info"))),
    details_panel("Estrutura das vari\u00e1veis", dt_out(ns("structure"))),
    details_panel("Converter tipo ou renomear coluna", shiny::uiOutput(ns("convert_ui"))),
    details_panel("Converter formato largo em longo", shiny::uiOutput(ns("wide_ui"))),
    details_panel("Valores potencialmente at\u00edpicos", shiny::uiOutput(ns("outlier_ui")), dt_out(ns("outliers")))
  )
}

mod_data_server <- function(id, module) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    exs <- app_examples(module)
    rv <- shiny::reactiveValues(data = NULL, name = NULL, exclude = integer(), example = NULL, tick = 0)

    output$file_opts <- shiny::renderUI({
      shiny::req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      if (ext %in% c("xlsx", "xls")) {
        sh <- tryCatch(readxl::excel_sheets(input$file$datapath), error = function(e) "1")
        if (length(sh) > 1) shiny::selectInput(ns("sheet"), "Planilha", sh)
      } else {
        details_panel("Op\u00e7\u00f5es de leitura",
          shiny::selectInput(ns("sep"), "Separador de colunas", c("Autom\u00e1tico" = "auto", "Ponto e v\u00edrgula (;)" = ";", "V\u00edrgula (,)" = ",", "Tabula\u00e7\u00e3o" = "\t")),
          shiny::selectInput(ns("dec"), "Separador decimal", c("Autom\u00e1tico" = "auto", "V\u00edrgula" = ",", "Ponto" = ".")))
      }
    })

    shiny::observeEvent(input$load_example, {
      key <- input$example_id %||% names(exs)[1]
      ex <- exs[[key]]
      rv$data <- ex$data(); rv$name <- paste0("exemplo_", key, ".csv"); rv$exclude <- integer()
      rv$example <- ex; rv$tick <- rv$tick + 1
    })
    shiny::observe({
      shiny::req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      d <- safe_run(read_data(input$file$datapath,
                              sheet = if (ext %in% c("xlsx", "xls")) (input$sheet %||% 1) else 1,
                              sep = if (is.null(input$sep) || input$sep == "auto") NULL else input$sep,
                              dec = if (is.null(input$dec) || input$dec == "auto") NULL else input$dec))
      if (!is.null(d)) {
        rv$data <- d; rv$name <- input$file$name; rv$exclude <- integer(); rv$example <- NULL
        rv$tick <- shiny::isolate(rv$tick) + 1
      }
    })

    output$data_badge <- shiny::renderUI({
      shiny::req(rv$data)
      shiny::div(class = "caagro-badge-data", shiny::icon("table"),
                 sprintf(" %s - %d linhas, %d colunas", rv$name, nrow(rv$data), ncol(rv$data)))
    })

    output$example_info <- shiny::renderUI({
      ex <- rv$example
      if (is.null(ex)) return(NULL)
      shiny::div(class = "caagro-example-card",
        shiny::div(class = "caagro-example-title", shiny::icon("flask"), " ", ex$title),
        shiny::tags$dl(class = "caagro-example-dl",
          shiny::tags$dt("Objetivo"), shiny::tags$dd(ex$objective),
          shiny::tags$dt("Vari\u00e1veis"), shiny::tags$dd(paste(names(ex$variables), ex$variables, sep = ": ", collapse = " | ")),
          shiny::tags$dt("Configura\u00e7\u00e3o"), shiny::tags$dd(ex$config),
          shiny::tags$dt("O que observar"), shiny::tags$dd(ex$expected)),
        shiny::div(class = "caagro-muted", "Os campos foram preenchidos. Clique em Executar an\u00e1lise."))
    })

    output$preview <- DT::renderDT({
      shiny::req(rv$data)
      d <- rv$data
      d <- cbind(`#` = seq_len(nrow(d)), d)
      dt <- DT::datatable(d, rownames = FALSE, selection = "multiple", class = "compact hover caagro-dt",
                          options = list(pageLength = 10, scrollX = TRUE, dom = "ftip",
                                         language = list(search = "Buscar:", info = "_START_ a _END_ de _TOTAL_",
                                                         paginate = list(previous = "Anterior", `next` = "Pr\u00f3xima"))))
      if (length(rv$exclude)) dt <- DT::formatStyle(dt, "#", target = "row",
                                                    backgroundColor = DT::styleEqual(rv$exclude, rep("#FEF4EA", length(rv$exclude))),
                                                    textDecoration = DT::styleEqual(rv$exclude, rep("line-through", length(rv$exclude))))
      dt
    })
    shiny::observeEvent(input$exclude_rows, {
      sel <- input$preview_rows_selected
      if (length(sel)) rv$exclude <- sort(unique(c(rv$exclude, sel)))
    })
    shiny::observeEvent(input$restore_rows, rv$exclude <- integer())
    output$excluded_info <- shiny::renderUI({
      if (!length(rv$exclude)) return(NULL)
      shiny::div(class = "caagro-msg-warn", sprintf("%d linha(s) exclu\u00edda(s) por voc\u00ea: %s. A exclus\u00e3o fica registrada nos resultados e no script.",
                                                    length(rv$exclude), paste(rv$exclude, collapse = ", ")))
    })
    output$download_data <- shiny::downloadHandler(filename = function() sub("\\.[^.]*$", ".csv", rv$name %||% "dados.csv"),
                                                   content = function(file) utils::write.csv2(rv$data, file, row.names = FALSE))

    output$structure <- DT::renderDT({
      shiny::req(rv$data)
      d <- rv$data
      st <- data.frame(coluna = names(d),
                       tipo = vapply(d, function(z) if (is.numeric(z)) "num\u00e9rica" else "qualitativa", ""),
                       ausentes = vapply(d, function(z) sum(is.na(z)), 1),
                       distintos = vapply(d, function(z) length(unique(z)), 1),
                       exemplo = vapply(d, function(z) paste(utils::head(unique(as.character(z)), 3), collapse = ", "), ""))
      names(st) <- c("Coluna", "Tipo", "Ausentes", "Valores distintos", "Exemplos")
      dt_table(st, digits = 0, page = 20)
    })

    output$convert_ui <- shiny::renderUI({
      shiny::req(rv$data)
      shiny::tagList(
        shiny::selectInput(ns("conv_col"), "Coluna", names(rv$data)),
        shiny::selectInput(ns("conv_type"), "Converter para", c("Qualitativa (fator)" = "factor", "Num\u00e9rica" = "numeric", "Texto" = "character")),
        shiny::actionButton(ns("conv_go"), "Converter", class = "btn-sm btn-primary"),
        shiny::tags$hr(),
        shiny::selectInput(ns("ren_col"), "Renomear a coluna", names(rv$data)),
        shiny::textInput(ns("ren_new"), "Novo nome"),
        shiny::actionButton(ns("ren_go"), "Renomear", class = "btn-sm btn-outline-primary"),
        shiny::p(class = "caagro-muted", "As altera\u00e7\u00f5es valem apenas para a c\u00f3pia de trabalho; o arquivo original n\u00e3o \u00e9 modificado.")
      )
    })
    shiny::observeEvent(input$conv_go, {
      d <- rv$data; cl <- input$conv_col
      v <- d[[cl]]
      new <- switch(input$conv_type,
        factor = factor(v),
        numeric = { y <- suppressWarnings(as.numeric(gsub(",", ".", as.character(v), fixed = TRUE)))
                    bad <- sum(!is.na(v) & is.na(y))
                    if (bad) { shiny::showNotification(sprintf("%d valor(es) de '%s' n\u00e3o s\u00e3o num\u00e9ricos. Convers\u00e3o cancelada.", bad, cl), type = "error"); return() }
                    y },
        character = as.character(v))
      d[[cl]] <- new; rv$data <- d
      shiny::showNotification(sprintf("Coluna '%s' convertida.", cl), type = "message")
    })
    shiny::observeEvent(input$ren_go, {
      nn <- trimws(input$ren_new)
      if (!nzchar(nn) || nn %in% names(rv$data)) { shiny::showNotification("Nome inv\u00e1lido ou j\u00e1 existente.", type = "error"); return() }
      d <- rv$data
      attr(d, "original_names") <- c(attr(d, "original_names"), stats::setNames(input$ren_col, nn))
      names(d)[names(d) == input$ren_col] <- nn; rv$data <- d
    })

    output$wide_ui <- shiny::renderUI({
      shiny::req(rv$data)
      shiny::tagList(
        shiny::p(class = "caagro-muted", "Use quando cada repeti\u00e7\u00e3o est\u00e1 em uma coluna (ex.: R1, R2, R3)."),
        shiny::selectizeInput(ns("wide_cols"), "Colunas das repeti\u00e7\u00f5es", names(rv$data), multiple = TRUE),
        shiny::textInput(ns("wide_names"), "Nome da nova coluna de identifica\u00e7\u00e3o", "repeticao"),
        shiny::textInput(ns("wide_values"), "Nome da nova coluna de valores", "valor"),
        shiny::actionButton(ns("wide_go"), "Converter", class = "btn-sm btn-outline-primary")
      )
    })
    shiny::observeEvent(input$wide_go, {
      shiny::req(length(input$wide_cols) >= 2)
      d <- safe_run(to_long(rv$data, input$wide_cols, input$wide_names, input$wide_values))
      if (!is.null(d)) { rv$data <- d; rv$exclude <- integer(); shiny::showNotification("Dados convertidos para o formato longo.", type = "message") }
    })

    output$outlier_ui <- shiny::renderUI({
      shiny::req(rv$data)
      shiny::tagList(
        shiny::p(class = "caagro-muted", "Crit\u00e9rio do boxplot (1,5 x amplitude interquartil). Os valores s\u00e3o apenas sinalizados."),
        shiny::selectInput(ns("out_resp"), "Vari\u00e1vel", col_choices(rv$data, numeric_only = TRUE)),
        shiny::selectInput(ns("out_group"), "Por grupo", c("(nenhum)", names(rv$data))))
    })
    output$outliers <- DT::renderDT({
      shiny::req(rv$data, input$out_resp, input$out_resp %in% names(rv$data))
      shiny::req(is.null(none_or(input$out_group)) || input$out_group %in% names(rv$data))
      fo <- safe_run(flag_outliers(rv$data, input$out_resp, none_or(input$out_group)))
      shiny::req(fo)
      fo <- fo[fo$flag, setdiff(names(fo), "flag"), drop = FALSE]
      names(fo) <- c("Linha", "Valor", "Grupo", "Limite inferior", "Limite superior")[seq_along(fo)]
      dt_table(fo, digits = 3, page = 8)
    })

    list(data = shiny::reactive(rv$data), name = shiny::reactive(rv$name %||% "dados"),
         exclude = shiny::reactive(if (length(rv$exclude)) rv$exclude else NULL),
         example = shiny::reactive(rv$example), tick = shiny::reactive(rv$tick))
  })
}

# Apply example settings to the module inputs (non-dynamic controls)
apply_example_inputs <- function(session, inputs) {
  for (nm in names(inputs)) session$sendInputMessage(nm, list(value = inputs[[nm]]))
}
