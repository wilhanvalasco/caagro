# Shiny module: data import, preview, validation, conversion, exclusions ------

mod_data_input_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::radioButtons(ns("source"), NULL, c("Arquivo" = "file", "Exemplo" = "example"), inline = TRUE),
    shiny::conditionalPanel(
      sprintf("input['%s'] == 'file'", ns("source")),
      shiny::fileInput(ns("file"), NULL, accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv"),
                       buttonLabel = "Selecionar...", placeholder = ".xlsx, .xls, .csv, .txt"),
      shiny::uiOutput(ns("file_opts"))
    ),
    shiny::conditionalPanel(
      sprintf("input['%s'] == 'example'", ns("source")),
      shiny::selectInput(ns("example"), NULL, example_datasets())
    )
  )
}

mod_data_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(12, 6, 6, 12),
    bslib::card(bslib::card_header("Visualizacao dos dados",
                                   shiny::span(class = "caagro-muted", " - selecione linhas para excluir explicitamente")),
                DT::DTOutput(ns("preview")),
                shiny::div(class = "caagro-btn-row",
                           shiny::actionButton(ns("exclude_rows"), "Excluir linhas selecionadas", class = "btn-sm btn-outline-warning"),
                           shiny::actionButton(ns("restore_rows"), "Restaurar todas", class = "btn-sm btn-outline-secondary")),
                shiny::uiOutput(ns("excluded_info"))),
    bslib::card(bslib::card_header("Estrutura das variaveis"), DT::DTOutput(ns("structure"))),
    bslib::card(bslib::card_header("Converter tipo / formato"),
                shiny::uiOutput(ns("convert_ui")),
                shiny::tags$hr(),
                shiny::tags$b("Formato largo para longo"),
                shiny::uiOutput(ns("wide_ui"))),
    bslib::card(bslib::card_header("Valores potencialmente atipicos (criterio IQR - apenas sinalizacao)"),
                shiny::uiOutput(ns("outlier_ui")), DT::DTOutput(ns("outliers")))
  )
}

mod_data_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- shiny::reactiveValues(data = NULL, name = NULL, exclude = integer())

    output$file_opts <- shiny::renderUI({
      shiny::req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      if (ext %in% c("xlsx", "xls")) {
        sh <- tryCatch(readxl::excel_sheets(input$file$datapath), error = function(e) "1")
        shiny::selectInput(ns("sheet"), "Planilha", sh)
      } else {
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(ns("sep"), "Delimitador", c("Automatico" = "auto", "; (ponto e virgula)" = ";", ", (virgula)" = ",", "Tabulacao" = "\t", "|" = "|"))),
          shiny::column(6, shiny::selectInput(ns("dec"), "Decimal", c("Automatico" = "auto", "," = ",", "." = ".")))
        )
      }
    })

    shiny::observe({
      if (identical(input$source, "example")) {
        shiny::req(input$example)
        rv$data <- get_example(input$example); rv$name <- input$example; rv$exclude <- integer()
      } else {
        shiny::req(input$file)
        ext <- tolower(tools::file_ext(input$file$name))
        d <- safe_run(read_data(input$file$datapath,
                                sheet = if (ext %in% c("xlsx", "xls")) (input$sheet %||% 1) else 1,
                                sep = if (is.null(input$sep) || input$sep == "auto") NULL else input$sep,
                                dec = if (is.null(input$dec) || input$dec == "auto") NULL else input$dec))
        if (!is.null(d)) { rv$data <- d; rv$name <- input$file$name; rv$exclude <- integer() }
      }
    })

    output$preview <- DT::renderDT({
      shiny::req(rv$data)
      d <- rv$data
      d <- cbind(`#` = seq_len(nrow(d)), d)
      dt <- DT::datatable(d, rownames = FALSE, selection = "multiple", class = "compact hover caagro-dt",
                          options = list(pageLength = 10, scrollX = TRUE, dom = "ftip"))
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
      if (!length(rv$exclude)) return(shiny::div(class = "caagro-muted", "Nenhuma observacao excluida. Nada e removido automaticamente."))
      shiny::div(class = "caagro-msg-warn", sprintf("%d linha(s) excluida(s) explicitamente: %s. A exclusao sera registrada nos resultados e no script.",
                                                    length(rv$exclude), paste(rv$exclude, collapse = ", ")))
    })

    output$structure <- DT::renderDT({
      shiny::req(rv$data)
      d <- rv$data
      st <- data.frame(coluna = names(d), tipo = vapply(d, function(z) class(z)[1], ""),
                       ausentes = vapply(d, function(z) sum(is.na(z)), 1),
                       distintos = vapply(d, function(z) length(unique(z)), 1),
                       exemplo = vapply(d, function(z) paste(utils::head(unique(as.character(z)), 3), collapse = ", "), ""))
      st$duplicadas <- c(sum(duplicated(d)), rep(NA, nrow(st) - 1))
      dt_table(st, digits = 0, page = 20)
    })

    output$convert_ui <- shiny::renderUI({
      shiny::req(rv$data)
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(5, shiny::selectInput(ns("conv_col"), "Coluna", names(rv$data))),
          shiny::column(4, shiny::selectInput(ns("conv_type"), "Converter para", c("Fator (qualitativa)" = "factor", "Numerica (quantitativa)" = "numeric", "Texto" = "character"))),
          shiny::column(3, shiny::div(style = "margin-top:1.9rem", shiny::actionButton(ns("conv_go"), "Aplicar", class = "btn-sm btn-primary")))
        ),
        shiny::fluidRow(
          shiny::column(5, shiny::selectInput(ns("ren_col"), "Renomear coluna", names(rv$data))),
          shiny::column(4, shiny::textInput(ns("ren_new"), "Novo nome")),
          shiny::column(3, shiny::div(style = "margin-top:1.9rem", shiny::actionButton(ns("ren_go"), "Renomear", class = "btn-sm btn-outline-primary")))
        ),
        shiny::div(class = "caagro-muted", "A renomeacao altera apenas a copia de trabalho; o arquivo original nao e modificado e o nome original fica registrado.")
      )
    })
    shiny::observeEvent(input$conv_go, {
      d <- rv$data; cl <- input$conv_col
      v <- d[[cl]]
      new <- switch(input$conv_type,
        factor = factor(v),
        numeric = { y <- suppressWarnings(as.numeric(gsub(",", ".", as.character(v), fixed = TRUE)))
                    bad <- sum(!is.na(v) & is.na(y))
                    if (bad) { shiny::showNotification(sprintf("%d valor(es) de '%s' nao sao numericos; conversao cancelada.", bad, cl), type = "error"); return() }
                    y },
        character = as.character(v))
      d[[cl]] <- new; rv$data <- d
      shiny::showNotification(sprintf("'%s' convertida para %s.", cl, input$conv_type), type = "message")
    })
    shiny::observeEvent(input$ren_go, {
      nn <- trimws(input$ren_new)
      if (!nzchar(nn) || nn %in% names(rv$data)) { shiny::showNotification("Nome invalido ou ja existente.", type = "error"); return() }
      d <- rv$data
      attr(d, "original_names") <- c(attr(d, "original_names"), stats::setNames(input$ren_col, nn))
      names(d)[names(d) == input$ren_col] <- nn; rv$data <- d
    })

    output$wide_ui <- shiny::renderUI({
      shiny::req(rv$data)
      shiny::tagList(
        shiny::selectizeInput(ns("wide_cols"), "Colunas com medidas repetidas (ex.: R1, R2, R3)", names(rv$data), multiple = TRUE),
        shiny::fluidRow(shiny::column(4, shiny::textInput(ns("wide_names"), "Nova coluna de nomes", "repeticao")),
                        shiny::column(4, shiny::textInput(ns("wide_values"), "Nova coluna de valores", "valor")),
                        shiny::column(4, shiny::div(style = "margin-top:1.9rem", shiny::actionButton(ns("wide_go"), "Converter", class = "btn-sm btn-outline-primary"))))
      )
    })
    shiny::observeEvent(input$wide_go, {
      shiny::req(length(input$wide_cols) >= 2)
      d <- safe_run(to_long(rv$data, input$wide_cols, input$wide_names, input$wide_values))
      if (!is.null(d)) { rv$data <- d; rv$exclude <- integer(); shiny::showNotification("Dados convertidos para o formato longo.", type = "message") }
    })

    output$outlier_ui <- shiny::renderUI({
      shiny::req(rv$data)
      shiny::fluidRow(
        shiny::column(6, shiny::selectInput(ns("out_resp"), "Resposta", col_choices(rv$data, numeric_only = TRUE))),
        shiny::column(6, shiny::selectInput(ns("out_group"), "Agrupar por", c("(nenhum)", names(rv$data))))
      )
    })
    output$outliers <- DT::renderDT({
      shiny::req(rv$data, input$out_resp, input$out_resp %in% names(rv$data))
      shiny::req(is.null(none_or(input$out_group)) || input$out_group %in% names(rv$data))
      fo <- safe_run(flag_outliers(rv$data, input$out_resp, none_or(input$out_group)))
      shiny::req(fo)
      dt_table(fo[fo$flag, , drop = FALSE], digits = 3, page = 8)
    })

    list(data = shiny::reactive(rv$data), name = shiny::reactive(rv$name %||% "dados"),
         exclude = shiny::reactive(if (length(rv$exclude)) rv$exclude else NULL))
  })
}
