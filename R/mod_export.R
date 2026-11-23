# Shiny module: exports (Excel, report, script, configuration) --------------

mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(6, 6, 6, 6),
    bslib::card(bslib::card_header("Planilha Excel"),
                shiny::p(class = "caagro-muted", "Dados originais, dados utilizados, estat\u00edstica descritiva, todas as tabelas de resultados e a aba de metodologia e par\u00e2metros."),
                shiny::downloadButton(ns("xlsx"), "Baixar .xlsx", class = "btn-primary")),
    bslib::card(bslib::card_header("Relat\u00f3rio cient\u00edfico"),
                shiny::fluidRow(shiny::column(6, shiny::selectInput(ns("rep_fmt"), "Formato", c("HTML" = "html", "Word (.docx)" = "docx", "PDF" = "pdf"))),
                                shiny::column(6, shiny::selectInput(ns("rep_level"), "Conte\u00fado", c("Completo" = "full", "Resumido" = "summary")))),
                shiny::textInput(ns("rep_title"), "T\u00edtulo", "Relat\u00f3rio de an\u00e1lise estat\u00edstica"),
                shiny::textInput(ns("rep_author"), "Autor(es)", ""),
                shiny::p(class = "caagro-muted", "HTML n\u00e3o depende de software externo. Word exige Pandoc; PDF exige Pandoc e um mecanismo de PDF."),
                shiny::downloadButton(ns("report"), "Gerar relat\u00f3rio", class = "btn-primary")),
    bslib::card(bslib::card_header("Script R reprodut\u00edvel"),
                shiny::p(class = "caagro-muted", "Reproduz a mesma an\u00e1lise por fun\u00e7\u00f5es do pacote, com os mesmos par\u00e2metros, exclusoes e semente."),
                shiny::verbatimTextOutput(ns("code_preview")),
                shiny::downloadButton(ns("script"), "Baixar .R")),
    bslib::card(bslib::card_header("Configura\u00e7\u00e3o e vers\u00f5es"),
                shiny::p(class = "caagro-muted", "Par\u00e2metros, metadados (vers\u00f5es do caagro e do R, data, semente) em JSON."),
                shiny::downloadButton(ns("config"), "Baixar .json"),
                shiny::tags$hr(),
                shiny::downloadButton(ns("session"), "Informa\u00e7\u00f5es da sess\u00e3o R (.txt)", class = "btn-sm btn-outline-secondary"))
  )
}

mod_export_server <- function(id, results, data_name, style = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    res_list <- shiny::reactive({
      r <- results()
      shiny::validate(shiny::need(length(r) > 0, "Execute uma an\u00e1lise primeiro."))
      Filter(Negate(is.null), r)
    })
    stamp <- function() format(Sys.time(), "%Y%m%d_%H%M")
    output$xlsx <- shiny::downloadHandler(filename = function() paste0("caagro_resultados_", stamp(), ".xlsx"),
                                          content = function(file) export_excel(res_list(), file))
    output$report <- shiny::downloadHandler(
      filename = function() paste0("caagro_relatorio_", stamp(), ".", input$rep_fmt),
      content = function(file) {
        res <- tryCatch(export_report(res_list(), file, title = input$rep_title, author = blank_null(input$rep_author),
                                      level = input$rep_level, style = style() %||% caagro_style()),
                        error = function(e) { shiny::showNotification(conditionMessage(e), type = "error", duration = 12); NULL })
        if (is.null(res)) writeLines(paste("N\u00e3o foi poss\u00edvel gerar o relat\u00f3rio neste formato. Use HTML."), file)
      })
    output$code_preview <- shiny::renderText({
      paste(c("library(caagro)", paste0("dados <- read_data(\"", data_name(), "\")"), vapply(res_list(), function(r) r$code %||% "", "")), collapse = "\n\n")
    })
    output$script <- shiny::downloadHandler(filename = function() paste0("caagro_script_", stamp(), ".R"),
                                            content = function(file) export_script(res_list(), file, data_file = data_name(), style = style()))
    output$config <- shiny::downloadHandler(filename = function() paste0("caagro_config_", stamp(), ".json"),
                                            content = function(file) export_config(res_list(), file))
    output$session <- shiny::downloadHandler(filename = function() "sessionInfo.txt",
                                             content = function(file) writeLines(utils::capture.output(utils::sessionInfo()), file))
  })
}
