# Applications ---------------------------------------------------------------

module_registry <- function() {
  list(
    exploratory = list(label = "Exploratoria", icon = "magnifying-glass-chart", ui = mod_exploratory_ui, server = mod_exploratory_server,
                       desc = "Primeira avaliacao: descritivas, distribuicoes, valores ausentes e correlacoes."),
    anova = list(label = "ANOVA", icon = "table-cells", ui = mod_anova_ui, server = mod_anova_server,
                 desc = "DIC, DBC e fatoriais; Tukey, Scott-Knott, SNK, LSD, Dunnett; diagnosticos; testes t e Welch."),
    regression = list(label = "Regressao", icon = "chart-line", ui = mod_regression_ui, server = mod_regression_server,
                      desc = "Modelos lineares, polinomiais, nao lineares e binomiais; bandas; doses otimas; multiplas curvas."),
    nonparametric = list(label = "Nao parametrica", icon = "ranking-star", ui = mod_nonparametric_ui, server = mod_nonparametric_server,
                         desc = "Kruskal-Wallis, Friedman, mediana de Mood, Mann-Whitney, Wilcoxon, sinais e permutacao."),
    confidence = list(label = "Intervalos", icon = "arrows-left-right-to-line", ui = mod_confidence_ui, server = mod_confidence_server,
                      desc = "Intervalos de confianca classicos e bootstrap para medias, variancias, proporcoes e diferencas."),
    multivariate = list(label = "Multivariada", icon = "circle-nodes", ui = mod_multivariate_ui, server = mod_multivariate_server,
                        desc = "PCA, biplot, dendrogramas, k-means e matrizes de correlacao.")
  )
}

mod_home_ui <- function(id, modules) {
  ns <- shiny::NS(id)
  reg <- module_registry()[modules]
  shiny::div(class = "caagro-home",
    shiny::div(class = "caagro-hero",
      shiny::tags$img(src = "caagro-www/logo.svg", class = "caagro-hero-logo", alt = "caagro"),
      shiny::div(shiny::h2("caagro"), shiny::p("Ciencia Aplicada em Analises do Agro - plataforma de analise estatistica para experimentacao agricola."),
                 shiny::p(class = "caagro-muted", "Todos os calculos sao feitos localmente; nenhum dado e enviado para servidores externos."))),
    bslib::layout_columns(col_widths = c(7, 5),
      bslib::card(bslib::card_header(shiny::icon("compass"), " Assistente de analise"),
        shiny::p(class = "caagro-muted", "Responda as perguntas para ver metodos candidatos. A decisao final e sua: as sugestoes seguem regras transparentes, nao testes de normalidade."),
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(ns("goal"), "1. Objetivo", c("Comparar tratamentos" = "compare", "Ajustar curvas (doses, tempo)" = "curve",
                                                                            "Relacao entre variaveis" = "relationship", "Explorar dados" = "explore", "Analise multivariada" = "multivariate"))),
          shiny::column(6, shiny::selectInput(ns("response"), "2. Variavel-resposta", c("Continua" = "continuous", "Binaria / proporcao" = "binary", "Contagem" = "count", "Ordinal (notas, escalas)" = "ordinal")))),
        shiny::fluidRow(
          shiny::column(4, shiny::numericInput(ns("n_groups"), "3. Grupos/tratamentos", 4, 2, 100)),
          shiny::column(4, shiny::checkboxInput(ns("related"), "4. Dados relacionados (pareados)", FALSE)),
          shiny::column(4, shiny::checkboxInput(ns("blocked"), "5. Ha blocos", FALSE))),
        shiny::checkboxInput(ns("factorial"), "Ha dois ou mais fatores cruzados (fatorial)", FALSE),
        shiny::uiOutput(ns("suggestions"))),
      bslib::card(bslib::card_header(shiny::icon("layer-group"), " Modulos"),
        shiny::div(class = "caagro-module-list", lapply(names(reg), function(m)
          shiny::actionLink(ns(paste0("go_", m)), shiny::div(class = "caagro-module-item", shiny::icon(reg[[m]]$icon),
                                                             shiny::div(shiny::strong(reg[[m]]$label), shiny::div(class = "caagro-muted", reg[[m]]$desc)))))))
    )
  )
}

mod_home_server <- function(id, modules, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    output$suggestions <- shiny::renderUI({
      s <- suggest_method(input$goal, input$response, input$n_groups, input$related, input$blocked, input$factorial)
      shiny::tagList(lapply(seq_len(nrow(s)), function(i) shiny::div(class = "caagro-suggestion",
        shiny::div(shiny::span(class = "caagro-rank", i), shiny::strong(s$method[i])),
        shiny::div(class = "caagro-muted", shiny::code(s$function_call[i]), " - ", s$app[i]),
        shiny::div(shiny::tags$small(shiny::tags$b("Pressupostos: "), s$assumptions[i])),
        shiny::div(shiny::tags$small(shiny::tags$b("Quando usar: "), s$when[i])))),
        shiny::div(class = "caagro-muted", attr(s, "note")))
    })
    for (m in modules) local({
      mm <- m
      shiny::observeEvent(input[[paste0("go_", mm)]], bslib::nav_select("main_nav", mm, session = parent_session))
    })
  })
}

caagro_app <- function(modules = names(module_registry()), home = TRUE) {
  reg <- module_registry()
  modules <- intersect(modules, names(reg))
  app_resources()
  panels <- lapply(modules, function(m) bslib::nav_panel(title = reg[[m]]$label, value = m, icon = shiny::icon(reg[[m]]$icon), reg[[m]]$ui(m)))
  if (home) panels <- c(list(bslib::nav_panel("Inicio", value = "home", icon = shiny::icon("house"), mod_home_ui("home", modules))), panels)
  title <- shiny::tags$span(class = "caagro-brand", shiny::tags$img(src = "caagro-www/logo-white.svg", height = "26px", alt = ""),
                            shiny::span("caagro"), if (length(modules) == 1 && !home) shiny::span(class = "caagro-brand-sub", reg[[modules]]$label))
  ui <- do.call(bslib::page_navbar, c(
    list(id = "main_nav", title = title, window_title = "caagro", theme = caagro_bs_theme(), fillable = FALSE,
         header = app_head(), bg = caagro_palette()[["azul_profundo"]], inverse = TRUE),
    panels,
    list(bslib::nav_spacer(),
         bslib::nav_item(shiny::div(class = "caagro-sci-toggle", bslib::input_switch("sci_mode", "Modo cientifico", FALSE))),
         bslib::nav_item(bslib::input_dark_mode(id = "dark", mode = "light")))
  ))
  server <- function(input, output, session) {
    sci <- shiny::reactive(isTRUE(input$sci_mode))
    if (home) mod_home_server("home", modules, session)
    for (m in modules) reg[[m]]$server(m, sci_mode = sci)
  }
  shiny::shinyApp(ui, server)
}

launch <- function(app, launch.browser, ...) {
  if (!interactive() && identical(launch.browser, "auto")) return(app)
  shiny::runApp(app, launch.browser = if (identical(launch.browser, "auto")) TRUE else launch.browser, ...)
}

#' Launch the caagro applications
#'
#' `run_app()` opens the main application with all modules and the analysis
#' assistant. The other functions open a single module with the same visual
#' identity, navigation and export features. All computations are done
#' locally by the same R functions available for scripting.
#'
#' @param launch.browser Passed to [shiny::runApp()]. In non-interactive
#'   sessions the default (`"auto"`) returns the `shiny.appobj` without
#'   running it (useful for testing and deployment).
#' @param ... Further arguments for [shiny::runApp()] (e.g. `port`).
#' @return Runs the application; invisibly returns the app object when not
#'   launched.
#' @examples
#' app <- run_app(launch.browser = "auto")
#' class(app)
#' \dontrun{
#' run_app()
#' run_anova()
#' }
#' @export
run_app <- function(launch.browser = "auto", ...) launch(caagro_app(), launch.browser, ...)

#' @rdname run_app
#' @export
run_anova <- function(launch.browser = "auto", ...) launch(caagro_app("anova", home = FALSE), launch.browser, ...)

#' @rdname run_app
#' @export
run_regression <- function(launch.browser = "auto", ...) launch(caagro_app("regression", home = FALSE), launch.browser, ...)

#' @rdname run_app
#' @export
run_nonparametric <- function(launch.browser = "auto", ...) launch(caagro_app("nonparametric", home = FALSE), launch.browser, ...)

#' @rdname run_app
#' @export
run_confidence <- function(launch.browser = "auto", ...) launch(caagro_app("confidence", home = FALSE), launch.browser, ...)

#' @rdname run_app
#' @export
run_multivariate <- function(launch.browser = "auto", ...) launch(caagro_app("multivariate", home = FALSE), launch.browser, ...)

#' @rdname run_app
#' @export
run_exploratory <- function(launch.browser = "auto", ...) launch(caagro_app("exploratory", home = FALSE), launch.browser, ...)

#' Legacy launchers (caagro <= 0.2)
#'
#' Kept for backward compatibility. `anQuali()` opens the parametric module
#' (qualitative factors: ANOVA and multiple comparisons), `anQuant()` the
#' regression module (quantitative factors) and `anIC()` the confidence
#' interval module. The argument `iniciar` keeps its original meaning.
#'
#' Differences from the legacy applications (intentional corrections):
#' outliers are never removed or replaced automatically; the confidence level
#' is used as entered (the legacy `anQuant` app used `1 - level`); curves are
#' fitted to individual observations by default (fitting to means remains
#' available as an option); the Shapiro-Wilk test is applied to model
#' residuals.
#'
#' @param iniciar Logical. If `TRUE`, launches the application.
#' @return Launches the application (or a message when `iniciar = FALSE`).
#' @examples
#' anQuali(iniciar = FALSE)
#' @name legacy
NULL

#' @rdname legacy
#' @export
anQuali <- function(iniciar = TRUE) {
  if (isTRUE(iniciar)) run_anova(launch.browser = TRUE) else message("Defina iniciar = TRUE para rodar o aplicativo Shiny (equivalente a run_anova()).")
}

#' @rdname legacy
#' @export
anQuant <- function(iniciar = TRUE) {
  if (isTRUE(iniciar)) run_regression(launch.browser = TRUE) else message("Defina iniciar = TRUE para rodar o aplicativo Shiny (equivalente a run_regression()).")
}

#' @rdname legacy
#' @export
anIC <- function(iniciar = TRUE) {
  if (isTRUE(iniciar)) run_confidence(launch.browser = TRUE) else message("Defina iniciar = TRUE para rodar o aplicativo Shiny (equivalente a run_confidence()).")
}
