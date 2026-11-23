# Applications ---------------------------------------------------------------

module_registry <- function() {
  list(
    exploratory = list(label = "Explorat\u00f3ria", icon = "magnifying-glass-chart", ui = mod_exploratory_ui, server = mod_exploratory_server,
                       desc = "Primeiro olhar sobre os dados: m\u00e9dias, dispers\u00e3o, gr\u00e1ficos e correla\u00e7\u00f5es."),
    anova = list(label = "ANOVA", icon = "table-cells", ui = mod_anova_ui, server = mod_anova_server,
                 desc = "Compare tratamentos em DIC, DBC ou fatorial, com Tukey, Scott-Knott e outros testes."),
    regression = list(label = "Regress\u00e3o", icon = "chart-line", ui = mod_regression_ui, server = mod_regression_server,
                      desc = "Ajuste curvas de resposta a doses ou tempo e encontre a dose de m\u00e1xima efici\u00eancia."),
    nonparametric = list(label = "N\u00e3o param\u00e9trica", icon = "ranking-star", ui = mod_nonparametric_ui, server = mod_nonparametric_server,
                         desc = "Compare tratamentos com notas, escalas ou dados muito assim\u00e9tricos."),
    confidence = list(label = "Intervalos", icon = "arrows-left-right-to-line", ui = mod_confidence_ui, server = mod_confidence_server,
                      desc = "Estime m\u00e9dias, diferen\u00e7as, vari\u00e2ncias e propor\u00e7\u00f5es com intervalo de confian\u00e7a."),
    multivariate = list(label = "Multivariada", icon = "circle-nodes", ui = mod_multivariate_ui, server = mod_multivariate_server,
                        desc = "Resuma muitas vari\u00e1veis (PCA), agrupe indiv\u00edduos e veja correla\u00e7\u00f5es.")
  )
}

mod_home_ui <- function(id, modules) {
  ns <- shiny::NS(id)
  reg <- module_registry()[modules]
  shiny::div(class = "caagro-home",
    shiny::div(class = "caagro-hero",
      shiny::tags$img(src = "caagro-www/logo.svg", class = "caagro-hero-logo", alt = "caagro"),
      shiny::div(shiny::h2("caagro"), shiny::p("An\u00e1lises estat\u00edsticas para experimenta\u00e7\u00e3o agr\u00edcola."),
                 shiny::p(class = "caagro-muted", "Escolha um m\u00f3dulo, clique em Carregar exemplo e depois em Executar an\u00e1lise. Os dados s\u00e3o processados apenas no seu computador."))),
    shiny::div(class = "caagro-module-grid", lapply(names(reg), function(m)
      shiny::actionLink(ns(paste0("go_", m)), class = "caagro-module-card",
                        label = shiny::tagList(shiny::div(class = "caagro-module-icon", shiny::icon(reg[[m]]$icon)),
                                               shiny::div(shiny::strong(reg[[m]]$label), shiny::div(class = "caagro-muted", reg[[m]]$desc)))))),
    details_panel("N\u00e3o sabe qual an\u00e1lise usar? Responda a tr\u00eas perguntas",
      bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, md = 4),
        shiny::selectInput(ns("goal"), "O que voc\u00ea quer fazer?", c("Comparar tratamentos" = "compare", "Ajustar uma curva (doses, tempo)" = "curve",
                                                                   "Relacionar vari\u00e1veis" = "relationship", "Explorar os dados" = "explore", "Analisar muitas vari\u00e1veis juntas" = "multivariate"), width = "100%"),
        shiny::selectInput(ns("response"), "Como \u00e9 a vari\u00e1vel-resposta?", c("Medida cont\u00ednua (kg, cm, sc/ha)" = "continuous", "Sim/n\u00e3o ou propor\u00e7\u00e3o" = "binary",
                                                                            "Contagem" = "count", "Nota ou escala" = "ordinal"), width = "100%"),
        shiny::selectInput(ns("design"), "Como \u00e9 o experimento?", c("DIC (sem blocos)" = "dic", "DBC (com blocos)" = "dbc", "Fatorial" = "fat",
                                                                    "Mesma unidade medida duas vezes" = "paired"), width = "100%")),
      shiny::uiOutput(ns("suggestions")))
  )
}

mod_home_server <- function(id, modules, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    output$suggestions <- shiny::renderUI({
      s <- suggest_method(input$goal, input$response, n_groups = 4, related = identical(input$design, "paired"),
                          blocked = identical(input$design, "dbc"), factorial = identical(input$design, "fat"))
      s <- utils::head(s, 3)
      shiny::tagList(lapply(seq_len(nrow(s)), function(i) shiny::div(class = "caagro-suggestion",
        shiny::div(shiny::span(class = "caagro-rank", i), shiny::strong(s$method[i]), shiny::span(class = "caagro-muted", paste0(" - m\u00f3dulo ", app_label(s$app[i])))),
        shiny::div(shiny::tags$small(shiny::tags$b("Quando usar: "), s$when[i])),
        shiny::div(shiny::tags$small(shiny::tags$b("Pressupostos: "), s$assumptions[i])))),
        shiny::p(class = "caagro-muted", "Sugest\u00f5es baseadas em regras. A escolha final depende do planejamento do experimento."))
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
  if (home) panels <- c(list(bslib::nav_panel("In\u00edcio", value = "home", icon = shiny::icon("house"), mod_home_ui("home", modules))), panels)
  title <- shiny::tags$span(class = "caagro-brand", shiny::tags$img(src = "caagro-www/logo-white.svg", height = "26px", alt = ""),
                            shiny::span("caagro"), if (length(modules) == 1 && !home) shiny::span(class = "caagro-brand-sub", reg[[modules]]$label))
  ui <- do.call(bslib::page_navbar, c(
    list(id = "main_nav", title = title, window_title = "caagro", theme = caagro_bs_theme(), fillable = FALSE,
         header = app_head(), bg = caagro_palette()[["azul_profundo"]], inverse = TRUE),
    panels,
    list(bslib::nav_spacer(),
         bslib::nav_item(bslib::input_dark_mode(id = "dark", mode = "light")))
  ))
  server <- function(input, output, session) {
    if (home) mod_home_server("home", modules, session)
    for (m in modules) reg[[m]]$server(m)
  }
  shiny::shinyApp(ui, server)
}

launch <- function(app, launch.browser, ...) {
  if (!interactive() && identical(launch.browser, "auto")) return(app)
  shiny::runApp(app, launch.browser = if (identical(launch.browser, "auto")) TRUE else launch.browser, ...)
}

#' Open the caagro application
#'
#' `run_app()` is the single entry point of caagro. It opens the analysis
#' platform in the browser, with the modules Exploratory analysis, ANOVA
#' (Fisher analysis of variance for completely randomised, randomised block
#' and factorial designs, with Tukey, Scott-Knott, SNK, LSD, Dunnett,
#' Shapiro-Wilk and Bartlett), Regression (linear, polynomial, non-linear and
#' binomial models), Non-parametric tests, Confidence intervals and
#' Multivariate analysis (PCA, clustering, correlation). Every module has a
#' "Carregar exemplo" button, a graphics editor and export to Excel, report
#' and a reproducible R script. All computations run locally.
#'
#' @param modulo Module to open: `"todos"` (default, home page with all
#'   modules), `"anova"`, `"regressao"`, `"nao_parametrica"`, `"intervalos"`,
#'   `"multivariada"` or `"exploratoria"`.
#' @param launch.browser Passed to [shiny::runApp()]. In non-interactive
#'   sessions the default (`"auto"`) returns the `shiny.appobj` without
#'   running it.
#' @param ... Further arguments for [shiny::runApp()] (e.g. `port`).
#' @return Runs the application; invisibly returns the app object when not
#'   launched.
#' @references
#' Fisher, R. A. (1935). *The Design of Experiments*. Oliver and Boyd.
#'
#' Montgomery, D. C. (2017). *Design and Analysis of Experiments* (9th ed.).
#' Wiley.
#' @examples
#' app <- run_app(launch.browser = "auto")
#' class(app)
#' \dontrun{
#' run_app()
#' run_app("anova")
#' }
#' @export
run_app <- function(modulo = c("todos", "anova", "regressao", "nao_parametrica", "intervalos", "multivariada", "exploratoria"),
                    launch.browser = "auto", ...) {
  modulo <- match.arg(modulo)
  key <- c(anova = "anova", regressao = "regression", nao_parametrica = "nonparametric", intervalos = "confidence",
           multivariada = "multivariate", exploratoria = "exploratory")
  app <- if (modulo == "todos") caagro_app() else caagro_app(key[[modulo]], home = FALSE)
  launch(app, launch.browser, ...)
}

app_label <- function(x) {
  map <- c("run_anova()" = "ANOVA", "run_regression()" = "Regress\u00e3o", "run_nonparametric()" = "N\u00e3o param\u00e9trica",
           "run_confidence()" = "Intervalos", "run_multivariate()" = "Multivariada", "run_exploratory()" = "Explorat\u00f3ria")
  out <- unname(map[x]); ifelse(is.na(out), x, out)
}
