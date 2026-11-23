# UI helpers shared by all caagro applications -------------------------------

caagro_bs_theme <- function() {
  pal <- caagro_palette()
  bslib::bs_theme(
    version = 5,
    bg = pal[["fundo"]], fg = pal[["azul_profundo"]],
    primary = pal[["azul_secundario"]], secondary = pal[["cinza_tecnico"]],
    success = pal[["verde_cientifico"]], info = pal[["verde_suave"]], warning = pal[["laranja"]],
    base_font = bslib::font_collection("Inter", "system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    heading_font = bslib::font_collection("Inter", "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"),
    code_font = bslib::font_collection("JetBrains Mono", "SFMono-Regular", "Menlo", "Consolas", "monospace"),
    "border-radius" = "0.5rem", "card-border-radius" = "0.75rem", "font-size-base" = "0.925rem",
    "navbar-bg" = pal[["azul_profundo"]]
  )
}

app_head <- function() {
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", href = "caagro-www/caagro.css"),
    shiny::tags$link(rel = "icon", href = "caagro-www/logo.svg")
  )
}

app_resources <- function() {
  www <- system.file("app", "www", package = "caagro")
  if (nzchar(www)) shiny::addResourcePath("caagro-www", www)
}

stat_card <- function(title, value, sub = NULL, tone = c("default", "good", "warn")) {
  tone <- match.arg(tone)
  shiny::div(class = paste("caagro-stat", paste0("tone-", tone)),
             shiny::div(class = "caagro-stat-title", title),
             shiny::div(class = "caagro-stat-value", value),
             if (!is.null(sub)) shiny::div(class = "caagro-stat-sub", sub))
}

stat_row <- function(...) shiny::div(class = "caagro-stat-row", ...)

notes_ui <- function(res, show = TRUE) {
  if (is.null(res)) return(NULL)
  items <- list()
  if (isTRUE(show) && length(res$notes)) {
    icons <- c(Resultado = "R", Interpretacao = "I", Cuidados = "!")
    for (nm in names(res$notes)) {
      items[[length(items) + 1]] <- shiny::div(class = paste("caagro-note", paste0("note-", tolower(nm))),
                                               shiny::span(class = "caagro-note-tag", nm), shiny::span(res$notes[[nm]]))
    }
  }
  if (length(res$warnings)) {
    items[[length(items) + 1]] <- shiny::div(class = "caagro-warn",
                                             shiny::strong("Advertencias"), shiny::tags$ul(lapply(res$warnings, shiny::tags$li)))
  }
  shiny::tagList(items)
}

check_ui <- function(chk) {
  if (is.null(chk) || !nrow(chk)) return(shiny::div(class = "caagro-ok", "Estrutura dos dados validada."))
  cls <- c(error = "caagro-msg-error", warning = "caagro-msg-warn", info = "caagro-msg-info")
  lab <- c(error = "Erro", warning = "Alerta", info = "Info")
  shiny::tagList(lapply(seq_len(nrow(chk)), function(i)
    shiny::div(class = cls[[chk$level[i]]], shiny::strong(lab[[chk$level[i]]], ": "), chk$message[i])))
}

dt_table <- function(df, digits = 4, page = 15, filter = FALSE) {
  if (is.null(df) || !is.data.frame(df)) return(NULL)
  df <- as.data.frame(df)
  df[] <- lapply(df, function(z) if (is.factor(z)) as.character(z) else z)
  num <- names(df)[vapply(df, function(z) is.double(z) && any(abs(z - round(z)) > 1e-9, na.rm = TRUE), logical(1))]
  pcols <- intersect(num, c("p", "p_adj", "p_unadj", "p_bonferroni", "p_model"))
  num <- setdiff(num, pcols)
  for (pc in pcols) df[[pc]] <- fmt_p(df[[pc]])
  dt <- DT::datatable(df, rownames = FALSE, class = "compact hover caagro-dt",
                      options = list(pageLength = page, dom = paste0(if (filter) "f" else "", if (nrow(df) > page) "tip" else "t"), scrollX = TRUE,
                                     columnDefs = list(list(className = "dt-right", targets = which(names(df) %in% c(num, pcols)) - 1))))
  if (length(num)) dt <- DT::formatRound(dt, num, digits = digits)
  dt
}

safe_run <- function(expr, session = shiny::getDefaultReactiveDomain()) {
  tryCatch(expr, error = function(e) {
    shiny::showNotification(conditionMessage(e), type = "error", duration = 10)
    NULL
  })
}

example_datasets <- function() {
  c("PlantGrowth (real, DIC)" = "PlantGrowth", "npk (real, fatorial em blocos)" = "npk",
    "warpbreaks (real, fatorial)" = "warpbreaks", "sleep (real, pareado)" = "sleep",
    "sim_dic (simulado)" = "sim_dic", "sim_dbc (simulado)" = "sim_dbc", "sim_fatorial (simulado)" = "sim_fatorial",
    "sim_dose (simulado, regressao)" = "sim_dose", "sim_germinacao (simulado, binomial)" = "sim_germinacao",
    "sim_multivariado (simulado)" = "sim_multivariado")
}

get_example <- function(name) {
  env <- new.env()
  if (name %in% c("PlantGrowth", "npk", "warpbreaks", "sleep")) {
    utils::data(list = name, package = "datasets", envir = env)
  } else {
    utils::data(list = name, package = "caagro", envir = env)
  }
  d <- as.data.frame(get(name, envir = env))
  if (name == "sleep") d$ID <- as.character(d$ID)
  d
}

col_choices <- function(d, numeric_only = FALSE, factor_only = FALSE) {
  if (is.null(d)) return(character())
  nm <- names(d)
  if (numeric_only) nm <- nm[vapply(d, is.numeric, logical(1))]
  if (factor_only) nm <- nm[!vapply(d, is.double, logical(1)) | vapply(d, function(z) length(unique(z)) <= 12, logical(1))]
  nm
}

none_or <- function(x) if (is.null(x) || identical(x, "") || identical(x, "(nenhum)")) NULL else x

guess_factor <- function(d, exclude = NULL) {
  nm <- setdiff(names(d), exclude)
  f <- nm[!vapply(d[nm], is.numeric, logical(1))]
  if (length(f)) f[1] else nm[1]
}
