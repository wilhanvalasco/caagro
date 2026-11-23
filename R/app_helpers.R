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
    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    shiny::tags$link(rel = "stylesheet", href = "caagro-www/caagro.css"),
    shiny::tags$link(rel = "icon", type = "image/svg+xml", href = "caagro-www/logo.svg"),
    # close the mobile menu after choosing a module
    shiny::tags$script(shiny::HTML("document.addEventListener('click', function(e) {
      var link = e.target.closest('.navbar-collapse .nav-link');
      var menu = document.querySelector('.navbar-collapse.show, .navbar-collapse.in');
      if (link && menu && window.bootstrap && bootstrap.Collapse) bootstrap.Collapse.getOrCreateInstance(menu).hide();
    });"))
  )
}

app_resources <- function() {
  www <- system.file("app", "www", package = "caagro")
  if (nzchar(www)) shiny::addResourcePath("caagro-www", www)
}

# Brazilian number formatting for the interface -------------------------------
fmt_br <- function(x, digits = 2) {
  ifelse(is.na(x), "-", formatC(x, format = "f", digits = digits, decimal.mark = ",", big.mark = "."))
}
fmt_p_br <- function(p) {
  ifelse(is.na(p), "-", ifelse(p < 0.001, "< 0,001", formatC(p, format = "f", digits = 3, decimal.mark = ",")))
}

# Portuguese display names for result columns (the R objects keep the API names)
col_labels_pt <- c(
  term = "Fonte de varia\u00e7\u00e3o", df = "GL", ss = "SQ", ms = "QM", F = "F", p = "Valor-p", signif = "Sig.",
  partial_eta_sq = "Eta\u00b2 parcial", eta_sq = "Eta\u00b2", partial_omega_sq = "\u00d4mega\u00b2 parcial",
  n = "n", mean = "M\u00e9dia observada", sd = "Desvio-padr\u00e3o", se_raw = "EP (observado)", emmean = "M\u00e9dia",
  se = "Erro-padr\u00e3o", lower = "Limite inferior", upper = "Limite superior", median = "Mediana",
  letras = "Grupo", vs_controle = "Versus controle", contrast = "Compara\u00e7\u00e3o", level1 = "N\u00edvel 1", level2 = "N\u00edvel 2",
  diff = "Diferen\u00e7a", statistic = "Estat\u00edstica", p_adj = "Valor-p ajustado", p_unadj = "Valor-p (n\u00e3o ajustado)",
  significant = "Significativa", range_p = "M\u00e9dias abrangidas", estimate = "Estimativa",
  grand_mean = "M\u00e9dia geral", mse = "QM do res\u00edduo", root_mse = "Raiz do QM", df_error = "GL do res\u00edduo",
  cv_percent = "CV (%)", r_squared = "R\u00b2", ss_type = "Tipo de SQ", balanced = "Balanceado",
  test = "Teste", assumption = "Pressuposto", interpretation = "Interpreta\u00e7\u00e3o", conclusion = "Conclus\u00e3o",
  fitted = "Valor ajustado", residual = "Res\u00edduo", std_residual = "Res\u00edduo padronizado", rstudent = "Res\u00edduo studentizado",
  leverage = "Alavancagem", cooks = "Dist\u00e2ncia de Cook", p_bonferroni = "Valor-p (Bonferroni)", row = "Linha", flag = "Sinalizada",
  model = "Modelo", group = "Grupo", k = "Par\u00e2metros", r2 = "R\u00b2", r2_type = "Tipo de R\u00b2", r2_adj = "R\u00b2 ajustado",
  rmse = "RMSE", mae = "MAE", mse_fit = "MSE", sigma = "Erro-padr\u00e3o residual", deviance = "Desvio", df_resid = "GL do res\u00edduo",
  dispersion = "Raz\u00e3o de dispers\u00e3o", logLik = "Log-verossimilhan\u00e7a", p_model = "Valor-p do modelo",
  converged = "Convergiu", iterations = "Itera\u00e7\u00f5es", delta_AICc = "\u0394 AICc", weight = "Peso de Akaike",
  equation = "Equa\u00e7\u00e3o", quantity = "Grandeza", within_range = "Dentro da faixa", status = "Situa\u00e7\u00e3o",
  x = "x", ci_half = "Meia-amplitude do IC", parameter = "Par\u00e2metro", conf_level = "N\u00edvel de confian\u00e7a",
  method = "M\u00e9todo", alpha = "Alfa", replications = "Repeti\u00e7\u00f5es", dms = "DMS", note = "Observa\u00e7\u00e3o",
  mean_rank = "Posto m\u00e9dio", q1 = "Q1", q3 = "Q3", iqr = "Amplitude interquartil", rank_sum = "Soma dos postos",
  epsilon_sq = "\u00c9psilon\u00b2", eta_sq_H = "Eta\u00b2 (H)", kendall_w = "W de Kendall", blocks = "Blocos",
  hodges_lehmann = "Estimador de Hodges-Lehmann", rank_biserial = "Correla\u00e7\u00e3o rank-bisserial",
  grand_median = "Mediana geral", permutations = "Permuta\u00e7\u00f5es", seed = "Semente",
  variable = "Vari\u00e1vel", eigenvalue = "Autovalor", variance = "Vari\u00e2ncia (%)", cumulative = "Acumulada (%)",
  label = "Indiv\u00edduo", cluster = "Grupo", silhouette = "Silhueta", size = "Tamanho", within_ss = "SQ dentro",
  mean_silhouette = "Silhueta m\u00e9dia", between_over_total = "SQ entre / SQ total", cophenetic_cor = "Correla\u00e7\u00e3o cofen\u00e9tica",
  distance = "Dist\u00e2ncia", scaled = "Padronizado", var1 = "Vari\u00e1vel 1", var2 = "Vari\u00e1vel 2", r = "r",
  missing = "Ausentes", min = "M\u00ednimo", max = "M\u00e1ximo", range = "Amplitude", skewness = "Assimetria", kurtosis = "Curtose",
  level = "N\u00edvel", percent = "Percentual (%)", successes = "Sucessos", bias = "Vi\u00e9s", se_boot = "EP bootstrap",
  type = "Tipo", R = "Reamostragens", df1 = "GL 1", df2 = "GL 2", t = "t", cohen_d = "d de Cohen", hedges_g = "g de Hedges",
  alternative = "Hip\u00f3tese alternativa", step = "Passo", height = "Altura", n_nonzero = "Pares n\u00e3o nulos",
  median_diff = "Mediana das diferen\u00e7as", pairs = "Pares"
)

pt_names <- function(df) {
  nm <- names(df)
  lab <- unname(col_labels_pt[nm])
  names(df) <- ifelse(is.na(lab), nm, lab)
  names(df) <- make.unique(names(df), sep = " ")
  df
}

pt_values <- function(df) {
  for (cl in names(df)) {
    if (is.character(df[[cl]]) || is.factor(df[[cl]])) {
      v <- as.character(df[[cl]])
      v[v == "Residuals"] <- "Res\u00edduo"
      df[[cl]] <- v
    }
    if (is.logical(df[[cl]])) df[[cl]] <- ifelse(df[[cl]], "Sim", "N\u00e3o")
  }
  df
}

# Tables in the interface: Portuguese headers, decimal comma, horizontal scroll
dt_table <- function(df, digits = 3, page = 15, filter = FALSE, drop = NULL) {
  if (is.null(df) || !is.data.frame(df)) return(NULL)
  df <- as.data.frame(df)
  df <- df[, setdiff(names(df), c(".row", drop)), drop = FALSE]
  df[] <- lapply(df, function(z) if (is.factor(z)) as.character(z) else z)
  pcols <- intersect(names(df), c("p", "p_adj", "p_unadj", "p_bonferroni", "p_model"))
  for (pc in pcols) df[[pc]] <- fmt_p_br(df[[pc]])
  df <- pt_values(df)
  num <- names(df)[vapply(df, function(z) is.double(z) && any(abs(z - round(z)) > 1e-9, na.rm = TRUE), logical(1))]
  right <- which(names(df) %in% c(num, pcols, names(df)[vapply(df, is.numeric, logical(1))])) - 1
  num_idx <- match(num, names(df))
  df <- pt_names(df)
  dt <- DT::datatable(df, rownames = FALSE, class = "compact hover caagro-dt", selection = "none",
                      options = list(pageLength = page, dom = paste0(if (filter) "f" else "", if (nrow(df) > page) "tip" else "t"),
                                     scrollX = TRUE, autoWidth = FALSE,
                                     language = list(search = "Buscar:", info = "_START_ a _END_ de _TOTAL_", infoEmpty = "",
                                                     paginate = list(previous = "Anterior", `next` = "Pr\u00f3xima"), zeroRecords = "Nenhum registro",
                                                     emptyTable = "Sem dados"),
                                     columnDefs = list(list(className = "dt-right", targets = right))))
  if (length(num_idx)) dt <- DT::formatRound(dt, num_idx, digits = digits, mark = ".", dec.mark = ",")
  dt
}

# Compact summary strip (replaces many cards)
summary_strip <- function(items, sentence = NULL, tone = "default") {
  items <- Filter(Negate(is.null), items)
  shiny::div(class = paste("caagro-summary", paste0("tone-", tone)),
             shiny::div(class = "caagro-summary-items",
                        lapply(names(items), function(nm) shiny::div(class = "caagro-summary-item",
                                                                     shiny::span(class = "caagro-summary-label", nm),
                                                                     shiny::span(class = "caagro-summary-value", items[[nm]])))),
             if (!is.null(sentence)) shiny::p(class = "caagro-summary-text", sentence))
}

note_titles <- c(Resultado = "Resultado", Interpretacao = "Interpreta\u00e7\u00e3o", Cuidados = "Cuidados")

# Notes go into a collapsed panel; warnings stay visible but compact
notes_ui <- function(res, collapsed = TRUE, show_warnings = TRUE) {
  if (is.null(res)) return(NULL)
  items <- lapply(names(res$notes), function(nm)
    shiny::div(class = "caagro-note", shiny::span(class = "caagro-note-tag", note_titles[nm] %||% nm), shiny::span(res$notes[[nm]])))
  out <- list()
  if (show_warnings && length(res$warnings)) {
    out[[1]] <- shiny::div(class = "caagro-warn", shiny::strong("Aten\u00e7\u00e3o"), shiny::tags$ul(lapply(res$warnings, shiny::tags$li)))
  }
  if (length(items)) {
    out[[length(out) + 1]] <- bslib::accordion(open = !collapsed, class = "caagro-accordion",
                                               bslib::accordion_panel("Interpreta\u00e7\u00e3o e cuidados", icon = shiny::icon("book-open"), items))
  }
  shiny::tagList(out)
}

warnings_ui <- function(w) {
  w <- unique(w[nzchar(w)])
  if (!length(w)) return(NULL)
  shiny::div(class = "caagro-warn", shiny::strong("Aten\u00e7\u00e3o"), shiny::tags$ul(lapply(w, shiny::tags$li)))
}

check_ui <- function(chk) {
  if (is.null(chk) || !nrow(chk)) return(NULL)
  chk <- chk[chk$level != "info", , drop = FALSE]
  if (!nrow(chk)) return(NULL)
  cls <- c(error = "caagro-msg-error", warning = "caagro-msg-warn")
  lab <- c(error = "Erro", warning = "Aten\u00e7\u00e3o")
  shiny::tagList(lapply(seq_len(nrow(chk)), function(i)
    shiny::div(class = cls[[chk$level[i]]], shiny::strong(lab[[chk$level[i]]], ": "), chk$message[i])))
}

details_panel <- function(title, ...) {
  bslib::accordion(open = FALSE, class = "caagro-accordion", bslib::accordion_panel(title, ...))
}

empty_state <- function(text = "Carregue um exemplo ou importe uma planilha, escolha as vari\u00e1veis e clique em Executar an\u00e1lise.") {
  shiny::div(class = "caagro-empty", shiny::icon("circle-info"), shiny::span(text))
}

step_title <- function(n, text) shiny::div(class = "caagro-step", shiny::span(class = "caagro-step-n", n), shiny::span(text))

safe_run <- function(expr, session = shiny::getDefaultReactiveDomain()) {
  tryCatch(expr, error = function(e) {
    shiny::showNotification(conditionMessage(e), type = "error", duration = 10)
    NULL
  })
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

col_choices <- function(d, numeric_only = FALSE) {
  if (is.null(d)) return(character())
  nm <- names(d)
  if (numeric_only) nm <- nm[vapply(d, is.numeric, logical(1))]
  nm
}

guess_factor <- function(d, exclude = NULL) {
  nm <- setdiff(names(d), exclude)
  f <- nm[!vapply(d[nm], is.numeric, logical(1))]
  if (length(f)) f[1] else nm[1]
}

none_or <- function(x) if (is.null(x) || identical(x, "") || identical(x, "(nenhum)") || identical(x, "(nenhuma)")) NULL else x

pick <- function(preset, name, default) {
  v <- preset[[name]]
  if (is.null(v)) default else v
}

# Table output that grows with its content (no fixed empty area)
dt_out <- function(id) DT::DTOutput(id, height = "auto", fill = FALSE)
