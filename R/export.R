#' Save a plot in publication quality
#'
#' The plot is re-rendered at the final size and resolution (never a screen
#' capture), so text, lines and proportions are preserved. The format is
#' taken from the file extension: `.png`, `.tiff`/`.tif`, `.svg`, `.pdf`.
#' PNG/TIFF use \pkg{ragg} when installed; SVG uses \pkg{svglite} when
#' installed; PDF uses `cairo_pdf` when available.
#'
#' @param plot A ggplot.
#' @param file Output path.
#' @param width,height Size.
#' @param units `"cm"`, `"mm"` or `"in"`.
#' @param dpi Resolution for raster formats (e.g. 300, 600, 1200).
#' @param bg `"white"` or `"transparent"` (PNG, TIFF, SVG, PDF).
#' @return The file path, invisibly.
#' @examples
#' p <- plot(anova_exp(PlantGrowth, "weight", "group"))
#' f <- tempfile(fileext = ".png")
#' save_plot(p, f, width = 12, height = 9, dpi = 150)
#' file.exists(f)
#' @export
save_plot <- function(plot, file, width = 17, height = 12, units = c("cm", "mm", "in"), dpi = 300,
                      bg = c("white", "transparent")) {
  units <- match.arg(units); bg <- match.arg(bg)
  if (!inherits(plot, "ggplot")) abort_caagro("`plot` deve ser um objeto ggplot.")
  if (!is.numeric(width) || !is.numeric(height) || width <= 0 || height <= 0) abort_caagro("Largura e altura devem ser positivas.")
  if (!is.numeric(dpi) || dpi < 72 || dpi > 2400) abort_caagro("Resolucao deve estar entre 72 e 2400 dpi.")
  ext <- tolower(tools::file_ext(file))
  bgcol <- if (bg == "transparent") "transparent" else "white"
  if (bg == "transparent") {
    plot <- plot + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                                  panel.background = ggplot2::element_rect(fill = "transparent", colour = NA))
  }
  dev <- switch(ext,
    png = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png",
    tif = , tiff = if (requireNamespace("ragg", quietly = TRUE)) {
      function(filename, width, height, units = "in", res = 300, bg = "white", ...)
        ragg::agg_tiff(filename, width = width, height = height, units = units, res = res, background = bg, compression = "lzw")
    } else {
      function(filename, width, height, units = "in", res = 300, bg = "white", ...)
        grDevices::tiff(filename, width = width, height = height, units = units, res = res, bg = bg, compression = "lzw")
    },
    svg = if (requireNamespace("svglite", quietly = TRUE)) svglite::svglite else grDevices::svg,
    pdf = if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else function(filename, ...) grDevices::pdf(filename, useDingbats = FALSE, ...),
    abort_caagro("Formato nao suportado: '.", ext, "'. Use png, tiff, svg ou pdf."))
  ggplot2::ggsave(filename = file, plot = plot, device = dev, width = width, height = height, units = units,
                  dpi = dpi, bg = bgcol, limitsize = FALSE)
  invisible(file)
}

table_labels <- c(anova = "ANOVA", precision = "Precisao experimental", means = "Medias", effect_size = "Tamanho de efeito",
                  pairs = "Comparacoes multiplas", tests = "Diagnosticos", influence = "Influencia", coefficients = "Coeficientes",
                  fit_stats = "Indicadores de ajuste", equations = "Equacoes", predictions = "Predicoes", comparison = "Comparacao de modelos",
                  curve_test = "Igualdade de curvas", failures = "Ajustes com falha", agronomic = "Grandezas agronomicas",
                  test = "Teste", descriptives = "Descritiva", contingency = "Contingencia", interval = "Intervalo",
                  intervals = "Intervalos", homogeneity = "Homogeneidade", eigenvalues = "Autovalores", loadings = "Cargas",
                  correlations = "Correlacoes var-CP", contributions = "Contribuicoes", scores = "Escores", quality = "Qualidade",
                  merge_heights = "Alturas de fusao", clusters = "Grupos", centers = "Centroides", summary = "Resumo dos grupos",
                  selection = "Selecao de k", fit = "Ajuste", matrix = "Matriz", statistics = "Estatisticas", frequencies = "Frequencias",
                  contrasts = "Contrastes")

as_result_list <- function(results) {
  if (inherits(results, "caagro_result")) results <- list(results)
  if (!is.list(results) || !all(vapply(results, inherits, logical(1), "caagro_result"))) {
    abort_caagro("`results` deve ser um resultado caagro ou uma lista de resultados.")
  }
  results
}

#' Export results to a structured Excel workbook
#'
#' Creates an `.xlsx` workbook with original data, data used, descriptive
#' statistics, every result table (one sheet each, with filters and numeric
#' formats) and a methodology/reproducibility sheet. Data remain structured
#' and reusable (no merged cells, one header row).
#'
#' @param results A `caagro_result` or a list of them.
#' @param file Output `.xlsx` path.
#' @param digits Number of decimals in the numeric display format (values are
#'   stored at full precision).
#' @return The file path, invisibly.
#' @examples
#' fit <- anova_exp(PlantGrowth, "weight", "group")
#' f <- tempfile(fileext = ".xlsx")
#' export_excel(list(fit, compare_means(fit)), f)
#' @export
export_excel <- function(results, file, digits = 4) {
  results <- as_result_list(results)
  wb <- openxlsx::createWorkbook(creator = "caagro")
  pal <- caagro_palette()
  hs <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = pal[["azul_profundo"]], textDecoration = "bold",
                              border = "Bottom", borderColour = pal[["verde_cientifico"]], halign = "center", valign = "center", wrapText = TRUE)
  ns <- openxlsx::createStyle(numFmt = paste0("0.", strrep("0", digits)))
  used <- character()
  sheet_name <- function(nm) {
    nm <- gsub("[][*?/\\\\:]", "-", nm); nm <- substr(nm, 1, 31)
    base <- nm; i <- 1
    while (tolower(nm) %in% tolower(used)) { i <- i + 1; nm <- paste0(substr(base, 1, 28), "_", i) }
    used <<- c(used, nm); nm
  }
  add <- function(name, df) {
    if (is.null(df) || !is.data.frame(df) || !ncol(df)) return()
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df[] <- lapply(df, function(z) if (is.factor(z)) as.character(z) else if (is.list(z)) vapply(z, paste, "", collapse = "; ") else z)
    sn <- sheet_name(name)
    openxlsx::addWorksheet(wb, sn, gridLines = FALSE)
    openxlsx::writeData(wb, sn, df, headerStyle = hs, withFilter = nrow(df) > 0)
    numc <- which(vapply(df, is.double, logical(1)))
    if (length(numc) && nrow(df)) openxlsx::addStyle(wb, sn, ns, rows = 2:(nrow(df) + 1), cols = numc, gridExpand = TRUE)
    openxlsx::setColWidths(wb, sn, cols = seq_len(ncol(df)), widths = "auto")
    openxlsx::freezePane(wb, sn, firstRow = TRUE)
  }
  r1 <- results[[1]]
  if (is.data.frame(r1$data)) add("Dados originais", r1$data)
  if (is.data.frame(r1$data_used)) add("Dados utilizados", r1$data_used)
  resp <- r1$settings$response %||% r1$settings$y
  if (is.data.frame(r1$data_used) && !is.null(resp) && resp %in% names(r1$data_used)) {
    grp <- r1$settings$treatment %||% r1$settings$group
    if (!is.null(grp) && !grp %in% names(r1$data_used)) grp <- NULL
    add("Estatistica descritiva", tryCatch(describe_data(r1$data_used, resp, group = grp)$tables$statistics, error = function(e) NULL))
  }
  multi <- length(results) > 1
  for (i in seq_along(results)) {
    r <- results[[i]]
    for (tn in names(r$tables)) {
      lbl <- table_labels[tn]; if (is.na(lbl)) lbl <- tn
      add(if (multi) paste0(i, " ", lbl) else lbl, r$tables[[tn]])
    }
  }
  meth <- do.call(rbind, lapply(seq_along(results), function(i) {
    r <- results[[i]]
    st <- vapply(r$settings, function(z) if (is.null(z)) "" else paste(format(unlist(z)), collapse = ", "), character(1))
    rbind(data.frame(analise = i, item = "Metodo", valor = r$method),
          data.frame(analise = i, item = "Delineamento", valor = r$design %||% ""),
          data.frame(analise = i, item = "Modelo", valor = if (is.null(r$formula)) "" else paste(deparse(r$formula), collapse = " ")),
          data.frame(analise = i, item = paste0("Parametro: ", names(st)), valor = unname(st)),
          if (length(r$warnings)) data.frame(analise = i, item = "Advertencia", valor = r$warnings),
          if (length(r$notes)) data.frame(analise = i, item = names(r$notes), valor = unlist(r$notes)),
          data.frame(analise = i, item = c("caagro", "R", "Data", "Semente"),
                     valor = c(r$meta$caagro_version, r$meta$r_version, r$meta$time, as.character(r$meta$seed %||% ""))),
          data.frame(analise = i, item = "Codigo R", valor = r$code %||% ""))
  }))
  add("Metodologia e parametros", meth)
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

method_references <- function(r) {
  refs <- list(
    anova = c("Fisher, R. A. (1935). The Design of Experiments. Oliver and Boyd.",
              "Montgomery, D. C. (2017). Design and Analysis of Experiments (9th ed.). Wiley."),
    multcomp = c("Tukey, J. W. (1949). Comparing individual means in the analysis of variance. Biometrics, 5(2), 99-114. doi:10.2307/3001913",
                 "Scott, A. J. and Knott, M. (1974). A cluster analysis method for grouping means in the analysis of variance. Biometrics, 30(3), 507-512. doi:10.2307/2529204",
                 "Piepho, H.-P. (2004). An algorithm for a letter-based representation of all-pairwise comparisons. Journal of Computational and Graphical Statistics, 13(2), 456-466. doi:10.1198/1061860043515"),
    diagnostics = c("Shapiro, S. S. and Wilk, M. B. (1965). An analysis of variance test for normality. Biometrika, 52(3/4), 591-611. doi:10.2307/2333709"),
    regression = c("Bates, D. M. and Watts, D. G. (1988). Nonlinear Regression Analysis and Its Applications. Wiley. doi:10.1002/9780470316757",
                   "Akaike, H. (1974). A new look at the statistical model identification. IEEE Transactions on Automatic Control, 19(6), 716-723. doi:10.1109/TAC.1974.1100705",
                   "Schwarz, G. (1978). Estimating the dimension of a model. The Annals of Statistics, 6(2), 461-464. doi:10.1214/aos/1176344136"),
    np = c("Kruskal, W. H. and Wallis, W. A. (1952). Use of ranks in one-criterion variance analysis. JASA, 47(260), 583-621. doi:10.1080/01621459.1952.10483441",
           "Friedman, M. (1937). The use of ranks to avoid the assumption of normality implicit in the analysis of variance. JASA, 32(200), 675-701. doi:10.1080/01621459.1937.10503522",
           "Dunn, O. J. (1964). Multiple comparisons using rank sums. Technometrics, 6(3), 241-252. doi:10.1080/00401706.1964.10490181"),
    ci = c("Efron, B. and Tibshirani, R. J. (1993). An Introduction to the Bootstrap. Chapman and Hall/CRC. doi:10.1201/9780429246593"),
    ci_groups = c("Montgomery, D. C. (2017). Design and Analysis of Experiments (9th ed.). Wiley."),
    ttest = c("Student (1908). The probable error of a mean. Biometrika, 6(1), 1-25. doi:10.2307/2331554",
              "Welch, B. L. (1947). The generalization of 'Student's' problem when several different population variances are involved. Biometrika, 34(1/2), 28-35. doi:10.2307/2332510"),
    pca = c("Jolliffe, I. T. (2002). Principal Component Analysis (2nd ed.). Springer. doi:10.1007/b98835"),
    cluster = c("Ward, J. H. (1963). Hierarchical grouping to optimize an objective function. JASA, 58(301), 236-244. doi:10.1080/01621459.1963.10500845"),
    kmeans = c("Rousseeuw, P. J. (1987). Silhouettes. Journal of Computational and Applied Mathematics, 20, 53-65. doi:10.1016/0377-0427(87)90125-7"),
    cor = c("Spearman, C. (1904). The proof and measurement of association between two things. American Journal of Psychology, 15(1), 72-101. doi:10.2307/1412159"),
    describe = c("Joanes, D. N. and Gill, C. A. (1998). Comparing measures of sample skewness and kurtosis. JRSS D, 47(1), 183-189. doi:10.1111/1467-9884.00122")
  )
  refs[[r$type]] %||% character()
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE)
}

html_table <- function(df, digits = 4) {
  if (is.null(df) || !nrow(df)) return("<p class='muted'>(sem linhas)</p>")
  df <- as.data.frame(df)
  is_num <- vapply(df, is.numeric, logical(1))
  hdr <- paste0("<tr>", paste0("<th", ifelse(is_num, " class='num'", ""), ">", html_escape(names(df)), "</th>", collapse = ""), "</tr>")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(seq_along(df), function(j) {
      v <- df[[j]][i]
      txt <- if (is_num[j]) { if (is.na(v)) "-" else if (names(df)[j] %in% c("p", "p_adj", "p_unadj", "p_bonferroni", "p_model")) fmt_p(v) else formatC(v, format = "g", digits = digits + 1) } else html_escape(as.character(v))
      paste0("<td", if (is_num[j]) " class='num'" else "", ">", txt, "</td>")
    }, "")
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, "")
  paste0("<table>", hdr, paste(rows, collapse = ""), "</table>")
}

plot_data_uri <- function(p, width = 17, height = 11) {
  f <- tempfile(fileext = ".png"); on.exit(unlink(f))
  save_plot(p, f, width = width, height = height, dpi = 150)
  paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size)))
}

#' Scientific report (HTML, Word or PDF)
#'
#' Builds a self-contained report with identification, date, method, data
#' characterisation, design, results tables, figures, diagnostics,
#' interpretation (result / interpretation / cautions), limitations,
#' methodological references and reproducibility information (R code,
#' versions, seed). HTML needs no external software. Word (`.docx`) and PDF
#' are produced by converting the HTML with Pandoc (via \pkg{rmarkdown});
#' PDF additionally needs a PDF engine (e.g. LaTeX, wkhtmltopdf or
#' weasyprint). When unavailable, a clear error suggests the HTML report.
#'
#' @param results A `caagro_result` or list of results.
#' @param file Output path (`.html`, `.docx` or `.pdf`).
#' @param title Report title.
#' @param author Optional author line.
#' @param level `"full"` or `"summary"` (main tables and first figure only).
#' @param plots Optional list of ggplots; by default the first plot type of
#'   each result is drawn.
#' @param style A [caagro_style()] for automatic plots.
#' @return The file path, invisibly.
#' @examples
#' fit <- anova_exp(PlantGrowth, "weight", "group")
#' f <- tempfile(fileext = ".html")
#' export_report(list(fit, compare_means(fit), check_assumptions(fit)), f)
#' @export
export_report <- function(results, file, title = "Relatorio de analise estatistica", author = NULL,
                          level = c("full", "summary"), plots = NULL, style = caagro_style()) {
  level <- match.arg(level)
  results <- as_result_list(results)
  ext <- tolower(tools::file_ext(file))
  if (!ext %in% c("html", "docx", "pdf")) abort_caagro("Use extensao .html, .docx ou .pdf.")
  pal <- caagro_palette()
  css <- sprintf("body{font-family:Inter,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;color:%s;background:%s;margin:0}
.wrap{max-width:960px;margin:0 auto;padding:32px 40px;background:#fff}
h1{font-size:26px;margin:0 0 4px} h2{font-size:19px;border-bottom:2px solid %s;padding-bottom:4px;margin-top:36px}
h3{font-size:15px;color:%s;margin-top:22px} .muted{color:%s;font-size:13px}
table{border-collapse:collapse;width:100%%;font-size:12.5px;margin:8px 0 14px} th{background:%s;color:#fff;text-align:left;padding:6px 8px;font-weight:600}
td{padding:5px 8px;border-bottom:1px solid %s} td.num,th.num{text-align:right;font-variant-numeric:tabular-nums}
.note{border-left:3px solid %s;background:%s;padding:8px 12px;margin:6px 0;font-size:13.5px}
.warn{border-left:3px solid %s;background:#FEF4EA;padding:8px 12px;margin:6px 0;font-size:13px}
pre{background:%s;padding:12px;font-size:12px;overflow:auto;border-radius:6px} img{max-width:100%%;border:1px solid %s;border-radius:6px}
.badge{display:inline-block;background:%s;color:#fff;border-radius:4px;padding:1px 8px;font-size:12px}",
    pal[["azul_profundo"]], pal[["fundo"]], pal[["verde_cientifico"]], pal[["azul_secundario"]], pal[["cinza_tecnico"]],
    pal[["azul_profundo"]], pal[["cinza_claro"]], pal[["verde_cientifico"]], pal[["fundo"]], pal[["laranja"]],
    pal[["fundo"]], pal[["cinza_claro"]], pal[["verde_cientifico"]])
  out <- c("<!DOCTYPE html><html lang='pt-BR'><head><meta charset='utf-8'><title>", html_escape(title), "</title><style>", css,
           "</style></head><body><div class='wrap'>",
           "<span class='badge'>caagro</span>",
           "<h1>", html_escape(title), "</h1>",
           "<p class='muted'>", if (!is.null(author)) paste0(html_escape(author), " &middot; "), "Gerado em ", format(Sys.time(), "%d/%m/%Y %H:%M"),
           " &middot; caagro ", caagro_version(), " &middot; ", html_escape(R.version.string), "</p>")
  r1 <- results[[1]]
  if (is.data.frame(r1$data_used)) {
    out <- c(out, "<h2>Dados</h2><p>Observacoes utilizadas: <b>", nrow(r1$data_used), "</b>",
             if (is.data.frame(r1$data)) paste0(" de ", nrow(r1$data), " fornecidas"), ". Variaveis: ",
             html_escape(paste(setdiff(names(r1$data_used), ".row"), collapse = ", ")), ".</p>")
  }
  all_refs <- character()
  for (i in seq_along(results)) {
    r <- results[[i]]
    out <- c(out, "<h2>", i, ". ", html_escape(r$method), "</h2>")
    meta <- c(if (!is.null(r$design)) paste0("<b>Delineamento:</b> ", html_escape(r$design)),
              if (!is.null(r$formula)) paste0("<b>Modelo:</b> <code>", html_escape(paste(deparse(r$formula), collapse = " ")), "</code>"))
    if (length(meta)) out <- c(out, "<p>", paste(meta, collapse = " &nbsp;|&nbsp; "), "</p>")
    for (nm in names(r$notes)) out <- c(out, "<div class='note'><b>", html_escape(nm), ":</b> ", html_escape(r$notes[[nm]]), "</div>")
    tabs <- if (level == "summary") r$tables[r$main] else r$tables
    if (level == "full") tabs <- tabs[setdiff(names(tabs), c("predictions", "influence", "scores", "merge_heights"))]
    for (tn in names(tabs)) {
      lbl <- table_labels[tn]; if (is.na(lbl)) lbl <- tn
      out <- c(out, "<h3>", html_escape(lbl), "</h3>", html_table(tabs[[tn]]))
    }
    p <- if (!is.null(plots) && length(plots) >= i) plots[[i]] else tryCatch(plot(r, style = style), error = function(e) NULL)
    if (inherits(p, "ggplot")) out <- c(out, "<h3>Figura</h3><img src='", plot_data_uri(p), "' alt='Figura'>")
    if (length(r$warnings)) out <- c(out, paste0("<div class='warn'>", html_escape(r$warnings), "</div>"))
    all_refs <- c(all_refs, method_references(r))
  }
  out <- c(out, "<h2>Limitacoes</h2><p>Os resultados dependem da validade dos pressupostos de cada metodo e do planejamento experimental. ",
           "Testes estatisticos nao estabelecem causalidade por si; resultados nao significativos nao demonstram igualdade entre tratamentos.</p>")
  if (length(all_refs)) out <- c(out, "<h2>Referencias metodologicas</h2><ul>", paste0("<li>", html_escape(unique(all_refs)), "</li>"), "</ul>")
  codes <- unique(unlist(lapply(results, `[[`, "code")))
  out <- c(out, "<h2>Reprodutibilidade</h2><pre>", html_escape(paste(c("library(caagro)", "# dados <- read_data(\"seu_arquivo.xlsx\")", codes), collapse = "\n\n")), "</pre>",
           "<p class='muted'>", html_escape(paste("Plataforma:", R.version$platform, "| Semente(s):", paste(unique(unlist(lapply(results, function(r) r$meta$seed))), collapse = ", "))), "</p>",
           "</div></body></html>")
  html <- paste(out, collapse = "")
  if (ext == "html") { writeLines(html, file, useBytes = TRUE); return(invisible(file)) }
  if (!requireNamespace("rmarkdown", quietly = TRUE) || !rmarkdown::pandoc_available()) {
    abort_caagro("Relatorios .docx/.pdf exigem o Pandoc (instalado com o RStudio ou em https://pandoc.org). Use o formato .html, que nao depende de software externo.")
  }
  tmp <- tempfile(fileext = ".html"); writeLines(html, tmp, useBytes = TRUE)
  if (ext == "docx") {
    rmarkdown::pandoc_convert(tmp, to = "docx", output = normalizePath(file, mustWork = FALSE), options = c("--standalone"))
  } else {
    engines <- c("weasyprint", "wkhtmltopdf", "pdflatex", "xelatex")
    eng <- engines[nzchar(Sys.which(engines))][1]
    if (is.na(eng)) abort_caagro("Nenhum mecanismo de PDF encontrado (weasyprint, wkhtmltopdf ou LaTeX). Gere o relatorio em .html e use 'Imprimir > Salvar como PDF' no navegador.")
    rmarkdown::pandoc_convert(tmp, to = if (eng %in% c("pdflatex", "xelatex")) "latex" else "html", output = normalizePath(file, mustWork = FALSE),
                              options = c(paste0("--pdf-engine=", eng)))
  }
  invisible(file)
}

#' Export reproducible R script and configuration
#'
#' `export_script()` writes an R script that reproduces the analyses with the
#' same functions and parameters used by the applications.
#' `export_config()` writes the settings and reproducibility metadata as
#' JSON.
#'
#' @param results A `caagro_result` or list of results.
#' @param file Output path.
#' @param data_file Path of the data file to be read in the script.
#' @param style Optional `caagro_style` to include as code.
#' @return The file path, invisibly.
#' @examples
#' fit <- anova_exp(PlantGrowth, "weight", "group")
#' f <- tempfile(fileext = ".R")
#' export_script(fit, f, data_file = "dados.xlsx")
#' cat(readLines(f), sep = "\n")
#' @export
export_script <- function(results, file, data_file = "dados.xlsx", style = NULL) {
  results <- as_result_list(results)
  lines <- c("# Script gerado pelo caagro", paste("# Data:", format(Sys.time(), "%Y-%m-%d %H:%M")),
             paste("# caagro", caagro_version(), "|", R.version.string), "",
             "library(caagro)", "", paste0("dados <- read_data(", deparse(data_file), ")"), "")
  for (i in seq_along(results)) {
    r <- results[[i]]
    if (!is.null(r$meta$seed)) lines <- c(lines, paste0("# semente utilizada: ", r$meta$seed))
    lines <- c(lines, paste0("res", i, " <- ", r$code %||% "# (analise sem codigo reproduzivel)"), paste0("print(res", i, ")"), "")
  }
  if (!is.null(style)) lines <- c(lines, style_code(style), paste0("grafico <- plot(res", length(results), ", style = estilo)"),
                                  "save_plot(grafico, \"grafico.png\", width = 17, height = 12, units = \"cm\", dpi = 600)", "")
  lines <- c(lines, paste0("export_excel(list(", paste0("res", seq_along(results), collapse = ", "), "), \"resultados.xlsx\")"))
  writeLines(lines, file, useBytes = TRUE)
  invisible(file)
}

#' @rdname export_script
#' @export
export_config <- function(results, file) {
  results <- as_result_list(results)
  cfg <- lapply(results, function(r) list(method = r$method, design = r$design,
                                          formula = if (is.null(r$formula)) NULL else paste(deparse(r$formula), collapse = " "),
                                          settings = r$settings, meta = r$meta, warnings = r$warnings))
  writeLines(jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", force = TRUE), file, useBytes = TRUE)
  invisible(file)
}

tlabel <- function(tn) { l <- unname(table_labels[tn]); if (is.na(l)) tn else l }
