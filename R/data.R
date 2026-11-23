#' Read experimental data from a file
#'
#' Reads `.xlsx`, `.xls`, `.csv`, `.txt` and `.tsv` files into a plain
#' `data.frame`. Original column names are preserved (`check.names = FALSE`).
#' For text files the delimiter and decimal mark can be given or detected
#' automatically from the first lines.
#'
#' @param path Path to the file.
#' @param sheet Sheet name or index for Excel files.
#' @param sep Field delimiter for text files (`NULL` = auto-detect among
#'   `;`, `,`, tab and `|`).
#' @param dec Decimal mark (`NULL` = `","` when `sep = ";"`, otherwise `"."`).
#' @param header Logical, whether the first row contains column names.
#' @param na Character vector of strings interpreted as missing.
#' @param encoding File encoding for text files.
#' @return A `data.frame`.
#' @examples
#' f <- system.file("extdata", "dose_resposta.csv", package = "caagro")
#' d <- read_data(f)
#' head(d)
#' @export
read_data <- function(path, sheet = 1, sep = NULL, dec = NULL, header = TRUE,
                      na = c("", "NA", "NaN", "N/A", "-", "."),
                      encoding = "UTF-8") {
  if (!file.exists(path)) abort_caagro("Arquivo nao encontrado: ", path)
  ext <- tolower(tools::file_ext(path))
  out <- switch(
    ext,
    xlsx = , xls = {
      as.data.frame(readxl::read_excel(path, sheet = sheet, col_names = header,
                                       na = na, .name_repair = "minimal"),
                    check.names = FALSE, stringsAsFactors = FALSE)
    },
    csv = , txt = , tsv = {
      first <- readLines(path, n = 5, warn = FALSE, encoding = encoding)
      first <- sub("^\ufeff", "", first)
      if (is.null(sep)) sep <- detect_sep(first, ext)
      if (is.null(dec)) dec <- if (sep == ";") "," else "."
      d <- utils::read.table(path, header = header, sep = sep, dec = dec,
                             na.strings = na, quote = "\"'",
                             stringsAsFactors = FALSE, check.names = FALSE,
                             comment.char = "", fileEncoding = if (toupper(encoding) %in% c("UTF-8", "UTF8")) "UTF-8-BOM" else encoding,
                             strip.white = TRUE)
      names(d) <- sub("^\ufeff", "", names(d))
      d
    },
    abort_caagro("Formato nao suportado: '.", ext,
                 "'. Use .xlsx, .xls, .csv, .txt ou .tsv.")
  )
  if (ncol(out) == 0 || nrow(out) == 0) abort_caagro("O arquivo nao contem dados.")
  attr(out, "source") <- basename(path)
  out
}

detect_sep <- function(lines, ext = "csv") {
  cand <- c(";", ",", "\t", "|")
  counts <- vapply(cand, function(s) {
    n <- lengths(regmatches(lines, gregexpr(s, lines, fixed = TRUE)))
    if (length(unique(n)) == 1 && n[1] > 0) n[1] else 0
  }, numeric(1))
  if (all(counts == 0)) return(if (ext == "tsv") "\t" else ",")
  cand[which.max(counts)]
}

#' Excel sheet names
#' @param path Path to an Excel file.
#' @return Character vector of sheet names.
#' @export
#' @examples
#' f <- system.file("extdata", "severidade.xlsx", package = "caagro")
#' data_sheets(f)
data_sheets <- function(path) readxl::excel_sheets(path)

#' Convert data from wide to long format
#'
#' @param data A data frame.
#' @param value_cols Columns holding repeated measurements (e.g. one column
#'   per block or replicate).
#' @param names_to Name of the new column storing former column names.
#' @param values_to Name of the new column storing the values.
#' @return A long data frame. Identifier columns are repeated.
#' @examples
#' w <- data.frame(trat = c("A", "B"), R1 = c(10, 12), R2 = c(11, 13))
#' to_long(w, value_cols = c("R1", "R2"), names_to = "bloco")
#' @export
to_long <- function(data, value_cols, names_to = "nome", values_to = "valor") {
  data <- check_data_frame(data)
  check_columns(data, value_cols)
  id <- setdiff(names(data), value_cols)
  out <- do.call(rbind, lapply(value_cols, function(v) {
    d <- data[, id, drop = FALSE]
    d[[names_to]] <- v
    d[[values_to]] <- data[[v]]
    d
  }))
  rownames(out) <- NULL
  out
}

#' Validate data against the structure required by a method
#'
#' Performs the mandatory checks of caagro before any analysis: missing
#' values, non-numeric responses, absence of variability, insufficient
#' replication, missing treatment x block combinations, duplicated rows,
#' unbalance and incompatibility between data and design.
#'
#' @param data A data frame.
#' @param response Response column.
#' @param treatment Treatment (factor) column, optional.
#' @param block Block column, optional.
#' @param factors Additional factor columns, optional.
#' @param x Quantitative explanatory column (regression), optional.
#' @param design One of `"none"`, `"DIC"`, `"DBC"`, `"factorial"`,
#'   `"regression"`, `"paired"`.
#' @return An object of class `caagro_check`: a data frame with columns
#'   `level` (`"error"`, `"warning"`, `"info"`) and `message`; attribute
#'   `ok` is `TRUE` when there are no errors.
#' @examples
#' check_data(PlantGrowth, response = "weight", treatment = "group", design = "DIC")
#' @export
check_data <- function(data, response, treatment = NULL, block = NULL,
                       factors = NULL, x = NULL, design = "none") {
  data <- check_data_frame(data)
  issues <- list()
  add <- function(level, msg) issues[[length(issues) + 1]] <<- data.frame(level = level, message = msg)
  cols <- c(response, treatment, block, factors, x)
  miss_cols <- setdiff(cols, names(data))
  if (length(miss_cols)) {
    add("error", paste0("Colunas inexistentes: ", paste(miss_cols, collapse = ", ")))
    return(finish_check(issues))
  }
  y <- data[[response]]
  yn <- suppressWarnings(as.numeric(gsub(",", ".", as.character(y), fixed = TRUE)))
  nonnum <- sum(!is.na(y) & is.na(yn))
  if (nonnum > 0) add("error", sprintf("A resposta '%s' tem %d valor(es) nao numerico(s).", response, nonnum))
  nas <- vapply(cols, function(cl) sum(is.na(data[[cl]]) | (is.character(data[[cl]]) & !nzchar(trimws(data[[cl]])))), numeric(1))
  for (cl in names(nas)[nas > 0]) {
    add("warning", sprintf("'%s' tem %d valor(es) ausente(s); as linhas correspondentes serao excluidas da analise (e registradas).", cl, nas[cl]))
  }
  ok_rows <- stats::complete.cases(data[, cols, drop = FALSE]) & !is.na(yn)
  yv <- yn[ok_rows]
  if (length(yv) < 3) add("error", "Menos de 3 observacoes validas.")
  if (length(yv) > 1 && isTRUE(stats::sd(yv) == 0)) add("error", "A resposta nao apresenta variabilidade (todos os valores iguais).")
  dups <- sum(duplicated(data[, cols, drop = FALSE]))
  if (dups > 0) add("info", sprintf("%d linha(s) totalmente duplicada(s) nas colunas selecionadas. Verifique se nao ha digitacao repetida.", dups))

  if (!is.null(treatment)) {
    tr <- factor(data[[treatment]][ok_rows])
    tab <- table(tr)
    if (nlevels(tr) < 2) add("error", "O fator de tratamentos precisa ter ao menos 2 niveis.")
    if (any(tab < 2) && is.null(block)) add("error", sprintf("Tratamento(s) sem repeticao: %s.", paste(names(tab)[tab < 2], collapse = ", ")))
    if (length(unique(tab)) > 1) add("info", sprintf("Dados desbalanceados (repeticoes por tratamento: %s).", paste(range(tab), collapse = " a ")))
    if (nlevels(tr) > 0.5 * length(yv) && is.null(x)) add("warning", "Numero de niveis do tratamento e alto em relacao ao numero de observacoes. Confirme se a coluna escolhida e realmente um fator.")
    if (is.numeric(data[[treatment]]) && length(unique(data[[treatment]])) > 6 && design %in% c("DIC", "DBC"))
      add("info", "O tratamento e numerico com varios niveis: considere a analise de regressao, alem (ou no lugar) da comparacao de medias.")
  }
  if (!is.null(block)) {
    bl <- factor(data[[block]][ok_rows])
    if (nlevels(bl) < 2) add("error", "O bloco precisa ter ao menos 2 niveis.")
    if (!is.null(treatment)) {
      tr <- factor(data[[treatment]][ok_rows])
      ct <- table(tr, bl)
      if (any(ct == 0)) add("warning", sprintf("Ha %d combinacao(oes) tratamento x bloco sem observacao (blocos incompletos). As medias serao ajustadas (medias marginais estimadas).", sum(ct == 0)))
      if (all(ct <= 1) && any(ct == 1) && design == "DIC") add("warning", "Existe uma coluna de bloco com um tratamento por bloco: os dados parecem de DBC. Nao analise como DIC.")
      if (any(ct > 1) && design == "DBC") add("info", "Ha mais de uma observacao por tratamento em algum bloco. O modelo aditivo de DBC sera usado; avalie se ha interacao tratamento x bloco.")
    }
  }
  if (design == "DBC" && is.null(block)) add("error", "DBC exige a coluna de blocos.")
  if (!is.null(x)) {
    xv <- suppressWarnings(as.numeric(data[[x]][ok_rows]))
    if (anyNA(xv)) add("error", sprintf("A variavel explicativa '%s' precisa ser numerica.", x))
    else if (length(unique(xv)) < 3) add("warning", "Menos de 3 niveis distintos da variavel explicativa: modelos com mais de 2 parametros nao sao estimaveis.")
  }
  if (length(yv) >= 4) {
    q <- stats::quantile(yv, c(.25, .75)); iq <- diff(q)
    nout <- sum(yv < q[1] - 3 * iq | yv > q[2] + 3 * iq)
    if (nout > 0) add("info", sprintf("%d valor(es) extremos (alem de 3 x IQR). Use flag_outliers() para investigar; nada foi removido.", nout))
  }
  finish_check(issues)
}

finish_check <- function(issues) {
  out <- if (length(issues)) do.call(rbind, issues) else
    data.frame(level = character(), message = character())
  attr(out, "ok") <- !any(out$level == "error")
  class(out) <- c("caagro_check", "data.frame")
  out
}

#' @export
print.caagro_check <- function(x, ...) {
  if (nrow(x) == 0) { cat("Dados validos: nenhum problema encontrado.\n"); return(invisible(x)) }
  icon <- c(error = "[ERRO]", warning = "[ALERTA]", info = "[INFO]")
  for (i in seq_len(nrow(x))) cat(icon[x$level[i]], x$message[i], "\n")
  invisible(x)
}

#' Flag potentially atypical observations
#'
#' Observations are **flagged, never removed**. Removal must be explicit
#' (argument `exclude` of the analysis functions) and is recorded in the
#' result.
#'
#' @param data A data frame.
#' @param response Response column.
#' @param group Optional grouping column (IQR rule applied within groups).
#' @param method `"iqr"` (Tukey fences, `k` x IQR) or `"studentized"`
#'   (externally studentised residuals of `model`, Bonferroni-corrected).
#' @param k IQR multiplier (1.5 = usual boxplot rule).
#' @param model For `method = "studentized"`, a fitted `lm` (e.g.
#'   `anova_exp(...)$model`). If `NULL`, a one-way model `response ~ group`
#'   is used.
#' @param alpha Significance level for the studentized rule.
#' @return A data frame with row index, value, group, limits/statistic and
#'   a logical `flag`.
#' @examples
#' flag_outliers(PlantGrowth, "weight", "group")
#' @export
flag_outliers <- function(data, response, group = NULL, method = c("iqr", "studentized"),
                          k = 1.5, model = NULL, alpha = 0.05) {
  method <- match.arg(method)
  data <- check_data_frame(data)
  check_columns(data, c(response, group))
  y <- as_numeric_strict(data[[response]], response)
  g <- if (is.null(group)) rep("(todos)", length(y)) else as.character(data[[group]])
  if (method == "iqr") {
    res <- lapply(split(seq_along(y), g), function(idx) {
      v <- y[idx]
      q <- stats::quantile(v, c(.25, .75), na.rm = TRUE, names = FALSE)
      iq <- q[2] - q[1]
      data.frame(row = idx, value = v, group = g[idx],
                 lower = q[1] - k * iq, upper = q[2] + k * iq,
                 flag = !is.na(v) & (v < q[1] - k * iq | v > q[2] + k * iq))
    })
    out <- do.call(rbind, res)
  } else {
    if (is.null(model)) {
      if (is.null(group)) abort_caagro("Informe `group` ou `model` para o criterio studentizado.")
      d <- data.frame(y = y, g = factor(g))
      model <- stats::lm(y ~ g, data = d)
    }
    rs <- stats::rstudent(model)
    dfr <- stats::df.residual(model) - 1
    p <- 2 * stats::pt(-abs(rs), dfr)
    p_bonf <- pmin(1, p * length(rs))
    rows <- as.integer(names(rs))
    if (anyNA(rows)) rows <- seq_along(rs)
    out <- data.frame(row = rows, value = y[rows], group = g[rows],
                      rstudent = unname(rs), p_bonferroni = unname(p_bonf),
                      flag = unname(p_bonf < alpha))
  }
  out <- out[order(out$row), ]
  rownames(out) <- NULL
  out
}

# Prepare data for an analysis: subset columns, coerce, drop NA, apply
# explicit exclusions; always reports what was removed.
prepare_data <- function(data, response = NULL, factors = NULL, numeric_cols = NULL,
                         exclude = NULL, keep_extra = NULL) {
  data <- check_data_frame(data)
  check_columns(data, c(response, factors, numeric_cols, keep_extra))
  data$.row <- seq_len(nrow(data))
  msgs <- character()
  if (!is.null(exclude) && length(exclude)) {
    exclude <- as.integer(exclude)
    bad <- exclude[exclude < 1 | exclude > nrow(data)]
    if (length(bad)) abort_caagro("Indices de exclusao fora do intervalo: ", paste(bad, collapse = ", "))
    msgs <- c(msgs, sprintf("%d observacao(oes) excluida(s) explicitamente pelo usuario (linhas %s).",
                            length(exclude), paste(exclude, collapse = ", ")))
    data <- data[!data$.row %in% exclude, , drop = FALSE]
  }
  d <- data[, unique(c(".row", response, factors, numeric_cols, keep_extra)), drop = FALSE]
  if (!is.null(response)) d[[response]] <- as_numeric_strict(d[[response]], response)
  for (cl in numeric_cols) d[[cl]] <- as_numeric_strict(d[[cl]], cl)
  for (cl in factors) {
    v <- d[[cl]]
    if (is.character(v)) v[!nzchar(trimws(v))] <- NA
    d[[cl]] <- if (is.factor(v)) droplevels(v) else factor(v, levels = unique(v[!is.na(v)]))
  }
  cc <- stats::complete.cases(d[, setdiff(names(d), keep_extra), drop = FALSE])
  if (any(!cc)) {
    msgs <- c(msgs, sprintf("%d linha(s) com valores ausentes foram desconsideradas (linhas %s).",
                            sum(!cc), paste(utils::head(d$.row[!cc], 20), collapse = ", ")))
  }
  d <- d[cc, , drop = FALSE]
  for (cl in factors) d[[cl]] <- droplevels(d[[cl]])
  rownames(d) <- NULL
  list(data = d, messages = msgs, excluded = exclude)
}
