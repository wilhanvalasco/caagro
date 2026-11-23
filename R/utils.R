# Internal helpers -----------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Signal a caagro error with a clear message
#' @noRd
abort_caagro <- function(..., class = NULL) {
  msg <- paste0(...)
  cond <- structure(
    class = c(class, "caagro_error", "error", "condition"),
    list(message = msg, call = sys.call(-1))
  )
  stop(cond)
}

#' Signal a caagro warning
#' @noRd
warn_caagro <- function(..., class = NULL) {
  msg <- paste0(...)
  cond <- structure(
    class = c(class, "caagro_warning", "warning", "condition"),
    list(message = msg, call = sys.call(-1))
  )
  warning(cond)
}

check_data_frame <- function(data, arg = "data") {
  if (!is.data.frame(data)) {
    abort_caagro("`", arg, "` deve ser um data.frame ou tibble.")
  }
  as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
}

check_columns <- function(data, cols) {
  cols <- cols[!is.null(cols) & !is.na(cols) & nzchar(cols)]
  miss <- setdiff(cols, names(data))
  if (length(miss)) {
    abort_caagro("Coluna(s) nao encontrada(s) nos dados: ",
                 paste0("'", miss, "'", collapse = ", "), ".")
  }
  invisible(TRUE)
}

check_alpha <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) ||
      alpha <= 0 || alpha >= 0.5) {
    abort_caagro("`alpha` deve ser um numero entre 0 e 0,5 (ex.: 0,05).")
  }
  alpha
}

check_conf <- function(conf_level) {
  if (!is.numeric(conf_level) || length(conf_level) != 1 ||
      !is.finite(conf_level) || conf_level <= 0.5 || conf_level >= 1) {
    abort_caagro("`conf_level` deve estar entre 0,5 e 1 (ex.: 0,95).")
  }
  conf_level
}

as_numeric_strict <- function(x, name = "resposta") {
  if (is.numeric(x)) return(as.numeric(x))
  if (is.logical(x)) return(as.numeric(x))
  y <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
  bad <- !is.na(x) & is.na(y)
  if (any(bad)) {
    abort_caagro("A variavel '", name, "' contem ", sum(bad),
                 " valor(es) nao numerico(s) (ex.: '",
                 as.character(x[which(bad)[1]]), "').")
  }
  y
}

harmonic_mean <- function(x) length(x) / sum(1 / x)

fmt_p <- function(p, digits = 4) {
  out <- ifelse(is.na(p), "-",
                ifelse(p < 10^(-digits), paste0("< ", format(10^(-digits), scientific = FALSE)),
                       formatC(p, format = "f", digits = digits)))
  out
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "-", formatC(x, format = "f", digits = digits, big.mark = ""))
}

signif_stars <- function(p) {
  as.character(cut(p, c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                   labels = c("***", "**", "*", ".", "ns"), right = FALSE))
}

valid_p_adjust <- function(method) {
  ok <- c("none", "holm", "bonferroni", "hochberg", "hommel", "BH", "BY", "fdr")
  if (!method %in% ok) {
    abort_caagro("Correcao de multiplicidade desconhecida: '", method,
                 "'. Use uma de: ", paste(ok, collapse = ", "), ".")
  }
  method
}

# Numerical gradient of f(theta) (vector valued) by central differences
num_grad <- function(f, theta, ...) {
  f0 <- f(theta, ...)
  G <- matrix(NA_real_, nrow = length(f0), ncol = length(theta))
  for (j in seq_along(theta)) {
    h <- 1e-6 * max(abs(theta[j]), 1e-3)
    tp <- theta; tm <- theta
    tp[j] <- tp[j] + h; tm[j] <- tm[j] - h
    G[, j] <- (f(tp, ...) - f(tm, ...)) / (2 * h)
  }
  G
}

caagro_version <- function() {
  tryCatch(as.character(utils::packageVersion("caagro")), error = function(e) "dev")
}

repro_meta <- function(seed = NULL) {
  list(
    caagro_version = caagro_version(),
    r_version = R.version.string,
    platform = R.version$platform,
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    seed = seed
  )
}

with_seed <- function(seed, expr) {
  if (is.null(seed)) return(expr)
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (is.null(old)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
        rm(".Random.seed", envir = globalenv())
    } else assign(".Random.seed", old, envir = globalenv())
  }, add = TRUE)
  set.seed(seed)
  expr
}

# Deparse a call compactly for reproducible scripts
code_call <- function(fun, args) {
  args <- args[!vapply(args, is.null, logical(1))]
  parts <- vapply(names(args), function(nm) {
    val <- args[[nm]]
    txt <- if (is.name(val) || is.call(val)) deparse(val, width.cutoff = 500L)
           else paste(deparse(val, width.cutoff = 500L), collapse = "")
    paste0(nm, " = ", txt)
  }, character(1))
  paste0(fun, "(\n  ", paste(parts, collapse = ",\n  "), "\n)")
}

round_df <- function(df, digits = 4) {
  df[] <- lapply(df, function(z) if (is.numeric(z)) round(z, digits) else z)
  df
}

fmt_p_eq <- function(p, digits = 4) {
  s <- fmt_p(p, digits)
  ifelse(grepl("^<", s), s, paste("=", s))
}
