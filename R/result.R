#' Standardised caagro result object
#'
#' All analysis functions of caagro return an object of class
#' `caagro_result` (plus a specific subclass such as `caagro_anova`). It is a
#' list with a fixed set of fields, so that the Shiny applications, the
#' exporters and the user's scripts read results in the same way.
#'
#' @section Fields:
#' \describe{
#'   \item{type}{Analysis family (e.g. `"anova"`, `"regression"`).}
#'   \item{method}{Human-readable method name.}
#'   \item{formula}{Model formula or description of the design.}
#'   \item{design}{Design label (`"DIC"`, `"DBC"`, ...) when applicable.}
#'   \item{settings}{Named list of all options used.}
#'   \item{data}{Data originally supplied.}
#'   \item{data_used}{Data actually used (after explicit exclusions/NA).}
#'   \item{tables}{Named list of data frames (estimates, tests, ...).}
#'   \item{main}{Name of the main table returned by `as.data.frame()`.}
#'   \item{model}{Fitted model object(s), when applicable.}
#'   \item{diagnostics}{Diagnostic results.}
#'   \item{warnings}{Character vector of methodological warnings.}
#'   \item{notes}{Interpretation notes (result / interpretation / cautions).}
#'   \item{code}{Reproducible R code, when available.}
#'   \item{meta}{Reproducibility metadata (versions, time, seed).}
#' }
#'
#' @param x,object A `caagro_result`.
#' @param name Table name. Default: the main table.
#' @param row.names,optional Ignored (S3 compatibility).
#' @param digits Number of digits to print.
#' @param ... Further arguments passed to methods.
#' @return `print()` returns `x` invisibly; `summary()` returns a
#'   `summary.caagro_result` list; `as.data.frame()` returns a data frame;
#'   `result_table()` returns a data frame.
#' @name caagro_result
NULL

new_result <- function(type, method, subclass = NULL, formula = NULL,
                       design = NULL, settings = list(), data = NULL,
                       data_used = NULL, tables = list(), main = NULL,
                       model = NULL, diagnostics = NULL, warnings = character(),
                       notes = list(), code = NULL, seed = NULL) {
  structure(
    list(
      type = type, method = method, formula = formula, design = design,
      settings = settings, data = data, data_used = data_used,
      tables = tables, main = main %||% names(tables)[1], model = model,
      diagnostics = diagnostics, warnings = unique(warnings), notes = notes,
      code = code, meta = repro_meta(seed)
    ),
    class = c(subclass, "caagro_result")
  )
}

#' @rdname caagro_result
#' @export
print.caagro_result <- function(x, digits = 4, ...) {
  cat("\n== caagro ::", x$method, "==\n")
  if (!is.null(x$design)) cat("Delineamento:", x$design, "\n")
  if (!is.null(x$formula)) cat("Modelo:", paste(deparse(x$formula), collapse = " "), "\n")
  if (!is.null(x$data_used)) cat("Observa\u00e7\u00f5es utilizadas:", nrow(x$data_used), "\n")
  for (nm in names(x$tables)) {
    tb <- x$tables[[nm]]
    if (!is.data.frame(tb) || nrow(tb) == 0) next
    cat("\n--", nm, "--\n")
    print(round_df(tb, digits), row.names = FALSE)
  }
  if (length(x$warnings)) {
    cat("\nAdvertencias:\n")
    for (w in x$warnings) cat(" *", w, "\n")
  }
  invisible(x)
}

#' @rdname caagro_result
#' @export
summary.caagro_result <- function(object, ...) {
  structure(list(method = object$method, design = object$design,
                 formula = object$formula, tables = object$tables,
                 notes = object$notes, warnings = object$warnings,
                 meta = object$meta),
            class = "summary.caagro_result")
}

#' @rdname caagro_result
#' @export
print.summary.caagro_result <- function(x, ...) {
  cat("\n== Resumo:", x$method, "==\n")
  if (length(x$notes)) {
    for (nm in names(x$notes)) {
      cat("\n[", nm, "]\n", sep = "")
      cat(strwrap(x$notes[[nm]], width = 78, prefix = "  "), sep = "\n")
    }
  }
  if (length(x$warnings)) {
    cat("\nAdvertencias:\n"); for (w in x$warnings) cat(" *", w, "\n")
  }
  cat("\ncaagro", x$meta$caagro_version, "|", x$meta$r_version, "|", x$meta$time, "\n")
  invisible(x)
}

#' @rdname caagro_result
#' @export
as.data.frame.caagro_result <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$tables[[x$main]]
}

#' @rdname caagro_result
#' @export
result_table <- function(x, name = NULL) {
  if (!inherits(x, "caagro_result")) abort_caagro("`x` n\u00e3o \u00e9 um resultado caagro.")
  name <- name %||% x$main
  if (!name %in% names(x$tables)) {
    abort_caagro("Tabela '", name, "' inexistente. Dispon\u00edveis: ",
                 paste(names(x$tables), collapse = ", "), ".")
  }
  x$tables[[name]]
}
