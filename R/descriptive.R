#' Descriptive statistics
#'
#' Computes n, missing values, mean, median, minimum, maximum, range,
#' variance, standard deviation, standard error, coefficient of variation,
#' quartiles, IQR, skewness and excess kurtosis, optionally by group.
#'
#' Skewness and kurtosis use the adjusted Fisher-Pearson estimators
#' \eqn{G_1} and \eqn{G_2} (Joanes and Gill, 1998), the same used by most
#' statistical software (type 2 in `e1071`). Quartiles use R's default
#' (type 7).
#'
#' @param data A data frame.
#' @param variables Numeric columns to describe (default: all numeric).
#' @param group Optional grouping column.
#' @return A `caagro_result` (class `caagro_describe`) with tables
#'   `statistics` and `frequencies` (for factor columns).
#' @references Joanes, D. N. and Gill, C. A. (1998). Comparing measures of
#'   sample skewness and kurtosis. *Journal of the Royal Statistical Society
#'   D*, 47(1), 183-189. \doi{10.1111/1467-9884.00122}
#' @examples
#' describe_data(PlantGrowth, "weight", group = "group")
#' @export
describe_data <- function(data, variables = NULL, group = NULL) {
  data <- check_data_frame(data)
  if (is.null(variables)) {
    variables <- names(data)[vapply(data, is.numeric, logical(1))]
    variables <- setdiff(variables, group)
  }
  if (!length(variables)) abort_caagro("Nenhuma vari\u00e1vel num\u00e9rica para descrever.")
  check_columns(data, c(variables, group))
  g <- if (is.null(group)) factor(rep("(todos)", nrow(data))) else factor(data[[group]], exclude = NULL)
  rows <- list()
  for (v in variables) {
    y <- as_numeric_strict(data[[v]], v)
    for (lv in levels(g)) {
      yy <- y[g == lv | (is.na(lv) & is.na(g))]
      rows[[length(rows) + 1]] <- cbind(data.frame(variable = v, group = lv), describe_vec(yy))
    }
  }
  st <- do.call(rbind, rows); rownames(st) <- NULL
  if (is.null(group)) st$group <- NULL else names(st)[2] <- group
  facs <- names(data)[!vapply(data, is.numeric, logical(1))]
  freq <- if (length(facs)) do.call(rbind, lapply(facs, function(f) {
    tb <- table(data[[f]], useNA = "ifany")
    data.frame(variable = f, level = as.character(names(tb)), n = as.vector(tb),
               percent = 100 * as.vector(tb) / sum(tb))
  })) else data.frame()
  warns <- character()
  if (any(st$missing > 0)) warns <- "H\u00e1 valores ausentes; as estat\u00edsticas foram calculadas com os valores dispon\u00edveis."
  if (any(st$mean <= 0, na.rm = TRUE)) warns <- c(warns, "O CV% s\u00f3 \u00e9 interpret\u00e1vel para vari\u00e1veis estritamente positivas em escala de raz\u00e3o.")
  new_result("describe", "Estat\u00edstica descritiva", "caagro_describe",
             settings = list(variables = variables, group = group),
             data = data, data_used = data, tables = list(statistics = st, frequencies = freq),
             warnings = warns,
             notes = list(
               Resultado = "Medidas de posi\u00e7\u00e3o, dispers\u00e3o e forma da distribui\u00e7\u00e3o.",
               Interpretacao = "M\u00e9dia e mediana muito diferentes sugerem assimetria; o CV% compara variabilidade relativa; assimetria > 0 indica cauda a direita; curtose > 0 indica caudas mais pesadas que a normal.",
               Cuidados = "Estat\u00edsticas descritivas n\u00e3o testam hip\u00f3teses. Com amostras pequenas, assimetria e curtose s\u00e3o muito imprecisas."),
             code = code_call("caagro::describe_data", list(data = as.name("dados"), variables = variables, group = group)))
}

describe_vec <- function(y) {
  miss <- sum(is.na(y)); y <- y[!is.na(y)]; n <- length(y)
  if (n == 0) {
    return(data.frame(n = 0, missing = miss, mean = NA, median = NA, min = NA, max = NA, range = NA,
                      variance = NA, sd = NA, se = NA, cv_percent = NA, q1 = NA, q3 = NA, iqr = NA,
                      skewness = NA, kurtosis = NA))
  }
  m <- mean(y); s <- if (n > 1) stats::sd(y) else NA
  q <- stats::quantile(y, c(.25, .75), names = FALSE)
  m2 <- mean((y - m)^2); m3 <- mean((y - m)^3); m4 <- mean((y - m)^4)
  g1 <- if (n > 2 && m2 > 0) (m3 / m2^1.5) * sqrt(n * (n - 1)) / (n - 2) else NA
  g2 <- if (n > 3 && m2 > 0) ((n + 1) * (m4 / m2^2 - 3) + 6) * (n - 1) / ((n - 2) * (n - 3)) else NA
  data.frame(n = n, missing = miss, mean = m, median = stats::median(y), min = min(y), max = max(y),
             range = max(y) - min(y), variance = s^2, sd = s, se = s / sqrt(n),
             cv_percent = if (m != 0) 100 * s / m else NA, q1 = q[1], q3 = q[2], iqr = q[2] - q[1],
             skewness = g1, kurtosis = g2)
}
