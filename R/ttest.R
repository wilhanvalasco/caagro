#' t tests for means (one-sample, independent, Welch, paired)
#'
#' Wrapper around [stats::t.test()] that returns a standardised result with
#' the confidence interval of the mean (difference) and effect sizes
#' (Cohen's d and Hedges' g).
#'
#' @param data A data frame.
#' @param response Numeric response column.
#' @param group Two-level grouping column (independent/Welch/paired).
#' @param type `"one_sample"`, `"independent"` (pooled variance, Student),
#'   `"welch"` (unequal variances) or `"paired"`.
#' @param id Column identifying pairs (required for `"paired"`).
#' @param mu Hypothesised value (one-sample) or difference.
#' @param alternative `"two.sided"`, `"less"` or `"greater"`.
#' @param conf_level Confidence level.
#' @return A `caagro_result` (class `caagro_test`) with table `test` and
#'   `descriptives`.
#' @references
#' Student (1908). The probable error of a mean. *Biometrika*, 6(1), 1-25.
#' \doi{10.2307/2331554}
#'
#' Welch, B. L. (1947). The generalization of 'Student's' problem when
#' several different population variances are involved. *Biometrika*,
#' 34(1/2), 28-35. \doi{10.2307/2332510}
#'
#' Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect
#' size and related estimators. *Journal of Educational Statistics*, 6(2),
#' 107-128. \doi{10.3102/10769986006002107}
#' @examples
#' d <- subset(PlantGrowth, group != "trt2")
#' mean_test(d, "weight", "group", type = "welch")
#' mean_test(sleep, "extra", "group", type = "paired", id = "ID")
#' @export
mean_test <- function(data, response, group = NULL,
                      type = c("welch", "independent", "paired", "one_sample"),
                      id = NULL, mu = 0, alternative = c("two.sided", "less", "greater"),
                      conf_level = 0.95) {
  type <- match.arg(type); alternative <- match.arg(alternative)
  check_conf(conf_level)
  if (type == "one_sample") {
    prep <- prepare_data(data, response)
    y <- prep$data[[response]]
    tt <- stats::t.test(y, mu = mu, alternative = alternative, conf.level = conf_level)
    d_eff <- (mean(y) - mu) / stats::sd(y)
    n <- length(y)
    desc <- data.frame(group = "(amostra)", n = n, mean = mean(y), sd = stats::sd(y), se = stats::sd(y) / sqrt(n))
    g_eff <- d_eff * (1 - 3 / (4 * (n - 1) - 1))
  } else {
    if (is.null(group)) abort_caagro("Informe a coluna `group` com dois niveis.")
    prep <- prepare_data(data, response, c(group, id))
    d <- prep$data
    g <- droplevels(d[[group]])
    if (nlevels(g) != 2) abort_caagro("O teste t exige exatamente 2 grupos; encontrado(s): ", nlevels(g), ". Use ANOVA para mais grupos.")
    lv <- levels(g)
    if (type == "paired") {
      if (is.null(id)) abort_caagro("O teste pareado exige a coluna `id` que identifica os pares.")
      w <- stats::reshape(d[, c(id, group, response)], idvar = id, timevar = group, direction = "wide")
      a <- w[[paste0(response, ".", lv[1])]]; b <- w[[paste0(response, ".", lv[2])]]
      ok <- stats::complete.cases(a, b)
      if (any(!ok)) prep$messages <- c(prep$messages, sprintf("%d par(es) incompleto(s) descartado(s).", sum(!ok)))
      a <- a[ok]; b <- b[ok]
      tt <- stats::t.test(a, b, paired = TRUE, mu = mu, alternative = alternative, conf.level = conf_level)
      dif <- a - b
      d_eff <- mean(dif) / stats::sd(dif)
      n <- length(dif)
      g_eff <- d_eff * (1 - 3 / (4 * (n - 1) - 1))
      desc <- data.frame(group = c(lv, "diferenca"), n = c(n, n, n), mean = c(mean(a), mean(b), mean(dif)),
                         sd = c(stats::sd(a), stats::sd(b), stats::sd(dif)))
      desc$se <- desc$sd / sqrt(desc$n)
    } else {
      a <- d[[response]][g == lv[1]]; b <- d[[response]][g == lv[2]]
      tt <- stats::t.test(a, b, var.equal = type == "independent", mu = mu,
                          alternative = alternative, conf.level = conf_level)
      n1 <- length(a); n2 <- length(b)
      sp <- sqrt(((n1 - 1) * stats::var(a) + (n2 - 1) * stats::var(b)) / (n1 + n2 - 2))
      d_eff <- (mean(a) - mean(b)) / sp
      g_eff <- d_eff * (1 - 3 / (4 * (n1 + n2) - 9))
      desc <- data.frame(group = lv, n = c(n1, n2), mean = c(mean(a), mean(b)), sd = c(stats::sd(a), stats::sd(b)))
      desc$se <- desc$sd / sqrt(desc$n)
    }
  }
  tb <- data.frame(estimate = unname(if (length(tt$estimate) == 2) diff(rev(tt$estimate)) else tt$estimate),
                   statistic = unname(tt$statistic), df = unname(tt$parameter), p = tt$p.value,
                   lower = tt$conf.int[1], upper = tt$conf.int[2], conf_level = conf_level,
                   cohen_d = d_eff, hedges_g = g_eff, alternative = alternative)
  label <- c(one_sample = "Teste t para uma amostra", independent = "Teste t para amostras independentes (variancias iguais)",
             welch = "Teste t de Welch (variancias desiguais)", paired = "Teste t pareado")[[type]]
  new_result("ttest", label, "caagro_test",
             settings = list(response = response, group = group, type = type, id = id, mu = mu,
                             alternative = alternative, conf_level = conf_level),
             data = data, data_used = prep$data, tables = list(test = tb, descriptives = desc),
             warnings = prep$messages,
             notes = list(Resultado = sprintf("%s: t = %.3f, gl = %.2f, p %s; IC %g%% = [%.4g; %.4g].",
                                              label, tb$statistic, tb$df, fmt_p_eq(tb$p), 100 * conf_level, tb$lower, tb$upper),
                          Interpretacao = "O intervalo de confianca descreve os valores plausiveis da diferenca (ou media). O tamanho de efeito (d de Cohen / g de Hedges) descreve a magnitude em unidades de desvio-padrao.",
                          Cuidados = if (type == "independent") "O teste de Student pressupoe variancias iguais; na duvida, prefira Welch." else "Pressupoe normalidade aproximada (da diferenca, no caso pareado) e independencia entre unidades."),
             code = code_call("caagro::mean_test", list(data = as.name("dados"), response = response, group = group,
                                                        type = type, id = id, mu = mu, alternative = alternative,
                                                        conf_level = conf_level)))
}
