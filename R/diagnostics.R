#' Residual diagnostics and assumption checks
#'
#' Tests and measures to support (not replace) the scientific evaluation of
#' the assumptions of a linear model:
#' * Normality of residuals: Shapiro-Wilk (on residuals, not on the raw
#'   response).
#' * Homogeneity of variances across treatments: Bartlett, Levene (centred on
#'   the mean) and Brown-Forsythe (Levene centred on the median), and
#'   Fligner-Killeen; all computed on model residuals grouped by treatment
#'   (for DIC this is equivalent to using the raw response).
#' * Influence: studentised residuals, leverage and Cook's distance.
#'
#' Independence cannot be verified by an automatic test: it depends on
#' randomisation and on the experimental plan.
#'
#' @param x A `caagro_anova` result or an `lm` object.
#' @param group Grouping factor for variance tests (default: treatment).
#' @param alpha Significance level used to flag results.
#' @return An object of classes `caagro_diagnostics` and `caagro_result`
#'   with tables `tests` and `influence`.
#' @references
#' Shapiro, S. S. and Wilk, M. B. (1965). An analysis of variance test for
#' normality (complete samples). *Biometrika*, 52(3/4), 591-611.
#' \doi{10.2307/2333709}
#'
#' Bartlett, M. S. (1937). Properties of sufficiency and statistical tests.
#' *Proceedings of the Royal Society A*, 160(901), 268-282.
#' \doi{10.1098/rspa.1937.0109}
#'
#' Levene, H. (1960). Robust tests for equality of variances. In I. Olkin
#' (Ed.), *Contributions to Probability and Statistics* (pp. 278-292).
#' Stanford University Press.
#'
#' Brown, M. B. and Forsythe, A. B. (1974). Robust tests for the equality of
#' variances. *Journal of the American Statistical Association*, 69(346),
#' 364-367. \doi{10.1080/01621459.1974.10482955}
#'
#' Fligner, M. A. and Killeen, T. J. (1976). Distribution-free two-sample
#' tests for scale. *Journal of the American Statistical Association*,
#' 71(353), 210-213. \doi{10.1080/01621459.1976.10481517}
#'
#' Cook, R. D. (1977). Detection of influential observation in linear
#' regression. *Technometrics*, 19(1), 15-18. \doi{10.2307/1268249}
#' @examples
#' fit <- anova_exp(PlantGrowth, "weight", "group")
#' check_assumptions(fit)
#' @export
check_assumptions <- function(x, group = NULL, alpha = 0.05) {
  if (inherits(x, "caagro_anova")) {
    fit <- x$model; d <- x$data_used
    group <- group %||% x$settings$treatment
  } else if (inherits(x, "lm")) {
    fit <- x; d <- stats::model.frame(fit)
  } else abort_caagro("`x` deve ser resultado de anova_exp() ou um objeto lm.")
  r <- stats::residuals(fit)
  n <- length(r)
  tests <- list()
  if (n >= 3 && n <= 5000 && stats::sd(r) > 0) {
    sw <- stats::shapiro.test(r)
    tests$shapiro <- data.frame(test = "Shapiro-Wilk (residuos)", assumption = "Normalidade",
                                statistic = unname(sw$statistic), df = NA, p = sw$p.value)
  }
  if (!is.null(group) && group %in% names(d)) {
    g <- droplevels(as.factor(d[[group]]))
    if (all(table(g) >= 2) && nlevels(g) >= 2) {
      bt <- stats::bartlett.test(r, g)
      tests$bartlett <- data.frame(test = "Bartlett", assumption = "Homogeneidade de variancias",
                                   statistic = unname(bt$statistic), df = unname(bt$parameter), p = bt$p.value)
      lv <- levene_test(r, g, center = "mean")
      tests$levene <- data.frame(test = "Levene (media)", assumption = "Homogeneidade de variancias",
                                 statistic = lv$F, df = paste(lv$df1, lv$df2, sep = ", "), p = lv$p)
      bf <- levene_test(r, g, center = "median")
      tests$bf <- data.frame(test = "Brown-Forsythe (Levene, mediana)", assumption = "Homogeneidade de variancias",
                             statistic = bf$F, df = paste(bf$df1, bf$df2, sep = ", "), p = bf$p)
      fk <- stats::fligner.test(r, g)
      tests$fligner <- data.frame(test = "Fligner-Killeen", assumption = "Homogeneidade de variancias",
                                  statistic = unname(fk$statistic), df = unname(fk$parameter), p = fk$p.value)
    }
  }
  tests <- do.call(rbind, lapply(tests, function(z) { z$df <- as.character(z$df); z }))
  if (!is.null(tests)) {
    tests$conclusion <- ifelse(tests$p < alpha, "Evidencia de violacao", "Sem evidencia de violacao")
    rownames(tests) <- NULL
  }
  h <- stats::hatvalues(fit)
  rs <- suppressWarnings(stats::rstudent(fit))
  ck <- stats::cooks.distance(fit)
  p_bonf <- pmin(1, 2 * stats::pt(-abs(rs), stats::df.residual(fit) - 1) * n)
  infl <- data.frame(row = if (".row" %in% names(d)) d$.row else seq_len(n),
                     fitted = unname(stats::fitted(fit)), residual = unname(r),
                     std_residual = unname(suppressWarnings(stats::rstandard(fit))),
                     rstudent = unname(rs), leverage = unname(h), cooks = unname(ck),
                     p_bonferroni = unname(p_bonf))
  infl$flag <- (!is.na(infl$cooks) & infl$cooks > 4 / n) | (!is.na(p_bonf) & p_bonf < alpha)
  warns <- character()
  if (!is.null(tests) && any(tests$p < alpha & tests$assumption == "Normalidade"))
    warns <- c(warns, "Shapiro-Wilk indica desvio da normalidade dos residuos. Avalie o grafico Q-Q, a natureza da variavel (contagem, proporcao) e considere transformacao, GLM ou metodo nao parametrico.")
  if (!is.null(tests) && any(tests$p < alpha & tests$assumption != "Normalidade"))
    warns <- c(warns, "Ha indicio de heterogeneidade de variancias. Considere ANOVA de Welch / Games-Howell, transformacao ou modelagem da variancia.")
  if (any(infl$flag)) warns <- c(warns, sprintf("%d observacao(oes) potencialmente influente(s) (Cook > 4/n ou residuo studentizado significativo apos Bonferroni). Nada foi removido.", sum(infl$flag)))
  new_result(
    type = "diagnostics", method = "Diagnostico de residuos e pressupostos",
    subclass = "caagro_diagnostics", formula = stats::formula(fit),
    settings = list(group = group, alpha = alpha), data_used = d,
    tables = list(tests = tests, influence = infl), model = fit, warnings = warns,
    notes = list(
      Resultado = "Testes formais de normalidade e homogeneidade e medidas de influencia.",
      Interpretacao = "Valores de p pequenos indicam evidencia contra o pressuposto. Com muitas observacoes, desvios irrelevantes podem ser significativos; com poucas, desvios importantes podem passar despercebidos. Priorize os graficos.",
      Cuidados = "A independencia depende da casualizacao e do planejamento experimental e nao pode ser garantida por um teste automatico. Testes de normalidade nao devem, sozinhos, decidir o metodo."
    )
  )
}

levene_test <- function(y, g, center = c("mean", "median")) {
  center <- match.arg(center)
  cfun <- if (center == "mean") mean else stats::median
  z <- abs(y - stats::ave(y, g, FUN = cfun))
  fit <- stats::lm(z ~ g)
  a <- stats::anova(fit)
  list(F = a$`F value`[1], df1 = a$Df[1], df2 = a$Df[2], p = a$`Pr(>F)`[1])
}
