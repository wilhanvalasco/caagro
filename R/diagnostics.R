#' Residual diagnostics and assumption checks
#'
#' Tests and measures to support (not replace) the scientific evaluation of
#' the assumptions of the ANOVA model:
#' * Table `tests` (main assumptions): Shapiro-Wilk for normality of the
#'   model residuals (3 <= n <= 5000) and Bartlett for homogeneity of
#'   variances, with a short interpretation.
#' * Table `robust`: Levene (mean), Brown-Forsythe (median) and
#'   Fligner-Killeen, robust alternatives to Bartlett, which is sensitive to
#'   non-normality. They are reported, never substituted silently.
#' * Table `influence`: studentised residuals, leverage and Cook's distance.
#'
#' Variance tests use the model residuals grouped by treatment; in factorial
#' experiments the groups are the treatment combinations (cells). In DIC
#' this is equivalent to using the raw response; in DBC it removes the block
#' effect before comparing variances.
#'
#' Independence cannot be verified by an automatic test: it depends on
#' randomisation and on the experimental plan.
#'
#' @param x A `caagro_anova` result or an `lm` object.
#' @param group Grouping factor for variance tests (default: treatment, or
#'   the combination of all factors in factorial experiments).
#' @param alpha Significance level used to flag results.
#' @return An object of classes `caagro_diagnostics` and `caagro_result`
#'   with tables `tests` (Shapiro-Wilk and Bartlett), `robust` and
#'   `influence`.
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
#' @noRd
check_assumptions <- function(x, group = NULL, alpha = 0.05) {
  gvec <- NULL
  if (inherits(x, "caagro_anova")) {
    fit <- x$model; d <- x$data_used
    facs <- c(x$settings$treatment, x$settings$factors)
    if (is.null(group) && length(facs) > 1) {
      gvec <- interaction(d[, facs, drop = FALSE], sep = " : ", drop = TRUE)
      group <- paste(facs, collapse = " x ")
    }
    group <- group %||% x$settings$treatment
  } else if (inherits(x, "lm")) {
    fit <- x; d <- stats::model.frame(fit)
  } else abort_caagro("`x` deve ser resultado de anova_exp() ou um objeto lm.")
  r <- stats::residuals(fit)
  n <- length(r)
  tests <- list(); robust <- list()
  sw_msg <- NULL
  if (n >= 3 && n <= 5000 && stats::sd(r) > 0) {
    sw <- stats::shapiro.test(r)
    tests$shapiro <- data.frame(test = "Shapiro-Wilk", assumption = "Normalidade dos res\u00edduos",
                                statistic = unname(sw$statistic), df = NA_character_, p = sw$p.value)
  } else {
    sw_msg <- sprintf("Shapiro-Wilk n\u00e3o aplicado: exige entre 3 e 5000 res\u00edduos (n = %d).", n)
  }
  g <- if (!is.null(gvec)) gvec else if (!is.null(group) && group %in% names(d)) droplevels(as.factor(d[[group]])) else NULL
  bt_msg <- NULL
  if (!is.null(g)) {
    if (all(table(g) >= 2) && nlevels(g) >= 2) {
      bt <- stats::bartlett.test(r, g)
      tests$bartlett <- data.frame(test = "Bartlett", assumption = "Homogeneidade de vari\u00e2ncias",
                                   statistic = unname(bt$statistic), df = as.character(unname(bt$parameter)), p = bt$p.value)
      lv <- levene_test(r, g, center = "mean")
      robust$levene <- data.frame(test = "Levene (m\u00e9dia)", statistic = lv$F, df = paste(lv$df1, lv$df2, sep = ", "), p = lv$p)
      bf <- levene_test(r, g, center = "median")
      robust$bf <- data.frame(test = "Brown-Forsythe (Levene, mediana)", statistic = bf$F, df = paste(bf$df1, bf$df2, sep = ", "), p = bf$p)
      fk <- stats::fligner.test(r, g)
      robust$fligner <- data.frame(test = "Fligner-Killeen", statistic = unname(fk$statistic), df = as.character(unname(fk$parameter)), p = fk$p.value)
    } else {
      bt_msg <- "Bartlett n\u00e3o aplicado: cada grupo precisa de ao menos 2 observa\u00e7\u00f5es."
    }
  }
  tests <- do.call(rbind, tests)
  if (!is.null(tests)) {
    tests$interpretation <- ifelse(tests$test == "Shapiro-Wilk",
      ifelse(tests$p > alpha,
             "O teste de Shapiro-Wilk n\u00e3o identificou evid\u00eancia estat\u00edstica suficiente para rejeitar a hip\u00f3tese de normalidade dos res\u00edduos.",
             "O teste de Shapiro-Wilk identificou evid\u00eancia de desvio da normalidade dos res\u00edduos."),
      ifelse(tests$p > alpha,
             "O teste de Bartlett n\u00e3o identificou evid\u00eancia estat\u00edstica suficiente de diferen\u00e7as entre as vari\u00e2ncias.",
             "O teste de Bartlett identificou evid\u00eancia de heterogeneidade das vari\u00e2ncias."))
    rownames(tests) <- NULL
  }
  robust <- do.call(rbind, robust)
  if (!is.null(robust)) {
    robust$conclusion <- ifelse(robust$p < alpha, "Evid\u00eancia de heterogeneidade", "Sem evid\u00eancia de heterogeneidade")
    rownames(robust) <- NULL
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
  warns <- c(sw_msg, bt_msg)
  if (!is.null(tests) && any(tests$p <= alpha & tests$test == "Shapiro-Wilk"))
    warns <- c(warns, "Shapiro-Wilk indica desvio da normalidade dos res\u00edduos. Avalie o gr\u00e1fico Q-Q, a natureza da vari\u00e1vel (contagem, propor\u00e7\u00e3o) e considere transforma\u00e7\u00e3o, GLM ou m\u00e9todo n\u00e3o param\u00e9trico.")
  if (!is.null(tests) && any(tests$p <= alpha & tests$test == "Bartlett"))
    warns <- c(warns, paste0("H\u00e1 ind\u00edcio de heterogeneidade de vari\u00e2ncias. Bartlett \u00e9 sens\u00edvel a desvios da normalidade: confira os testes robustos (Levene/Brown-Forsythe). ",
                             "Alternativas: transforma\u00e7\u00e3o, ANOVA de Welch / Games-Howell (DIC) ou modelagem da vari\u00e2ncia."))
  if (any(infl$flag)) warns <- c(warns, sprintf("%d observa\u00e7\u00e3o(\u00f5es) potencialmente influente(s) (Cook > 4/n ou res\u00edduo studentizado significativo ap\u00f3s Bonferroni). Nada foi removido.", sum(infl$flag)))
  new_result(
    type = "diagnostics", method = "Diagn\u00f3stico de res\u00edduos e pressupostos",
    subclass = "caagro_diagnostics", formula = stats::formula(fit),
    settings = list(group = group, alpha = alpha), data_used = d,
    tables = list(tests = tests, robust = robust, influence = infl), model = fit, warnings = warns,
    notes = list(
      Resultado = "Testes formais de normalidade e homogeneidade e medidas de influ\u00eancia.",
      Interpretacao = "Valores de p pequenos indicam evid\u00eancia contra o pressuposto. Com muitas observa\u00e7\u00f5es, desvios irrelevantes podem ser significativos; com poucas, desvios importantes podem passar despercebidos. Priorize os gr\u00e1ficos.",
      Cuidados = "A independ\u00eancia depende da casualiza\u00e7\u00e3o e do planejamento experimental e n\u00e3o pode ser garantida por um teste autom\u00e1tico. Testes de normalidade n\u00e3o devem, sozinhos, decidir o m\u00e9todo."
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
