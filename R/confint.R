#' Confidence intervals for one or two samples
#'
#' Classical (analytical) confidence intervals:
#' * `"mean"`: \eqn{\bar{y} \pm t_{1-\alpha/2, n-1} s/\sqrt{n}}.
#' * `"variance"`, `"sd"`: chi-square interval
#'   \eqn{[(n-1)s^2/\chi^2_{1-\alpha/2}, (n-1)s^2/\chi^2_{\alpha/2}]}
#'   (sensitive to non-normality).
#' * `"proportion"`: Wilson score (default), Clopper-Pearson exact or Wald.
#' * `"diff_means"`: pooled-variance (Student) interval for
#'   \eqn{\mu_1 - \mu_2}.
#' * `"diff_means_welch"`: Welch interval, no equal-variance assumption.
#' * `"var_ratio"`: F interval for \eqn{\sigma^2_1/\sigma^2_2} (assumes
#'   normality).
#' * `"paired_diff"`: interval for the mean paired difference.
#' * `"group_means"`: one interval per treatment mean (modernised engine of
#'   the legacy `anIC()`), with pooled (model error) or separate variances
#'   and optional Bonferroni simultaneous coverage. Overlap of individual
#'   intervals is not a test of differences.
#' * `"median"`, `"diff_medians"`: bootstrap only.
#'
#' Bootstrap intervals (`method = "bootstrap"`) use non-parametric
#' resampling (stratified by group for differences) with percentile or BCa
#' limits (Efron, 1987; Efron and Tibshirani, 1993).
#'
#' @param data A data frame.
#' @param response Response column (numeric; for proportions, numeric 0/1,
#'   logical or a column compared to `success`).
#' @param parameter Parameter (see Description).
#' @param group Two-level group column for two-sample parameters.
#' @param id Pair identifier for `"paired_diff"`.
#' @param conf_level Confidence level.
#' @param success Value of `response` counted as success (proportions).
#' @param prop_method `"wilson"`, `"exact"` (Clopper-Pearson) or `"wald"`.
#' @param method `"classic"` (analytical, default) or `"bootstrap"`
#'   (percentile or BCa; available for `"mean"`, `"median"`, `"sd"`,
#'   `"diff_means"` and `"diff_medians"`).
#' @param block,variance,simultaneous,exclude For `parameter =
#'   "group_means"`: optional block column; `"pooled"` (model error, assumes
#'   equal variances) or `"separate"` variances; `"none"` or `"bonferroni"`
#'   simultaneous coverage; rows to exclude.
#' @param R,boot_type,seed Bootstrap settings: number of resamples,
#'   `"bca"` or `"percentile"`, random seed.
#' @return A `caagro_result` (class `caagro_ci`, or `caagro_ci_groups` for
#'   `"group_means"`) with table `interval` (or `intervals`).
#' @references
#' Wilson, E. B. (1927). Probable inference, the law of succession, and
#' statistical inference. *Journal of the American Statistical
#' Association*, 22(158), 209-212. \doi{10.1080/01621459.1927.10502953}
#'
#' Clopper, C. J. and Pearson, E. S. (1934). The use of confidence or
#' fiducial limits illustrated in the case of the binomial. *Biometrika*,
#' 26(4), 404-413. \doi{10.2307/2331986}
#'
#' Welch, B. L. (1947). *Biometrika*, 34(1/2), 28-35. \doi{10.2307/2332510}
#'
#' Efron, B. (1987). Better bootstrap confidence intervals. *Journal of the
#' American Statistical Association*, 82(397), 171-185.
#' \doi{10.1080/01621459.1987.10478410}
#'
#' Efron, B. and Tibshirani, R. J. (1993). *An Introduction to the
#' Bootstrap*. Chapman and Hall/CRC. \doi{10.1201/9780429246593}
#' @examples
#' conf_int(PlantGrowth, "weight", "mean")
#' conf_int(PlantGrowth, "weight", "variance")
#' conf_int(subset(PlantGrowth, group != "trt2"), "weight", "diff_means_welch", group = "group")
#' r <- conf_int(PlantGrowth, "weight", "group_means", group = "group")
#' plot(r)
#' conf_int(PlantGrowth, "weight", "median", method = "bootstrap", R = 999, seed = 1)
#' @noRd
conf_int <- function(data, response,
                     parameter = c("mean", "variance", "sd", "proportion", "diff_means",
                                   "diff_means_welch", "var_ratio", "paired_diff", "group_means",
                                   "median", "diff_medians"),
                     group = NULL, id = NULL, conf_level = 0.95, success = NULL,
                     prop_method = c("wilson", "exact", "wald"), method = c("classic", "bootstrap"),
                     block = NULL, variance = c("pooled", "separate"), simultaneous = c("none", "bonferroni"),
                     exclude = NULL, R = 2000, boot_type = c("bca", "percentile"), seed = 123) {
  parameter <- match.arg(parameter); prop_method <- match.arg(prop_method); method <- match.arg(method)
  if (parameter == "group_means") {
    if (is.null(group)) abort_caagro("Informe a coluna de tratamentos em `group`.")
    return(conf_int_groups(data, response, group, block = block, variance = match.arg(variance),
                           simultaneous = match.arg(simultaneous), conf_level = conf_level, exclude = exclude))
  }
  if (method == "bootstrap" || parameter %in% c("median", "diff_medians")) {
    stat <- switch(parameter, mean = "mean", median = "median", sd = "sd", diff_means = , diff_means_welch = "diff_means",
                   diff_medians = "diff_medians",
                   abort_caagro("Bootstrap dispon\u00edvel para m\u00e9dia, mediana, desvio-padr\u00e3o e diferen\u00e7as de m\u00e9dias/medianas."))
    return(conf_int_boot(data, response, stat, group = group, R = R, type = match.arg(boot_type), conf_level = conf_level, seed = seed))
  }
  check_conf(conf_level)
  a <- 1 - conf_level
  data <- check_data_frame(data)
  if (parameter == "proportion") {
    check_columns(data, response)
    v <- data[[response]]
    v <- v[!is.na(v)]
    s <- if (!is.null(success)) v == success else if (is.logical(v)) v else {
      vn <- as_numeric_strict(v, response)
      if (!all(vn %in% c(0, 1))) abort_caagro("Para propor\u00e7\u00f5es, a resposta deve ser 0/1, l\u00f3gica, ou informe `success`.")
      vn == 1
    }
    x <- sum(s); n <- length(s); p <- x / n
    z <- stats::qnorm(1 - a / 2)
    ci <- switch(prop_method,
      wilson = { c0 <- (p + z^2 / (2 * n)) / (1 + z^2 / n); h <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n); c(c0 - h, c0 + h) },
      exact = as.numeric(stats::binom.test(x, n, conf.level = conf_level)$conf.int),
      wald = p + c(-1, 1) * z * sqrt(p * (1 - p) / n))
    tb <- data.frame(parameter = "propor\u00e7\u00e3o", estimate = p, lower = max(0, ci[1]), upper = min(1, ci[2]),
                     n = n, successes = x, method = prop_method, conf_level = conf_level)
    used <- data
    warns <- if (prop_method == "wald" && (n * p < 5 || n * (1 - p) < 5)) "O intervalo de Wald e pouco confi\u00e1vel com n*p ou n*(1-p) < 5; prefira Wilson ou exato." else character()
  } else if (parameter %in% c("mean", "variance", "sd")) {
    prep <- prepare_data(data, response); used <- prep$data
    y <- used[[response]]; n <- length(y)
    if (n < 2) abort_caagro("S\u00e3o necessarias ao menos 2 observa\u00e7\u00f5es.")
    s2 <- stats::var(y)
    if (parameter == "mean") {
      se <- sqrt(s2 / n); tq <- stats::qt(1 - a / 2, n - 1)
      tb <- data.frame(parameter = "m\u00e9dia", estimate = mean(y), se = se, df = n - 1,
                       lower = mean(y) - tq * se, upper = mean(y) + tq * se, n = n, conf_level = conf_level)
    } else {
      lo <- (n - 1) * s2 / stats::qchisq(1 - a / 2, n - 1); hi <- (n - 1) * s2 / stats::qchisq(a / 2, n - 1)
      if (parameter == "sd") { est <- sqrt(s2); lo <- sqrt(lo); hi <- sqrt(hi) } else est <- s2
      tb <- data.frame(parameter = if (parameter == "sd") "desvio-padr\u00e3o" else "vari\u00e2ncia",
                       estimate = est, df = n - 1, lower = lo, upper = hi, n = n, conf_level = conf_level)
    }
    warns <- c(prep$messages, if (parameter != "mean") "O intervalo qui-quadrado para vari\u00e2ncia e muito sens\u00edvel a desvios da normalidade; considere bootstrap." else character())
  } else if (parameter == "paired_diff") {
    r <- mean_test(data, response, group, type = "paired", id = id, conf_level = conf_level)
    tb <- data.frame(parameter = "diferen\u00e7a m\u00e9dia pareada", estimate = r$tables$test$estimate,
                     df = r$tables$test$df, lower = r$tables$test$lower, upper = r$tables$test$upper,
                     n = r$tables$descriptives$n[1], conf_level = conf_level)
    used <- r$data_used; warns <- r$warnings
  } else {
    if (is.null(group)) abort_caagro("Informe `group` com dois n\u00edveis.")
    prep <- prepare_data(data, response, group); used <- prep$data
    g <- droplevels(used[[group]])
    if (nlevels(g) != 2) abort_caagro("S\u00e3o necessarios exatamente 2 grupos.")
    lv <- levels(g)
    y1 <- used[[response]][g == lv[1]]; y2 <- used[[response]][g == lv[2]]
    n1 <- length(y1); n2 <- length(y2); v1 <- stats::var(y1); v2 <- stats::var(y2)
    if (parameter == "var_ratio") {
      est <- v1 / v2
      tb <- data.frame(parameter = paste0("var(", lv[1], ")/var(", lv[2], ")"), estimate = est,
                       df1 = n1 - 1, df2 = n2 - 1,
                       lower = est / stats::qf(1 - a / 2, n1 - 1, n2 - 1),
                       upper = est / stats::qf(a / 2, n1 - 1, n2 - 1), conf_level = conf_level)
      warns <- c(prep$messages, "O intervalo F para raz\u00e3o de vari\u00e2ncias pressup\u00f5e normalidade e e pouco robusto.")
    } else {
      est <- mean(y1) - mean(y2)
      if (parameter == "diff_means") {
        sp2 <- ((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2)
        se <- sqrt(sp2 * (1 / n1 + 1 / n2)); df <- n1 + n2 - 2
      } else {
        se <- sqrt(v1 / n1 + v2 / n2)
        df <- (v1 / n1 + v2 / n2)^2 / ((v1 / n1)^2 / (n1 - 1) + (v2 / n2)^2 / (n2 - 1))
      }
      tq <- stats::qt(1 - a / 2, df)
      tb <- data.frame(parameter = paste(lv[1], "-", lv[2]), estimate = est, se = se, df = df,
                       lower = est - tq * se, upper = est + tq * se, conf_level = conf_level)
      warns <- prep$messages
    }
  }
  labels <- c(mean = "IC para a m\u00e9dia", variance = "IC para a vari\u00e2ncia", sd = "IC para o desvio-padr\u00e3o",
              proportion = "IC para propor\u00e7\u00e3o", diff_means = "IC para diferen\u00e7a de m\u00e9dias (vari\u00e2ncias iguais)",
              diff_means_welch = "IC para diferen\u00e7a de m\u00e9dias (Welch)", var_ratio = "IC para raz\u00e3o de vari\u00e2ncias",
              paired_diff = "IC para diferen\u00e7a m\u00e9dia pareada")
  new_result("ci", labels[[parameter]], "caagro_ci",
             settings = list(response = response, parameter = parameter, group = group, id = id,
                             conf_level = conf_level, prop_method = prop_method),
             data = data, data_used = used, tables = list(interval = tb), warnings = warns,
             notes = list(Resultado = sprintf("%s: estimativa = %.4g; IC %g%% = [%.4g; %.4g].", labels[[parameter]], tb$estimate, 100 * conf_level, tb$lower, tb$upper),
                          Interpretacao = sprintf("Se o procedimento fosse repetido muitas vezes, cerca de %g%% dos intervalos conteriam o par\u00e2metro verdadeiro.", 100 * conf_level),
                          Cuidados = "O n\u00edvel de confian\u00e7a refere-se ao procedimento, n\u00e3o a probabilidade de o par\u00e2metro estar neste intervalo espec\u00edfico."),
             code = code_call("caagro:::conf_int", list(data = as.name("dados"), response = response, parameter = parameter,
                                                       group = group, id = id, conf_level = conf_level, success = success,
                                                       prop_method = if (parameter == "proportion") prop_method)))
}

# Group mean intervals (called by conf_int(parameter = "group_means"))
conf_int_groups <- function(data, response, group, block = NULL,
                            variance = c("pooled", "separate"),
                            simultaneous = c("none", "bonferroni"),
                            conf_level = 0.95, exclude = NULL) {
  variance <- match.arg(variance); simultaneous <- match.arg(simultaneous)
  check_conf(conf_level)
  prep <- prepare_data(data, response, c(group, block), exclude = exclude)
  d <- prep$data
  k <- nlevels(d[[group]])
  a <- 1 - conf_level
  a_eff <- if (simultaneous == "bonferroni") a / k else a
  y <- d[[response]]; g <- d[[group]]
  desc <- data.frame(level = levels(g), n = as.vector(table(g)),
                     mean = as.vector(tapply(y, g, mean)), sd = as.vector(tapply(y, g, stats::sd)))
  tables <- list()
  warns <- prep$messages
  if (variance == "pooled") {
    av <- anova_exp(d, response, group, block = block, exclude = NULL)
    mt <- av$tables$means
    dfe <- av$tables$precision$df_error
    tq <- stats::qt(1 - a_eff / 2, dfe)
    desc$estimate <- mt$emmean[match(desc$level, as.character(mt[[group]]))]
    desc$se <- mt$se[match(desc$level, as.character(mt[[group]]))]
    desc$df <- dfe
    tables$anova <- av$tables$anova
    tables$precision <- av$tables$precision
    ca <- check_assumptions(av)
    bt <- ca$tables$tests
    tables$homogeneity <- bt[bt$test == "Bartlett", setdiff(names(bt), "assumption"), drop = FALSE]
    if (any(tables$homogeneity$p < 0.05)) warns <- c(warns, "Ind\u00edcio de vari\u00e2ncias heterog\u00eaneas: considere variance = 'separate'.")
  } else {
    desc$estimate <- desc$mean
    desc$se <- desc$sd / sqrt(desc$n)
    desc$df <- desc$n - 1
    tq <- stats::qt(1 - a_eff / 2, desc$df)
    if (any(desc$n < 2)) warns <- c(warns, "Grupos com n < 2 n\u00e3o tem intervalo com vari\u00e2ncia separada.")
    if (!is.null(block)) warns <- c(warns, "Com vari\u00e2ncia separada, o efeito de blocos n\u00e3o \u00e9 considerado nos intervalos.")
  }
  desc$lower <- desc$estimate - tq * desc$se
  desc$upper <- desc$estimate + tq * desc$se
  names(desc)[1] <- group
  tables <- c(list(intervals = desc), tables)
  new_result("ci_groups", "Intervalos de confian\u00e7a por tratamento", "caagro_ci_groups",
             design = if (is.null(block)) "DIC" else "DBC",
             settings = list(response = response, group = group, block = block, variance = variance,
                             simultaneous = simultaneous, conf_level = conf_level, exclude = exclude),
             data = data, data_used = d, tables = tables, main = "intervals", warnings = warns,
             notes = list(Resultado = sprintf("Intervalos de %g%% %s para as m\u00e9dias de %d tratamentos.", 100 * conf_level,
                                              if (simultaneous == "bonferroni") "simultaneos (Bonferroni)" else "individuais", k),
                          Interpretacao = "Cada intervalo descreve a precis\u00e3o da m\u00e9dia do respectivo tratamento.",
                          Cuidados = "A sobreposi\u00e7\u00e3o (ou n\u00e3o) de intervalos individuais N\u00c3O e um teste de diferen\u00e7a entre tratamentos. Para isso, use intervalos para diferen\u00e7as ou compara\u00e7\u00f5es m\u00faltiplas."),
             code = code_call("caagro:::conf_int", list(data = as.name("dados"), response = response, parameter = "group_means", group = group,
                                                              block = block, variance = variance, simultaneous = simultaneous,
                                                              conf_level = conf_level, exclude = exclude)))
}

# Bootstrap intervals (called by conf_int(method = "bootstrap"))
conf_int_boot <- function(data, response, statistic = c("mean", "median", "sd", "diff_means", "diff_medians"),
                          group = NULL, R = 2000, type = c("bca", "percentile"),
                          conf_level = 0.95, seed = 123) {
  statistic <- match.arg(statistic); type <- match.arg(type)
  check_conf(conf_level)
  if (R < 200) abort_caagro("Use ao menos 200 reamostragens (recomendado >= 2000).")
  two <- statistic %in% c("diff_means", "diff_medians")
  prep <- prepare_data(data, response, if (two) group)
  d <- prep$data
  y <- d[[response]]
  f1 <- switch(statistic, mean = mean, median = stats::median, sd = stats::sd,
               diff_means = mean, diff_medians = stats::median)
  if (two) {
    if (is.null(group)) abort_caagro("Informe `group`.")
    g <- droplevels(d[[group]]); lv <- levels(g)
    if (length(lv) != 2) abort_caagro("S\u00e3o necessarios exatamente 2 grupos.")
    stat_fun <- function(yy, gg) f1(yy[gg == lv[1]]) - f1(yy[gg == lv[2]])
    t0 <- stat_fun(y, g)
    i1 <- which(g == lv[1]); i2 <- which(g == lv[2])
    reps <- with_seed(seed, vapply(seq_len(R), function(b) {
      idx <- c(i1[sample.int(length(i1), replace = TRUE)], i2[sample.int(length(i2), replace = TRUE)])
      stat_fun(y[idx], g[idx])
    }, numeric(1)))
    jack <- vapply(seq_along(y), function(i) stat_fun(y[-i], g[-i]), numeric(1))
  } else {
    t0 <- f1(y)
    reps <- with_seed(seed, vapply(seq_len(R), function(b) f1(y[sample.int(length(y), replace = TRUE)]), numeric(1)))
    jack <- vapply(seq_along(y), function(i) f1(y[-i]), numeric(1))
  }
  a <- 1 - conf_level
  probs <- c(a / 2, 1 - a / 2)
  if (type == "bca") {
    z0 <- stats::qnorm(mean(reps < t0) + 0.5 * mean(reps == t0))
    jm <- mean(jack)
    acc <- sum((jm - jack)^3) / (6 * sum((jm - jack)^2)^1.5)
    if (!is.finite(acc)) acc <- 0
    zq <- stats::qnorm(probs)
    probs <- stats::pnorm(z0 + (z0 + zq) / (1 - acc * (z0 + zq)))
  }
  ci <- stats::quantile(reps, probs, names = FALSE, type = 6)
  tb <- data.frame(parameter = statistic, estimate = t0, bias = mean(reps) - t0, se_boot = stats::sd(reps),
                   lower = ci[1], upper = ci[2], R = R, type = type, seed = seed, conf_level = conf_level)
  warns <- prep$messages
  if (type == "bca" && R < 1000) warns <- c(warns, "BCa com poucas reamostragens pode ser inst\u00e1vel; use R >= 2000.")
  if (length(y) < 10) warns <- c(warns, "Amostra pequena: o bootstrap pode subestimar a variabilidade.")
  new_result("ci", sprintf("IC bootstrap (%s) para %s", if (type == "bca") "BCa" else "percentil", statistic),
             "caagro_ci",
             settings = list(response = response, statistic = statistic, group = group, R = R, type = type,
                             conf_level = conf_level, seed = seed),
             data = data, data_used = d, tables = list(interval = tb), warnings = warns,
             model = list(replicates = reps), seed = seed,
             notes = list(Resultado = sprintf("Estimativa = %.4g; IC %g%% bootstrap = [%.4g; %.4g] (R = %d, semente = %s).", t0, 100 * conf_level, ci[1], ci[2], R, seed),
                          Cuidados = "Os resultados dependem da semente e do n\u00famero de reamostragens; o bootstrap pressup\u00f5e que a amostra representa bem a popula\u00e7\u00e3o."),
             code = code_call("caagro:::conf_int", list(data = as.name("dados"), response = response, parameter = statistic, method = "bootstrap",
                                                            group = group, R = R, boot_type = type, conf_level = conf_level, seed = seed)))
}
