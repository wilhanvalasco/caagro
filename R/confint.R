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
#' @return A `caagro_result` (class `caagro_ci`) with table `interval`.
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
#' @examples
#' conf_int(PlantGrowth, "weight", "mean")
#' conf_int(PlantGrowth, "weight", "variance")
#' conf_int(subset(PlantGrowth, group != "trt2"), "weight", "diff_means_welch", group = "group")
#' @export
conf_int <- function(data, response,
                     parameter = c("mean", "variance", "sd", "proportion", "diff_means",
                                   "diff_means_welch", "var_ratio", "paired_diff"),
                     group = NULL, id = NULL, conf_level = 0.95, success = NULL,
                     prop_method = c("wilson", "exact", "wald")) {
  parameter <- match.arg(parameter); prop_method <- match.arg(prop_method)
  check_conf(conf_level)
  a <- 1 - conf_level
  data <- check_data_frame(data)
  if (parameter == "proportion") {
    check_columns(data, response)
    v <- data[[response]]
    v <- v[!is.na(v)]
    s <- if (!is.null(success)) v == success else if (is.logical(v)) v else {
      vn <- as_numeric_strict(v, response)
      if (!all(vn %in% c(0, 1))) abort_caagro("Para proporcoes, a resposta deve ser 0/1, logica, ou informe `success`.")
      vn == 1
    }
    x <- sum(s); n <- length(s); p <- x / n
    z <- stats::qnorm(1 - a / 2)
    ci <- switch(prop_method,
      wilson = { c0 <- (p + z^2 / (2 * n)) / (1 + z^2 / n); h <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n); c(c0 - h, c0 + h) },
      exact = as.numeric(stats::binom.test(x, n, conf.level = conf_level)$conf.int),
      wald = p + c(-1, 1) * z * sqrt(p * (1 - p) / n))
    tb <- data.frame(parameter = "proporcao", estimate = p, lower = max(0, ci[1]), upper = min(1, ci[2]),
                     n = n, successes = x, method = prop_method, conf_level = conf_level)
    used <- data
    warns <- if (prop_method == "wald" && (n * p < 5 || n * (1 - p) < 5)) "O intervalo de Wald e pouco confiavel com n*p ou n*(1-p) < 5; prefira Wilson ou exato." else character()
  } else if (parameter %in% c("mean", "variance", "sd")) {
    prep <- prepare_data(data, response); used <- prep$data
    y <- used[[response]]; n <- length(y)
    if (n < 2) abort_caagro("Sao necessarias ao menos 2 observacoes.")
    s2 <- stats::var(y)
    if (parameter == "mean") {
      se <- sqrt(s2 / n); tq <- stats::qt(1 - a / 2, n - 1)
      tb <- data.frame(parameter = "media", estimate = mean(y), se = se, df = n - 1,
                       lower = mean(y) - tq * se, upper = mean(y) + tq * se, n = n, conf_level = conf_level)
    } else {
      lo <- (n - 1) * s2 / stats::qchisq(1 - a / 2, n - 1); hi <- (n - 1) * s2 / stats::qchisq(a / 2, n - 1)
      if (parameter == "sd") { est <- sqrt(s2); lo <- sqrt(lo); hi <- sqrt(hi) } else est <- s2
      tb <- data.frame(parameter = if (parameter == "sd") "desvio-padrao" else "variancia",
                       estimate = est, df = n - 1, lower = lo, upper = hi, n = n, conf_level = conf_level)
    }
    warns <- c(prep$messages, if (parameter != "mean") "O intervalo qui-quadrado para variancia e muito sensivel a desvios da normalidade; considere bootstrap." else character())
  } else if (parameter == "paired_diff") {
    r <- mean_test(data, response, group, type = "paired", id = id, conf_level = conf_level)
    tb <- data.frame(parameter = "diferenca media pareada", estimate = r$tables$test$estimate,
                     df = r$tables$test$df, lower = r$tables$test$lower, upper = r$tables$test$upper,
                     n = r$tables$descriptives$n[1], conf_level = conf_level)
    used <- r$data_used; warns <- r$warnings
  } else {
    if (is.null(group)) abort_caagro("Informe `group` com dois niveis.")
    prep <- prepare_data(data, response, group); used <- prep$data
    g <- droplevels(used[[group]])
    if (nlevels(g) != 2) abort_caagro("Sao necessarios exatamente 2 grupos.")
    lv <- levels(g)
    y1 <- used[[response]][g == lv[1]]; y2 <- used[[response]][g == lv[2]]
    n1 <- length(y1); n2 <- length(y2); v1 <- stats::var(y1); v2 <- stats::var(y2)
    if (parameter == "var_ratio") {
      est <- v1 / v2
      tb <- data.frame(parameter = paste0("var(", lv[1], ")/var(", lv[2], ")"), estimate = est,
                       df1 = n1 - 1, df2 = n2 - 1,
                       lower = est / stats::qf(1 - a / 2, n1 - 1, n2 - 1),
                       upper = est / stats::qf(a / 2, n1 - 1, n2 - 1), conf_level = conf_level)
      warns <- c(prep$messages, "O intervalo F para razao de variancias pressupoe normalidade e e pouco robusto.")
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
  labels <- c(mean = "IC para a media", variance = "IC para a variancia", sd = "IC para o desvio-padrao",
              proportion = "IC para proporcao", diff_means = "IC para diferenca de medias (variancias iguais)",
              diff_means_welch = "IC para diferenca de medias (Welch)", var_ratio = "IC para razao de variancias",
              paired_diff = "IC para diferenca media pareada")
  new_result("ci", labels[[parameter]], "caagro_ci",
             settings = list(response = response, parameter = parameter, group = group, id = id,
                             conf_level = conf_level, prop_method = prop_method),
             data = data, data_used = used, tables = list(interval = tb), warnings = warns,
             notes = list(Resultado = sprintf("%s: estimativa = %.4g; IC %g%% = [%.4g; %.4g].", labels[[parameter]], tb$estimate, 100 * conf_level, tb$lower, tb$upper),
                          Interpretacao = sprintf("Se o procedimento fosse repetido muitas vezes, cerca de %g%% dos intervalos conteriam o parametro verdadeiro.", 100 * conf_level),
                          Cuidados = "O nivel de confianca refere-se ao procedimento, nao a probabilidade de o parametro estar neste intervalo especifico."),
             code = code_call("caagro::conf_int", list(data = as.name("dados"), response = response, parameter = parameter,
                                                       group = group, id = id, conf_level = conf_level, success = success,
                                                       prop_method = if (parameter == "proportion") prop_method)))
}

#' Confidence intervals for the means of several treatments
#'
#' Modernised engine of the legacy `anIC()` application.
#'
#' @param data A data frame.
#' @param response Response column.
#' @param group Treatment column.
#' @param block Optional block column. With `variance = "pooled"`, the error
#'   variance comes from the DBC model.
#' @param variance `"pooled"`: intervals use the model MSE and its degrees of
#'   freedom (assumes homogeneous variances); `"separate"`: each group uses
#'   its own standard deviation and `n - 1` df.
#' @param simultaneous `"none"` (individual intervals) or `"bonferroni"`
#'   (simultaneous coverage for all groups).
#' @param conf_level Confidence level.
#' @param exclude Rows to exclude explicitly.
#' @return A `caagro_result` (class `caagro_ci_groups`) with table
#'   `intervals` plus `anova` and `precision` when pooled.
#' @examples
#' r <- conf_int_groups(PlantGrowth, "weight", "group")
#' r
#' plot(r)
#' @export
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
    bt <- check_assumptions(av)$tables$tests
    tables$homogeneity <- bt[bt$assumption != "Normalidade", , drop = FALSE]
    if (any(tables$homogeneity$p < 0.05)) warns <- c(warns, "Indicio de variancias heterogeneas: considere variance = 'separate'.")
  } else {
    desc$estimate <- desc$mean
    desc$se <- desc$sd / sqrt(desc$n)
    desc$df <- desc$n - 1
    tq <- stats::qt(1 - a_eff / 2, desc$df)
    if (any(desc$n < 2)) warns <- c(warns, "Grupos com n < 2 nao tem intervalo com variancia separada.")
    if (!is.null(block)) warns <- c(warns, "Com variancia separada, o efeito de blocos nao e considerado nos intervalos.")
  }
  desc$lower <- desc$estimate - tq * desc$se
  desc$upper <- desc$estimate + tq * desc$se
  names(desc)[1] <- group
  tables <- c(list(intervals = desc), tables)
  new_result("ci_groups", "Intervalos de confianca por tratamento", "caagro_ci_groups",
             design = if (is.null(block)) "DIC" else "DBC",
             settings = list(response = response, group = group, block = block, variance = variance,
                             simultaneous = simultaneous, conf_level = conf_level, exclude = exclude),
             data = data, data_used = d, tables = tables, main = "intervals", warnings = warns,
             notes = list(Resultado = sprintf("Intervalos de %g%% %s para as medias de %d tratamentos.", 100 * conf_level,
                                              if (simultaneous == "bonferroni") "simultaneos (Bonferroni)" else "individuais", k),
                          Interpretacao = "Cada intervalo descreve a precisao da media do respectivo tratamento.",
                          Cuidados = "A sobreposicao (ou nao) de intervalos individuais NAO e um teste de diferenca entre tratamentos. Para isso, use intervalos para diferencas ou comparacoes multiplas."),
             code = code_call("caagro::conf_int_groups", list(data = as.name("dados"), response = response, group = group,
                                                              block = block, variance = variance, simultaneous = simultaneous,
                                                              conf_level = conf_level, exclude = exclude)))
}

#' Bootstrap confidence intervals
#'
#' Non-parametric bootstrap with percentile or bias-corrected and
#' accelerated (BCa) intervals. For two-group statistics resampling is
#' stratified by group. The acceleration constant of BCa is estimated by the
#' jackknife.
#'
#' @param data A data frame.
#' @param response Response column.
#' @param statistic `"mean"`, `"median"`, `"sd"`, `"diff_means"` or
#'   `"diff_medians"`.
#' @param group Two-level group column for differences.
#' @param R Number of bootstrap resamples (>= 999 recommended; >= 2000 for
#'   BCa).
#' @param type `"percentile"` or `"bca"`.
#' @param conf_level Confidence level.
#' @param seed Random seed (recorded in the result).
#' @return A `caagro_result` (class `caagro_ci`) with table `interval` and
#'   the bootstrap replicates in `model$replicates`.
#' @references
#' Efron, B. (1987). Better bootstrap confidence intervals. *Journal of the
#' American Statistical Association*, 82(397), 171-185.
#' \doi{10.1080/01621459.1987.10478410}
#'
#' Efron, B. and Tibshirani, R. J. (1993). *An Introduction to the
#' Bootstrap*. Chapman and Hall/CRC. \doi{10.1201/9780429246593}
#' @examples
#' conf_int_boot(PlantGrowth, "weight", "median", R = 999, seed = 1)
#' @export
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
    if (length(lv) != 2) abort_caagro("Sao necessarios exatamente 2 grupos.")
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
  if (type == "bca" && R < 1000) warns <- c(warns, "BCa com poucas reamostragens pode ser instavel; use R >= 2000.")
  if (length(y) < 10) warns <- c(warns, "Amostra pequena: o bootstrap pode subestimar a variabilidade.")
  new_result("ci", sprintf("IC bootstrap (%s) para %s", if (type == "bca") "BCa" else "percentil", statistic),
             "caagro_ci",
             settings = list(response = response, statistic = statistic, group = group, R = R, type = type,
                             conf_level = conf_level, seed = seed),
             data = data, data_used = d, tables = list(interval = tb), warnings = warns,
             model = list(replicates = reps), seed = seed,
             notes = list(Resultado = sprintf("Estimativa = %.4g; IC %g%% bootstrap = [%.4g; %.4g] (R = %d, semente = %s).", t0, 100 * conf_level, ci[1], ci[2], R, seed),
                          Cuidados = "Os resultados dependem da semente e do numero de reamostragens; o bootstrap pressupoe que a amostra representa bem a populacao."),
             code = code_call("caagro::conf_int_boot", list(data = as.name("dados"), response = response, statistic = statistic,
                                                            group = group, R = R, type = type, conf_level = conf_level, seed = seed)))
}
