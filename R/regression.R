#' Regression analysis: linear, polynomial, non-linear and binomial models
#'
#' Fits one or several regression models of a response on a quantitative
#' explanatory variable, optionally for each level of a grouping variable
#' (several curves). Models are fitted to the individual observations by
#' default (not to means), so that residual variance, standard errors, bands
#' and information criteria refer to the experimental units.
#'
#' @details
#' Available models:
#' `linear`, `quadratic`, `cubic`, `logarithmic` (fitted by least squares),
#' `exponential`, `power`, `logistic3`, `logistic4`, `gompertz`,
#' `michaelis_menten`, `asymptotic`, `mitscherlich`, `loglogistic`
#' (four-parameter log-logistic dose-response; `e` = ED50) and
#' `linear_plateau` (non-linear least squares, Gauss-Newton with fallback to
#' the bounded PORT algorithm and several controlled starting values), and
#' `binomial`, `binomial_logdose` (logistic regression for binary or
#' successes/trials data, `glm` with logit link).
#'
#' Non-linear models are fitted on the original response scale. The
#' response is never transformed implicitly.
#'
#' **Fit statistics.** For `lm` fits R2 and adjusted R2 are the usual
#' ones. For non-linear fits caagro reports the *pseudo*-R2
#' \eqn{1 - RSS/TSS}, explicitly flagged (`r2_type = "pseudo"`), because the
#' classical decomposition does not hold. For binomial models no R2 is
#' computed; McFadden's pseudo-R2, deviance and a dispersion ratio are
#' reported. RMSE = sqrt(RSS/n); `sigma` = residual standard error
#' sqrt(RSS/(n - p)). AIC/BIC use the full log-likelihood (including the
#' variance parameter for Gaussian models), so gaussian models fitted to the
#' *same* data are comparable; AICc is the small-sample correction
#' (Hurvich and Tsai, 1989). Information criteria are never compared between
#' Gaussian and binomial models, nor between fits on different data.
#'
#' **Bands.** `band = "confidence"` gives the interval for the mean response;
#' `"prediction"` the interval for a new individual observation. For `lm`
#' they are exact (t-based); for `nls` the delta method (first-order Taylor
#' expansion with numerical gradients; Bates and Watts, 1988) or a residual
#' bootstrap (`band_method = "bootstrap"`); for `glm`, Wald intervals on the
#' link scale transformed to the response scale (prediction bands are not
#' defined for binomial models).
#'
#' @param data A data frame.
#' @param x Quantitative explanatory variable (e.g. dose).
#' @param y Response. For binomial models: number of successes (with
#'   `trials`) or a 0/1 variable.
#' @param model Character vector of model ids (several models are fitted and
#'   compared).
#' @param group Optional grouping column (one curve per level).
#' @param trials Column with the number of trials (binomial models).
#' @param conf_level Confidence level for coefficients and bands.
#' @param band `"confidence"`, `"prediction"` or `"none"`.
#' @param band_method `"delta"` or `"bootstrap"` (non-linear models).
#' @param nboot Number of bootstrap refits.
#' @param seed Random seed for the bootstrap.
#' @param aggregate `"none"` (default, individual observations) or a summary
#'   (`"mean"`, `"median"`, `"min"`, `"max"`) per x level. Fitting to
#'   summaries reproduces the legacy behaviour of `anQuant` but ignores
#'   within-level variability: standard errors, R2 and bands then refer to
#'   the summaries, not to the experimental units.
#' @param n_grid Number of points of the prediction grid.
#' @param exclude Row numbers of `data` to exclude explicitly.
#' @return An object of classes `caagro_regression` and `caagro_result`.
#'   Tables: `coefficients`, `fit_stats`, `equations`, `predictions`,
#'   `means`, plus `comparison` (several models) and `curve_test`
#'   (several groups).
#' @references
#' Bates, D. M. and Watts, D. G. (1988). *Nonlinear Regression Analysis and
#' Its Applications*. Wiley. \doi{10.1002/9780470316757}
#'
#' Akaike, H. (1974). A new look at the statistical model identification.
#' *IEEE Transactions on Automatic Control*, 19(6), 716-723.
#' \doi{10.1109/TAC.1974.1100705}
#'
#' Schwarz, G. (1978). Estimating the dimension of a model. *The Annals of
#' Statistics*, 6(2), 461-464. \doi{10.1214/aos/1176344136}
#'
#' Hurvich, C. M. and Tsai, C.-L. (1989). Regression and time series model
#' selection in small samples. *Biometrika*, 76(2), 297-307.
#' \doi{10.1093/biomet/76.2.297}
#'
#' Ritz, C., Baty, F., Streibig, J. C. and Gerhard, D. (2015). Dose-response
#' analysis using R. *PLoS ONE*, 10(12), e0146021.
#' \doi{10.1371/journal.pone.0146021}
#'
#' McFadden, D. (1974). Conditional logit analysis of qualitative choice
#' behavior. In P. Zarembka (Ed.), *Frontiers in Econometrics* (pp.
#' 105-142). Academic Press.
#' When `group` is given and the model fits every group, table `curve_test`
#' compares one common curve with separate curves (extra-sum-of-squares F
#' test; likelihood-ratio test for binomial models), assuming equal residual
#' variance.
#' @seealso [optimum_dose()]
#' @examples
#' r <- fit_regression(sim_dose, x = "dose", y = "produtividade",
#'                     model = c("linear", "quadratic", "linear_plateau"),
#'                     group = "cultivar")
#' result_table(r, "comparison")
#' plot(r)
#'
#' # Binomial dose-response
#' b <- fit_regression(subset(sim_germinacao, dose > 0), "dose", "germinadas",
#'                     model = "binomial_logdose", trials = "total", group = "produto")
#' optimum_dose(b)
#' @export
fit_regression <- function(data, x, y, model = "quadratic", group = NULL, trials = NULL,
                           conf_level = 0.95, band = c("confidence", "prediction", "none"),
                           band_method = c("delta", "bootstrap"), nboot = 500, seed = 123,
                           aggregate = c("none", "mean", "median", "min", "max"),
                           n_grid = 200, exclude = NULL) {
  band <- match.arg(band); band_method <- match.arg(band_method); aggregate <- match.arg(aggregate)
  check_conf(conf_level)
  reg <- model_registry()
  bad <- setdiff(model, names(reg))
  if (length(bad)) abort_caagro("Modelo(s) desconhecido(s): ", paste(bad, collapse = ", "), ". Veja a lista em ?fit_regression.")
  types <- unique(vapply(reg[model], `[[`, "", "type"))
  is_bin <- types == "glm"
  if (length(unique(is_bin)) > 1) abort_caagro("N\u00e3o misture modelos binomiais com modelos gaussianos na mesma compara\u00e7\u00e3o.")
  is_bin <- any(is_bin)
  prep <- prepare_data(data, response = y, factors = group, numeric_cols = c(x, trials), exclude = exclude)
  d0 <- prep$data
  warns <- prep$messages
  d <- data.frame(x = d0[[x]], y = d0[[y]], .group = if (is.null(group)) factor("(todos)") else d0[[group]], .row = d0$.row)
  if (is_bin) {
    if (!is.null(trials)) {
      d$n <- d0[[trials]]; d$s <- d$y
      if (any(d$s > d$n | d$s < 0)) abort_caagro("Sucessos devem estar entre 0 e o n\u00famero de ensaios.")
    } else {
      if (!all(d$y %in% c(0, 1))) abort_caagro("Para regress\u00e3o binomial sem `trials`, a resposta deve ser 0/1.")
      d$n <- 1; d$s <- d$y
    }
    if (aggregate != "none") abort_caagro("Agrega\u00e7\u00e3o n\u00e3o se aplica a modelos binomiais.")
  }
  if (aggregate != "none") {
    fn <- switch(aggregate, mean = mean, median = stats::median, min = min, max = max)
    d <- do.call(rbind, lapply(split(d, list(d$.group, d$x), drop = TRUE), function(z)
      data.frame(x = z$x[1], y = fn(z$y), .group = z$.group[1], .row = NA)))
    warns <- c(warns, sprintf("Ajuste feito sobre a %s por n\u00edvel de x (modo legado). Erros-padr\u00e3o, R2 e bandas referem-se aos resumos, n\u00e3o as unidades experimentais.", aggregate))
  }
  # means table (raw data)
  means <- do.call(rbind, lapply(split(d0, list(if (is.null(group)) rep("(todos)", nrow(d0)) else d0[[group]], d0[[x]]), drop = TRUE), function(z) {
    yy <- if (is_bin && !is.null(trials)) z[[y]] / z[[trials]] else z[[y]]
    n <- length(yy); s <- if (n > 1) stats::sd(yy) else NA
    data.frame(group = if (is.null(group)) "(todos)" else as.character(z[[group]][1]), x = z[[x]][1], n = n, mean = mean(yy),
               sd = s, se = s / sqrt(n), ci_half = if (n > 1) stats::qt(1 - (1 - conf_level) / 2, n - 1) * s / sqrt(n) else NA)
  }))
  means <- means[order(means$group, means$x), ]; rownames(means) <- NULL

  fits <- list(); coefs <- list(); stats_l <- list(); eqs <- list(); preds <- list(); fails <- list()
  for (m in model) {
    spec <- reg[[m]]
    for (gname in levels(d$.group)) {
      dg <- d[d$.group == gname, , drop = FALSE]
      key <- paste(m, gname, sep = "||")
      chk <- reg_domain_check(spec, dg)
      if (!is.null(chk)) { fails[[key]] <- data.frame(model = m, group = gname, status = chk); next }
      fw <- character()
      fit <- withCallingHandlers(tryCatch(spec$fit(dg), error = function(e) e),
                                 warning = function(w) { fw <<- c(fw, conditionMessage(w)); invokeRestart("muffleWarning") })
      fw <- setdiff(unique(fw), "NaNs produced")
      if (length(fw)) warns <- c(warns, sprintf("[%s | %s] %s", m, gname, fw))
      if (inherits(fit, "condition")) {
        fails[[key]] <- data.frame(model = m, group = gname, status = conditionMessage(fit)); next
      }
      if (spec$type == "glm" && !isTRUE(fit$converged)) {
        fails[[key]] <- data.frame(model = m, group = gname, status = "glm n\u00e3o convergiu"); next
      }
      th <- stats::coef(fit)
      if (any(is.na(th))) { fails[[key]] <- data.frame(model = m, group = gname, status = "Par\u00e2metros n\u00e3o estim\u00e1veis (n\u00edveis de x insuficientes)."); next }
      V <- tryCatch(stats::vcov(fit), error = function(e) NULL)
      if (is.null(V) || any(!is.finite(V))) { fails[[key]] <- data.frame(model = m, group = gname, status = "Matriz de covari\u00e2ncia singular: par\u00e2metros mal determinados."); next }
      fits[[m]][[gname]] <- fit
      coefs[[key]] <- reg_coef_table(fit, spec, m, gname, conf_level)
      stats_l[[key]] <- reg_fit_stats(fit, spec, m, gname, dg)
      eqs[[key]] <- data.frame(model = m, group = gname, equation = spec$eq(unname(th)))
      grid <- seq(min(dg$x), max(dg$x), length.out = n_grid)
      if (isTRUE(spec$x_positive)) grid <- grid[grid > 0]
      preds[[key]] <- reg_bands(fit, spec, m, gname, grid, dg, conf_level, band, band_method, nboot, seed)
    }
  }
  if (!length(fits)) {
    abort_caagro("Nenhum modelo pode ser ajustado. ", paste(unique(vapply(fails, function(f) f$status, "")), collapse = " | "))
  }
  bind <- function(l) { if (!length(l)) return(data.frame()); z <- do.call(rbind, l); rownames(z) <- NULL; z }
  tables <- list(coefficients = bind(coefs), fit_stats = bind(stats_l), equations = bind(eqs),
                 predictions = bind(preds), means = means)
  if (length(fails)) {
    tables$failures <- bind(fails)
    warns <- c(warns, sprintf("%d ajuste(s) falharam (ver tabela 'failures'); nenhum resultado foi inventado para eles.", length(fails)))
  }
  if (length(model) > 1) tables$comparison <- reg_compare_table(tables$fit_stats, is_bin)
  if ("cubic" %in% model) warns <- c(warns, "O polin\u00f4mio de 3o grau tende a sobreajustar; verifique se os par\u00e2metros tem interpreta\u00e7\u00e3o biol\u00f3gica.")
  warns <- c(warns, unique(unlist(lapply(stats_l, attr, "warn"))))
  res <- new_result(
    type = "regression", method = if (length(model) == 1) paste("Regress\u00e3o -", reg[[model]]$label) else "Regress\u00e3o - compara\u00e7\u00e3o de modelos",
    subclass = "caagro_regression", formula = paste(vapply(reg[model], `[[`, "", "formula"), collapse = " | "),
    settings = list(x = x, y = y, model = model, group = group, trials = trials, conf_level = conf_level,
                    band = band, band_method = band_method, nboot = nboot, seed = seed, aggregate = aggregate,
                    exclude = exclude, binomial = is_bin),
    data = data, data_used = d0, tables = tables, main = "fit_stats", model = fits, warnings = warns,
    seed = if (band_method == "bootstrap") seed,
    notes = list(
      Resultado = paste(apply(tables$equations, 1, function(r) paste0("[", r[["model"]], if (!is.null(group)) paste0(" | ", r[["group"]]), "] ", r[["equation"]])), collapse = "; "),
      Interpretacao = "Os coeficientes descrevem a forma da resposta dentro da faixa experimental de x. Compare modelos por AICc/BIC, res\u00edduos e plausibilidade biol\u00f3gica, n\u00e3o apenas pelo R2.",
      Cuidados = "N\u00e3o extrapole alem da faixa de doses estudada. R2 de modelos n\u00e3o lineares e um pseudo-R2. A sobreposi\u00e7\u00e3o visual de curvas n\u00e3o prova igualdade: veja o teste de igualdade de curvas."),
    code = code_call("caagro::fit_regression", list(data = as.name("dados"), x = x, y = y, model = model, group = group,
                                                    trials = trials, conf_level = conf_level, band = band,
                                                    band_method = band_method, nboot = if (band_method == "bootstrap") nboot,
                                                    seed = if (band_method == "bootstrap") seed,
                                                    aggregate = if (aggregate != "none") aggregate, exclude = exclude))
  )
  if (!is.null(group) && nlevels(d$.group) > 1) {
    res$tables$curve_test <- tryCatch(compare_curves(res)$tables$test, error = function(e) NULL)
  }
  res
}

reg_domain_check <- function(spec, dg) {
  if (length(unique(dg$x)) < length(spec$params) && spec$type != "glm") return(sprintf("N\u00edveis distintos de x (%d) insuficientes para %d par\u00e2metros.", length(unique(dg$x)), length(spec$params)))
  if (nrow(dg) <= length(spec$params) && spec$type != "glm") return("Observa\u00e7\u00f5es insuficientes para estimar o modelo e o erro.")
  if (isTRUE(spec$x_positive) && any(dg$x <= 0)) return("O modelo exige x > 0 (ha doses iguais ou menores que zero).")
  if (isTRUE(spec$x_nonneg) && any(dg$x < 0)) return("O modelo exige x >= 0.")
  if (spec$type != "glm" && stats::sd(dg$y) == 0) return("Resposta sem variabilidade.")
  NULL
}

reg_coef_table <- function(fit, spec, m, gname, conf_level) {
  sm <- summary(fit)$coefficients
  dfr <- if (spec$type == "glm") Inf else stats::df.residual(fit)
  q <- if (spec$type == "glm") stats::qnorm(1 - (1 - conf_level) / 2) else stats::qt(1 - (1 - conf_level) / 2, dfr)
  data.frame(model = m, group = gname, term = spec$params, estimate = sm[, 1], se = sm[, 2],
             statistic = sm[, 3], p = sm[, 4], lower = sm[, 1] - q * sm[, 2], upper = sm[, 1] + q * sm[, 2],
             row.names = NULL)
}

reg_fit_stats <- function(fit, spec, m, gname, dg) {
  n <- nrow(dg)
  ll <- stats::logLik(fit)
  k <- attr(ll, "df")
  aic <- -2 * as.numeric(ll) + 2 * k
  aicc <- if (n - k - 1 > 0) aic + 2 * k * (k + 1) / (n - k - 1) else NA
  bic <- -2 * as.numeric(ll) + log(n) * k
  w <- character()
  if (spec$type == "glm") {
    null_ll <- as.numeric(stats::logLik(stats::glm(cbind(s, n - s) ~ 1, family = stats::binomial(), data = dg)))
    disp <- sum(stats::residuals(fit, type = "pearson")^2) / stats::df.residual(fit)
    if (disp > 1.5 && any(dg$n > 1)) w <- sprintf("[%s | %s] Raz\u00e3o de dispers\u00e3o = %.2f: ind\u00edcio de superdispers\u00e3o; erros-padr\u00e3o podem estar subestimados (considere quasibinomial ou efeitos aleat\u00f3rios).", m, gname, disp)
    out <- data.frame(model = m, group = gname, n = n, k = k, r2 = 1 - as.numeric(ll) / null_ll, r2_type = "McFadden",
                      r2_adj = NA, rmse = NA, mae = NA, mse = NA, sigma = NA, deviance = stats::deviance(fit),
                      df_resid = stats::df.residual(fit), dispersion = disp, logLik = as.numeric(ll),
                      AIC = aic, AICc = aicc, BIC = bic, p_model = stats::pchisq(fit$null.deviance - fit$deviance, fit$df.null - fit$df.residual, lower.tail = FALSE),
                      converged = isTRUE(fit$converged), iterations = fit$iter)
  } else {
    r <- stats::residuals(fit)
    rss <- sum(r^2); tss <- sum((dg$y - mean(dg$y))^2)
    p <- length(stats::coef(fit))
    r2 <- 1 - rss / tss
    r2a <- 1 - (1 - r2) * (n - 1) / (n - p)
    pm <- if (spec$type == "lm") { fs <- summary(fit)$fstatistic; stats::pf(fs[1], fs[2], fs[3], lower.tail = FALSE) } else NA
    conv <- if (spec$type == "nls") isTRUE(fit$convInfo$isConv) else TRUE
    it <- if (spec$type == "nls") fit$convInfo$finIter else NA
    out <- data.frame(model = m, group = gname, n = n, k = k, r2 = r2, r2_type = if (spec$type == "lm") "cl\u00e1ssico" else "pseudo",
                      r2_adj = r2a, rmse = sqrt(rss / n), mae = mean(abs(r)), mse = rss / n,
                      sigma = sqrt(rss / (n - p)), deviance = rss, df_resid = n - p, dispersion = NA,
                      logLik = as.numeric(ll), AIC = aic, AICc = aicc, BIC = bic, p_model = unname(pm),
                      converged = conv, iterations = it)
  }
  rownames(out) <- NULL
  attr(out, "warn") <- w
  out
}

reg_bands <- function(fit, spec, m, gname, grid, dg, conf_level, band, band_method, nboot, seed) {
  a <- 1 - conf_level
  nd <- data.frame(x = grid)
  out <- data.frame(model = m, group = gname, x = grid, fit = NA_real_, lower = NA_real_, upper = NA_real_)
  if (spec$type == "lm") {
    if (band == "none") { out$fit <- stats::predict(fit, nd); return(out) }
    pr <- stats::predict(fit, nd, interval = band, level = conf_level)
    out$fit <- pr[, 1]; out$lower <- pr[, 2]; out$upper <- pr[, 3]
  } else if (spec$type == "glm") {
    pr <- stats::predict(fit, nd, type = "link", se.fit = TRUE)
    z <- stats::qnorm(1 - a / 2)
    out$fit <- stats::plogis(pr$fit)
    if (band != "none") { out$lower <- stats::plogis(pr$fit - z * pr$se.fit); out$upper <- stats::plogis(pr$fit + z * pr$se.fit) }
  } else {
    th <- stats::coef(fit)
    out$fit <- spec$f(unname(th), grid)
    if (band == "none") return(out)
    sig2 <- sum(stats::residuals(fit)^2) / stats::df.residual(fit)
    if (band_method == "delta") {
      G <- num_grad(function(t) spec$f(t, grid), unname(th))
      v <- rowSums((G %*% stats::vcov(fit)) * G)
      if (band == "prediction") v <- v + sig2
      tq <- stats::qt(1 - a / 2, stats::df.residual(fit))
      out$lower <- out$fit - tq * sqrt(v); out$upper <- out$fit + tq * sqrt(v)
    } else {
      fv <- stats::fitted(fit); rr <- stats::residuals(fit) - mean(stats::residuals(fit))
      sims <- with_seed(seed, lapply(seq_len(nboot), function(b) {
        db <- dg; db$y <- fv + sample(rr, replace = TRUE)
        fb <- tryCatch(spec$fit(db), error = function(e) NULL)
        if (is.null(fb) || inherits(fb, "condition")) return(NULL)
        mu <- spec$f(unname(stats::coef(fb)), grid)
        if (band == "prediction") mu <- mu + sample(rr, length(grid), replace = TRUE)
        mu
      }))
      ok <- !vapply(sims, is.null, logical(1))
      S <- do.call(cbind, sims[ok])
      out$lower <- apply(S, 1, stats::quantile, a / 2, na.rm = TRUE)
      out$upper <- apply(S, 1, stats::quantile, 1 - a / 2, na.rm = TRUE)
      attr(out, "boot_success") <- mean(ok)
    }
  }
  out
}

reg_compare_table <- function(fs, is_bin) {
  out <- list()
  for (g in unique(fs$group)) {
    z <- fs[fs$group == g, c("model", "group", "n", "k", "r2", "r2_type", "r2_adj", "rmse", "AIC", "AICc", "BIC")]
    if (length(unique(z$n)) > 1) { z$delta_AICc <- NA; z$weight <- NA } else {
      crit <- if (all(is.finite(z$AICc))) z$AICc else z$AIC
      z$delta_AICc <- crit - min(crit)
      z$weight <- exp(-0.5 * z$delta_AICc) / sum(exp(-0.5 * z$delta_AICc))
    }
    out[[g]] <- z[order(z$delta_AICc, -z$r2_adj), ]
  }
  out <- do.call(rbind, out); rownames(out) <- NULL
  out
}

# Test equality of curves among groups (internal; result in table curve_test)
compare_curves <- function(x, model = NULL) {
  if (!inherits(x, "caagro_regression")) abort_caagro("`x` deve ser resultado de fit_regression().")
  if (is.null(x$settings$group)) abort_caagro("O ajuste n\u00e3o tem vari\u00e1vel de agrupamento.")
  model <- model %||% names(x$model)[1]
  spec <- model_registry()[[model]]
  fits <- x$model[[model]]
  ng <- length(unique(x$data_used[[x$settings$group]]))
  if (length(fits) < ng) abort_caagro("O modelo n\u00e3o foi ajustado em todos os grupos; teste indispon\u00edvel.")
  d0 <- x$data_used
  d <- data.frame(x = d0[[x$settings$x]], y = d0[[x$settings$y]])
  if (isTRUE(x$settings$binomial)) {
    d$n <- if (is.null(x$settings$trials)) 1 else d0[[x$settings$trials]]; d$s <- d$y
  }
  pooled <- tryCatch(spec$fit(d), error = function(e) e)
  if (inherits(pooled, "condition")) abort_caagro("A curva comum n\u00e3o convergiu: ", conditionMessage(pooled))
  if (spec$type == "glm") {
    dev_s <- sum(vapply(fits, stats::deviance, 1)); df_s <- sum(vapply(fits, stats::df.residual, 1))
    lr <- stats::deviance(pooled) - dev_s; dfl <- stats::df.residual(pooled) - df_s
    tb <- data.frame(model = model, test = "Raz\u00e3o de verossimilhan\u00e7a (deviance)", statistic = lr, df1 = dfl, df2 = NA,
                     p = stats::pchisq(lr, dfl, lower.tail = FALSE))
  } else {
    rss_s <- sum(vapply(fits, function(f) sum(stats::residuals(f)^2), 1)); df_s <- sum(vapply(fits, stats::df.residual, 1))
    rss_p <- sum(stats::residuals(pooled)^2); df_p <- stats::df.residual(pooled)
    Fv <- ((rss_p - rss_s) / (df_p - df_s)) / (rss_s / df_s)
    tb <- data.frame(model = model, test = "F (soma de quadrados extra)", statistic = Fv, df1 = df_p - df_s, df2 = df_s,
                     p = stats::pf(Fv, df_p - df_s, df_s, lower.tail = FALSE))
  }
  new_result("regression", "Teste de igualdade de curvas", "caagro_test", tables = list(test = tb),
             settings = list(model = model, group = x$settings$group),
             notes = list(Resultado = sprintf("%s: estat\u00edstica = %.3f, p %s.", tb$test, tb$statistic, fmt_p_eq(tb$p)),
                          Interpretacao = "p pequeno indica que uma curva \u00fanica n\u00e3o descreve adequadamente todos os grupos (ao menos um par\u00e2metro difere).",
                          Cuidados = "Pressup\u00f5e vari\u00e2ncia residual comum entre grupos e o mesmo modelo em todos. p grande n\u00e3o prova curvas iguais."))
}

#' Agronomic quantities from fitted curves
#'
#' * Quadratic: dose of maximum technical efficiency (MTE)
#'   \eqn{x^* = -b/(2c)} (when \eqn{c < 0}) and maximum response; economic
#'   optimum dose \eqn{x_e = (r - b)/(2c)}, with
#'   \eqn{r = } price of input / price of product (both per unit).
#' * Linear-plateau: critical dose \eqn{x_0} and plateau.
#' * Logistic (3/4): inflection point \eqn{x_0}. Log-logistic: ED50 =
#'   \eqn{e}. Binomial: ED50 = dose at which p = 0.5.
#' * Any model: marginal response (first derivative) at `at`.
#'
#' Standard errors use the delta method; intervals are Wald-type. Doses
#' outside the experimental range are flagged and never reported silently.
#'
#' @param x A `caagro_regression` result.
#' @param model Model id (default: first).
#' @param price_ratio Ratio input price / product price (same units as x and
#'   y), for the economic optimum.
#' @param at Doses at which to evaluate the marginal response.
#' @param conf_level Confidence level.
#' @return A `caagro_result` with table `agronomic`.
#' @examples
#' r <- fit_regression(sim_dose, "dose", "produtividade", "quadratic", group = "cultivar")
#' optimum_dose(r, price_ratio = 0.004)
#' @export
optimum_dose <- function(x, model = NULL, price_ratio = NULL, at = NULL, conf_level = NULL) {
  if (!inherits(x, "caagro_regression")) abort_caagro("`x` deve ser resultado de fit_regression().")
  model <- model %||% names(x$model)[1]
  conf_level <- conf_level %||% x$settings$conf_level
  spec <- model_registry()[[model]]
  if (is.null(spec)) abort_caagro("Modelo n\u00e3o ajustado: ", model)
  rows <- list(); warns <- character()
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  for (gname in names(x$model[[model]])) {
    fit <- x$model[[model]][[gname]]
    th <- unname(stats::coef(fit)); V <- stats::vcov(fit)
    xr <- range(if (is.null(x$settings$group)) x$data_used[[x$settings$x]] else x$data_used[[x$settings$x]][as.character(x$data_used[[x$settings$group]]) == gname])
    add <- function(q, fun) {
      est <- fun(th)
      if (!is.finite(est)) return(invisible())
      g <- num_grad(function(t) fun(t), th)
      se <- sqrt(as.numeric(g %*% V %*% t(g)))
      inside <- est >= xr[1] && est <= xr[2]
      rows[[length(rows) + 1]] <<- data.frame(model = model, group = gname, quantity = q, estimate = est, se = se,
                                              lower = est - z * se, upper = est + z * se, within_range = inside)
      if (!inside && grepl("dose|ED|x0|MET|\u00f3tima", q)) warns <<- c(warns, sprintf("[%s] %s = %.4g esta fora da faixa experimental (%.4g a %.4g): extrapola\u00e7\u00e3o n\u00e3o confi\u00e1vel.", gname, q, est, xr[1], xr[2]))
    }
    if (model == "quadratic") {
      if (th[3] < 0) {
        add("Dose de m\u00e1xima efici\u00eancia t\u00e9cnica (MET)", function(t) -t[2] / (2 * t[3]))
        add("Resposta m\u00e1xima (y na MET)", function(t) t[1] - t[2]^2 / (4 * t[3]))
        if (!is.null(price_ratio)) add("Dose \u00f3tima econ\u00f4mica", function(t) (price_ratio - t[2]) / (2 * t[3]))
      } else warns <- c(warns, sprintf("[%s] Coeficiente quadr\u00e1tico >= 0: n\u00e3o ha ponto de m\u00e1ximo.", gname))
    } else if (model == "linear_plateau") {
      add("Dose cr\u00edtica (x0, in\u00edcio do plat\u00f4)", function(t) t[3])
      add("Plat\u00f4 (resposta m\u00e1xima)", function(t) t[1] + t[2] * t[3])
    } else if (model == "logistic3") {
      add("Ponto de inflex\u00e3o x0", function(t) t[2]); add("Ass\u00edntota A", function(t) t[1])
    } else if (model == "logistic4") {
      add("Ponto de inflex\u00e3o x0", function(t) t[3])
    } else if (model == "loglogistic") {
      add("ED50 (e)", function(t) t[4])
    } else if (model == "binomial") {
      add("ED50 (dose com p = 0,5)", function(t) -t[1] / t[2])
    } else if (model == "binomial_logdose") {
      add("ED50 (dose com p = 0,5)", function(t) exp(-t[1] / t[2]))
    } else if (model %in% c("mitscherlich", "asymptotic", "gompertz", "michaelis_menten")) {
      add("Ass\u00edntota (resposta m\u00e1xima te\u00f3rica)", function(t) t[1])
    }
    for (a in at) add(paste0("Resposta marginal dy/dx em x = ", a), function(t) (spec$f(t, a + 1e-4) - spec$f(t, a - 1e-4)) / 2e-4)
  }
  tb <- if (length(rows)) do.call(rbind, rows) else data.frame()
  new_result("regression", "Grandezas agron\u00f4micas", "caagro_test", tables = list(agronomic = tb),
             settings = list(model = model, price_ratio = price_ratio, at = at, conf_level = conf_level), warnings = warns,
             notes = list(Cuidados = "Intervalos pelo m\u00e9todo delta (aproxima\u00e7\u00e3o assint\u00f3tica); para pontos como MET, intervalos podem ser assim\u00e9tricos na pr\u00e1tica. A dose econ\u00f4mica depende da validade dos precos informados."))
}
