#' Analysis of variance for designed experiments (DIC, DBC, factorial)
#'
#' Fits the fixed-effects linear model that corresponds to the experimental
#' design and returns the ANOVA table, experimental precision indicators,
#' treatment means (raw and estimated marginal means), and effect sizes.
#'
#' @details
#' Models:
#' * **DIC** (completely randomised design): \eqn{y_{ij} = \mu + \tau_i + \varepsilon_{ij}}.
#' * **DBC** (randomised complete block design): \eqn{y_{ij} = \mu + \tau_i + \beta_j + \varepsilon_{ij}}.
#' * **Factorial**: main effects and all interactions of `treatment` and
#'   `factors`, optionally with blocks (additive block effect).
#'
#' If a `block` column is supplied the analysis is **never** carried out as
#' DIC: `design = "DIC"` with a block column is an error.
#'
#' The default is the conventional Fisher analysis of variance with
#' sequential (type I) sums of squares, as in [stats::aov()]: blocks are
#' entered first, so in a DBC the treatment sum of squares is always
#' adjusted for blocks. Types `"II"` (each term adjusted for all terms not
#' containing it) and `"III"` (sum-to-zero contrasts) are available for
#' unbalanced factorial experiments. For balanced data the three coincide;
#' the type used is always reported.
#'
#' `method = "welch"` performs Welch's one-way ANOVA (Welch, 1951), which
#' does not assume equal variances. It is an alternative for DIC only.
#'
#' CV (%) = 100 x sqrt(MSE) / grand mean. Standard errors of the treatment
#' means are model based, sqrt(MSE / n_i) for balanced DIC/DBC, and come
#' from the estimated marginal means otherwise.
#'
#' Effect sizes: eta-squared, partial eta-squared and partial
#' omega-squared (Olejnik and Algina, 2003).
#'
#' @param data A data frame in long format (one row per experimental unit).
#' @param response Name of the numeric response column.
#' @param treatment Name of the treatment column (factor).
#' @param block Name of the block column (DBC), optional.
#' @param factors Further factor columns for factorial experiments.
#' @param design `"auto"`, `"DIC"`, `"DBC"` or `"factorial"`.
#' @param ss_type Type of sums of squares: `"I"` (default, Fisher/aov),
#'   `"II"` or `"III"`.
#' @param method `"fisher"` (default, conventional F test) or `"welch"`
#'   (one-way ANOVA without the equal-variance assumption; DIC only).
#' @param conf_level Confidence level for intervals of the means.
#' @param exclude Integer vector of row numbers of `data` to exclude
#'   explicitly (e.g. confirmed outliers). Exclusions are recorded.
#' @return An object of classes `caagro_anova` and `caagro_result`, with
#'   tables `anova`, `precision`, `means` (plus `means_<factor>` for
#'   factorial designs) and `effect_size`; `model` holds the `lm` fit.
#' @references
#' Fisher, R. A. (1935). *The Design of Experiments*. Oliver and Boyd.
#'
#' Montgomery, D. C. (2017). *Design and Analysis of Experiments* (9th ed.).
#' Wiley.
#'
#' Welch, B. L. (1951). On the comparison of several mean values: an
#' alternative approach. *Biometrika*, 38(3/4), 330-336.
#' \doi{10.2307/2332579}
#'
#' Olejnik, S. and Algina, J. (2003). Generalized eta and omega squared
#' statistics. *Psychological Methods*, 8(4), 434-447.
#' \doi{10.1037/1082-989X.8.4.434}
#' @seealso [compare_means()], [check_assumptions()], [plot.caagro_anova()]
#' @examples
#' # DIC (real data: Dobson, 1983)
#' fit <- anova_exp(PlantGrowth, "weight", "group")
#' fit
#'
#' # DBC
#' dbc <- anova_exp(sim_dbc, "produtividade", "tratamento", block = "bloco")
#' result_table(dbc, "precision")
#'
#' # Factorial in blocks (real data: npk)
#' f <- anova_exp(npk, "yield", "N", block = "block", factors = c("P", "K"))
#' result_table(f, "anova")
#' @export
anova_exp <- function(data, response, treatment, block = NULL, factors = NULL,
                      design = c("auto", "DIC", "DBC", "factorial"),
                      ss_type = c("I", "II", "III"), conf_level = 0.95,
                      exclude = NULL, method = c("fisher", "welch")) {
  design <- match.arg(design)
  ss_type <- match.arg(ss_type)
  method <- match.arg(method)
  if (method == "welch") {
    if (!is.null(block) || !is.null(factors)) abort_caagro("A ANOVA de Welch s\u00f3 se aplica ao DIC com um fator (sem blocos ou fatorial).")
    return(welch_anova(data, response, treatment, exclude = exclude))
  }
  check_conf(conf_level)
  if (design == "DIC" && !is.null(block)) {
    abort_caagro("Foi informada uma coluna de blocos, mas o delineamento escolhido e DIC. ",
                 "Dados de DBC n\u00e3o devem ser analisados como DIC. Use design = 'DBC'.")
  }
  if (design == "DBC" && is.null(block)) abort_caagro("DBC exige a coluna de blocos (`block`).")
  if (design == "factorial" && is.null(factors)) abort_caagro("Fatorial exige ao menos um fator adicional em `factors`.")
  if (design == "auto") {
    design <- if (!is.null(factors)) "factorial" else if (!is.null(block)) "DBC" else "DIC"
  }
  facs <- c(treatment, factors)
  prep <- prepare_data(data, response = response, factors = c(block, facs), exclude = exclude)
  d <- prep$data
  warns <- prep$messages
  chk <- check_data(d, response, treatment, block,
                    design = if (design == "factorial") "none" else design)
  if (!attr(chk, "ok")) abort_caagro(paste(chk$message[chk$level == "error"], collapse = " "))
  warns <- c(warns, chk$message[chk$level == "warning"])

  bq <- function(v) paste0("`", v, "`")
  rhs <- paste(bq(facs), collapse = " * ")
  if (!is.null(block)) rhs <- paste(bq(block), "+", rhs)
  fml <- stats::as.formula(paste(bq(response), "~", rhs))
  fit <- stats::lm(fml, data = d)
  if (any(is.na(stats::coef(fit)))) {
    warns <- c(warns, "H\u00e1 par\u00e2metros n\u00e3o estim\u00e1veis (matriz singular), geralmente por combina\u00e7\u00f5es de n\u00edveis ausentes. Interprete apenas os efeitos estim\u00e1veis.")
  }
  if (stats::df.residual(fit) < 1) abort_caagro("Graus de liberdade do res\u00edduo insuficientes (sem repeti\u00e7\u00f5es suficientes para estimar o erro).")

  cells <- table(d[, facs, drop = FALSE])
  balanced <- length(unique(as.vector(cells))) == 1 &&
    (is.null(block) || length(unique(as.vector(table(d[[block]], interaction(d[, facs, drop = FALSE]))))) == 1)

  tab <- anova_table(fit, ss_type, d)
  aliased <- tab$term[is.na(tab$df) & tab$term != "Residuals"]
  if (length(aliased)) {
    tab <- tab[!tab$term %in% aliased, , drop = FALSE]
    warns <- c(warns, sprintf("Termo(s) sem graus de liberdade (confundido(s) com outros efeitos, p.ex. blocos): %s. Foram omitidos da tabela.",
                              paste(aliased, collapse = ", ")))
  }
  rownames(tab) <- NULL
  mse <- tab$ms[tab$term == "Residuals"]
  dfe <- tab$df[tab$term == "Residuals"]
  grand <- mean(d[[response]])
  sst <- sum((d[[response]] - grand)^2)
  precision <- data.frame(
    n = nrow(d), grand_mean = grand, mse = mse, root_mse = sqrt(mse),
    df_error = dfe, cv_percent = 100 * sqrt(mse) / grand,
    r_squared = summary(fit)$r.squared, ss_type = ss_type,
    balanced = balanced
  )
  if (grand <= 0) warns <- c(warns, "M\u00e9dia geral <= 0: o CV% n\u00e3o \u00e9 interpret\u00e1vel.")

  # effect sizes
  eff <- tab[tab$term != "Residuals", c("term", "df", "ss", "ms", "F")]
  eff$eta_sq <- eff$ss / sst
  eff$partial_eta_sq <- eff$ss / (eff$ss + tab$ss[tab$term == "Residuals"])
  N <- nrow(d)
  eff$partial_omega_sq <- pmax(0, (eff$df * (eff$ms - mse)) / (eff$df * eff$ms + (N - eff$df) * mse))
  eff <- eff[, c("term", "eta_sq", "partial_eta_sq", "partial_omega_sq")]
  tab$partial_eta_sq <- c(eff$partial_eta_sq, NA)

  tables <- list(anova = tab, precision = precision)
  for (f in facs) {
    tables[[if (f == treatment) "means" else paste0("means_", f)]] <- means_table(fit, d, response, f, conf_level)
  }
  if (length(facs) > 1) {
    tables[["means_interaction"]] <- means_table(fit, d, response, facs, conf_level)
  }
  tables$effect_size <- eff

  if (!balanced) warns <- c(warns, sprintf("Dados desbalanceados: foram usadas somas de quadrados tipo %s e m\u00e9dias marginais estimadas (ajustadas).", ss_type))
  ptr <- tab$p[tab$term == treatment]
  notes <- anova_notes(tab, precision, treatment, design)
  code <- code_call("caagro::anova_exp", list(
    data = as.name("dados"), response = response, treatment = treatment,
    block = block, factors = factors, design = design, ss_type = ss_type,
    conf_level = conf_level, exclude = exclude))
  new_result(
    type = "anova", method = paste0("ANOVA - ", switch(design, DIC = "Delineamento inteiramente casualizado (DIC)",
                                                      DBC = "Delineamento em blocos casualizados (DBC)",
                                                      factorial = "Experimento fatorial")),
    subclass = "caagro_anova", formula = fml, design = design,
    settings = list(response = response, treatment = treatment, block = block,
                    factors = factors, ss_type = ss_type, conf_level = conf_level,
                    exclude = exclude),
    data = data, data_used = d, tables = tables, main = "anova", model = fit,
    warnings = warns, notes = notes, code = code
  )
}

anova_table <- function(fit, ss_type, d) {
  a1 <- stats::anova(fit)
  labs <- attr(stats::terms(fit), "term.labels")
  res_ss <- sum(stats::residuals(fit)^2)
  dfe <- stats::df.residual(fit)
  if (ss_type == "I") {
    idx <- match(labs, trimws(rownames(a1)))
    df <- a1$Df[idx]; ss <- a1$`Sum Sq`[idx]
    df[is.na(df)] <- 0; ss[is.na(ss)] <- 0
  } else if (ss_type == "II") {
    fac_mat <- attr(stats::terms(fit), "factors")
    df <- ss <- numeric(length(labs))
    for (i in seq_along(labs)) {
      contains <- vapply(labs, function(t2) all(fac_mat[, labs[i]] <= fac_mat[, t2]) && t2 != labs[i], logical(1))
      keep <- labs[!contains & labs != labs[i]]
      rhs0 <- if (length(keep)) paste(keep, collapse = " + ") else "1"
      f0 <- stats::update(stats::formula(fit), stats::as.formula(paste(". ~", rhs0)))
      f1 <- stats::update(stats::formula(fit), stats::as.formula(paste(". ~", rhs0, "+", labs[i])))
      m0 <- stats::lm(f0, data = fit$model)
      m1 <- stats::lm(f1, data = fit$model)
      ss[i] <- sum(stats::residuals(m0)^2) - sum(stats::residuals(m1)^2)
      df[i] <- m1$rank - m0$rank
    }
  } else {
    old <- options(contrasts = c("contr.sum", "contr.poly"))
    on.exit(options(old), add = TRUE)
    f3 <- stats::lm(stats::formula(fit), data = fit$model)
    dr <- stats::drop1(f3, scope = labs, test = "F")
    df <- dr$Df[-1]; ss <- dr$`Sum of Sq`[-1]
  }
  out <- data.frame(term = c(labs, "Residuals"), df = c(df, dfe), ss = c(ss, res_ss))
  out$ms <- out$ss / out$df
  mse <- res_ss / dfe
  out$F <- c(out$ms[-nrow(out)] / mse, NA)
  out$p <- c(stats::pf(out$F[-nrow(out)], out$df[-nrow(out)], dfe, lower.tail = FALSE), NA)
  out$signif <- c(signif_stars(out$p[-nrow(out)]), "")
  out$df[out$df == 0] <- NA
  out
}

means_table <- function(fit, d, response, specs, conf_level) {
  em <- emm_compute(fit, specs, conf_level)
  key <- interaction(d[, specs, drop = FALSE], sep = " : ", lex.order = TRUE, drop = TRUE)
  y <- d[[response]]
  raw <- data.frame(
    .key = levels(key),
    n = as.vector(tapply(y, key, length)),
    mean = as.vector(tapply(y, key, mean)),
    sd = as.vector(tapply(y, key, stats::sd)),
    median = as.vector(tapply(y, key, stats::median))
  )
  emk <- attr(em, "label")
  out <- em
  idx <- match(emk, raw$.key)
  out$n <- raw$n[idx]; out$mean <- raw$mean[idx]; out$sd <- raw$sd[idx]
  out$se_raw <- out$sd / sqrt(out$n)
  out <- out[, c(specs, "n", "mean", "sd", "se_raw", "emmean", "se", "df", "lower", "upper")]
  attr(out, "L") <- attr(em, "L"); attr(out, "V") <- attr(em, "V")
  out
}

anova_notes <- function(tab, precision, treatment, design) {
  p <- tab$p[tab$term == treatment]
  Fv <- tab$F[tab$term == treatment]
  d1 <- tab$df[tab$term == treatment]; d2 <- tab$df[tab$term == "Residuals"]
  list(
    Resultado = sprintf("Efeito de '%s': F(%s, %s) = %.3f, p %s. CV experimental = %.2f%%; QM do res\u00edduo = %.4g.",
                        treatment, d1, d2, Fv, fmt_p_eq(p), precision$cv_percent, precision$mse),
    Interpretacao = if (isTRUE(p < 0.05))
      "H\u00e1 evid\u00eancia de diferen\u00e7a entre ao menos duas m\u00e9dias de tratamentos ao n\u00edvel de 5%. Use um teste de compara\u00e7\u00f5es m\u00faltiplas adequado para identificar quais diferem."
    else
      "N\u00e3o houve evid\u00eancia suficiente de diferen\u00e7a entre as m\u00e9dias ao n\u00edvel de 5%. Isso N\u00c3O demonstra que os tratamentos s\u00e3o iguais: a aus\u00eancia de signific\u00e2ncia pode refletir baixo poder (poucas repeti\u00e7\u00f5es ou alta variabilidade).",
    Cuidados = paste(
      "A validade depende de independ\u00eancia (garantida pela casualiza\u00e7\u00e3o e pelo planejamento, n\u00e3o por um teste),",
      "normalidade dos res\u00edduos e homogeneidade de vari\u00e2ncias. Verifique os diagn\u00f3sticos.",
      if (design == "DBC") "No DBC assume-se aditividade entre blocos e tratamentos (sem intera\u00e7\u00e3o)." else ""
    )
  )
}

# Welch's one-way ANOVA (called by anova_exp(method = "welch"))
# Welch, B. L. (1951). Biometrika, 38(3/4), 330-336. doi:10.2307/2332579
welch_anova <- function(data, response, treatment, exclude = NULL) {
  prep <- prepare_data(data, response, treatment, exclude = exclude)
  d <- prep$data
  fml <- stats::as.formula(paste0("`", response, "` ~ `", treatment, "`"))
  ot <- stats::oneway.test(fml, data = d, var.equal = FALSE)
  tb <- data.frame(statistic = unname(ot$statistic), df1 = unname(ot$parameter[1]),
                   df2 = unname(ot$parameter[2]), p = ot$p.value)
  new_result("anova", "ANOVA de Welch (vari\u00e2ncias heterog\u00eaneas)", "caagro_test",
             formula = fml, design = "DIC", settings = list(response = response, treatment = treatment),
             data = data, data_used = d, tables = list(test = tb), warnings = prep$messages,
             notes = list(Resultado = sprintf("F de Welch(%.0f; %.2f) = %.3f, p %s.", tb$df1, tb$df2, tb$statistic, fmt_p_eq(tb$p)),
                          Cuidados = "N\u00e3o pressup\u00f5e vari\u00e2ncias iguais. Para compara\u00e7\u00f5es par a par, use Games-Howell."),
             code = code_call("caagro::anova_exp", list(data = as.name("dados"), response = response, treatment = treatment, method = "welch")))
}

# Planned contrasts (called by compare_means(x, contrasts = ...))
contrast_test <- function(x, contrasts, factor = NULL, p_adjust = "none", conf_level = 0.95) {
  if (!inherits(x, "caagro_anova")) abort_caagro("`x` deve ser resultado de anova_exp().")
  factor <- factor %||% x$settings$treatment
  valid_p_adjust(p_adjust)
  em <- emm_compute(x$model, factor, conf_level)
  k <- nrow(em)
  C <- do.call(rbind, lapply(contrasts, function(cc) {
    if (length(cc) != k) abort_caagro("Cada contraste deve ter ", k, " coeficientes (n\u00edveis: ", paste(em[[factor]], collapse = ", "), ").")
    cc
  }))
  if (any(abs(rowSums(C)) > 1e-8)) warn_caagro("Algum contraste n\u00e3o soma zero; ele n\u00e3o \u00e9 um contraste propriamente dito.")
  est <- as.vector(C %*% em$emmean)
  V <- attr(em, "V")
  se <- sqrt(diag(C %*% V %*% t(C)))
  dfe <- stats::df.residual(x$model)
  tval <- est / se
  p <- 2 * stats::pt(-abs(tval), dfe)
  tq <- stats::qt(1 - (1 - conf_level) / 2, dfe)
  tb <- data.frame(contrast = names(contrasts), estimate = est, se = se, df = dfe,
                   t = tval, p = p, p_adj = stats::p.adjust(p, p_adjust),
                   lower = est - tq * se, upper = est + tq * se)
  new_result("anova", "Contrastes planejados", "caagro_test", design = x$design,
             settings = list(factor = factor, p_adjust = p_adjust, contrasts = contrasts),
             data_used = x$data_used, tables = list(contrasts = tb), model = x$model,
             notes = list(Cuidados = "Contrastes devem ser definidos antes da an\u00e1lise dos dados. Intervalos n\u00e3o ajustados para multiplicidade quando p_adjust = 'none'."))
}
