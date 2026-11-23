#' Multiple comparison of treatment means
#'
#' Post-hoc comparison of the (estimated marginal) means of a fitted
#' `anova_exp()` model. All procedures use the model error variance (MSE)
#' and residual degrees of freedom, except Games-Howell, which uses
#' separate group variances.
#'
#' @details
#' * `"tukey"`: Tukey HSD; Tukey-Kramer standard errors for unbalanced data.
#'   Single-step, controls the family-wise error rate (FWER).
#' * `"snk"`: Student-Newman-Keuls, step-down with the studentised range for
#'   `p` means spanned. The closure rule is enforced: a pair inside a
#'   non-significant range is never declared different. SNK does **not**
#'   control the FWER when there are more than three means.
#' * `"lsd"`: Fisher's LSD. With `protected = TRUE` (default) pairwise tests
#'   are only interpreted when the global F test is significant; otherwise
#'   no difference is declared. Optional `p_adjust`.
#' * `"scott_knott"`: Scott and Knott (1974) clustering of means by
#'   maximising the between-group sum of squares, with the
#'   \eqn{\lambda} statistic compared to \eqn{\chi^2_{k/(\pi-2)}}. Produces
#'   non-overlapping groups. Uses the harmonic mean of replications when
#'   unbalanced (approximation, reported in warnings).
#' * `"dunnett"`: each treatment versus a `control`, multivariate-t
#'   adjusted (computed with \pkg{mvtnorm}).
#' * `"bonferroni"`, `"holm"` (or any [stats::p.adjust()] method through
#'   `method = "pairwise"` and `p_adjust`): pairwise t tests with pooled MSE.
#' * `"games_howell"`: for heterogeneous variances; only for one-way (DIC)
#'   designs.
#'
#' Compact letter displays use the insert-absorb algorithm (Piepho, 2004).
#' Means sharing a letter are not significantly different. Letters are only
#' produced when the procedure supports that interpretation.
#'
#' @param x A `caagro_anova` result.
#' @param method Procedure (see Details).
#' @param factor Factor to compare (default: the treatment).
#' @param by Optional second factor: comparisons are performed within each
#'   of its levels (simple effects in factorial designs).
#' @param alpha Significance level.
#' @param control Control level for Dunnett.
#' @param protected For LSD: require a significant global F test.
#' @param p_adjust Multiplicity correction for LSD/pairwise methods.
#' @param conf_level Confidence level for intervals of differences
#'   (default `1 - alpha`).
#' @return An object of classes `caagro_multcomp` and `caagro_result` with
#'   tables `means` (with `group` letters) and `pairs`.
#' @references
#' Tukey, J. W. (1949). Comparing individual means in the analysis of
#' variance. *Biometrics*, 5(2), 99-114. \doi{10.2307/3001913}
#'
#' Kramer, C. Y. (1956). Extension of multiple range tests to group means
#' with unequal numbers of replications. *Biometrics*, 12(3), 307-310.
#' \doi{10.2307/3001469}
#'
#' Newman, D. (1939). The distribution of range in samples from a normal
#' population, expressed in terms of an independent estimate of standard
#' deviation. *Biometrika*, 31(1/2), 20-30. \doi{10.2307/2334973}
#'
#' Keuls, M. (1952). The use of the "studentized range" in connection with
#' an analysis of variance. *Euphytica*, 1, 112-122. \doi{10.1007/BF01908269}
#'
#' Scott, A. J. and Knott, M. (1974). A cluster analysis method for grouping
#' means in the analysis of variance. *Biometrics*, 30(3), 507-512.
#' \doi{10.2307/2529204}
#'
#' Dunnett, C. W. (1955). A multiple comparison procedure for comparing
#' several treatments with a control. *Journal of the American Statistical
#' Association*, 50(272), 1096-1121. \doi{10.1080/01621459.1955.10501294}
#'
#' Games, P. A. and Howell, J. F. (1976). Pairwise multiple comparison
#' procedures with unequal n's and/or variances. *Journal of Educational
#' Statistics*, 1(2), 113-125. \doi{10.3102/10769986001002113}
#'
#' Holm, S. (1979). A simple sequentially rejective multiple test procedure.
#' *Scandinavian Journal of Statistics*, 6(2), 65-70.
#'
#' Piepho, H.-P. (2004). An algorithm for a letter-based representation of
#' all-pairwise comparisons. *Journal of Computational and Graphical
#' Statistics*, 13(2), 456-466. \doi{10.1198/1061860043515}
#' @examples
#' fit <- anova_exp(PlantGrowth, "weight", "group")
#' compare_means(fit, "tukey")
#' compare_means(fit, "scott_knott")
#' compare_means(fit, "dunnett", control = "ctrl")
#' @export
compare_means <- function(x, method = c("tukey", "snk", "lsd", "scott_knott", "dunnett",
                                        "bonferroni", "holm", "pairwise", "games_howell"),
                          factor = NULL, by = NULL, alpha = 0.05, control = NULL,
                          protected = TRUE, p_adjust = "none", conf_level = 1 - alpha) {
  if (!inherits(x, "caagro_anova")) abort_caagro("`x` deve ser resultado de anova_exp().")
  method <- match.arg(method)
  check_alpha(alpha)
  valid_p_adjust(p_adjust)
  factor <- factor %||% x$settings$treatment
  fit <- x$model
  if (!is.null(by) && !by %in% names(fit$xlevels)) abort_caagro("Fator `by` nao esta no modelo.")
  tab <- x$tables$anova
  warns <- character()
  # Interaction check
  int_terms <- grep(":", tab$term, value = TRUE)
  if (is.null(by) && length(int_terms)) {
    pint <- tab$p[tab$term %in% int_terms & grepl(factor, tab$term, fixed = TRUE)]
    if (any(pint < alpha, na.rm = TRUE)) {
      warns <- c(warns, sprintf("Interacao significativa envolvendo '%s'. Comparar medias marginais pode ser enganoso; use `by` para comparar dentro dos niveis do outro fator.", factor))
    }
  }
  if (method == "games_howell" && (!is.null(x$settings$block) || length(x$settings$factors))) {
    abort_caagro("Games-Howell esta implementado apenas para DIC com um fator.")
  }
  specs <- c(factor, by)
  em <- emm_compute(fit, specs, conf_level)
  V <- attr(em, "V")
  if (anyNA(em$emmean)) {
    warns <- c(warns, "Algumas medias nao sao estimaveis e foram excluidas das comparacoes.")
  }
  groups <- if (is.null(by)) list(all = seq_len(nrow(em))) else split(seq_len(nrow(em)), em[[by]])
  dfe <- stats::df.residual(fit)
  mse <- sum(stats::residuals(fit)^2) / dfe
  p_global <- tab$p[tab$term == factor]

  means_out <- list(); pairs_out <- list()
  for (gname in names(groups)) {
    idx <- groups[[gname]]
    idx <- idx[!is.na(em$emmean[idx])]
    m <- em[idx, , drop = FALSE]
    Vg <- V[idx, idx, drop = FALSE]
    lv <- as.character(m[[factor]])
    est <- stats::setNames(m$emmean, lv)
    res <- switch(
      method,
      tukey = mc_tukey(est, Vg, dfe, alpha, conf_level),
      snk = mc_snk(est, Vg, dfe, alpha, conf_level),
      lsd = mc_lsd(est, Vg, dfe, alpha, conf_level, p_adjust,
                   protected_ok = !protected || isTRUE(p_global < alpha)),
      bonferroni = mc_lsd(est, Vg, dfe, alpha, conf_level, "bonferroni", TRUE),
      holm = mc_lsd(est, Vg, dfe, alpha, conf_level, "holm", TRUE),
      pairwise = mc_lsd(est, Vg, dfe, alpha, conf_level, p_adjust, TRUE),
      scott_knott = mc_scott_knott(est, mse, dfe, x$data_used, factor, by, gname, alpha),
      dunnett = mc_dunnett(est, Vg, dfe, alpha, conf_level, control %||% lv[1]),
      games_howell = mc_games_howell(x$data_used, x$settings$response, factor, alpha, conf_level)
    )
    mm <- m
    mm$letras <- res$letters[lv] %||% NA_character_
    if (!is.null(by)) res$pairs[[by]] <- gname
    means_out[[gname]] <- mm
    pairs_out[[gname]] <- res$pairs
    warns <- c(warns, res$warnings)
  }
  means <- do.call(rbind, means_out); rownames(means) <- NULL
  pairs <- do.call(rbind, pairs_out); rownames(pairs) <- NULL
  okey <- if (is.null(by)) rep(0L, nrow(means)) else as.integer(means[[by]])
  means <- means[order(okey, -means$emmean), , drop = FALSE]
  rownames(means) <- NULL
  if (method == "lsd") {
    if (protected && !isTRUE(p_global < alpha)) {
      warns <- c(warns, "LSD protegido: o teste F global nao foi significativo; nenhuma diferenca e declarada.")
    } else if (!protected) {
      warns <- c(warns, "LSD nao protegido: a taxa de erro por familia de comparacoes nao e controlada.")
    }
  }
  if (method == "snk" && nrow(em) > 3) warns <- c(warns, "SNK nao controla a taxa de erro por familia (FWER) com mais de 3 medias.")
  if (method %in% c("tukey", "snk", "lsd", "bonferroni", "holm", "pairwise", "dunnett", "scott_knott") && !isTRUE(p_global < alpha) && method != "lsd") {
    warns <- c(warns, "O teste F global nao foi significativo; diferencas pontuais devem ser interpretadas com cautela.")
  }
  label <- c(tukey = "Tukey HSD (Tukey-Kramer)", snk = "Student-Newman-Keuls",
             lsd = paste0("LSD de Fisher", if (protected) " (protegido)" else " (nao protegido)",
                          if (p_adjust != "none") paste0(", ajuste ", p_adjust) else ""),
             scott_knott = "Scott-Knott", dunnett = "Dunnett (versus controle)",
             bonferroni = "t pareado com correcao de Bonferroni", holm = "t pareado com correcao de Holm",
             pairwise = paste0("t pareado, ajuste ", p_adjust), games_howell = "Games-Howell")[[method]]
  new_result(
    type = "multcomp", method = paste0("Comparacao de medias - ", label), subclass = "caagro_multcomp",
    formula = x$formula, design = x$design,
    settings = list(method = method, factor = factor, by = by, alpha = alpha, control = control,
                    protected = protected, p_adjust = p_adjust, conf_level = conf_level,
                    response = x$settings$response),
    data = x$data, data_used = x$data_used, tables = list(means = means, pairs = pairs),
    model = fit, warnings = c(x$warnings, warns),
    notes = list(
      Resultado = sprintf("%s ao nivel alfa = %s.", label, alpha),
      Interpretacao = "Medias seguidas pela mesma letra nao diferem significativamente pelo procedimento utilizado. Diferencas nao significativas nao provam igualdade.",
      Cuidados = "As comparacoes dependem dos pressupostos do modelo (normalidade e homogeneidade dos residuos)."
    ),
    code = paste0(x$code, " |>\n  caagro::compare_means(", deparse(method),
                  if (!is.null(by)) paste0(", by = ", deparse(by)), ", alpha = ", alpha,
                  if (method == "dunnett") paste0(", control = ", deparse(control %||% em[[factor]][1])),
                  if (method == "lsd") paste0(", protected = ", protected, ", p_adjust = ", deparse(p_adjust)), ")")
  )
}

pair_index <- function(k) {
  cmb <- utils::combn(k, 2)
  list(i = cmb[1, ], j = cmb[2, ])
}

pairs_frame <- function(est, V, i, j, dfe) {
  lv <- names(est)
  diff <- est[i] - est[j]
  se <- sqrt(V[cbind(i, i)] + V[cbind(j, j)] - 2 * V[cbind(i, j)])
  data.frame(contrast = paste(lv[i], "-", lv[j]), level1 = lv[i], level2 = lv[j],
             diff = unname(diff), se = unname(se), df = dfe, stringsAsFactors = FALSE)
}

mc_tukey <- function(est, V, dfe, alpha, conf_level) {
  k <- length(est); ij <- pair_index(k)
  pf <- pairs_frame(est, V, ij$i, ij$j, dfe)
  pf$statistic <- sqrt(2) * abs(pf$diff) / pf$se
  pf$p_adj <- stats::ptukey(pf$statistic, k, dfe, lower.tail = FALSE)
  crit <- stats::qtukey(conf_level, k, dfe) / sqrt(2)
  pf$lower <- pf$diff - crit * pf$se; pf$upper <- pf$diff + crit * pf$se
  pf$significant <- pf$p_adj < alpha
  list(pairs = pf, letters = cld_letters(est, pf), warnings = character())
}

mc_snk <- function(est, V, dfe, alpha, conf_level) {
  k <- length(est)
  ord <- order(est, decreasing = TRUE)
  ij <- pair_index(k)
  pf <- pairs_frame(est, V, ij$i, ij$j, dfe)
  rank_pos <- match(seq_len(k), ord)
  lo <- pmin(rank_pos[ij$i], rank_pos[ij$j]); hi <- pmax(rank_pos[ij$i], rank_pos[ij$j])
  pf$range_p <- hi - lo + 1
  pf$statistic <- sqrt(2) * abs(pf$diff) / pf$se
  pf$p_unadj <- stats::ptukey(pf$statistic, pf$range_p, dfe, lower.tail = FALSE)
  raw_sig <- pf$p_unadj < alpha
  # closure: significant only if every enclosing range is significant
  sig <- raw_sig
  for (r in order(-pf$range_p)) {
    enc <- which(lo <= lo[r] & hi >= hi[r] & pf$range_p > pf$range_p[r])
    if (length(enc) && any(!sig[enc])) sig[r] <- FALSE
  }
  pf$significant <- sig
  pf$p_adj <- pf$p_unadj
  pf$lower <- NA_real_; pf$upper <- NA_real_
  list(pairs = pf, letters = cld_letters(est, pf), warnings = character())
}

mc_lsd <- function(est, V, dfe, alpha, conf_level, p_adjust, protected_ok) {
  k <- length(est); ij <- pair_index(k)
  pf <- pairs_frame(est, V, ij$i, ij$j, dfe)
  pf$statistic <- pf$diff / pf$se
  pf$p_unadj <- 2 * stats::pt(-abs(pf$statistic), dfe)
  pf$p_adj <- stats::p.adjust(pf$p_unadj, p_adjust)
  tq <- stats::qt(1 - (1 - conf_level) / 2, dfe)
  pf$lower <- pf$diff - tq * pf$se; pf$upper <- pf$diff + tq * pf$se
  pf$significant <- protected_ok & pf$p_adj < alpha
  list(pairs = pf, letters = cld_letters(est, pf), warnings = character())
}

mc_dunnett <- function(est, V, dfe, alpha, conf_level, control) {
  lv <- names(est)
  if (!control %in% lv) abort_caagro("Nivel controle '", control, "' nao encontrado. Niveis: ", paste(lv, collapse = ", "))
  c0 <- match(control, lv)
  oth <- setdiff(seq_along(lv), c0)
  pf <- pairs_frame(est, V, oth, rep(c0, length(oth)), dfe)
  pf$statistic <- pf$diff / pf$se
  # correlation among contrasts
  C <- matrix(0, length(oth), length(lv))
  C[cbind(seq_along(oth), oth)] <- 1; C[, c0] <- -1
  S <- C %*% V %*% t(C)
  R <- stats::cov2cor(S)
  alg <- mvtnorm::GenzBretz(maxpts = 50000, abseps = 1e-5, releps = 0)
  pf$p_adj <- with_seed(20240101, vapply(abs(pf$statistic), function(tt) {
    if (length(oth) == 1) return(2 * stats::pt(-tt, dfe))
    1 - as.numeric(mvtnorm::pmvt(lower = rep(-tt, length(oth)), upper = rep(tt, length(oth)),
                                 df = dfe, corr = R, algorithm = alg))
  }, numeric(1)))
  crit <- if (length(oth) == 1) stats::qt(1 - (1 - conf_level) / 2, dfe) else
    with_seed(20240101, mvtnorm::qmvt(conf_level, tail = "both.tails", df = dfe, corr = R, algorithm = alg)$quantile)
  pf$lower <- pf$diff - crit * pf$se; pf$upper <- pf$diff + crit * pf$se
  pf$p_adj <- pmin(pmax(pf$p_adj, 0), 1)
  pf$significant <- pf$p_adj < alpha
  lets <- stats::setNames(ifelse(seq_along(lv) == c0, "controle",
                                 ifelse(lv %in% pf$level1[pf$significant], "*", "ns")), lv)
  list(pairs = pf, letters = lets,
       warnings = "Dunnett compara cada tratamento apenas com o controle; '*' indica diferenca significativa em relacao ao controle (nao sao letras de agrupamento).")
}

mc_games_howell <- function(d, response, factor, alpha, conf_level) {
  y <- d[[response]]; g <- droplevels(d[[factor]])
  n <- tapply(y, g, length); m <- tapply(y, g, mean); v <- tapply(y, g, stats::var)
  if (any(n < 2)) abort_caagro("Games-Howell exige ao menos 2 observacoes por grupo.")
  k <- length(m); ij <- pair_index(k)
  lv <- names(m)
  se2 <- v[ij$i] / n[ij$i] + v[ij$j] / n[ij$j]
  dfw <- se2^2 / ((v[ij$i] / n[ij$i])^2 / (n[ij$i] - 1) + (v[ij$j] / n[ij$j])^2 / (n[ij$j] - 1))
  diff <- m[ij$i] - m[ij$j]
  se <- sqrt(se2)
  qstat <- sqrt(2) * abs(diff) / se
  p <- stats::ptukey(qstat, k, dfw, lower.tail = FALSE)
  crit <- stats::qtukey(conf_level, k, dfw) / sqrt(2)
  pf <- data.frame(contrast = paste(lv[ij$i], "-", lv[ij$j]), level1 = lv[ij$i], level2 = lv[ij$j],
                   diff = unname(diff), se = unname(se), df = unname(dfw), statistic = unname(qstat),
                   p_adj = unname(p), lower = unname(diff - crit * se), upper = unname(diff + crit * se),
                   stringsAsFactors = FALSE)
  pf$significant <- pf$p_adj < alpha
  list(pairs = pf, letters = cld_letters(stats::setNames(as.vector(m), lv), pf), warnings = character())
}

mc_scott_knott <- function(est, mse, dfe, d, factor, by, gname, alpha) {
  lv <- names(est)
  if (is.null(by)) {
    reps <- as.vector(table(droplevels(d[[factor]]))[lv])
  } else {
    dd <- d[as.character(d[[by]]) == gname, , drop = FALSE]
    reps <- as.vector(table(droplevels(dd[[factor]]))[lv])
  }
  r <- harmonic_mean(reps)
  warns <- character()
  if (length(unique(reps)) > 1) warns <- "Scott-Knott com repeticoes desiguais: usada a media harmonica do numero de repeticoes (aproximacao)."
  s2ybar <- mse / r
  ord <- order(est, decreasing = TRUE)
  y <- est[ord]
  grp <- sk_split(y, s2ybar, dfe, alpha)
  # assign letters in descending order of group mean
  ug <- unique(grp)
  lets <- stats::setNames(letters_seq(length(ug))[match(grp, ug)], names(y))
  # pairs: different if in different groups
  k <- length(est); ij <- pair_index(k)
  pf <- data.frame(contrast = paste(lv[ij$i], "-", lv[ij$j]), level1 = lv[ij$i], level2 = lv[ij$j],
                   diff = unname(est[ij$i] - est[ij$j]), se = sqrt(2 * s2ybar), df = dfe,
                   stringsAsFactors = FALSE)
  pf$significant <- lets[pf$level1] != lets[pf$level2]
  list(pairs = pf, letters = lets[lv], warnings = warns)
}

# Recursive Scott-Knott on sorted means; returns integer group ids
sk_split <- function(y, s2ybar, v, alpha) {
  k <- length(y)
  grp <- rep(1L, k)
  next_id <- 2L
  queue <- list(seq_len(k))
  final <- list()
  while (length(queue)) {
    idx <- queue[[1]]; queue <- queue[-1]
    kk <- length(idx)
    if (kk < 2) { final[[length(final) + 1]] <- idx; next }
    yy <- y[idx]
    tot <- sum(yy)
    b0 <- vapply(seq_len(kk - 1), function(s) {
      t1 <- sum(yy[1:s]); t2 <- tot - t1
      t1^2 / s + t2^2 / (kk - s) - tot^2 / kk
    }, numeric(1))
    best <- which.max(b0)
    B0 <- b0[best]
    sigma0 <- (sum((yy - mean(yy))^2) + v * s2ybar) / (kk + v)
    lambda <- (pi / (2 * (pi - 2))) * B0 / sigma0
    crit <- stats::qchisq(1 - alpha, kk / (pi - 2))
    if (lambda > crit) {
      queue <- c(queue, list(idx[1:best], idx[(best + 1):kk]))
    } else {
      final[[length(final) + 1]] <- idx
    }
  }
  # order final groups by position (sorted means descending)
  final <- final[order(vapply(final, min, numeric(1)))]
  for (g in seq_along(final)) grp[final[[g]]] <- g
  grp
}

letters_seq <- function(n) {
  base <- c(letters, LETTERS)
  if (n <= length(base)) return(base[seq_len(n)])
  c(base, paste0(rep(base, each = length(base)), base))[seq_len(n)]
}

# Insert-absorb compact letter display (Piepho, 2004)
cld_letters <- function(est, pairs) {
  lv <- names(est)
  k <- length(lv)
  ord <- lv[order(est, decreasing = TRUE)]
  M <- matrix(TRUE, nrow = k, ncol = 1, dimnames = list(ord, NULL))
  sig <- pairs[pairs$significant %in% TRUE, c("level1", "level2")]
  if (nrow(sig)) {
    for (r in seq_len(nrow(sig))) {
      a <- sig$level1[r]; b <- sig$level2[r]
      cols <- which(M[a, ] & M[b, ])
      for (cc in cols) {
        newc <- M[, cc]; newc[a] <- FALSE
        M[b, cc] <- FALSE
        M <- cbind(M, newc)
      }
      # absorb
      keep <- rep(TRUE, ncol(M))
      for (p in seq_len(ncol(M))) for (q in seq_len(ncol(M))) {
        if (p != q && keep[p] && keep[q] && all(M[, p] <= M[, q]) && (any(M[, p] < M[, q]) || p > q)) keep[p] <- FALSE
      }
      M <- M[, keep, drop = FALSE]
    }
  }
  # order columns by first row having TRUE
  first <- apply(M, 2, function(z) which(z)[1])
  M <- M[, order(first), drop = FALSE]
  L <- letters_seq(ncol(M))
  out <- vapply(ord, function(l) paste0(L[M[l, ]], collapse = ""), character(1))
  out[lv]
}
