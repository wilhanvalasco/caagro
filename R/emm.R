# Estimated marginal means (least-squares means) for fixed-effect lm fits.
# Used by anova_exp(), compare_means() and contrast_test(). Equal weights over
# the levels of the factors not in `specs`; numeric covariates at their mean.

emm_grid <- function(fit, specs) {
  xlev <- fit$xlevels
  if (!all(specs %in% names(xlev))) {
    abort_caagro("Fator(es) nao presente(s) no modelo: ",
                 paste(setdiff(specs, names(xlev)), collapse = ", "))
  }
  mf <- stats::model.frame(fit)
  tt <- stats::delete.response(stats::terms(fit))
  vars <- all.vars(tt)
  grid_list <- lapply(vars, function(v) {
    if (v %in% names(xlev)) factor(xlev[[v]], levels = xlev[[v]]) else mean(mf[[v]])
  })
  names(grid_list) <- vars
  grid <- expand.grid(grid_list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  for (v in intersect(vars, names(xlev))) grid[[v]] <- factor(grid[[v]], levels = xlev[[v]])
  X <- stats::model.matrix(tt, grid, contrasts.arg = fit$contrasts, xlev = xlev)
  key <- interaction(grid[, specs, drop = FALSE], sep = " : ", lex.order = TRUE, drop = TRUE)
  L <- do.call(rbind, lapply(split(seq_len(nrow(X)), key), function(i) colMeans(X[i, , drop = FALSE])))
  combos <- grid[!duplicated(key), specs, drop = FALSE]
  keys <- as.character(key[!duplicated(key)])
  lv <- combos[match(rownames(L), keys), , drop = FALSE]
  rownames(lv) <- NULL
  list(L = L, levels = lv, label = rownames(L))
}

# Estimability of rows of L given the design matrix of fit
estimable_rows <- function(fit, L, tol = 1e-8) {
  X <- stats::model.matrix(fit)
  q <- qr(X)
  if (q$rank == ncol(X)) return(rep(TRUE, nrow(L)))
  # projection onto row space of X
  N <- qr.Q(qr(t(X)))[, seq_len(q$rank), drop = FALSE]
  resid <- L - (L %*% N) %*% t(N)
  sqrt(rowSums(resid^2)) < tol * pmax(1, sqrt(rowSums(L^2)))
}

emm_compute <- function(fit, specs, conf_level = 0.95) {
  g <- emm_grid(fit, specs)
  b <- stats::coef(fit)
  ok <- !is.na(b)
  L <- g$L[, ok, drop = FALSE]
  V <- stats::vcov(fit)[ok, ok, drop = FALSE]
  est <- as.vector(L %*% b[ok])
  VL <- L %*% V %*% t(L)
  se <- sqrt(pmax(diag(VL), 0))
  estim <- estimable_rows(fit, g$L)
  est[!estim] <- NA; se[!estim] <- NA
  dfe <- stats::df.residual(fit)
  tq <- stats::qt(1 - (1 - conf_level) / 2, dfe)
  out <- cbind(g$levels,
               data.frame(emmean = est, se = se, df = dfe,
                          lower = est - tq * se, upper = est + tq * se))
  attr(out, "L") <- L
  attr(out, "V") <- VL
  attr(out, "label") <- g$label
  out
}
