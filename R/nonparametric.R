#' Non-parametric tests
#'
#' Rank-based and permutation tests with descriptive summaries (medians,
#' quartiles, IQR, mean ranks) and effect sizes.
#'
#' @details
#' * `"kruskal"`: Kruskal-Wallis for k independent groups. Effect sizes
#'   \eqn{\epsilon^2_H = H/(N-1)} and \eqn{\eta^2_H = (H-k+1)/(N-k)}.
#' * `"friedman"`: Friedman for k treatments in b complete blocks (one
#'   observation per treatment x block). Kendall's
#'   \eqn{W = \chi^2_F/(b(k-1))}.
#' * `"mood"`: Mood's median test. It compares the proportions of
#'   observations above the grand median; it has low power compared with
#'   Kruskal-Wallis and only addresses medians when distributions have the
#'   same shape. Fisher's exact test is used when expected counts are < 5.
#' * `"mann_whitney"`: Wilcoxon rank-sum, with Hodges-Lehmann estimate and
#'   rank-biserial correlation.
#' * `"wilcoxon_paired"`: Wilcoxon signed-rank for paired data (`id`).
#' * `"sign"`: sign test for paired data.
#' * `"permutation"`: Monte Carlo permutation test of the one-way F (or the
#'   difference of means for two groups); p = (1 + #{T* >= T})/(R + 1).
#'
#' Non-parametric tests are not automatic substitutes for ANOVA: they test
#' different hypotheses (stochastic dominance / location shift under equal
#' shapes) and generally do not accommodate complex designs.
#'
#' @param data A data frame.
#' @param response Response column.
#' @param group Group/treatment column.
#' @param test Test (see Details).
#' @param block Block column (Friedman).
#' @param id Pair identifier (paired tests).
#' @param conf_level Confidence level for Hodges-Lehmann intervals.
#' @param R Number of permutations.
#' @param seed Random seed for permutations.
#' @param exclude Rows to exclude explicitly.
#' @return A `caagro_result` of class `caagro_np` with tables `test`,
#'   `descriptives` and (where applicable) `contingency`.
#' @references
#' Kruskal, W. H. and Wallis, W. A. (1952). Use of ranks in one-criterion
#' variance analysis. *Journal of the American Statistical Association*,
#' 47(260), 583-621. \doi{10.1080/01621459.1952.10483441}
#'
#' Friedman, M. (1937). The use of ranks to avoid the assumption of
#' normality implicit in the analysis of variance. *Journal of the American
#' Statistical Association*, 32(200), 675-701.
#' \doi{10.1080/01621459.1937.10503522}
#'
#' Mood, A. M. (1950). *Introduction to the Theory of Statistics*.
#' McGraw-Hill.
#'
#' Mann, H. B. and Whitney, D. R. (1947). On a test of whether one of two
#' random variables is stochastically larger than the other. *Annals of
#' Mathematical Statistics*, 18(1), 50-60. \doi{10.1214/aoms/1177730491}
#'
#' Wilcoxon, F. (1945). Individual comparisons by ranking methods.
#' *Biometrics Bulletin*, 1(6), 80-83. \doi{10.2307/3001968}
#'
#' Tomczak, M. and Tomczak, E. (2014). The need to report effect size
#' estimates revisited. *Trends in Sport Sciences*, 1(21), 19-25.
#' @examples
#' kw <- np_test(PlantGrowth, "weight", "group", "kruskal")
#' kw
#' np_posthoc(kw, "dunn")
#' @export
np_test <- function(data, response, group, test = c("kruskal", "friedman", "mood", "mann_whitney",
                                                     "wilcoxon_paired", "sign", "permutation"),
                    block = NULL, id = NULL, conf_level = 0.95, R = 4999, seed = 123, exclude = NULL) {
  test <- match.arg(test)
  if (test == "friedman" && is.null(block)) abort_caagro("Friedman exige a coluna de blocos.")
  if (test %in% c("wilcoxon_paired", "sign") && is.null(id)) abort_caagro("Testes pareados exigem `id` (identificador do par).")
  prep <- prepare_data(data, response, c(group, block, id), exclude = exclude)
  d <- prep$data
  warns <- prep$messages
  y <- d[[response]]; g <- droplevels(d[[group]]); k <- nlevels(g); N <- length(y)
  if (k < 2) abort_caagro("S\u00e3o necessarios ao menos 2 grupos.")
  rk <- rank(y)
  desc <- data.frame(level = levels(g), n = as.vector(table(g)),
                     median = as.vector(tapply(y, g, stats::median)),
                     q1 = as.vector(tapply(y, g, stats::quantile, 0.25)),
                     q3 = as.vector(tapply(y, g, stats::quantile, 0.75)),
                     mean = as.vector(tapply(y, g, mean)),
                     mean_rank = as.vector(tapply(rk, g, mean)))
  desc$iqr <- desc$q3 - desc$q1
  names(desc)[1] <- group
  tables <- list()
  two_needed <- c("mann_whitney", "wilcoxon_paired", "sign")
  if (test %in% two_needed && k != 2) abort_caagro("Este teste compara exatamente 2 grupos; ha ", k, ". Use Kruskal-Wallis ou Friedman.")
  lv <- levels(g)
  if (test == "kruskal") {
    kt <- stats::kruskal.test(y, g)
    H <- unname(kt$statistic)
    tb <- data.frame(test = "Kruskal-Wallis", statistic = H, df = unname(kt$parameter), p = kt$p.value,
                     epsilon_sq = H / (N - 1), eta_sq_H = max(0, (H - k + 1) / (N - k)))
  } else if (test == "friedman") {
    b <- droplevels(d[[block]])
    ct <- table(g, b)
    if (any(ct != 1)) abort_caagro("Friedman exige exatamente uma observa\u00e7\u00e3o por tratamento em cada bloco (blocos completos). ",
                                   "Para blocos incompletos, considere o teste de Skillings-Mack (n\u00e3o implementado nesta vers\u00e3o).")
    ft <- stats::friedman.test(y, g, b)
    Q <- unname(ft$statistic); nb <- nlevels(b)
    rb <- stats::ave(y, b, FUN = rank)
    desc$mean_rank <- as.vector(tapply(rb, g, mean))
    desc$rank_sum <- as.vector(tapply(rb, g, sum))
    tb <- data.frame(test = "Friedman", statistic = Q, df = unname(ft$parameter), p = ft$p.value,
                     kendall_w = Q / (nb * (k - 1)), blocks = nb)
  } else if (test == "mood") {
    med <- stats::median(y)
    above <- factor(ifelse(y > med, "acima", "n\u00e3o acima"), levels = c("acima", "n\u00e3o acima"))
    ct <- table(g, above)
    expct <- outer(rowSums(ct), colSums(ct)) / sum(ct)
    if (any(expct < 5)) {
      ft <- stats::fisher.test(ct, simulate.p.value = k > 4 || N > 200, B = 1e4)
      tb <- data.frame(test = "Mediana de Mood (exato de Fisher)", statistic = NA, df = NA, p = ft$p.value, grand_median = med)
      warns <- c(warns, "Frequ\u00eancias esperadas < 5: usado o teste exato de Fisher.")
    } else {
      cs <- stats::chisq.test(ct, correct = FALSE)
      tb <- data.frame(test = "Mediana de Mood (qui-quadrado)", statistic = unname(cs$statistic), df = unname(cs$parameter),
                       p = cs$p.value, grand_median = med)
    }
    tables$contingency <- as.data.frame.matrix(ct)
    tables$contingency <- cbind(stats::setNames(data.frame(rownames(ct)), group), tables$contingency)
    warns <- c(warns, "O teste da mediana tem baixo poder e compara propor\u00e7\u00f5es acima da mediana geral; s\u00f3 testa medianas se as distribui\u00e7\u00f5es tiverem a mesma forma.")
  } else if (test == "mann_whitney") {
    a <- y[g == lv[1]]; bb <- y[g == lv[2]]
    wt <- suppressWarnings(stats::wilcox.test(a, bb, conf.int = TRUE, conf.level = conf_level, exact = NULL))
    W <- unname(wt$statistic); n1 <- length(a); n2 <- length(bb)
    tb <- data.frame(test = "Mann-Whitney (Wilcoxon soma de postos)", statistic = W, df = NA, p = wt$p.value,
                     hodges_lehmann = unname(wt$estimate), lower = wt$conf.int[1], upper = wt$conf.int[2],
                     rank_biserial = 2 * W / (n1 * n2) - 1)
    if (any(duplicated(y))) warns <- c(warns, "H\u00e1 empates: p-valor pela aproxima\u00e7\u00e3o normal com corre\u00e7\u00e3o de continuidade.")
  } else if (test %in% c("wilcoxon_paired", "sign")) {
    w <- stats::reshape(d[, c(id, group, response)], idvar = id, timevar = group, direction = "wide")
    a <- w[[paste0(response, ".", lv[1])]]; bb <- w[[paste0(response, ".", lv[2])]]
    ok <- stats::complete.cases(a, bb); a <- a[ok]; bb <- bb[ok]
    dif <- a - bb
    if (test == "wilcoxon_paired") {
      wt <- suppressWarnings(stats::wilcox.test(a, bb, paired = TRUE, conf.int = TRUE, conf.level = conf_level))
      nz <- sum(dif != 0)
      tb <- data.frame(test = "Wilcoxon pareado (postos sinalizados)", statistic = unname(wt$statistic), df = NA, p = wt$p.value,
                       hodges_lehmann = unname(wt$estimate), lower = wt$conf.int[1], upper = wt$conf.int[2],
                       rank_biserial = (2 * unname(wt$statistic) / (nz * (nz + 1) / 2)) - 1, pairs = length(dif))
    } else {
      pos <- sum(dif > 0); nz <- sum(dif != 0)
      bt <- stats::binom.test(pos, nz)
      tb <- data.frame(test = "Teste dos sinais", statistic = pos, df = NA, p = bt$p.value, n_nonzero = nz,
                       median_diff = stats::median(dif), pairs = length(dif))
      warns <- c(warns, sprintf("%d diferen\u00e7a(s) nula(s) descartada(s) no teste dos sinais.", sum(dif == 0)))
    }
  } else if (test == "permutation") {
    stat_f <- if (k == 2) function(yy) mean(yy[g == lv[1]]) - mean(yy[g == lv[2]]) else
      function(yy) { m <- tapply(yy, g, mean); n <- tapply(yy, g, length)
        ssb <- sum(n * (m - mean(yy))^2); ssw <- sum((yy - m[g])^2); (ssb / (k - 1)) / (ssw / (N - k)) }
    T0 <- stat_f(y)
    perm <- with_seed(seed, vapply(seq_len(R), function(i) stat_f(sample(y)), numeric(1)))
    p <- if (k == 2) (1 + sum(abs(perm) >= abs(T0) - 1e-12)) / (R + 1) else (1 + sum(perm >= T0 - 1e-12)) / (R + 1)
    tb <- data.frame(test = if (k == 2) "Permuta\u00e7\u00e3o (diferen\u00e7a de m\u00e9dias)" else "Permuta\u00e7\u00e3o (F de um fator)",
                     statistic = T0, df = NA, p = p, permutations = R, seed = seed)
  }
  labels <- c(kruskal = "Kruskal-Wallis", friedman = "Friedman", mood = "Teste da mediana de Mood",
              mann_whitney = "Mann-Whitney", wilcoxon_paired = "Wilcoxon pareado", sign = "Teste dos sinais",
              permutation = "Teste de permuta\u00e7\u00e3o")
  new_result("np", paste("N\u00e3o param\u00e9trico -", labels[[test]]), "caagro_np",
             design = if (!is.null(block)) "Blocos" else NULL,
             settings = list(response = response, group = group, test = test, block = block, id = id,
                             conf_level = conf_level, R = R, seed = seed, exclude = exclude),
             data = data, data_used = d, tables = c(list(test = tb, descriptives = desc), tables),
             warnings = warns, seed = if (test == "permutation") seed,
             notes = list(Resultado = sprintf("%s: estat\u00edstica = %s, p %s.", labels[[test]], fmt_num(tb$statistic), fmt_p_eq(tb$p)),
                          Interpretacao = "p pequeno indica que ao menos um grupo tende a apresentar valores sistematicamente maiores ou menores. Use compara\u00e7\u00f5es post-hoc com corre\u00e7\u00e3o de multiplicidade para localizar as diferen\u00e7as.",
                          Cuidados = "Testes de postos n\u00e3o s\u00e3o substitutos autom\u00e1ticos da ANOVA: testam hip\u00f3teses diferentes e perdem informa\u00e7\u00e3o de magnitude. Escolha o m\u00e9todo pelo delineamento e pela natureza da vari\u00e1vel, n\u00e3o apenas por um teste de normalidade."),
             code = code_call("caagro::np_test", list(data = as.name("dados"), response = response, group = group, test = test,
                                                      block = block, id = id, R = if (test == "permutation") R,
                                                      seed = if (test == "permutation") seed, exclude = exclude)))
}

#' Post-hoc comparisons after non-parametric tests
#'
#' * After Kruskal-Wallis: `"dunn"` (Dunn, 1964; tie-corrected z on mean
#'   ranks), `"conover"` (Conover-Iman t on ranks) or `"wilcoxon"`
#'   (pairwise rank-sum tests).
#' * After Friedman: `"conover"` (Conover's test for blocked data) or
#'   `"wilcoxon"` (pairwise signed-rank tests on blocks).
#'
#' @param x A `caagro_np` result from `np_test()` with `"kruskal"` or
#'   `"friedman"`.
#' @param method `"dunn"`, `"conover"` or `"wilcoxon"`.
#' @param p_adjust Multiplicity correction (default Holm).
#' @param alpha Significance level for letters.
#' @return A `caagro_result` (class `caagro_np`) with tables `pairs` and
#'   `descriptives` (with grouping letters based on adjusted p-values).
#' @references
#' Dunn, O. J. (1964). Multiple comparisons using rank sums.
#' *Technometrics*, 6(3), 241-252. \doi{10.1080/00401706.1964.10490181}
#'
#' Conover, W. J. and Iman, R. L. (1979). *On multiple-comparisons
#' procedures* (Technical Report LA-7677-MS). Los Alamos Scientific
#' Laboratory. \doi{10.2172/6057803}
#'
#' Conover, W. J. (1999). *Practical Nonparametric Statistics* (3rd ed.).
#' Wiley.
#' @examples
#' kw <- np_test(PlantGrowth, "weight", "group", "kruskal")
#' np_posthoc(kw, "dunn", p_adjust = "holm")
#' @export
np_posthoc <- function(x, method = c("dunn", "conover", "wilcoxon"), p_adjust = "holm", alpha = 0.05) {
  if (!inherits(x, "caagro_np") || !x$settings$test %in% c("kruskal", "friedman")) {
    abort_caagro("`x` deve ser resultado de np_test() com Kruskal-Wallis ou Friedman.")
  }
  method <- match.arg(method); valid_p_adjust(p_adjust); check_alpha(alpha)
  d <- x$data_used; resp <- x$settings$response; grp <- x$settings$group
  y <- d[[resp]]; g <- droplevels(d[[grp]]); lv <- levels(g); k <- length(lv); N <- length(y)
  ij <- pair_index(k)
  if (x$settings$test == "kruskal") {
    if (method == "wilcoxon") {
      p <- mapply(function(i, j) suppressWarnings(stats::wilcox.test(y[g == lv[i]], y[g == lv[j]])$p.value), ij$i, ij$j)
      stat <- mapply(function(i, j) suppressWarnings(unname(stats::wilcox.test(y[g == lv[i]], y[g == lv[j]])$statistic)), ij$i, ij$j)
    } else {
      r <- rank(y); Rb <- tapply(r, g, mean); n <- tapply(r, g, length)
      ties <- table(y); tc <- sum(ties^3 - ties)
      if (method == "dunn") {
        s2 <- N * (N + 1) / 12 - tc / (12 * (N - 1))
        stat <- (Rb[ij$i] - Rb[ij$j]) / sqrt(s2 * (1 / n[ij$i] + 1 / n[ij$j]))
        p <- 2 * stats::pnorm(-abs(stat))
      } else {
        H <- x$tables$test$statistic
        S2 <- (sum(r^2) - N * (N + 1)^2 / 4) / (N - 1)
        stat <- (Rb[ij$i] - Rb[ij$j]) / sqrt(S2 * (N - 1 - H) / (N - k) * (1 / n[ij$i] + 1 / n[ij$j]))
        p <- 2 * stats::pt(-abs(stat), N - k)
      }
    }
  } else {
    if (method == "dunn") abort_caagro("Para Friedman use 'conover' ou 'wilcoxon'.")
    b <- droplevels(d[[x$settings$block]]); nb <- nlevels(b)
    w <- tapply(y, list(b, g), function(z) z[1])
    if (method == "wilcoxon") {
      p <- mapply(function(i, j) suppressWarnings(stats::wilcox.test(w[, i], w[, j], paired = TRUE)$p.value), ij$i, ij$j)
      stat <- mapply(function(i, j) suppressWarnings(unname(stats::wilcox.test(w[, i], w[, j], paired = TRUE)$statistic)), ij$i, ij$j)
    } else {
      rw <- t(apply(w, 1, rank))
      Rj <- colSums(rw); A1 <- sum(rw^2)
      den <- sqrt(2 * (nb * A1 - sum(Rj^2)) / ((nb - 1) * (k - 1)))
      stat <- (Rj[ij$i] - Rj[ij$j]) / den
      p <- 2 * stats::pt(-abs(stat), (nb - 1) * (k - 1))
    }
  }
  pr <- data.frame(contrast = paste(lv[ij$i], "-", lv[ij$j]), level1 = lv[ij$i], level2 = lv[ij$j],
                   statistic = unname(stat), p = unname(p), p_adj = stats::p.adjust(unname(p), p_adjust))
  pr$significant <- pr$p_adj < alpha
  desc <- x$tables$descriptives
  est <- stats::setNames(desc$mean_rank, desc[[grp]])
  lets <- cld_letters(est, pr)
  desc$letras <- lets[as.character(desc[[grp]])]
  lab <- c(dunn = "Dunn", conover = "Conover", wilcoxon = "Wilcoxon par a par")[[method]]
  out <- x
  out$method <- paste0(x$method, " + post-hoc de ", lab, " (ajuste ", p_adjust, ")")
  out$settings <- c(x$settings, list(posthoc = method, p_adjust = p_adjust, alpha = alpha))
  out$tables <- list(test = x$tables$test, descriptives = desc, pairs = pr)
  out$main <- "pairs"
  out$notes$Interpretacao <- "Grupos com a mesma letra n\u00e3o diferem pelo post-hoc (valores de p ajustados); letras ordenadas pelo posto medio."
  out$code <- paste0(x$code, " |>\n  caagro::np_posthoc(", deparse(method), ", p_adjust = ", deparse(p_adjust), ", alpha = ", alpha, ")")
  out
}
