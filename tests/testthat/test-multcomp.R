test_that("Tukey matches TukeyHSD (balanced and unbalanced)", {
  fit <- anova_exp(PlantGrowth, "weight", "group")
  tk <- result_table(compare_means(fit, "tukey"), "pairs")
  ref <- TukeyHSD(aov(weight ~ group, PlantGrowth))$group
  expect_equal(abs(tk$diff), abs(unname(ref[, "diff"])), tolerance = 1e-10)
  expect_equal(tk$p_adj, unname(ref[, "p adj"]), tolerance = 1e-6)
  u <- PlantGrowth[-c(1, 2, 15), ]
  tku <- result_table(compare_means(anova_exp(u, "weight", "group"), "tukey"), "pairs")
  expect_equal(tku$p_adj, unname(TukeyHSD(aov(weight ~ group, u))$group[, "p adj"]), tolerance = 1e-6)
})

test_that("letters follow significance (Piepho algorithm)", {
  fit <- anova_exp(PlantGrowth, "weight", "group")
  m <- result_table(compare_means(fit, "tukey"), "means")
  expect_equal(m$letras[m$group == "trt2"], "a")
  expect_equal(m$letras[m$group == "trt1"], "b")
  expect_equal(m$letras[m$group == "ctrl"], "ab")
  skip_if_not_installed("multcompView")
  pr <- result_table(compare_means(anova_exp(sim_dbc, "produtividade", "tratamento", block = "bloco"), "tukey"), "pairs")
  sig <- stats::setNames(pr$significant, paste(pr$level1, pr$level2, sep = "-"))
  mv <- multcompView::multcompLetters(sig)$Letters
  mine <- caagro:::cld_letters(stats::setNames(rep(1, 6), paste0("T", 1:6)), pr)
  # same partition of non-significant pairs
  share <- function(L, a, b) any(strsplit(L[a], "")[[1]] %in% strsplit(L[b], "")[[1]])
  for (i in seq_len(nrow(pr))) expect_equal(share(mine, pr$level1[i], pr$level2[i]), share(mv, pr$level1[i], pr$level2[i]))
})

test_that("Dunnett matches emmeans multivariate-t adjustment", {
  skip_if_not_installed("emmeans")
  d <- sim_dbc[-c(3, 10), ]
  fit <- anova_exp(d, "produtividade", "tratamento", block = "bloco")
  mine <- result_table(compare_means(fit, "dunnett", control = "T1"), "pairs")
  em <- emmeans::emmeans(lm(produtividade ~ bloco + tratamento, d), "tratamento")
  ref <- summary(emmeans::contrast(em, "trt.vs.ctrl", ref = 1, adjust = "mvt"))
  expect_equal(mine$p_adj, ref$p.value, tolerance = 2e-3)
})

test_that("Games-Howell matches rstatix", {
  skip_if_not_installed("rstatix")
  fit <- anova_exp(PlantGrowth, "weight", "group")
  gh <- result_table(compare_means(fit, "games_howell"), "pairs")
  ref <- rstatix::games_howell_test(PlantGrowth, weight ~ group)
  expect_equal(round(gh$p_adj, 3), ref$p.adj)
})

test_that("SNK: hand computation and closure", {
  # Balanced: q = |diff| / sqrt(MSE/n), p from studentized range with p means spanned
  fit <- anova_exp(PlantGrowth, "weight", "group")
  pr <- result_table(compare_means(fit, "snk"), "pairs")
  mse <- result_table(fit, "precision")$mse
  q <- 0.865 / sqrt(mse / 10)
  expect_equal(pr$p_unadj[pr$contrast == "trt1 - trt2"], ptukey(q, 3, 27, lower.tail = FALSE), tolerance = 1e-8)
  q2 <- 0.494 / sqrt(mse / 10)
  expect_equal(pr$p_unadj[pr$contrast == "ctrl - trt2"], ptukey(q2, 2, 27, lower.tail = FALSE), tolerance = 1e-8)
  # closure: build data where widest range is not significant
  set.seed(1)
  d <- data.frame(g = rep(letters[1:4], each = 3), y = rep(c(10, 10.1, 10.2, 10.3), each = 3) + rnorm(12, 0, 2))
  s <- result_table(compare_means(anova_exp(d, "y", "g"), "snk"), "pairs")
  expect_false(any(s$significant))
})

test_that("Scott-Knott follows the published algorithm", {
  # Independent computation for the first split
  fit <- anova_exp(sim_dbc, "produtividade", "tratamento", block = "bloco")
  sk <- compare_means(fit, "scott_knott")
  m <- result_table(sk, "means")
  y <- sort(m$emmean, decreasing = TRUE)
  k <- length(y); v <- result_table(fit, "precision")$df_error; s2 <- result_table(fit, "precision")$mse / 4
  B <- sapply(1:(k - 1), function(i) sum(y[1:i])^2 / i + sum(y[(i + 1):k])^2 / (k - i) - sum(y)^2 / k)
  sig0 <- (sum((y - mean(y))^2) + v * s2) / (k + v)
  lam <- pi / (2 * (pi - 2)) * max(B) / sig0
  first_split <- lam > qchisq(0.95, k / (pi - 2))
  expect_equal(length(unique(m$letras)) > 1, first_split)
  # groups are contiguous in the ordered means and non-overlapping
  expect_true(all(nchar(m$letras) == 1))
  expect_identical(rle(m$letras)$values, unique(m$letras))
})

test_that("LSD protection and warnings", {
  set.seed(2)
  d <- data.frame(g = rep(letters[1:4], each = 4), y = rnorm(16))
  fit <- anova_exp(d, "y", "g")
  skip_if(result_table(fit, "anova")$p[1] < 0.05)
  l <- compare_means(fit, "lsd", protected = TRUE)
  expect_false(any(result_table(l, "pairs")$significant))
  expect_true(any(grepl("protegido", l$warnings)))
  expect_error(compare_means(fit, "lsd", p_adjust = "xx"), "desconhecida")
})

test_that("Games-Howell refuses blocked designs", {
  fit <- anova_exp(sim_dbc, "produtividade", "tratamento", block = "bloco")
  expect_error(compare_means(fit, "games_howell"), "DIC")
})
