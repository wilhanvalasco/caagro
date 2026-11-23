test_that("DIC ANOVA matches stats::aov", {
  fit <- anova_exp(PlantGrowth, "weight", "group")
  ref <- anova(aov(weight ~ group, data = PlantGrowth))
  tb <- result_table(fit, "anova")
  expect_equal(tb$ss, ref$`Sum Sq`, tolerance = 1e-10)
  expect_equal(tb$F[1], ref$`F value`[1], tolerance = 1e-10)
  expect_equal(tb$p[1], ref$`Pr(>F)`[1], tolerance = 1e-10)
  pr <- result_table(fit, "precision")
  expect_equal(pr$cv_percent, 100 * sqrt(ref$`Mean Sq`[2]) / mean(PlantGrowth$weight))
  expect_s3_class(fit, "caagro_result")
})

test_that("DBC uses block term and never silently becomes DIC", {
  fit <- anova_exp(sim_dbc, "produtividade", "tratamento", block = "bloco")
  ref <- anova(lm(produtividade ~ bloco + tratamento, sim_dbc))
  expect_equal(result_table(fit, "anova")$ss[1:2], ref$`Sum Sq`[1:2])
  expect_equal(fit$design, "DBC")
  expect_error(anova_exp(sim_dbc, "produtividade", "tratamento", block = "bloco", design = "DIC"), "DIC")
  expect_error(anova_exp(sim_dbc, "produtividade", "tratamento", design = "DBC"), "bloco")
})

test_that("Type II and III sums of squares match car::Anova on unbalanced data", {
  skip_if_not_installed("car")
  d <- sim_fatorial[-c(1, 5, 9), ]
  r2 <- result_table(anova_exp(d, "altura", "cultivar", block = "bloco", factors = "adubacao", ss_type = "II"), "anova")
  c2 <- car::Anova(lm(altura ~ bloco + cultivar * adubacao, d), type = 2)
  expect_equal(unname(r2$ss), unname(c2$`Sum Sq`), tolerance = 1e-8)
  old <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(old))
  c3 <- car::Anova(lm(altura ~ bloco + cultivar * adubacao, d), type = 3)[-1, ]
  options(old)
  r3 <- result_table(anova_exp(d, "altura", "cultivar", block = "bloco", factors = "adubacao", ss_type = "III"), "anova")
  expect_equal(unname(r3$ss), unname(c3$`Sum Sq`), tolerance = 1e-8)
})

test_that("estimated marginal means match emmeans for unbalanced DBC", {
  skip_if_not_installed("emmeans")
  d <- sim_dbc[-c(3, 10), ]
  fit <- anova_exp(d, "produtividade", "tratamento", block = "bloco")
  em <- summary(emmeans::emmeans(lm(produtividade ~ bloco + tratamento, d), "tratamento"))
  m <- result_table(fit, "means")
  expect_equal(m$emmean, em$emmean, tolerance = 1e-10)
  expect_equal(m$se, em$SE, tolerance = 1e-10)
  expect_true(any(grepl("desbalanceados", fit$warnings)))
})

test_that("aliased terms are reported, not hidden", {
  f <- anova_exp(npk, "yield", "N", block = "block", factors = c("P", "K"))
  expect_false("N:P:K" %in% result_table(f, "anova")$term)
  expect_true(any(grepl("N:P:K", f$warnings)))
})

test_that("insufficient data produce clear errors", {
  d <- data.frame(t = c("a", "b"), y = c(1, 2))
  expect_error(anova_exp(d, "y", "t"))
  d2 <- data.frame(t = rep(c("a", "b"), each = 3), y = 5)
  expect_error(anova_exp(d2, "y", "t"), "variabilidade")
  d3 <- data.frame(t = rep(c("a", "b"), each = 3), y = c(1, 2, "x", 4, 5, 6))
  expect_error(anova_exp(d3, "y", "t"), "n\u00e3o num\u00e9rico")
})

test_that("explicit exclusions are recorded and missing values reported", {
  d <- PlantGrowth; d$weight[3] <- NA
  f <- anova_exp(d, "weight", "group", exclude = c(1, 2))
  expect_equal(nrow(f$data_used), 27)
  expect_true(any(grepl("explicitamente", f$warnings)))
  expect_true(any(grepl("ausentes", f$warnings)))
})

test_that("Welch ANOVA, t tests and contrasts match stats", {
  w <- anova_exp(PlantGrowth, "weight", "group", method = "welch")
  ref <- oneway.test(weight ~ group, PlantGrowth)
  expect_equal(result_table(w)$p, ref$p.value)
  d <- subset(PlantGrowth, group != "trt2")
  t1 <- mean_test(d, "weight", "group", type = "independent")
  expect_equal(result_table(t1)$p, t.test(weight ~ group, d, var.equal = TRUE)$p.value)
  t2 <- mean_test(sleep, "extra", "group", type = "paired", id = "ID")
  expect_equal(result_table(t2)$p, t.test(sleep$extra[1:10], sleep$extra[11:20], paired = TRUE)$p.value)
  fit <- anova_exp(PlantGrowth, "weight", "group")
  ct <- compare_means(fit, contrasts = list(a = c(0, 1, -1)))
  ref2 <- t.test(weight ~ group, subset(PlantGrowth, group != "ctrl"), var.equal = TRUE)
  expect_equal(abs(result_table(ct)$estimate), abs(unname(diff(ref2$estimate))))
})

test_that("default is the conventional Fisher ANOVA (sequential SS, as aov)", {
  d <- sim_fatorial[-c(1, 5, 9), ]
  f <- anova_exp(d, "altura", "cultivar", block = "bloco", factors = "adubacao")
  ref <- anova(aov(altura ~ bloco + cultivar * adubacao, d))
  expect_equal(result_table(f)$ss, ref$`Sum Sq`, tolerance = 1e-10)
  expect_equal(result_table(f)$p[1:4], ref$`Pr(>F)`[1:4], tolerance = 1e-10)
  expect_equal(result_table(f, "precision")$ss_type, "I")
  expect_error(anova_exp(sim_dbc, "produtividade", "tratamento", block = "bloco", method = "welch"), "DIC")
})

test_that("Shapiro-Wilk and Bartlett: values, grouping by design and interpretation", {
  f <- anova_exp(sim_fatorial, "altura", "cultivar", block = "bloco", factors = "adubacao")
  tb <- result_table(check_assumptions(f))
  cells <- interaction(sim_fatorial$cultivar, sim_fatorial$adubacao)
  expect_equal(tb$p[tb$test == "Bartlett"], bartlett.test(residuals(f$model), cells)$p.value)
  expect_equal(tb$df[tb$test == "Bartlett"], "8")
  expect_equal(tb$p[tb$test == "Shapiro-Wilk"], shapiro.test(residuals(f$model))$p.value)
  expect_match(tb$interpretation[tb$test == "Shapiro-Wilk"], "não identificou evidência")
  expect_match(tb$interpretation[tb$test == "Bartlett"], "identificou evidência de heterogeneidade")
})

test_that("DMS of Tukey and LSD follow the textbook formulas; Dunnett has no letters", {
  f <- anova_exp(sim_soja, "produtividade", "tratamento")
  mse <- result_table(f, "precision")$mse
  expect_equal(result_table(compare_means(f, "tukey"), "dms")$dms, qtukey(0.95, 4, 16) * sqrt(mse / 5))
  expect_equal(result_table(compare_means(f, "lsd"), "dms")$dms, qt(0.975, 16) * sqrt(2 * mse / 5))
  dn <- result_table(compare_means(f, "dunnett", control = "Testemunha"), "means")
  expect_true(all(is.na(dn$letras)))
  expect_equal(dn$vs_controle[dn$tratamento == "Testemunha"], "controle")
})
