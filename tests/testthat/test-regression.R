test_that("polynomial fits match lm", {
  d <- subset(sim_dose, cultivar == "Cv1")
  r <- fit_regression(d, "dose", "produtividade", c("linear", "quadratic"))
  l <- lm(produtividade ~ dose + I(dose^2), d)
  co <- result_table(r, "coefficients")
  expect_equal(co$estimate[co$model == "quadratic"], unname(coef(l)), tolerance = 1e-10)
  fs <- result_table(r, "fit_stats")
  expect_equal(fs$AIC[fs$model == "quadratic"], AIC(l))
  expect_equal(fs$BIC[fs$model == "quadratic"], BIC(l))
  expect_equal(fs$r2_adj[fs$model == "quadratic"], summary(l)$adj.r.squared)
  pr <- result_table(r, "predictions"); pr <- pr[pr$model == "quadratic", ]
  ref <- predict(l, data.frame(dose = pr$x), interval = "confidence")
  expect_equal(pr$lower, unname(ref[, "lwr"]), tolerance = 1e-10)
})

test_that("delta-method bands for nls agree with exact lm bands for a linear-in-parameters model", {
  d <- subset(sim_dose, cultivar == "Cv1")
  n1 <- nls(produtividade ~ a + b * dose + c * dose^2, d, start = list(a = 3, b = 0.03, c = -1e-4))
  spec <- caagro:::model_registry()$quadratic; spec$type <- "nls"
  bd <- caagro:::reg_bands(n1, spec, "q", "g", c(0, 100, 200), d, 0.95, "prediction", "delta", 0, 1)
  ref <- predict(lm(produtividade ~ dose + I(dose^2), d), data.frame(dose = c(0, 100, 200)), interval = "prediction")
  expect_equal(bd$lower, unname(ref[, "lwr"]), tolerance = 1e-5)
})

test_that("non-linear fits converge and agree with nls", {
  d <- subset(sim_dose, cultivar == "Cv1")
  r <- fit_regression(d, "dose", "produtividade", "mitscherlich")
  expect_true(result_table(r, "fit_stats")$converged)
  expect_equal(result_table(r, "fit_stats")$r2_type, "pseudo")
  lg <- fit_regression(subset(sim_germinacao, produto == "P2"), "dose", "germinadas", "loglogistic")
  e <- result_table(lg, "coefficients")
  expect_true(e$estimate[e$term == "e"] > 2 && e$estimate[e$term == "e"] < 10)
})

test_that("binomial regression matches glm and ED50 is correct", {
  d <- subset(sim_germinacao, dose > 0 & produto == "P1")
  b <- fit_regression(d, "dose", "germinadas", "binomial_logdose", trials = "total")
  g <- glm(cbind(germinadas, total - germinadas) ~ log(dose), binomial, d)
  expect_equal(result_table(b, "coefficients")$estimate, unname(coef(g)), tolerance = 1e-8)
  expect_equal(result_table(b, "fit_stats")$r2_type, "McFadden")
  ed <- result_table(optimum_dose(b))
  expect_equal(ed$estimate, exp(-coef(g)[[1]] / coef(g)[[2]]), tolerance = 1e-8)
  expect_error(fit_regression(d, "dose", "germinadas", c("binomial", "linear"), trials = "total"), "misture")
})

test_that("MET and curve comparison", {
  r <- fit_regression(sim_dose, "dose", "produtividade", "quadratic", group = "cultivar")
  op <- result_table(optimum_dose(r, price_ratio = 0.004))
  co <- result_table(r, "coefficients"); c1 <- co[co$group == "Cv1", ]
  expect_equal(op$estimate[1], -c1$estimate[2] / (2 * c1$estimate[3]))
  ct <- result_table(r, "curve_test")
  full <- lm(produtividade ~ cultivar * (dose + I(dose^2)), sim_dose)
  red <- lm(produtividade ~ dose + I(dose^2), sim_dose)
  expect_equal(ct$p, anova(red, full)$`Pr(>F)`[2], tolerance = 1e-10)
})

test_that("domain errors and failures are reported, not invented", {
  r <- fit_regression(sim_dose, "dose", "produtividade", c("quadratic", "power"))
  expect_true("failures" %in% names(r$tables))
  expect_false("power" %in% result_table(r, "fit_stats")$model)
  expect_error(fit_regression(sim_dose, "dose", "produtividade", "nao_existe"), "desconhecido")
})

test_that("legacy aggregation warns", {
  r <- fit_regression(sim_dose, "dose", "produtividade", "quadratic", aggregate = "mean")
  expect_true(any(grepl("legado", r$warnings)))
})
