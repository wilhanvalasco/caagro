test_that("assumption tests match stats/car", {
  fit <- anova_exp(PlantGrowth, "weight", "group")
  tb <- result_table(check_assumptions(fit), "tests")
  r <- residuals(aov(weight ~ group, PlantGrowth))
  expect_equal(tb$p[tb$test == "Shapiro-Wilk (residuos)"], shapiro.test(r)$p.value)
  expect_equal(tb$p[tb$test == "Bartlett"], bartlett.test(weight ~ group, PlantGrowth)$p.value)
  skip_if_not_installed("car")
  expect_equal(tb$p[tb$test == "Levene (media)"], car::leveneTest(weight ~ group, PlantGrowth, center = mean)$`Pr(>F)`[1], tolerance = 1e-10)
  expect_equal(tb$p[grepl("Brown", tb$test)], car::leveneTest(weight ~ group, PlantGrowth, center = median)$`Pr(>F)`[1], tolerance = 1e-10)
})

test_that("analytical confidence intervals are correct", {
  y <- PlantGrowth$weight; n <- length(y)
  m <- result_table(conf_int(PlantGrowth, "weight", "mean"))
  expect_equal(c(m$lower, m$upper), as.numeric(t.test(y)$conf.int))
  v <- result_table(conf_int(PlantGrowth, "weight", "variance"))
  expect_equal(v$lower, (n - 1) * var(y) / qchisq(0.975, n - 1))
  d <- subset(PlantGrowth, group != "trt2")
  w <- result_table(conf_int(d, "weight", "diff_means_welch", group = "group"))
  expect_equal(c(w$lower, w$upper), as.numeric(t.test(weight ~ group, d)$conf.int))
  vr <- result_table(conf_int(d, "weight", "var_ratio", group = "group"))
  expect_equal(c(vr$lower, vr$upper), as.numeric(var.test(weight ~ group, d)$conf.int))
  pr <- result_table(conf_int(data.frame(s = c(rep(1, 7), rep(0, 13))), "s", "proportion", prop_method = "wilson"))
  ref <- prop.test(7, 20, correct = FALSE)$conf.int
  expect_equal(c(pr$lower, pr$upper), as.numeric(ref), tolerance = 1e-8)
  ex <- result_table(conf_int(data.frame(s = c(rep(1, 7), rep(0, 13))), "s", "proportion", prop_method = "exact"))
  expect_equal(c(ex$lower, ex$upper), as.numeric(binom.test(7, 20)$conf.int))
})

test_that("group intervals with pooled variance use MSE", {
  r <- conf_int_groups(PlantGrowth, "weight", "group")
  it <- result_table(r)
  mse <- anova(lm(weight ~ group, PlantGrowth))$`Mean Sq`[2]
  expect_equal(it$se, rep(sqrt(mse / 10), 3))
  s <- result_table(conf_int_groups(PlantGrowth, "weight", "group", variance = "separate"))
  expect_equal(s$upper[1], as.numeric(t.test(PlantGrowth$weight[1:10])$conf.int[2]))
})

test_that("bootstrap intervals are reproducible and close to boot", {
  a <- conf_int_boot(PlantGrowth, "weight", "mean", R = 2000, seed = 10)
  b <- conf_int_boot(PlantGrowth, "weight", "mean", R = 2000, seed = 10)
  expect_identical(result_table(a), result_table(b))
  skip_if_not_installed("boot")
  set.seed(10)
  bb <- boot::boot(PlantGrowth$weight, function(x, i) mean(x[i]), R = 2000)
  ci <- boot::boot.ci(bb, type = "bca")$bca[4:5]
  expect_equal(c(result_table(a)$lower, result_table(a)$upper), ci, tolerance = 0.01)
})

test_that("descriptives are correct", {
  s <- result_table(describe_data(PlantGrowth, "weight"))
  expect_equal(s$mean, mean(PlantGrowth$weight))
  expect_equal(s$sd, sd(PlantGrowth$weight))
  expect_equal(s$iqr, IQR(PlantGrowth$weight))
})

test_that("outliers are flagged, never removed", {
  d <- PlantGrowth; d$weight[1] <- 20
  fo <- flag_outliers(d, "weight", "group")
  expect_true(fo$flag[fo$row == 1])
  expect_equal(nrow(fo), nrow(d))
  st <- flag_outliers(d, "weight", "group", method = "studentized")
  expect_true(st$flag[st$row == 1])
})
