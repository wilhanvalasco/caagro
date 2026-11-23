test_that("rank tests match stats", {
  kw <- np_test(PlantGrowth, "weight", "group", "kruskal")
  expect_equal(result_table(kw)$p, kruskal.test(weight ~ group, PlantGrowth)$p.value)
  wb <- warpbreaks[warpbreaks$wool == "A", ]; wb$blk <- rep(1:9, 3)
  fr <- np_test(wb, "breaks", "tension", "friedman", block = "blk")
  expect_equal(result_table(fr)$p, friedman.test(breaks ~ tension | blk, wb)$p.value)
  d <- subset(PlantGrowth, group != "trt2")
  mw <- np_test(d, "weight", "group", "mann_whitney")
  expect_equal(result_table(mw)$p, suppressWarnings(wilcox.test(weight ~ group, d)$p.value))
  expect_error(np_test(wb[-1, ], "breaks", "tension", "friedman", block = "blk"), "uma observacao")
})

test_that("Dunn test matches rstatix", {
  skip_if_not_installed("rstatix")
  ph <- result_table(np_posthoc(np_test(PlantGrowth, "weight", "group"), "dunn", "holm"), "pairs")
  ref <- rstatix::dunn_test(PlantGrowth, weight ~ group, p.adjust.method = "holm")
  expect_equal(ph$p_adj, ref$p.adj, tolerance = 1e-8)
})

test_that("Mood median test and permutation", {
  md <- np_test(PlantGrowth, "weight", "group", "mood")
  ct <- md$tables$contingency
  expect_equal(sum(ct$acima), sum(PlantGrowth$weight > median(PlantGrowth$weight)))
  p1 <- np_test(PlantGrowth, "weight", "group", "permutation", R = 999, seed = 5)
  p2 <- np_test(PlantGrowth, "weight", "group", "permutation", R = 999, seed = 5)
  expect_identical(result_table(p1), result_table(p2))
})

test_that("PCA matches prcomp; clustering matches hclust", {
  p <- pca_exp(sim_multivariado)
  ref <- prcomp(sim_multivariado[, 3:8], scale. = TRUE)
  expect_equal(result_table(p, "eigenvalues")$eigenvalue, ref$sdev^2)
  h <- cluster_exp(sim_multivariado, label = "genotipo", method = "average", k = 3)
  ref_h <- hclust(dist(scale(sim_multivariado[, 3:8])), "average")
  expect_equal(h$model$height, ref_h$height)
  expect_error(cluster_exp(sim_multivariado, distance = "manhattan", method = "ward.D2"), "euclidianas")
  km <- kmeans_exp(sim_multivariado, k = 3, seed = 1)
  km2 <- kmeans_exp(sim_multivariado, k = 3, seed = 1)
  expect_identical(result_table(km, "clusters"), result_table(km2, "clusters"))
  cm <- cor_matrix(sim_multivariado, c("altura", "ciclo"))
  expect_equal(result_table(cm)$r, cor(sim_multivariado$altura, sim_multivariado$ciclo))
  expect_error(pca_exp(data.frame(a = 1:5, b = 1)), "variabilidade")
})
