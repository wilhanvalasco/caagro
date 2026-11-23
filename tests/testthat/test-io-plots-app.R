test_that("read_data handles csv/txt/xlsx", {
  d1 <- read_data(system.file("extdata", "dose_resposta.csv", package = "caagro"))
  expect_true(is.numeric(d1$produtividade))
  d2 <- read_data(system.file("extdata", "dbc.txt", package = "caagro"))
  expect_equal(nrow(d2), 24)
  d3 <- read_data(system.file("extdata", "severidade.xlsx", package = "caagro"))
  expect_equal(ncol(d3), 4)
  expect_error(read_data("nao_existe.csv"), "n\u00e3o encontrado")
  w <- to_long(data.frame(t = c("A", "B"), R1 = 1:2, R2 = 3:4), c("R1", "R2"), "rep", "y")
  expect_equal(nrow(w), 4)
})

test_that("check_data flags problems", {
  chk <- check_data(sim_dbc, "produtividade", "tratamento", design = "DIC", block = "bloco")
  expect_true(attr(chk, "ok"))
  chk2 <- check_data(data.frame(t = c("a", "b", "c"), y = c(1, 2, 3)), "y", "t")
  expect_false(attr(chk2, "ok"))
})

test_that("all plot types render and styles round-trip", {
  fit <- anova_exp(PlantGrowth, "weight", "group")
  objs <- list(fit, compare_means(fit), check_assumptions(fit), conf_int(PlantGrowth, "weight", "group_means", group = "group"),
               fit_regression(sim_dose, "dose", "produtividade", c("quadratic", "linear_plateau"), group = "cultivar"),
               np_posthoc(np_test(PlantGrowth, "weight", "group"), "dunn"), pca_exp(sim_multivariado, group = "grupo"),
               cluster_exp(sim_multivariado, k = 3), cluster_exp(sim_multivariado, method = "kmeans", k = 3), cor_matrix(sim_multivariado),
               describe_data(PlantGrowth, "weight", "group"))
  for (o in objs) for (tp in caagro:::plot_types(o)) {
    p <- plot(o, type = tp)
    expect_s3_class(p, "ggplot")
    expect_silent(ggplot2::ggplot_build(p))
  }
  st <- caagro_style(theme = "bw", y = axis_spec(min = 0, max = 10, unit = "g"), legend = legend_spec(labels = c(ctrl = "Controle")))
  st2 <- style_from_json(style_to_json(st))
  expect_equal(st2$y$max, 10); expect_equal(st2$theme, "bw")
  expect_match(caagro:::style_code(st), "axis_spec")
})

test_that("exports produce files with the right formats", {
  fit <- anova_exp(PlantGrowth, "weight", "group")
  p <- plot(compare_means(fit))
  for (ext in c("png", "tiff", "svg", "pdf")) {
    f <- tempfile(fileext = paste0(".", ext))
    save_plot(p, f, width = 10, height = 8, dpi = 150)
    expect_true(file.exists(f) && file.size(f) > 1000)
  }
  f <- tempfile(fileext = ".png"); save_plot(p, f, width = 4, height = 2, units = "in", dpi = 300)
  if (requireNamespace("png", quietly = TRUE)) expect_equal(dim(png::readPNG(f))[2:1], c(1200, 600))
  x <- tempfile(fileext = ".xlsx"); export_excel(list(fit, compare_means(fit)), x)
  sh <- readxl::excel_sheets(x)
  expect_true(all(c("Dados originais", "Metodologia e par\u00e2metros") %in% sh))
  h <- tempfile(fileext = ".html"); export_report(list(fit, check_assumptions(fit)), h)
  expect_true(any(grepl("Reprodutibilidade", readLines(h, warn = FALSE))))
  s <- tempfile(fileext = ".R"); export_script(compare_means(fit), s)
  code <- readLines(s)
  expect_true(any(grepl("compare_means", code)))
  # the exported code reproduces the result
  dados <- PlantGrowth
  res <- eval(parse(text = compare_means(fit)$code))
  expect_equal(result_table(res, "pairs")$p_adj, result_table(compare_means(fit), "pairs")$p_adj)
  s2 <- tempfile(fileext = ".R"); export_script(fit, s2, config = TRUE); j <- sub("[.]R$", ".json", s2)
  expect_true(jsonlite::validate(paste(readLines(j), collapse = "")))
})

test_that("assistant suggests appropriate methods", {
  s <- suggest_method("compare", "continuous", 5, blocked = TRUE)
  expect_match(s$method[1], "DBC")
  expect_match(suggest_method("curve", "binary")$method[1], "binomial")
})


test_that("plots have a transparent background", {
  p <- plot(compare_means(anova_exp(sim_soja, "produtividade", "tratamento")))
  th <- ggplot2::theme_get(); b <- ggplot2::ggplot_build(p)$plot$theme
  expect_equal(b$plot.background$fill, "transparent")
  expect_equal(b$panel.background$fill, "transparent")
  f <- tempfile(fileext = ".png"); save_plot(p, f, 8, 6, dpi = 100, bg = "transparent")
  if (requireNamespace("png", quietly = TRUE)) expect_equal(dim(png::readPNG(f))[3], 4)
})
