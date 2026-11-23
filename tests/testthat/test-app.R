# Usability flow of every module: load example -> fill fields -> run -> result

run_example <- function(server, module, key, extra = list()) {
  ex <- caagro:::app_examples(module)[[key]]
  out <- NULL
  shiny::testServer(server, {
    session$setInputs(`data-example_id` = key, `data-load_example` = 1)
    expect_false(is.null(dat$data()))
    expect_identical(dat$example()$title, ex$title)
    vals <- c(ex$vars, ex$inputs, extra)
    vals <- vals[!vapply(vals, function(v) length(v) == 0, logical(1))]
    do.call(session$setInputs, vals)
    session$setInputs(run = 1)
    out <<- res()
  })
  out
}

test_that("ANOVA examples run: DIC (Tukey), DBC (Scott-Knott), factorial (sliced)", {
  r <- run_example(caagro:::mod_anova_server, "anova", "soja", list(kind = "anova", ss_type = "I"))
  expect_s3_class(r$anova, "caagro_anova")
  expect_equal(r$anova$design, "DIC")
  expect_equal(r$mc$settings$method, "tukey")
  expect_equal(r$diag$tables$tests$test, c("Shapiro-Wilk", "Bartlett"))
  r2 <- run_example(caagro:::mod_anova_server, "anova", "dbc", list(kind = "anova", ss_type = "I"))
  expect_equal(r2$anova$design, "DBC")
  expect_equal(r2$mc$settings$method, "scott_knott")
  r3 <- run_example(caagro:::mod_anova_server, "anova", "fatorial", list(kind = "anova", ss_type = "I"))
  expect_equal(r3$anova$design, "factorial")
  expect_equal(r3$sliced, "adubacao")            # interaction significant -> comparisons within levels
  expect_equal(r3$mc$settings$by, "adubacao")
})

test_that("other module examples run", {
  r <- run_example(caagro:::mod_regression_server, "regression", "dose",
                   list(conf = "0.95", band_method = "delta", nboot = 100, seed = 1, aggregate = "none", price = NA))
  expect_s3_class(r$reg, "caagro_regression")
  expect_equal(r$reg$tables$comparison$model[1], "quadratic")
  rb <- run_example(caagro:::mod_regression_server, "regression", "germ",
                    list(conf = "0.95", band_method = "delta", nboot = 100, seed = 1, aggregate = "none", price = NA))
  expect_true(all(result_table(rb$opt)$within_range))
  np <- run_example(caagro:::mod_nonparametric_server, "nonparametric", "notas", list(alpha = "0.05", R = 999, seed = 1))
  expect_true(!is.null(np$tables$pairs))
  expect_lt(result_table(np, "test")$p, 0.05)
  ci <- run_example(caagro:::mod_confidence_server, "confidence", "plant",
                    list(conf = "0.95", simultaneous = "none", prop_method = "wilson", boot = FALSE, R = 500, seed = 1))
  expect_s3_class(ci, "caagro_ci_groups")
  mv <- run_example(caagro:::mod_multivariate_server, "multivariate", "gen",
                    list(variables = c("altura", "diam_colmo", "massa_100", "n_fileiras", "ciclo", "produtividade"),
                         scale = TRUE, k = 3, distance = "euclidean", linkage = "ward.D2", cor_method = "pearson",
                         p_adjust = "holm", nstart = 10, seed = 1))
  expect_s3_class(mv, "caagro_pca")
  shiny::testServer(caagro:::mod_exploratory_server, {
    session$setInputs(`data-load_example` = 1)
    session$setInputs(variables = "altura", group = "cultivar")
    expect_equal(nrow(r()$tables$statistics), 3)
  })
})

test_that("run_app() is the only entry point and opens every module", {
  expect_identical(getNamespaceExports("caagro"), "run_app")
  for (m in c("todos", "anova", "regressao", "nao_parametrica", "intervalos", "multivariada", "exploratoria")) {
    expect_s3_class(run_app(m, launch.browser = "auto"), "shiny.appobj")
  }
  expect_error(run_app("xyz"))
})

test_that("interface tables use Portuguese headers and decimal comma", {
  tb <- caagro:::pt_names(caagro:::pt_values(result_table(anova_exp(sim_soja, "produtividade", "tratamento"), "anova")))
  expect_true(all(c("Fonte de variação", "GL", "SQ", "QM", "F", "Valor-p") %in% names(tb)))
  expect_true("Resíduo" %in% tb[[1]])
  expect_equal(caagro:::fmt_br(1234.5, 1), "1.234,5")
  expect_equal(caagro:::fmt_p_br(0.0004), "< 0,001")
})
