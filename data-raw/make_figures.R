# Regenerates the README figures with the package itself.
# Run from the package root after installing caagro: Rscript data-raw/make_figures.R
# App screenshots (man/figures/app-*.jpg) are real captures of run_app().
library(caagro)
fd <- "man/figures"
fit <- anova_exp(sim_dbc, "produtividade", "tratamento", block = "bloco")
save_plot(plot(compare_means(fit, "scott_knott"), "bars",
               style = caagro_style(y = axis_spec(title = "Produtividade", unit = "t/ha"), x = axis_spec(title = "Tratamento"))),
          file.path(fd, "fig-anova.png"), 16, 11, dpi = 160)
r <- fit_regression(sim_dose, "dose", "produtividade", "quadratic", group = "cultivar")
save_plot(plot(r, style = caagro_style(geoms = geom_spec(show_optimum = TRUE), x = axis_spec(title = "Dose de N", unit = "kg/ha"),
                                       y = axis_spec(title = "Produtividade", unit = "t/ha"))), file.path(fd, "fig-regression.png"), 16, 11, dpi = 160)
b <- fit_regression(subset(sim_germinacao, dose > 0), "dose", "germinadas", "binomial_logdose", trials = "total", group = "produto")
save_plot(plot(b, style = caagro_style(x = axis_spec(trans = "log10", title = "Dose"), y = axis_spec(title = "Proporcao de germinacao"))),
          file.path(fd, "fig-binomial.png"), 16, 11, dpi = 160)
save_plot(plot(np_posthoc(np_test(PlantGrowth, "weight", "group"), "dunn"), "dots"), file.path(fd, "fig-np.png"), 16, 11, dpi = 160)
save_plot(plot(pca_exp(sim_multivariado, group = "grupo", label = "genotipo"), "biplot"), file.path(fd, "fig-pca.png"), 16, 12, dpi = 160)
save_plot(plot(cluster_exp(sim_multivariado, label = "genotipo", k = 3)), file.path(fd, "fig-dendro.png"), 18, 11, dpi = 160)
save_plot(plot(check_assumptions(fit), "qq"), file.path(fd, "fig-qq.png"), 14, 10, dpi = 160)
save_plot(plot(conf_int_groups(PlantGrowth, "weight", "group"), "forest"), file.path(fd, "fig-ci.png"), 16, 10, dpi = 160)
