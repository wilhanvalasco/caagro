#' caagro: Applied Science for Agricultural Data Analysis
#'
#' `caagro` (Ciencia Aplicada em Analises do Agro) is a statistical analysis
#' platform for agricultural experimentation. Every analysis is available both
#' as a plain R function (scriptable and testable) and through Shiny
#' applications that call exactly the same functions.
#'
#' @section Main entry points:
#' * Applications: [run_app()], [run_anova()], [run_regression()],
#'   [run_nonparametric()], [run_confidence()], [run_multivariate()],
#'   [run_exploratory()]. Legacy launchers [anQuali()], [anQuant()] and
#'   [anIC()] are kept.
#' * Data: [read_data()], [check_data()], [flag_outliers()], [to_long()].
#' * Parametric: [anova_exp()], [compare_means()], [check_assumptions()],
#'   [mean_test()], [welch_anova()], [contrast_test()].
#' * Regression: [fit_regression()], [compare_curves()], [optimum_dose()].
#' * Non-parametric: [np_test()], [np_posthoc()].
#' * Confidence intervals: [conf_int()], [conf_int_groups()], [conf_int_boot()].
#' * Multivariate: [pca_exp()], [cluster_exp()], [kmeans_exp()],
#'   [cor_matrix()].
#' * Exploratory: [describe_data()].
#' * Graphics: [caagro_style()], [theme_caagro()], [save_plot()].
#' * Export: [export_excel()], [export_report()], [export_script()].
#' * Guidance: [suggest_method()].
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats aggregate anova as.formula coef complete.cases cor
#' @importFrom stats cor.test df.residual fitted lm median model.matrix na.omit
#' @importFrom stats nls pchisq pf pnorm pt ptukey qchisq qf qnorm qt qtukey
#' @importFrom stats quantile residuals sd setNames terms var vcov predict
#' @importFrom stats glm binomial logLik AIC BIC deviance nobs p.adjust
#' @importFrom stats rstandard rstudent cooks.distance hatvalues shapiro.test
#' @importFrom stats bartlett.test fligner.test kruskal.test friedman.test
#' @importFrom stats wilcox.test t.test binom.test chisq.test fisher.test
#' @importFrom stats prcomp dist hclust cutree kmeans cophenetic reorder
#' @importFrom stats oneway.test delete.response model.frame uniroot
#' @importFrom stats contrasts<- contr.sum drop1 update reformulate optimize
#' @importFrom stats pbinom qbinom family plogis qlogis
#' @importFrom utils head packageVersion sessionInfo
#' @importFrom rlang .data
## usethis namespace: end
NULL

utils::globalVariables(c(
  ".x", ".y", ".group", ".lower", ".upper", ".fit", ".resid", ".std_resid",
  ".cooks", ".obs", "estimate", "lower", "upper", "level", "group", "letters",
  "mean", "se", "n", "term", "x", "y", "fit", "PC", "variance", "cumulative",
  "k", "value", "Var1", "Var2", "label", "from", "to", "contrast", "p_adj",
  "height", "cluster", "theoretical", "sample", "sil_width", "within_ss",
  "median", "q1", "q3", "contribution", "variable", "xend", "yend", "model"
))
