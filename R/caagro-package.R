#' caagro: Applied Statistics for Agricultural Experimentation
#'
#' `caagro` (Ciencia Aplicada em Analises do Agro) is a statistical analysis
#' platform for agricultural experimentation used through a single function,
#' [run_app()], which opens a point-and-click 'shiny' application. Results can
#' be exported to Excel, to an HTML/Word report and to a reproducible R
#' script.
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
