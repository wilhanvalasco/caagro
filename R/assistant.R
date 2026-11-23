#' Analysis assistant: transparent rules to suggest candidate methods
#'
#' A rule-based (non-AI) guide. It does **not** choose a method from a
#' normality test; it lists candidate methods compatible with the objective,
#' the type of response and the experimental structure, with assumptions and
#' alternatives, so the decision remains with the researcher.
#'
#' @param goal `"compare"` (compare treatments), `"curve"` (fit dose/time
#'   response), `"relationship"` (association between variables),
#'   `"explore"` or `"multivariate"`.
#' @param response `"continuous"`, `"binary"`, `"count"` or `"ordinal"`.
#' @param n_groups Number of groups/treatments (for `"compare"`).
#' @param related `TRUE` if observations are paired/related (same unit
#'   measured more than once).
#' @param blocked `TRUE` if there is experimental blocking.
#' @param factorial `TRUE` if there are two or more crossed factors.
#' @return A data frame of class `caagro_suggestion` with columns
#'   `method`, `function_call`, `app`, `assumptions`, `when` and
#'   `priority` (1 = usual first choice).
#' @examples
#' suggest_method("compare", "continuous", n_groups = 5, blocked = TRUE)
#' suggest_method("curve", "binary")
#' @noRd
suggest_method <- function(goal = c("compare", "curve", "relationship", "explore", "multivariate"),
                           response = c("continuous", "binary", "count", "ordinal"),
                           n_groups = 2, related = FALSE, blocked = FALSE, factorial = FALSE) {
  goal <- match.arg(goal); response <- match.arg(response)
  rows <- list()
  add <- function(method, fun, app, assumptions, when, priority) rows[[length(rows) + 1]] <<-
    data.frame(method = method, function_call = fun, app = app, assumptions = assumptions, when = when, priority = priority)
  if (goal == "explore") {
    add("Estat\u00edstica descritiva e gr\u00e1ficos", "describe_data()", "run_exploratory()", "Nenhum (descritivo).", "Primeiro contato com os dados; verificar erros, ausentes e distribui\u00e7\u00e3o.", 1)
  } else if (goal == "multivariate") {
    add("Componentes principais (PCA)", "pca_exp()", "run_multivariate()", "Vari\u00e1veis quantitativas; rela\u00e7\u00f5es aproximadamente lineares.", "Resumir muitas vari\u00e1veis correlacionadas.", 1)
    add("Agrupamento hier\u00e1rquico", "cluster_exp()", "run_multivariate()", "Escolha de dist\u00e2ncia e padroniza\u00e7\u00e3o.", "Identificar grupos de indiv\u00edduos semelhantes.", 2)
    add("K-means", "cluster_exp(method = 'kmeans')", "run_multivariate()", "Grupos aproximadamente esf\u00e9ricos; k definido.", "Particionar indiv\u00edduos em k grupos.", 3)
  } else if (goal == "relationship") {
    add("Correla\u00e7\u00e3o de Pearson", "cor_matrix(method = 'pearson')", "run_multivariate()", "Rela\u00e7\u00e3o linear; aus\u00eancia de valores extremos influentes.", "Duas vari\u00e1veis continuas.", if (response == "continuous") 1 else 3)
    add("Correla\u00e7\u00e3o de Spearman / Kendall", "cor_matrix(method = 'spearman')", "run_multivariate()", "Rela\u00e7\u00e3o monot\u00f4nica.", "Dados ordinais, n\u00e3o lineares monot\u00f4nicos ou com outliers.", if (response == "continuous") 2 else 1)
  } else if (goal == "curve") {
    if (response == "binary") {
      add("Regress\u00e3o binomial (logit)", "fit_regression(model = 'binomial' ou 'binomial_logdose', trials = ...)", "run_regression()",
          "Respostas independentes; aus\u00eancia de superdispers\u00e3o.", "Propor\u00e7\u00e3o de sucessos (germina\u00e7\u00e3o, mortalidade) em fun\u00e7\u00e3o da dose.", 1)
    } else {
      add("Regress\u00e3o polinomial (linear/quadr\u00e1tica)", "fit_regression(model = c('linear','quadratic'))", "run_regression()",
          "Normalidade e homogeneidade dos res\u00edduos; forma adequada na faixa estudada.", "Resposta a doses com poucos n\u00edveis; obten\u00e7\u00e3o de MET.", 1)
      add("Modelos n\u00e3o lineares (log\u00edstico, Mitscherlich, plat\u00f4...)", "fit_regression(model = c('logistic3','mitscherlich','linear_plateau'))", "run_regression()",
          "Bons valores iniciais; converg\u00eancia; par\u00e2metros com significado biol\u00f3gico.", "Crescimento, satura\u00e7\u00e3o, plat\u00f4, dose-resposta.", 2)
      if (response == "count") add("Aten\u00e7\u00e3o: contagens", "-", "-", "Contagens podem exigir modelo de Poisson/binomial negativa (n\u00e3o implementado).", "Verifique a vari\u00e2ncia crescente com a m\u00e9dia.", 3)
    }
  } else {
    if (response == "binary") {
      add("Compara\u00e7\u00e3o de propor\u00e7\u00f5es / regress\u00e3o binomial", "conf_int(parameter = 'proportion'); fit_regression(model = 'binomial')", "run_confidence()",
          "Ensaios independentes.", "Resposta 0/1 ou sucessos/ensaios.", 1)
    } else if (n_groups == 2 && !blocked && !factorial) {
      if (related) {
        add("Teste t pareado", "mean_test(type = 'paired')", "run_anova()", "Diferen\u00e7as aproximadamente normais.", "Mesma unidade medida duas vezes.", if (response == "continuous") 1 else 2)
        add("Wilcoxon pareado / teste dos sinais", "np_test(test = 'wilcoxon_paired')", "run_nonparametric()", "Diferen\u00e7as sim\u00e9tricas (Wilcoxon).", "Dados ordinais ou com valores extremos.", if (response == "continuous") 2 else 1)
      } else {
        add("Teste t de Welch", "mean_test(type = 'welch')", "run_anova()", "Normalidade aproximada em cada grupo; n\u00e3o exige vari\u00e2ncias iguais.", "Comparar duas m\u00e9dias independentes.", if (response == "continuous") 1 else 2)
        add("Mann-Whitney", "np_test(test = 'mann_whitney')", "run_nonparametric()", "Mesma forma das distribui\u00e7\u00f5es para interpretar como deslocamento.", "Dados ordinais ou fortemente assim\u00e9tricos.", if (response == "continuous") 2 else 1)
      }
    } else {
      design <- if (factorial) "fatorial" else if (blocked) "DBC" else "DIC"
      add(paste0("ANOVA (", design, ") + compara\u00e7\u00f5es m\u00faltiplas"),
          paste0("anova_exp(", if (blocked) "block = ..., " else "", if (factorial) "factors = ..., " else "", "...) |> compare_means('tukey')"),
          "run_anova()", "Independ\u00eancia (casualiza\u00e7\u00e3o), normalidade e homogeneidade dos res\u00edduos; aditividade bloco x tratamento no DBC.",
          "Comparar m\u00e9dias de tratamentos qualitativos. Se os tratamentos forem doses, prefira regress\u00e3o.", if (response == "continuous") 1 else 3)
      if (!blocked && !factorial) {
        add("ANOVA de Welch + Games-Howell", "anova_exp(method = 'welch'); compare_means('games_howell')", "run_anova()", "Normalidade; vari\u00e2ncias podem diferir.", "Vari\u00e2ncias claramente heterog\u00eaneas em DIC.", 2)
        add("Kruskal-Wallis + Dunn", "np_test(test = 'kruskal') |> np_posthoc('dunn')", "run_nonparametric()", "Amostras independentes; mesma forma para interpretar medianas.", "Resposta ordinal ou com forte assimetria.", if (response == "continuous") 3 else 1)
      }
      if (blocked && !factorial) add("Friedman + Conover", "np_test(test = 'friedman', block = ...) |> np_posthoc('conover')", "run_nonparametric()", "Um valor por tratamento em cada bloco completo.", "Resposta ordinal em blocos.", if (response == "continuous") 3 else 1)
    }
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$priority), ]; rownames(out) <- NULL
  attr(out, "note") <- "Sugest\u00f5es baseadas em regras transparentes. A escolha final depende do planejamento experimental e do conhecimento da \u00e1rea; n\u00e3o use apenas testes de normalidade para decidir."
  class(out) <- c("caagro_suggestion", "data.frame")
  out
}

#' @export
print.caagro_suggestion <- function(x, ...) {
  cat("M\u00e9todos candidatos (ordem de prioridade usual):\n\n")
  for (i in seq_len(nrow(x))) {
    cat(sprintf("%d. %s\n   Fun\u00e7\u00e3o: %s | App: %s\n   Pressupostos: %s\n   Quando: %s\n\n", i, x$method[i], x$function_call[i], x$app[i], x$assumptions[i], x$when[i]))
  }
  cat(attr(x, "note"), "\n")
  invisible(x)
}
