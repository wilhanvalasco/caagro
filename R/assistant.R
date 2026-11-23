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
#' @export
suggest_method <- function(goal = c("compare", "curve", "relationship", "explore", "multivariate"),
                           response = c("continuous", "binary", "count", "ordinal"),
                           n_groups = 2, related = FALSE, blocked = FALSE, factorial = FALSE) {
  goal <- match.arg(goal); response <- match.arg(response)
  rows <- list()
  add <- function(method, fun, app, assumptions, when, priority) rows[[length(rows) + 1]] <<-
    data.frame(method = method, function_call = fun, app = app, assumptions = assumptions, when = when, priority = priority)
  if (goal == "explore") {
    add("Estatistica descritiva e graficos", "describe_data()", "run_exploratory()", "Nenhum (descritivo).", "Primeiro contato com os dados; verificar erros, ausentes e distribuicao.", 1)
  } else if (goal == "multivariate") {
    add("Componentes principais (PCA)", "pca_exp()", "run_multivariate()", "Variaveis quantitativas; relacoes aproximadamente lineares.", "Resumir muitas variaveis correlacionadas.", 1)
    add("Agrupamento hierarquico", "cluster_exp()", "run_multivariate()", "Escolha de distancia e padronizacao.", "Identificar grupos de individuos semelhantes.", 2)
    add("K-means", "kmeans_exp()", "run_multivariate()", "Grupos aproximadamente esfericos; k definido.", "Particionar individuos em k grupos.", 3)
  } else if (goal == "relationship") {
    add("Correlacao de Pearson", "cor_matrix(method = 'pearson')", "run_multivariate()", "Relacao linear; ausencia de valores extremos influentes.", "Duas variaveis continuas.", if (response == "continuous") 1 else 3)
    add("Correlacao de Spearman / Kendall", "cor_matrix(method = 'spearman')", "run_multivariate()", "Relacao monotonica.", "Dados ordinais, nao lineares monotonicos ou com outliers.", if (response == "continuous") 2 else 1)
  } else if (goal == "curve") {
    if (response == "binary") {
      add("Regressao binomial (logit)", "fit_regression(model = 'binomial' ou 'binomial_logdose', trials = ...)", "run_regression()",
          "Respostas independentes; ausencia de superdispersao.", "Proporcao de sucessos (germinacao, mortalidade) em funcao da dose.", 1)
    } else {
      add("Regressao polinomial (linear/quadratica)", "fit_regression(model = c('linear','quadratic'))", "run_regression()",
          "Normalidade e homogeneidade dos residuos; forma adequada na faixa estudada.", "Resposta a doses com poucos niveis; obtencao de MET.", 1)
      add("Modelos nao lineares (logistico, Mitscherlich, plato...)", "fit_regression(model = c('logistic3','mitscherlich','linear_plateau'))", "run_regression()",
          "Bons valores iniciais; convergencia; parametros com significado biologico.", "Crescimento, saturacao, plato, dose-resposta.", 2)
      if (response == "count") add("Atencao: contagens", "-", "-", "Contagens podem exigir modelo de Poisson/binomial negativa (nao implementado).", "Verifique a variancia crescente com a media.", 3)
    }
  } else {
    if (response == "binary") {
      add("Comparacao de proporcoes / regressao binomial", "conf_int(parameter = 'proportion'); fit_regression(model = 'binomial')", "run_confidence()",
          "Ensaios independentes.", "Resposta 0/1 ou sucessos/ensaios.", 1)
    } else if (n_groups == 2 && !blocked && !factorial) {
      if (related) {
        add("Teste t pareado", "mean_test(type = 'paired')", "run_anova()", "Diferencas aproximadamente normais.", "Mesma unidade medida duas vezes.", if (response == "continuous") 1 else 2)
        add("Wilcoxon pareado / teste dos sinais", "np_test(test = 'wilcoxon_paired')", "run_nonparametric()", "Diferencas simetricas (Wilcoxon).", "Dados ordinais ou com valores extremos.", if (response == "continuous") 2 else 1)
      } else {
        add("Teste t de Welch", "mean_test(type = 'welch')", "run_anova()", "Normalidade aproximada em cada grupo; nao exige variancias iguais.", "Comparar duas medias independentes.", if (response == "continuous") 1 else 2)
        add("Mann-Whitney", "np_test(test = 'mann_whitney')", "run_nonparametric()", "Mesma forma das distribuicoes para interpretar como deslocamento.", "Dados ordinais ou fortemente assimetricos.", if (response == "continuous") 2 else 1)
      }
    } else {
      design <- if (factorial) "fatorial" else if (blocked) "DBC" else "DIC"
      add(paste0("ANOVA (", design, ") + comparacoes multiplas"),
          paste0("anova_exp(", if (blocked) "block = ..., " else "", if (factorial) "factors = ..., " else "", "...) |> compare_means('tukey')"),
          "run_anova()", "Independencia (casualizacao), normalidade e homogeneidade dos residuos; aditividade bloco x tratamento no DBC.",
          "Comparar medias de tratamentos qualitativos. Se os tratamentos forem doses, prefira regressao.", if (response == "continuous") 1 else 3)
      if (!blocked && !factorial) {
        add("ANOVA de Welch + Games-Howell", "welch_anova(); compare_means('games_howell')", "run_anova()", "Normalidade; variancias podem diferir.", "Variancias claramente heterogeneas em DIC.", 2)
        add("Kruskal-Wallis + Dunn", "np_test(test = 'kruskal') |> np_posthoc('dunn')", "run_nonparametric()", "Amostras independentes; mesma forma para interpretar medianas.", "Resposta ordinal ou com forte assimetria.", if (response == "continuous") 3 else 1)
      }
      if (blocked && !factorial) add("Friedman + Conover", "np_test(test = 'friedman', block = ...) |> np_posthoc('conover')", "run_nonparametric()", "Um valor por tratamento em cada bloco completo.", "Resposta ordinal em blocos.", if (response == "continuous") 3 else 1)
    }
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$priority), ]; rownames(out) <- NULL
  attr(out, "note") <- "Sugestoes baseadas em regras transparentes. A escolha final depende do planejamento experimental e do conhecimento da area; nao use apenas testes de normalidade para decidir."
  class(out) <- c("caagro_suggestion", "data.frame")
  out
}

#' @export
print.caagro_suggestion <- function(x, ...) {
  cat("Metodos candidatos (ordem de prioridade usual):\n\n")
  for (i in seq_len(nrow(x))) {
    cat(sprintf("%d. %s\n   Funcao: %s | App: %s\n   Pressupostos: %s\n   Quando: %s\n\n", i, x$method[i], x$function_call[i], x$app[i], x$assumptions[i], x$when[i]))
  }
  cat(attr(x, "note"), "\n")
  invisible(x)
}
