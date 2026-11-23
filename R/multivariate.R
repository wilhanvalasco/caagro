mv_matrix <- function(data, variables, label = NULL, group = NULL) {
  data <- check_data_frame(data)
  if (is.null(variables)) variables <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], c(label, group))
  check_columns(data, c(variables, label, group))
  if (length(variables) < 2) abort_caagro("Selecione ao menos 2 variaveis quantitativas.")
  X <- as.data.frame(lapply(data[, variables, drop = FALSE], as_numeric_strict))
  names(X) <- variables
  ok <- stats::complete.cases(X)
  msg <- if (any(!ok)) sprintf("%d linha(s) com valores ausentes removida(s) da analise multivariada (linhas %s).", sum(!ok), paste(utils::head(which(!ok), 20), collapse = ", ")) else character()
  sds <- vapply(X[ok, , drop = FALSE], stats::sd, numeric(1))
  if (any(sds == 0)) abort_caagro("Variavel(is) sem variabilidade: ", paste(variables[sds == 0], collapse = ", "), ".")
  lab <- if (is.null(label)) as.character(which(ok)) else make.unique(as.character(data[[label]][ok]))
  M <- as.matrix(X[ok, , drop = FALSE]); rownames(M) <- lab
  list(M = M, ok = ok, msg = msg, variables = variables,
       group = if (!is.null(group)) factor(data[[group]][ok]) else NULL, labels = lab)
}

#' Principal component analysis
#'
#' PCA by singular value decomposition ([stats::prcomp()]). With
#' `scale = TRUE` the analysis is based on the correlation matrix (variables
#' in different units); with `scale = FALSE`, on the covariance matrix.
#'
#' @param data A data frame.
#' @param variables Quantitative columns (default: all numeric).
#' @param group Optional grouping column (for colouring/ellipses).
#' @param label Optional column with individual labels.
#' @param center,scale Centre and standardise the variables.
#' @return A `caagro_result` (class `caagro_pca`) with tables
#'   `eigenvalues`, `loadings`, `correlations` (variable-component
#'   correlations), `contributions` (percent) and `scores`.
#' @references Jolliffe, I. T. (2002). *Principal Component Analysis* (2nd
#'   ed.). Springer. \doi{10.1007/b98835}
#'
#' Jolliffe, I. T. and Cadima, J. (2016). Principal component analysis: a
#' review and recent developments. *Philosophical Transactions of the Royal
#' Society A*, 374(2065), 20150202. \doi{10.1098/rsta.2015.0202}
#' @examples
#' p <- pca_exp(sim_multivariado, group = "grupo", label = "genotipo")
#' result_table(p, "eigenvalues")
#' plot(p, "biplot")
#' @export
pca_exp <- function(data, variables = NULL, group = NULL, label = NULL, center = TRUE, scale = TRUE) {
  mv <- mv_matrix(data, variables, label, group)
  pc <- stats::prcomp(mv$M, center = center, scale. = scale)
  ev <- pc$sdev^2
  eig <- data.frame(PC = paste0("PC", seq_along(ev)), eigenvalue = ev, variance = 100 * ev / sum(ev),
                    cumulative = cumsum(100 * ev / sum(ev)))
  L <- pc$rotation
  load <- data.frame(variable = rownames(L), L, row.names = NULL, check.names = FALSE)
  Xs <- scale(mv$M, center = center, scale = scale)
  corr <- stats::cor(Xs, pc$x)
  if (!scale) corr <- stats::cor(mv$M, pc$x)
  cr <- data.frame(variable = rownames(corr), corr, row.names = NULL, check.names = FALSE)
  contrib <- data.frame(variable = rownames(L), 100 * L^2, row.names = NULL, check.names = FALSE)
  sc <- data.frame(label = mv$labels, pc$x, row.names = NULL, check.names = FALSE)
  if (!is.null(group)) sc$group <- mv$group
  warns <- mv$msg
  if (!scale) warns <- c(warns, "PCA sobre a matriz de covariancia: variaveis com maior variancia (ou unidades maiores) dominam os componentes.")
  if (nrow(mv$M) < 2 * ncol(mv$M)) warns <- c(warns, "Poucas observacoes em relacao ao numero de variaveis: autovalores e cargas podem ser instaveis.")
  kaiser <- sum(ev > 1)
  new_result("pca", paste0("Analise de componentes principais (", if (scale) "matriz de correlacao" else "matriz de covariancia", ")"),
             "caagro_pca", settings = list(variables = mv$variables, group = group, label = label, center = center, scale = scale),
             data = data, data_used = as.data.frame(mv$M), model = pc, warnings = warns,
             tables = list(eigenvalues = eig, loadings = load, correlations = cr, contributions = contrib, scores = sc),
             notes = list(Resultado = sprintf("PC1 e PC2 explicam %.1f%% e %.1f%% da variancia (acumulado %.1f%%).", eig$variance[1], eig$variance[2], eig$cumulative[2]),
                          Interpretacao = sprintf("Cargas (loadings) indicam o peso de cada variavel no componente; correlacoes variavel-componente mostram quanto cada variavel e representada. %d componente(s) com autovalor > 1 (criterio de Kaiser, apenas orientativo para matriz de correlacao).", kaiser),
                          Cuidados = "PCA descreve a estrutura de correlacao; nao testa hipoteses. O sinal dos componentes e arbitrario. Elipses por grupo sao descritivas."),
             code = code_call("caagro::pca_exp", list(data = as.name("dados"), variables = mv$variables, group = group,
                                                      label = label, center = center, scale = scale)))
}

#' Hierarchical cluster analysis
#'
#' @param data A data frame.
#' @param variables Quantitative columns.
#' @param label Column with labels of the individuals.
#' @param distance Distance: `"euclidean"`, `"manhattan"`, `"maximum"`,
#'   `"canberra"` or `"minkowski"`.
#' @param method Linkage: `"ward.D2"`, `"complete"`, `"average"` (UPGMA),
#'   `"single"`, `"mcquitty"`. Ward.D2 requires Euclidean distances.
#' @param scale Standardise variables before computing distances.
#' @param k Number of groups to cut (optional).
#' @param h Height at which to cut (optional, alternative to `k`).
#' @param group Optional known grouping column (to compare with clusters).
#' @return A `caagro_result` (class `caagro_cluster`) with tables
#'   `merge_heights`, `clusters` (if `k`/`h`) and `quality` (cophenetic
#'   correlation).
#' @references Ward, J. H. (1963). Hierarchical grouping to optimize an
#'   objective function. *Journal of the American Statistical Association*,
#'   58(301), 236-244. \doi{10.1080/01621459.1963.10500845}
#'
#' Murtagh, F. and Legendre, P. (2014). Ward's hierarchical agglomerative
#' clustering method: which algorithms implement Ward's criterion?
#' *Journal of Classification*, 31, 274-295. \doi{10.1007/s00357-014-9161-z}
#'
#' Sokal, R. R. and Rohlf, F. J. (1962). The comparison of dendrograms by
#' objective methods. *Taxon*, 11(2), 33-40. \doi{10.2307/1217208}
#' @examples
#' h <- cluster_exp(sim_multivariado, label = "genotipo", k = 3)
#' plot(h)
#' @export
cluster_exp <- function(data, variables = NULL, label = NULL, distance = "euclidean", method = "ward.D2",
                        scale = TRUE, k = NULL, h = NULL, group = NULL) {
  distance <- match.arg(distance, c("euclidean", "manhattan", "maximum", "canberra", "minkowski"))
  method <- match.arg(method, c("ward.D2", "complete", "average", "single", "mcquitty"))
  if (method == "ward.D2" && distance != "euclidean") {
    abort_caagro("O metodo de Ward (ward.D2) pressupoe distancias euclidianas. Use distance = 'euclidean' ou outro metodo de ligacao.")
  }
  mv <- mv_matrix(data, variables, label, group)
  M <- if (scale) scale(mv$M) else mv$M
  D <- stats::dist(M, method = distance)
  hc <- stats::hclust(D, method = method)
  coph <- stats::cor(D, stats::cophenetic(hc))
  tables <- list(quality = data.frame(distance = distance, method = method, scaled = scale, cophenetic_cor = coph, n = nrow(M)),
                 merge_heights = data.frame(step = seq_along(hc$height), height = hc$height))
  if (!is.null(k) || !is.null(h)) {
    cl <- stats::cutree(hc, k = k, h = h)
    tables$clusters <- data.frame(label = names(cl), cluster = cl, row.names = NULL)
    if (!is.null(group)) tables$clusters$group <- mv$group
  }
  warns <- mv$msg
  if (coph < 0.7) warns <- c(warns, sprintf("Correlacao cofenetica baixa (%.2f): o dendrograma representa mal as distancias originais.", coph))
  new_result("cluster", sprintf("Agrupamento hierarquico (%s, distancia %s)", method, distance), "caagro_cluster",
             settings = list(variables = mv$variables, label = label, distance = distance, method = method, scale = scale, k = k, h = h, group = group),
             data = data, data_used = as.data.frame(mv$M), model = hc, tables = tables, main = "quality", warnings = warns,
             notes = list(Resultado = sprintf("Correlacao cofenetica = %.3f.", coph),
                          Interpretacao = "Individuos unidos a menor altura sao mais semelhantes. O corte (k ou altura) define grupos; avalie a estabilidade com outros metodos.",
                          Cuidados = "O agrupamento sempre produz grupos, mesmo sem estrutura real. Resultados dependem da padronizacao, da distancia e do metodo de ligacao."),
             code = code_call("caagro::cluster_exp", list(data = as.name("dados"), variables = mv$variables, label = label,
                                                          distance = distance, method = method, scale = scale, k = k, h = h)))
}

#' K-means clustering
#'
#' @inheritParams cluster_exp
#' @param k Number of clusters.
#' @param nstart Number of random starts.
#' @param k_max Maximum k evaluated in the elbow and silhouette diagnostics.
#' @param seed Random seed (recorded).
#' @return A `caagro_result` (class `caagro_kmeans`) with tables
#'   `clusters`, `centers`, `summary` and `selection` (total within SS and
#'   mean silhouette width for k = 2..k_max).
#' @references
#' Hartigan, J. A. and Wong, M. A. (1979). Algorithm AS 136: A k-means
#' clustering algorithm. *Journal of the Royal Statistical Society C*,
#' 28(1), 100-108. \doi{10.2307/2346830}
#'
#' Rousseeuw, P. J. (1987). Silhouettes: a graphical aid to the
#' interpretation and validation of cluster analysis. *Journal of
#' Computational and Applied Mathematics*, 20, 53-65.
#' \doi{10.1016/0377-0427(87)90125-7}
#' @examples
#' km <- kmeans_exp(sim_multivariado, k = 3, label = "genotipo", seed = 1)
#' result_table(km, "selection")
#' @export
kmeans_exp <- function(data, variables = NULL, k = 3, label = NULL, scale = TRUE, nstart = 25, k_max = 8,
                       seed = 123, group = NULL) {
  mv <- mv_matrix(data, variables, label, group)
  M <- if (scale) scale(mv$M) else mv$M
  n <- nrow(M)
  if (k < 2 || k >= n) abort_caagro("k deve estar entre 2 e n - 1.")
  k_max <- min(k_max, n - 1)
  D <- stats::dist(M)
  run <- function(kk) stats::kmeans(M, centers = kk, nstart = nstart, iter.max = 100)
  km <- with_seed(seed, run(k))
  sel <- with_seed(seed, do.call(rbind, lapply(seq_len(k_max), function(kk) {
    if (kk == 1) return(data.frame(k = 1, within_ss = sum(scale(M, scale = FALSE)^2), silhouette = NA))
    fk <- run(kk)
    data.frame(k = kk, within_ss = fk$tot.withinss, silhouette = mean(cluster::silhouette(fk$cluster, D)[, 3]))
  })))
  sil <- cluster::silhouette(km$cluster, D)
  cl <- data.frame(label = mv$labels, cluster = km$cluster, silhouette = sil[, 3], row.names = NULL)
  if (!is.null(group)) cl$group <- mv$group
  centers <- data.frame(cluster = seq_len(k), km$centers, check.names = FALSE)
  summ <- data.frame(cluster = seq_len(k), size = km$size, within_ss = km$withinss,
                     mean_silhouette = as.vector(tapply(sil[, 3], km$cluster, mean)))
  new_result("kmeans", sprintf("K-means (k = %d)", k), "caagro_kmeans",
             settings = list(variables = mv$variables, k = k, label = label, scale = scale, nstart = nstart, seed = seed, group = group),
             data = data, data_used = as.data.frame(mv$M), model = km, seed = seed,
             tables = list(summary = summ, clusters = cl, centers = centers, selection = sel,
                           fit = data.frame(between_over_total = km$betweenss / km$totss, mean_silhouette = mean(sil[, 3]))),
             warnings = mv$msg,
             notes = list(Resultado = sprintf("SQ entre / SQ total = %.1f%%; silhueta media = %.2f.", 100 * km$betweenss / km$totss, mean(sil[, 3])),
                          Interpretacao = "Silhueta proxima de 1 indica individuos bem alocados; proxima de 0, na fronteira; negativa, possivelmente no grupo errado. O cotovelo e a silhueta orientam a escolha de k.",
                          Cuidados = "K-means pressupoe grupos aproximadamente esfericos e de tamanho semelhante; o resultado depende da semente e da padronizacao."),
             code = code_call("caagro::kmeans_exp", list(data = as.name("dados"), variables = mv$variables, k = k, label = label,
                                                         scale = scale, nstart = nstart, seed = seed)))
}

#' Correlation matrix with significance tests
#'
#' @param data A data frame.
#' @param variables Quantitative columns.
#' @param method `"pearson"`, `"spearman"` or `"kendall"`.
#' @param p_adjust Multiplicity correction over all pairs.
#' @param conf_level Confidence level (Pearson: Fisher z interval).
#' @return A `caagro_result` (class `caagro_cor`) with tables `pairs` and
#'   `matrix`.
#' @references
#' Pearson, K. (1895). Notes on regression and inheritance in the case of
#' two parents. *Proceedings of the Royal Society of London*, 58, 240-242.
#' \doi{10.1098/rspl.1895.0041}
#'
#' Spearman, C. (1904). The proof and measurement of association between
#' two things. *The American Journal of Psychology*, 15(1), 72-101.
#' \doi{10.2307/1412159}
#'
#' Kendall, M. G. (1938). A new measure of rank correlation. *Biometrika*,
#' 30(1/2), 81-93. \doi{10.2307/2332226}
#' @examples
#' cor_matrix(sim_multivariado, method = "spearman")
#' @export
cor_matrix <- function(data, variables = NULL, method = c("pearson", "spearman", "kendall"),
                       p_adjust = "holm", conf_level = 0.95) {
  method <- match.arg(method); valid_p_adjust(p_adjust)
  data <- check_data_frame(data)
  if (is.null(variables)) variables <- names(data)[vapply(data, is.numeric, logical(1))]
  check_columns(data, variables)
  X <- as.data.frame(lapply(data[, variables, drop = FALSE], as_numeric_strict)); names(X) <- variables
  ij <- pair_index(length(variables))
  rows <- lapply(seq_along(ij$i), function(r) {
    a <- X[[ij$i[r]]]; b <- X[[ij$j[r]]]; ok <- stats::complete.cases(a, b)
    ct <- suppressWarnings(stats::cor.test(a[ok], b[ok], method = method, conf.level = conf_level, exact = FALSE))
    data.frame(var1 = variables[ij$i[r]], var2 = variables[ij$j[r]], r = unname(ct$estimate), n = sum(ok),
               statistic = unname(ct$statistic), p = ct$p.value,
               lower = if (!is.null(ct$conf.int)) ct$conf.int[1] else NA, upper = if (!is.null(ct$conf.int)) ct$conf.int[2] else NA)
  })
  pr <- do.call(rbind, rows)
  pr$p_adj <- stats::p.adjust(pr$p, p_adjust)
  pr$signif <- signif_stars(pr$p_adj)
  R <- stats::cor(X, use = "pairwise.complete.obs", method = method)
  new_result("cor", paste("Correlacao de", c(pearson = "Pearson", spearman = "Spearman", kendall = "Kendall")[[method]]), "caagro_cor",
             settings = list(variables = variables, method = method, p_adjust = p_adjust, conf_level = conf_level),
             data = data, data_used = X, tables = list(pairs = pr, matrix = data.frame(variable = variables, R, row.names = NULL, check.names = FALSE)),
             warnings = if (anyNA(X)) "Valores ausentes tratados par a par (pairwise)." else character(),
             notes = list(Interpretacao = "Pearson mede associacao linear; Spearman e Kendall, associacao monotonica baseada em postos.",
                          Cuidados = "Correlacao nao implica causalidade. Valores de p ajustados para multiplas comparacoes."),
             code = code_call("caagro::cor_matrix", list(data = as.name("dados"), variables = variables, method = method, p_adjust = p_adjust)))
}
