finish_plot <- function(p, axes = c(x = "continuous", y = "continuous"), groups = NULL, defaults = list()) {
  attr(p, "caagro_axes") <- axes
  attr(p, "caagro_groups") <- groups
  attr(p, "caagro_defaults") <- defaults
  p
}

#' Plot types available for a caagro result
#' @param x A `caagro_result`.
#' @return Named character vector (labels in Portuguese, values = `type`).
#' @examples
#' plot_types(anova_exp(PlantGrowth, "weight", "group"))
#' @export
plot_types <- function(x) {
  cls <- class(x)[1]
  switch(cls,
    caagro_anova = c("Medias e IC" = "means", "Boxplot" = "box", "Residuos" = "residuals"),
    caagro_multcomp = c("Barras com letras" = "bars", "Pontos com letras" = "points", "Boxplot com letras" = "box", "Diferencas (IC)" = "differences"),
    caagro_diagnostics = c("Q-Q dos residuos" = "qq", "Residuos x ajustados" = "resid_fitted", "Escala-locacao" = "scale_location", "Distancia de Cook" = "cooks", "Histograma dos residuos" = "hist"),
    caagro_ci_groups = c("Intervalos (vertical)" = "pointrange", "Intervalos (horizontal)" = "forest", "Barras com IC" = "bars"),
    caagro_ci = c("Intervalo" = "pointrange", "Distribuicao bootstrap" = "bootstrap"),
    caagro_test = c("Distribuicao por grupo" = "box", "Estimativa e IC" = "estimate"),
    caagro_describe = c("Histograma" = "hist", "Boxplot" = "box", "Densidade" = "density", "Q-Q" = "qq", "Violino" = "violin"),
    caagro_regression = c("Curvas ajustadas" = "fit", "Painel por grupo" = "facet", "Residuos x ajustados" = "residuals", "Q-Q dos residuos" = "qq"),
    caagro_model_comparison = c("Curvas sobrepostas" = "overlay", "Criterio de informacao" = "ic"),
    caagro_np = c(c("Boxplot" = "box", "Violino" = "violin", "Pontos e mediana/IQR" = "dots"),
                  if (!is.null(x$tables$pairs)) c("Mapa de p (pares)" = "heatmap", "Estatistica das comparacoes" = "effect"),
                  if (!is.null(x$settings$block)) c("Perfis por bloco" = "blocks")),
    caagro_pca = c("Scree plot" = "scree", "Biplot" = "biplot", "Scores" = "scores", "Loadings" = "loadings", "Circulo de correlacoes" = "circle", "Contribuicoes" = "contrib"),
    caagro_cluster = c("Dendrograma" = "dendrogram"),
    caagro_kmeans = c("Grupos (PCs)" = "clusters", "Cotovelo" = "elbow", "Silhueta" = "silhouette"),
    caagro_cor = c("Mapa de calor" = "heatmap"),
    c("Grafico" = "default"))
}

#' Plot methods for caagro results
#'
#' Every plot method returns a ggplot object styled with [apply_style()].
#' Available `type`s are listed by [plot_types()].
#'
#' @param x A caagro result.
#' @param type Plot type (see [plot_types()]).
#' @param style A [caagro_style()].
#' @param ... Unused.
#' @return A ggplot object.
#' @name caagro_plots
#' @examples
#' fit <- anova_exp(PlantGrowth, "weight", "group")
#' plot(fit)
#' plot(compare_means(fit, "tukey"), type = "bars")
#' plot(check_assumptions(fit), type = "qq")
NULL

#' @rdname caagro_plots
#' @export
plot.caagro_anova <- function(x, type = c("means", "box", "residuals"), style = caagro_style(), ...) {
  type <- match.arg(type)
  if (type == "residuals") return(plot(check_assumptions(x), type = "resid_fitted", style = style))
  mc <- structure(list(tables = list(means = transform(x$tables$means, letras = "")), settings = c(x$settings, list(method = "none")),
                       data_used = x$data_used, method = x$method), class = c("caagro_multcomp", "caagro_result"))
  st <- style; st$geoms$show_letters <- FALSE
  plot.caagro_multcomp(mc, type = if (type == "box") "box" else "points", style = st)
}

#' @rdname caagro_plots
#' @export
plot.caagro_multcomp <- function(x, type = c("bars", "points", "box", "differences"), style = caagro_style(), ...) {
  type <- match.arg(type)
  g <- style$geoms
  fac <- x$settings$factor %||% x$settings$treatment
  resp <- x$settings$response
  by <- x$settings$by
  m <- x$tables$means
  m$.x <- factor(m[[fac]], levels = unique(m[[fac]]))
  d <- x$data_used
  d$.x <- factor(d[[fac]], levels = levels(m$.x))
  ttl <- list(title = resp, subtitle = x$method, x = x$settings$x_label %||% fac, y = resp, legend = by %||% fac)
  if (type == "differences") {
    pr <- x$tables$pairs
    if (all(is.na(pr$lower %||% NA))) {
      p <- ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0, label = "Este procedimento nao fornece intervalos para as diferencas.") + ggplot2::theme_void()
      return(p)
    }
    pr$contrast <- factor(pr$contrast, levels = rev(pr$contrast))
    p <- ggplot2::ggplot(pr, ggplot2::aes(y = .data$contrast, x = .data$diff)) +
      ggplot2::geom_vline(xintercept = 0, colour = "#64748B", linetype = "dashed", linewidth = 0.4) +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$lower, xmax = .data$upper), height = g$errorbar_width,
                              colour = "#315775", linewidth = g$line_width) +
      ggplot2::geom_point(ggplot2::aes(colour = .data$significant), size = g$point_size + 0.6) +
      ggplot2::scale_colour_manual(values = c(`TRUE` = "#F28C28", `FALSE` = "#315775"), labels = c(`TRUE` = "Significativa", `FALSE` = "Nao significativa"), name = NULL)
    if (!is.null(by)) p <- p + ggplot2::facet_wrap(stats::as.formula(paste0("~`", by, "`")), scales = "free_y")
    return(apply_style(finish_plot(p, c(x = "continuous", y = "discrete"),
                                   defaults = list(title = "Diferencas estimadas", subtitle = x$method, x = "Diferenca", y = "")), style))
  }
  colour_var <- if (!is.null(by)) by else ".x"
  m$.col <- as.character(m[[colour_var]]); d$.col <- as.character(d[[colour_var]])
  groups <- unique(as.character(m$.col))
  dodge <- ggplot2::position_dodge(width = if (is.null(by)) 0 else 0.75)
  ytop <- max(c(m$upper, if (g$show_points) d[[resp]]), na.rm = TRUE)
  yr <- diff(range(c(m$lower, m$upper, d[[resp]], 0), na.rm = TRUE))
  p <- ggplot2::ggplot(m, ggplot2::aes(x = .data$.x, y = .data$emmean, group = .data$.col))
  if (type == "bars") {
    p <- p + ggplot2::geom_col(ggplot2::aes(fill = .data$.col), width = g$bar_width, alpha = 0.9,
                               position = ggplot2::position_dodge(width = 0.75))
    dodge <- ggplot2::position_dodge(width = 0.75)
  }
  if (type == "box") {
    p <- p + ggplot2::geom_boxplot(data = d, ggplot2::aes(x = .data$.x, y = .data[[resp]], fill = .data$.col, group = interaction(.data$.x, .data$.col)),
                                   inherit.aes = FALSE, width = g$bar_width, alpha = 0.25, outlier.shape = NA, linewidth = 0.4,
                                   position = ggplot2::position_dodge(width = 0.75))
    dodge <- ggplot2::position_dodge(width = 0.75)
  }
  if (isTRUE(g$show_points) && type == "bars") {
    if (is.null(by)) {
      p <- p + ggplot2::geom_point(data = d, ggplot2::aes(x = .data$.x, y = .data[[resp]]), inherit.aes = FALSE,
                                   shape = 21, fill = "white", colour = "#14283F", stroke = 0.5, size = g$point_size * 0.7, alpha = 0.85,
                                   position = ggplot2::position_jitter(width = 0.07, height = 0, seed = 1))
    } else {
      p <- p + ggplot2::geom_point(data = d, ggplot2::aes(x = .data$.x, y = .data[[resp]], group = .data$.col, colour = .data$.col),
                                   inherit.aes = FALSE, shape = 21, fill = "white", stroke = 0.5, size = g$point_size * 0.7, alpha = 0.85,
                                   position = ggplot2::position_jitterdodge(jitter.width = 0.08, dodge.width = 0.75, seed = 1))
    }
  } else if (isTRUE(g$show_points)) {
    p <- p + ggplot2::geom_point(data = d, ggplot2::aes(x = .data$.x, y = .data[[resp]], fill = .data$.col, group = .data$.col),
                                 inherit.aes = FALSE, shape = 21, colour = "white", stroke = 0.25,
                                 size = g$point_size * 0.75, alpha = g$alpha,
                                 position = if (is.null(by)) ggplot2::position_jitter(width = 0.07, height = 0, seed = 1) else
                                   ggplot2::position_jitterdodge(jitter.width = 0.08, dodge.width = 0.75, seed = 1))
  }
  if (type != "box") {
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), width = g$errorbar_width,
                                    linewidth = g$line_width * 0.8, colour = if (type == "bars") "#14283F" else NA, position = dodge)
    if (type == "points") {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lower, ymax = .data$upper, colour = .data$.col),
                                      width = g$errorbar_width, linewidth = g$line_width, position = dodge) +
        ggplot2::geom_point(ggplot2::aes(fill = .data$.col), shape = g$point_shape, size = g$point_size + 1.2,
                            colour = "#14283F", stroke = 0.5, position = dodge)
    }
  } else {
    p <- p + ggplot2::geom_point(ggplot2::aes(fill = .data$.col), shape = 23, size = g$point_size + 0.8, colour = "#14283F", position = dodge)
  }
  top_raw <- tapply(d[[resp]], interaction(d$.x, d$.col, drop = TRUE), max)[as.character(interaction(m$.x, m$.col, drop = TRUE))]
  lab_y <- if (isTRUE(g$show_points) && type != "box") pmax(m$upper, top_raw, na.rm = TRUE) else if (type == "box") pmax(m$upper, tapply(d[[resp]], interaction(d$.x, d$.col, drop = TRUE), max)[as.character(interaction(m$.x, m$.col, drop = TRUE))], na.rm = TRUE) else m$upper
  m$.lab_y <- lab_y + 0.04 * yr
  lab <- character(nrow(m))
  if (isTRUE(g$show_values)) lab <- formatC(m$emmean, format = "f", digits = g$label_decimals)
  if (isTRUE(g$show_letters) && !all(is.na(m$letras) | m$letras == "")) lab <- trimws(paste(lab, m$letras))
  if (any(nzchar(lab))) {
    m$.lab <- lab
    p <- p + ggplot2::geom_text(data = m, ggplot2::aes(y = .data$.lab_y, label = .data$.lab), vjust = 0,
                                size = g$label_size, colour = "#14283F", fontface = "bold", position = dodge)
  }
  if (type == "bars") p <- p + ggplot2::expand_limits(y = 0)
  p <- p + ggplot2::expand_limits(y = max(m$.lab_y, na.rm = TRUE) + 0.06 * yr)
  ttl$caption <- paste0("Barras: IC de ", round(100 * (x$settings$conf_level %||% 0.95)), "% da media (erro do modelo).",
                        if (isTRUE(g$show_letters) && !all(m$letras == "")) " Medias com a mesma letra nao diferem." else "")
  p <- p + ggplot2::guides(colour = "none")
  if (is.null(by)) p <- p + ggplot2::guides(fill = "none")
  apply_style(finish_plot(p, c(x = "discrete", y = "continuous"), groups = groups, defaults = ttl), style)
}

#' @rdname caagro_plots
#' @export
plot.caagro_diagnostics <- function(x, type = c("qq", "resid_fitted", "scale_location", "cooks", "hist"), style = caagro_style(), ...) {
  type <- match.arg(type)
  inf <- x$tables$influence
  g <- style$geoms
  pal <- caagro_palette()
  inf$.flag <- ifelse(inf$flag, "Potencialmente influente", "Demais")
  base_pts <- function(p) p + ggplot2::geom_point(ggplot2::aes(fill = .data$.flag), shape = 21, colour = "white",
                                                  size = g$point_size, alpha = 0.9) +
    ggplot2::scale_fill_manual(values = c("Potencialmente influente" = pal[["laranja"]], "Demais" = pal[["azul_secundario"]]), name = NULL)
  if (type == "qq") {
    n <- nrow(inf)
    o <- order(inf$std_residual)
    qd <- data.frame(theoretical = stats::qnorm(stats::ppoints(n)), sample = inf$std_residual[o], .flag = inf$.flag[o])
    # pointwise 95% envelope (normal order statistics, approximate)
    se <- (1 / stats::dnorm(qd$theoretical)) * sqrt(stats::ppoints(n) * (1 - stats::ppoints(n)) / n)
    qd$lo <- qd$theoretical - 1.96 * se; qd$hi <- qd$theoretical + 1.96 * se
    p <- ggplot2::ggplot(qd, ggplot2::aes(.data$theoretical, .data$sample)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo, ymax = .data$hi), fill = pal[["verde_suave"]], alpha = g$band_alpha) +
      ggplot2::geom_abline(slope = 1, intercept = 0, colour = pal[["verde_cientifico"]], linewidth = g$line_width)
    p <- base_pts(p)
    def <- list(title = "Grafico Q-Q dos residuos", subtitle = "Residuos padronizados versus quantis da normal (faixa: envelope aproximado de 95%)",
                x = "Quantis teoricos", y = "Residuos padronizados")
  } else if (type == "resid_fitted") {
    p <- ggplot2::ggplot(inf, ggplot2::aes(.data$fitted, .data$residual)) +
      ggplot2::geom_hline(yintercept = 0, colour = pal[["cinza_tecnico"]], linetype = "dashed")
    p <- base_pts(p)
    def <- list(title = "Residuos versus valores ajustados", subtitle = "Padroes (funil, curvatura) indicam heterocedasticidade ou falta de ajuste", x = "Valores ajustados", y = "Residuos")
  } else if (type == "scale_location") {
    inf$.sr <- sqrt(abs(inf$std_residual))
    p <- ggplot2::ggplot(inf, ggplot2::aes(.data$fitted, .data$.sr))
    p <- base_pts(p)
    def <- list(title = "Escala-locacao", subtitle = "Tendencia crescente sugere variancia nao constante", x = "Valores ajustados", y = "Raiz de |residuos padronizados|")
  } else if (type == "cooks") {
    inf$.i <- seq_len(nrow(inf))
    p <- ggplot2::ggplot(inf, ggplot2::aes(.data$.i, .data$cooks)) +
      ggplot2::geom_segment(ggplot2::aes(xend = .data$.i, yend = 0), colour = pal[["cinza_tecnico"]], linewidth = 0.4) +
      ggplot2::geom_hline(yintercept = 4 / nrow(inf), colour = pal[["laranja"]], linetype = "dashed")
    p <- base_pts(p)
    def <- list(title = "Distancia de Cook", subtitle = "Linha tracejada: 4/n", x = "Observacao (ordem)", y = "Distancia de Cook")
  } else {
    p <- ggplot2::ggplot(inf, ggplot2::aes(.data$residual)) +
      ggplot2::geom_histogram(bins = max(5, min(30, ceiling(sqrt(nrow(inf))) + 2)), fill = pal[["azul_secundario"]], colour = "white", alpha = 0.9)
    def <- list(title = "Distribuicao dos residuos", x = "Residuos", y = "Frequencia")
  }
  apply_style(finish_plot(p, defaults = def), style)
}

#' @rdname caagro_plots
#' @export
plot.caagro_ci_groups <- function(x, type = c("pointrange", "forest", "bars"), style = caagro_style(), ...) {
  type <- match.arg(type)
  g <- style$geoms
  grp <- x$settings$group; resp <- x$settings$response
  it <- x$tables$intervals
  it$.x <- stats::reorder(factor(it[[grp]]), it$estimate)
  d <- x$data_used; d$.x <- factor(d[[grp]], levels = levels(it$.x))
  groups <- levels(it$.x)
  def <- list(title = resp, subtitle = x$method, x = grp, y = resp,
              caption = paste0(round(100 * x$settings$conf_level), "% ", if (x$settings$simultaneous == "bonferroni") "(simultaneos, Bonferroni)" else "(individuais)",
                               "; variancia ", if (x$settings$variance == "pooled") "combinada (QM do residuo)." else "separada por grupo.",
                               " Sobreposicao de intervalos nao e teste de diferenca."))
  if (type == "forest") {
    p <- ggplot2::ggplot(it, ggplot2::aes(y = .data$.x, x = .data$estimate)) +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$lower, xmax = .data$upper, colour = .data$.x), height = g$errorbar_width, linewidth = g$line_width) +
      ggplot2::geom_point(ggplot2::aes(fill = .data$.x), shape = g$point_shape, size = g$point_size + 1.2, colour = "#14283F")
    if (isTRUE(g$show_points)) p <- p + ggplot2::geom_point(data = d, ggplot2::aes(x = .data[[resp]], y = .data$.x), inherit.aes = FALSE,
                                                         alpha = g$alpha * 0.7, size = g$point_size * 0.6, colour = "#64748B",
                                                         position = ggplot2::position_jitter(height = 0.06, width = 0, seed = 1))
    def$x <- resp; def$y <- grp
    p <- p + ggplot2::guides(colour = "none", fill = "none")
    return(apply_style(finish_plot(p, c(x = "continuous", y = "discrete"), groups, def), style))
  }
  p <- ggplot2::ggplot(it, ggplot2::aes(x = .data$.x, y = .data$estimate))
  if (type == "bars") p <- p + ggplot2::geom_col(ggplot2::aes(fill = .data$.x), width = g$bar_width, alpha = 0.85) + ggplot2::expand_limits(y = 0)
  if (isTRUE(g$show_points)) p <- p + ggplot2::geom_point(data = d, ggplot2::aes(x = .data$.x, y = .data[[resp]]), inherit.aes = FALSE,
                                                       alpha = g$alpha * 0.7, size = g$point_size * 0.6, colour = "#64748B",
                                                       position = ggplot2::position_jitter(width = 0.06, height = 0, seed = 1))
  p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lower, ymax = .data$upper, colour = .data$.x), width = g$errorbar_width, linewidth = g$line_width)
  if (type == "pointrange") p <- p + ggplot2::geom_point(ggplot2::aes(fill = .data$.x), shape = g$point_shape, size = g$point_size + 1.2, colour = "#14283F")
  if (isTRUE(g$show_values)) p <- p + ggplot2::geom_text(ggplot2::aes(y = .data$upper, label = formatC(.data$estimate, format = "f", digits = g$label_decimals)),
                                                      vjust = -0.6, size = g$label_size, colour = "#14283F")
  p <- p + ggplot2::guides(colour = "none", fill = "none")
  apply_style(finish_plot(p, c(x = "discrete", y = "continuous"), groups, def), style)
}

#' @rdname caagro_plots
#' @export
plot.caagro_ci <- function(x, type = c("pointrange", "bootstrap"), style = caagro_style(), ...) {
  type <- match.arg(type)
  it <- x$tables$interval
  g <- style$geoms; pal <- caagro_palette()
  if (type == "bootstrap" && !is.null(x$model$replicates)) {
    bd <- data.frame(value = x$model$replicates)
    p <- ggplot2::ggplot(bd, ggplot2::aes(.data$value)) +
      ggplot2::geom_histogram(bins = 40, fill = pal[["azul_secundario"]], colour = "white", alpha = 0.9) +
      ggplot2::annotate("rect", xmin = it$lower, xmax = it$upper, ymin = -Inf, ymax = Inf, fill = pal[["verde_suave"]], alpha = g$band_alpha) +
      ggplot2::geom_vline(xintercept = it$estimate, colour = pal[["laranja"]], linewidth = g$line_width)
    return(apply_style(finish_plot(p, defaults = list(title = "Distribuicao bootstrap", subtitle = x$method, x = x$settings$statistic, y = "Frequencia")), style))
  }
  it$.lab <- it$parameter
  p <- ggplot2::ggplot(it, ggplot2::aes(y = .data$.lab, x = .data$estimate)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$lower, xmax = .data$upper), height = g$errorbar_width, colour = pal[["azul_secundario"]], linewidth = g$line_width) +
    ggplot2::geom_point(shape = g$point_shape, size = g$point_size + 1.5, fill = pal[["verde_cientifico"]], colour = "#14283F")
  if (grepl("diff|paired", x$settings$parameter %||% x$settings$statistic %||% "")) p <- p + ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = pal[["cinza_tecnico"]])
  apply_style(finish_plot(p, c(x = "continuous", y = "discrete"), defaults = list(title = x$method, x = "Estimativa", y = "")), style)
}

#' @rdname caagro_plots
#' @export
plot.caagro_test <- function(x, type = c("box", "estimate"), style = caagro_style(), ...) {
  type <- match.arg(type)
  resp <- x$settings$response; grp <- x$settings$group
  if (is.null(resp) || is.null(x$data_used) || type == "estimate" || is.null(grp)) {
    tb <- x$tables$test %||% x$tables[[1]]
    if (!all(c("estimate", "lower", "upper") %in% names(tb))) {
      p <- ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0, label = "Sem grafico especifico para este resultado.") + ggplot2::theme_void()
      return(p)
    }
    fake <- structure(list(tables = list(interval = transform(tb, parameter = "estimativa")), settings = list(parameter = "diff"), method = x$method),
                      class = c("caagro_ci", "caagro_result"))
    return(plot.caagro_ci(fake, style = style))
  }
  d <- x$data_used
  mc <- structure(list(tables = list(means = {
    dd <- x$tables$descriptives; dd <- dd[dd$group %in% levels(d[[grp]]), ]
    tq <- stats::qt(0.975, dd$n - 1)
    data.frame(.g = dd$group, emmean = dd$mean, lower = dd$mean - tq * dd$se, upper = dd$mean + tq * dd$se, letras = "")
  }), settings = list(factor = ".g", response = resp, conf_level = 0.95, x_label = grp), data_used = transform(d, .g = d[[grp]]), method = x$method),
  class = c("caagro_multcomp", "caagro_result"))
  st <- style; st$geoms$show_letters <- FALSE
  plot.caagro_multcomp(mc, type = "box", style = st)
}

#' @rdname caagro_plots
#' @param variable For descriptive plots: variable to draw (default: first).
#' @export
plot.caagro_describe <- function(x, type = c("hist", "box", "density", "qq", "violin"), style = caagro_style(), variable = NULL, ...) {
  type <- match.arg(type)
  v <- variable %||% x$settings$variables[1]
  grp <- x$settings$group
  d <- x$data_used
  d$.v <- as_numeric_strict(d[[v]], v)
  d <- d[!is.na(d$.v), , drop = FALSE]
  d$.g <- if (is.null(grp)) "(todos)" else as.character(d[[grp]])
  groups <- unique(d$.g)
  g <- style$geoms
  def <- list(title = v, subtitle = "Analise exploratoria", x = v, y = NULL, legend = grp)
  p <- switch(type,
    hist = ggplot2::ggplot(d, ggplot2::aes(.data$.v, fill = .data$.g)) +
      ggplot2::geom_histogram(bins = max(5, min(30, ceiling(log2(nrow(d)) + 1) * 2)), colour = "white", alpha = 0.85, position = "identity"),
    density = ggplot2::ggplot(d, ggplot2::aes(.data$.v, fill = .data$.g, colour = .data$.g)) +
      ggplot2::geom_density(alpha = g$band_alpha + 0.1, linewidth = g$line_width),
    box = ggplot2::ggplot(d, ggplot2::aes(.data$.g, .data$.v, fill = .data$.g)) +
      ggplot2::geom_boxplot(alpha = 0.35, width = g$bar_width, outlier.shape = 4, linewidth = 0.45) +
      (if (g$show_points) ggplot2::geom_point(shape = 21, colour = "white", size = g$point_size * 0.8, alpha = g$alpha,
                                              position = ggplot2::position_jitter(width = 0.08, height = 0, seed = 1))),
    violin = ggplot2::ggplot(d, ggplot2::aes(.data$.g, .data$.v, fill = .data$.g)) +
      ggplot2::geom_violin(alpha = 0.35, linewidth = 0.4, trim = FALSE) +
      ggplot2::geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA, linewidth = 0.4),
    qq = ggplot2::ggplot(d, ggplot2::aes(sample = .data$.v, colour = .data$.g)) +
      ggplot2::stat_qq(size = g$point_size * 0.8) + ggplot2::stat_qq_line(linewidth = g$line_width))
  axes <- c(x = "continuous", y = "continuous")
  if (type %in% c("box", "violin")) { axes["x"] <- "discrete"; def$x <- grp %||% ""; def$y <- v }
  if (type == "hist") def$y <- "Frequencia"
  if (type == "density") def$y <- "Densidade"
  if (type == "qq") { def$x <- "Quantis teoricos"; def$y <- v }
  if (is.null(grp)) p <- p + ggplot2::guides(fill = "none", colour = "none")
  apply_style(finish_plot(p, axes, groups, def), style)
}
