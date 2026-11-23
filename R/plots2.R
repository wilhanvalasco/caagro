#' @rdname caagro_plots
#' @export
plot.caagro_regression <- function(x, type = c("fit", "facet", "residuals", "qq", "ic"), style = caagro_style(), ...) {
  type <- match.arg(type)
  g <- style$geoms; pal <- caagro_palette()
  s <- x$settings
  grouped <- !is.null(s$group)
  multi_model <- length(unique(x$tables$predictions$model)) > 1
  pr <- x$tables$predictions
  lab_model <- stats::setNames(regression_models()$label, regression_models()$model)
  pr$.model <- lab_model[pr$model]
  pr$.col <- if (grouped) as.character(pr$group) else pr$.model
  d0 <- x$data_used
  pts <- data.frame(x = d0[[s$x]], y = if (isTRUE(s$binomial) && !is.null(s$trials)) d0[[s$y]] / d0[[s$trials]] else d0[[s$y]],
                    .col = if (grouped) as.character(d0[[s$group]]) else "(todos)")
  if (type %in% c("residuals", "qq")) {
    rr <- do.call(rbind, lapply(names(x$model), function(m) do.call(rbind, lapply(names(x$model[[m]]), function(gr) {
      f <- x$model[[m]][[gr]]
      rs <- if (inherits(f, "glm")) stats::residuals(f, type = "pearson") else stats::residuals(f)
      data.frame(model = lab_model[[m]], group = gr, fitted = as.numeric(stats::fitted(f)), resid = as.numeric(rs))
    }))))
    rr$.col <- if (grouped) rr$group else rr$model
    groups <- unique(rr$.col)
    if (type == "residuals") {
      p <- ggplot2::ggplot(rr, ggplot2::aes(.data$fitted, .data$resid, fill = .data$.col)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = pal[["cinza_tecnico"]]) +
        ggplot2::geom_point(shape = 21, colour = "white", size = g$point_size, alpha = 0.9)
      def <- list(title = "Res\u00edduos versus ajustados", subtitle = x$method, x = "Valores ajustados", y = "Res\u00edduos", legend = s$group %||% "Modelo")
    } else {
      p <- ggplot2::ggplot(rr, ggplot2::aes(sample = .data$resid, colour = .data$.col)) +
        ggplot2::stat_qq(size = g$point_size * 0.8) + ggplot2::stat_qq_line(linewidth = g$line_width * 0.8)
      def <- list(title = "Q-Q dos res\u00edduos", subtitle = x$method, x = "Quantis te\u00f3ricos", y = "Res\u00edduos", legend = s$group %||% "Modelo")
    }
    if (multi_model) p <- p + ggplot2::facet_wrap(~model, scales = "free")
    return(apply_style(finish_plot(p, groups = groups, defaults = def), style))
  }
  if (type == "ic") {
    cmp <- x$tables$comparison %||% x$tables$fit_stats
    cmp$.model <- lab_model[cmp$model]
    crit <- if (all(is.finite(cmp$AICc))) "AICc" else "AIC"
    cmp$.v <- stats::ave(cmp[[crit]], cmp$group, FUN = function(z) z - min(z))
    p <- ggplot2::ggplot(cmp, ggplot2::aes(x = .data$.v, y = stats::reorder(.data$.model, -.data$.v), fill = .data$group)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7)
    return(apply_style(finish_plot(p, c(x = "continuous", y = "discrete"), groups = unique(cmp$group),
                                   list(title = paste("Compara\u00e7\u00e3o de modelos por", crit), subtitle = "Diferen\u00e7a em rela\u00e7\u00e3o ao melhor modelo de cada grupo (0 = melhor); valores < 2 indicam suporte semelhante", x = paste("Delta", crit), y = "", legend = s$group %||% "Grupo")), style))
  }
  groups <- unique(pr$.col)
  pts$.col <- if (grouped) pts$.col else if (multi_model) NA else groups[1]
  p <- ggplot2::ggplot()
  if (isTRUE(g$show_band) && s$band != "none" && any(!is.na(pr$lower))) {
    p <- p + ggplot2::geom_ribbon(data = pr, ggplot2::aes(x = .data$x, ymin = .data$lower, ymax = .data$upper, fill = .data$.col,
                                                          group = interaction(.data$model, .data$group)), alpha = g$band_alpha)
  }
  if (isTRUE(g$show_points)) {
    p <- p + ggplot2::geom_point(data = pts, ggplot2::aes(.data$x, .data$y, fill = .data$.col), shape = 21, colour = "white",
                                 size = g$point_size * 0.85, alpha = g$alpha, stroke = 0.25)
  }
  if (isTRUE(g$show_means)) {
    mm <- x$tables$means
    mm$.col <- if (grouped) mm$group else if (multi_model) NA else groups[1]
    half <- switch(g$error_type %||% "se", se = mm$se, sd = mm$sd, ci = mm$ci_half, none = NA)
    mm$lo <- mm$mean - half; mm$hi <- mm$mean + half
    if (!identical(g$error_type, "none")) {
      p <- p + ggplot2::geom_errorbar(data = mm, ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi, colour = .data$.col),
                                      width = if (identical(style$x$trans %||% "identity", "identity")) g$errorbar_width * diff(range(mm$x)) / max(1, length(unique(mm$x))) else 0, linewidth = 0.5)
    }
    p <- p + ggplot2::geom_point(data = mm, ggplot2::aes(.data$x, .data$mean, fill = .data$.col), shape = g$point_shape,
                                 colour = "#14283F", size = g$point_size + 0.8, stroke = 0.5)
  }
  p <- p + ggplot2::geom_line(data = pr, ggplot2::aes(.data$x, .data$fit, colour = .data$.col, group = interaction(.data$model, .data$group),
                                                      linetype = if (multi_model && grouped) .data$.model else NULL),
                              linewidth = g$line_width)
  if (!(multi_model && grouped)) p <- p + ggplot2::scale_linetype_manual(values = g$line_type, guide = "none") else
    p <- p + ggplot2::scale_linetype_discrete(name = "Modelo")
  if (isTRUE(g$show_optimum)) {
    op <- tryCatch(optimum_dose(x)$tables$agronomic, error = function(e) NULL)
    if (!is.null(op) && nrow(op)) {
      xo <- op[grepl("^(Dose|Ponto|ED50)", op$quantity), , drop = FALSE]
      if (nrow(xo)) {
        xo$fit <- mapply(function(m, gr, xx) model_registry()[[m]]$f(unname(stats::coef(x$model[[m]][[gr]])), xx), xo$model, xo$group, xo$estimate)
        xo$.col <- if (grouped) xo$group else lab_model[xo$model]
        p <- p + ggplot2::geom_segment(data = xo, ggplot2::aes(x = .data$estimate, xend = .data$estimate, y = -Inf, yend = .data$fit, colour = .data$.col),
                                       linetype = "dotted", linewidth = 0.5) +
          ggplot2::geom_point(data = xo, ggplot2::aes(.data$estimate, .data$fit), shape = 23, fill = pal[["laranja"]], colour = "#14283F", size = g$point_size + 1)
      }
    }
  }
  if (type == "facet" && grouped) p <- p + ggplot2::facet_wrap(~.col)
  # equations
  eq_in_caption <- FALSE
  if (!identical(g$eq_position, "none")) {
    eq <- merge(x$tables$equations, x$tables$fit_stats, by = c("model", "group"))
    met <- g$eq_metrics %||% character()
    eq$label <- vapply(seq_len(nrow(eq)), function(i) {
      r <- eq[i, ]
      parts <- c(if ("r2" %in% met && !is.na(r$r2)) sprintf("%s = %.3f", if (r$r2_type == "cl\u00e1ssico") r2_lab() else if (r$r2_type == "McFadden") r2_lab("pseudo-") else r2_lab("pseudo-"), r$r2),
                 if ("p" %in% met && !is.na(r$p_model)) paste0("p ", if (r$p_model < 0.001) "< 0,001" else paste("=", formatC(r$p_model, format = "f", digits = 3))),
                 if ("aic" %in% met) sprintf("AIC = %.1f", r$AIC),
                 if ("rmse" %in% met && !is.na(r$rmse)) sprintf("RMSE = %.3g", r$rmse),
                 if ("n" %in% met) sprintf("n = %d", r$n))
      paste0(if (grouped && type != "facet") paste0(r$group, ": ") else if (multi_model) paste0(lab_model[[r$model]], ": ") else "",
             r$equation, if (length(parts)) paste0("   ", paste(parts, collapse = "; ")) else "")
    }, character(1))
    eq$.col <- if (grouped) eq$group else lab_model[eq$model]
    eq_in_caption <- nrow(eq) > 3 && type != "facet"
  }
  if (!identical(g$eq_position, "none") && !eq_in_caption) {
    yr <- range(c(pts$y, pr$fit, pr$lower, pr$upper), na.rm = TRUE)
    xr <- range(c(pts$x, pr$x), na.rm = TRUE)
    top <- grepl("top", g$eq_position); left <- grepl("left", g$eq_position)
    step <- diff(yr) * 0.065
    idx <- if (type == "facet") stats::ave(seq_len(nrow(eq)), eq$group, FUN = seq_along) - 1 else seq_len(nrow(eq)) - 1
    if (type == "facet") eq$.col_f <- eq$group
    nrow_eq <- if (type == "facet") max(idx) + 1 else nrow(eq)
    eq$y <- if (top) yr[2] + step * (nrow_eq - idx) else yr[1] - step * (idx + 1)
    eq$x <- if (left) xr[1] else xr[2]
    if (type == "facet") eq$.col <- eq$group
    p <- p + ggplot2::geom_text(data = eq, ggplot2::aes(.data$x, .data$y, label = .data$label, colour = .data$.col),
                                hjust = if (left) 0 else 1, size = g$label_size * 0.85, show.legend = FALSE)
  }
  def <- list(title = s$y, subtitle = x$method, x = s$x, y = if (isTRUE(s$binomial)) "Propor\u00e7\u00e3o" else s$y,
              legend = s$group %||% "Modelo",
              caption = paste0(if (s$band == "confidence") "Faixa: intervalo de confian\u00e7a da resposta m\u00e9dia" else if (s$band == "prediction") "Faixa: intervalo de predi\u00e7\u00e3o para novas observa\u00e7\u00f5es" else "",
                               if (s$band != "none") paste0(" (", round(100 * s$conf_level), "%", if (s$band_method == "bootstrap") ", bootstrap" else "", "). ") else "",
                               if (isTRUE(g$show_means) && g$error_type != "none") paste0("Barras: ", c(se = "erro-padr\u00e3o", sd = "desvio-padr\u00e3o", ci = "IC 95% da m\u00e9dia")[[g$error_type]], ".") else ""))
  if (eq_in_caption) def$caption <- paste(c(def$caption, eq$label), collapse = "\n")
  if (!grouped && !multi_model) p <- p + ggplot2::guides(fill = "none", colour = "none")
  apply_style(finish_plot(p, groups = groups, defaults = def), style)
}

#' @rdname caagro_plots
#' @export
plot.caagro_np <- function(x, type = c("box", "violin", "dots", "heatmap", "effect", "blocks"), style = caagro_style(), ...) {
  type <- match.arg(type)
  g <- style$geoms; pal <- caagro_palette()
  s <- x$settings; d <- x$data_used
  resp <- s$response; grp <- s$group
  desc <- x$tables$descriptives
  ord <- as.character(desc[[grp]][order(-desc$median)])
  d$.g <- factor(d[[grp]], levels = ord); desc$.g <- factor(desc[[grp]], levels = ord)
  groups <- ord
  def <- list(title = resp, subtitle = x$method, x = grp, y = resp)
  if (type == "heatmap") {
    pr <- x$tables$pairs
    if (is.null(pr)) { pr <- data.frame(level1 = character(), level2 = character(), p_adj = numeric()) }
    hm <- rbind(data.frame(a = pr$level1, b = pr$level2, p = pr$p_adj), data.frame(a = pr$level2, b = pr$level1, p = pr$p_adj))
    hm$a <- factor(hm$a, levels = ord); hm$b <- factor(hm$b, levels = rev(ord))
    p <- ggplot2::ggplot(hm, ggplot2::aes(.data$a, .data$b, fill = .data$p)) + ggplot2::geom_tile(colour = "white", linewidth = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = formatC(.data$p, format = "f", digits = 3)), size = g$label_size * 0.85, colour = "#14283F") +
      ggplot2::scale_fill_gradientn(colours = c(pal[["laranja"]], "#F6C08A", "#E7ECF0", "#FFFFFF"), values = c(0, 0.05, 0.2, 1), limits = c(0, 1), name = "p ajustado")
    return(apply_style(finish_plot(p, c(x = "discrete", y = "discrete"), defaults = list(title = "Valores de p ajustados (pares)", subtitle = x$method, x = "", y = "")), style))
  }
  if (type == "effect") {
    pr <- x$tables$pairs
    if (is.null(pr)) abort_caagro("Execute np_posthoc() para obter o gr\u00e1fico de efeitos par a par.")
    pr$contrast <- factor(pr$contrast, levels = pr$contrast[order(pr$statistic)])
    p <- ggplot2::ggplot(pr, ggplot2::aes(.data$statistic, .data$contrast, fill = .data$significant)) +
      ggplot2::geom_vline(xintercept = 0, colour = pal[["cinza_tecnico"]]) + ggplot2::geom_col(width = 0.6) +
      ggplot2::scale_fill_manual(values = c(`TRUE` = pal[["laranja"]], `FALSE` = pal[["azul_secundario"]]), labels = c(`TRUE` = "Significativa", `FALSE` = "N\u00e3o significativa"), name = NULL)
    return(apply_style(finish_plot(p, c(x = "continuous", y = "discrete"), defaults = list(title = "Estat\u00edstica das compara\u00e7\u00f5es", subtitle = x$method, x = "Estat\u00edstica (z ou t)", y = "")), style))
  }
  if (type == "blocks") {
    if (is.null(s$block)) abort_caagro("Gr\u00e1fico por blocos exige um teste com blocos (Friedman).")
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$.g, .data[[resp]], group = .data[[s$block]])) +
      ggplot2::geom_line(colour = pal[["cinza_tecnico"]], alpha = 0.6, linewidth = g$line_width * 0.6) +
      ggplot2::geom_point(ggplot2::aes(fill = .data$.g), shape = 21, colour = "white", size = g$point_size + 0.5)
    p <- p + ggplot2::guides(fill = "none")
    return(apply_style(finish_plot(p, c(x = "discrete", y = "continuous"), groups, def), style))
  }
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$.g, .data[[resp]]))
  if (type == "box") p <- p + ggplot2::geom_boxplot(ggplot2::aes(fill = .data$.g), alpha = 0.3, width = g$bar_width, outlier.shape = NA, linewidth = 0.45)
  if (type == "violin") p <- p + ggplot2::geom_violin(ggplot2::aes(fill = .data$.g), alpha = 0.3, linewidth = 0.4, trim = FALSE)
  if (isTRUE(g$show_points) || type == "dots") {
    p <- p + ggplot2::geom_point(ggplot2::aes(fill = .data$.g), shape = 21, colour = "white", size = g$point_size * 0.8, alpha = g$alpha + 0.2,
                                 position = ggplot2::position_jitter(width = 0.08, height = 0, seed = 1))
  }
  if (type %in% c("dots", "violin")) {
    p <- p + ggplot2::geom_errorbar(data = desc, ggplot2::aes(x = .data$.g, ymin = .data$q1, ymax = .data$q3), inherit.aes = FALSE,
                                    width = g$errorbar_width, linewidth = g$line_width, colour = "#14283F") +
      ggplot2::geom_point(data = desc, ggplot2::aes(x = .data$.g, y = .data$median), inherit.aes = FALSE, shape = 23, size = g$point_size + 1,
                          fill = pal[["laranja"]], colour = "#14283F")
    def$caption <- "Losango: mediana; barras: intervalo interquartil (Q1-Q3)."
  }
  if (!is.null(desc$letras) && isTRUE(g$show_letters)) {
    ytop <- tapply(d[[resp]], d$.g, max)
    desc$.y <- ytop[as.character(desc$.g)] + 0.05 * diff(range(d[[resp]]))
    p <- p + ggplot2::geom_text(data = desc, ggplot2::aes(x = .data$.g, y = .data$.y, label = .data$letras), inherit.aes = FALSE,
                                fontface = "bold", size = g$label_size, colour = "#14283F")
  }
  p <- p + ggplot2::guides(fill = "none")
  apply_style(finish_plot(p, c(x = "discrete", y = "continuous"), groups, def), style)
}

#' @rdname caagro_plots
#' @param axes Components to display (PCA), e.g. `c(1, 2)`.
#' @param ellipse Draw 95 percent concentration ellipses per group (PCA scores).
#' @export
plot.caagro_pca <- function(x, type = c("biplot", "scree", "scores", "loadings", "circle", "contrib"), style = caagro_style(),
                            axes = c(1, 2), ellipse = TRUE, ...) {
  type <- match.arg(type)
  g <- style$geoms; pal <- caagro_palette()
  eig <- x$tables$eigenvalues
  pcs <- paste0("PC", axes)
  axlab <- sprintf("%s (%.1f%%)", pcs, eig$variance[axes])
  sc <- x$tables$scores
  has_g <- !is.null(x$settings$group)
  sc$.g <- if (has_g) as.character(sc$group) else "(todos)"
  groups <- unique(sc$.g)
  if (type == "scree") {
    eig$PC <- factor(eig$PC, levels = eig$PC)
    p <- ggplot2::ggplot(eig, ggplot2::aes(.data$PC, .data$variance)) +
      ggplot2::geom_col(fill = pal[["azul_secundario"]], width = g$bar_width) +
      ggplot2::geom_line(ggplot2::aes(y = .data$cumulative, group = 1), colour = pal[["verde_cientifico"]], linewidth = g$line_width) +
      ggplot2::geom_point(ggplot2::aes(y = .data$cumulative), colour = pal[["verde_cientifico"]], size = g$point_size) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", .data$variance)), vjust = -0.5, size = g$label_size * 0.8, colour = "#14283F")
    return(apply_style(finish_plot(p, c(x = "discrete", y = "continuous"), defaults = list(title = "Vari\u00e2ncia explicada", subtitle = "Barras: por componente; linha: acumulada", x = NULL, y = "Vari\u00e2ncia (%)")), style))
  }
  cr <- x$tables$correlations
  if (type == "circle") {
    circ <- data.frame(t = seq(0, 2 * pi, length.out = 200))
    p <- ggplot2::ggplot() + ggplot2::geom_path(data = circ, ggplot2::aes(cos(.data$t), sin(.data$t)), colour = pal[["cinza_tecnico"]], linewidth = 0.4) +
      ggplot2::geom_hline(yintercept = 0, colour = "#E7ECF0") + ggplot2::geom_vline(xintercept = 0, colour = "#E7ECF0") +
      ggplot2::geom_segment(data = cr, ggplot2::aes(x = 0, y = 0, xend = .data[[pcs[1]]], yend = .data[[pcs[2]]]),
                            arrow = grid::arrow(length = grid::unit(0.18, "cm")), colour = pal[["azul_secundario"]], linewidth = g$line_width) +
      ggplot2::geom_text(data = cr, ggplot2::aes(x = .data[[pcs[1]]] * 1.1, y = .data[[pcs[2]]] * 1.1, label = .data$variable), size = g$label_size, colour = "#14283F") +
      ggplot2::coord_equal(xlim = c(-1.15, 1.15), ylim = c(-1.15, 1.15))
    return(apply_style(finish_plot(p, defaults = list(title = "Circulo de correla\u00e7\u00f5es", subtitle = x$method, x = axlab[1], y = axlab[2])), style))
  }
  if (type == "loadings") {
    ld <- x$tables$loadings
    lg <- rbind(data.frame(variable = ld$variable, PC = pcs[1], value = ld[[pcs[1]]]), data.frame(variable = ld$variable, PC = pcs[2], value = ld[[pcs[2]]]))
    p <- ggplot2::ggplot(lg, ggplot2::aes(.data$value, .data$variable, fill = .data$value > 0)) + ggplot2::geom_col(width = 0.65) +
      ggplot2::geom_vline(xintercept = 0, colour = pal[["cinza_tecnico"]]) + ggplot2::facet_wrap(~PC) +
      ggplot2::scale_fill_manual(values = c(`TRUE` = pal[["verde_cientifico"]], `FALSE` = pal[["azul_secundario"]]), guide = "none")
    return(apply_style(finish_plot(p, c(x = "continuous", y = "discrete"), defaults = list(title = "Cargas (loadings)", subtitle = x$method, x = "Carga", y = "")), style))
  }
  if (type == "contrib") {
    ct <- x$tables$contributions
    k <- min(ncol(ct) - 1, 6)
    lg <- do.call(rbind, lapply(names(ct)[2:(k + 1)], function(pc) data.frame(variable = ct$variable, PC = pc, contribution = ct[[pc]])))
    lg$PC <- factor(lg$PC, levels = names(ct)[2:(k + 1)])
    p <- ggplot2::ggplot(lg, ggplot2::aes(.data$PC, .data$variable, fill = .data$contribution)) + ggplot2::geom_tile(colour = "white") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f", .data$contribution)), size = g$label_size * 0.8, colour = "#14283F") +
      ggplot2::scale_fill_gradient(low = "#F5F7FA", high = pal[["verde_cientifico"]], name = "Contribui\u00e7\u00e3o (%)")
    return(apply_style(finish_plot(p, c(x = "discrete", y = "discrete"), defaults = list(title = "Contribui\u00e7\u00e3o das vari\u00e1veis", subtitle = x$method, x = "", y = "")), style))
  }
  p <- ggplot2::ggplot(sc, ggplot2::aes(.data[[pcs[1]]], .data[[pcs[2]]])) +
    ggplot2::geom_hline(yintercept = 0, colour = "#E7ECF0") + ggplot2::geom_vline(xintercept = 0, colour = "#E7ECF0")
  if (has_g && ellipse) p <- p + ggplot2::stat_ellipse(ggplot2::aes(colour = .data$.g, fill = .data$.g), geom = "polygon", alpha = g$band_alpha * 0.7, level = 0.95, linewidth = 0.4)
  p <- p + ggplot2::geom_point(ggplot2::aes(fill = .data$.g), shape = 21, colour = "white", size = g$point_size + 0.4, alpha = 0.95)
  if (isTRUE(g$show_values)) p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$label), size = g$label_size * 0.75, vjust = -0.8, colour = "#64748B")
  if (type == "biplot") {
    ld <- x$tables$correlations
    sf <- 0.85 * max(abs(c(sc[[pcs[1]]], sc[[pcs[2]]])))
    p <- p + ggplot2::geom_segment(data = ld, ggplot2::aes(x = 0, y = 0, xend = .data[[pcs[1]]] * sf, yend = .data[[pcs[2]]] * sf),
                                   arrow = grid::arrow(length = grid::unit(0.16, "cm")), colour = pal[["laranja"]], linewidth = g$line_width * 0.8) +
      ggplot2::geom_text(data = ld, ggplot2::aes(x = .data[[pcs[1]]] * sf * 1.1, y = .data[[pcs[2]]] * sf * 1.1, label = .data$variable),
                         colour = "#14283F", size = g$label_size, fontface = "bold")
  }
  if (!has_g) p <- p + ggplot2::guides(fill = "none", colour = "none")
  apply_style(finish_plot(p, groups = groups, defaults = list(
    title = if (type == "biplot") "Biplot" else "Escores", subtitle = x$method, x = axlab[1], y = axlab[2], legend = x$settings$group,
    caption = if (has_g && ellipse) "Elipses: regi\u00f5es de 95% de concentra\u00e7\u00e3o (t multivariada), descritivas." else NULL)), style)
}

dendro_segments <- function(hc) {
  n <- length(hc$order)
  xpos <- numeric(n); xpos[hc$order] <- seq_len(n)
  node_x <- numeric(n - 1); node_h <- hc$height
  segs <- list()
  getx <- function(i) if (i < 0) xpos[-i] else node_x[i]
  geth <- function(i) if (i < 0) 0 else node_h[i]
  for (i in seq_len(n - 1)) {
    a <- hc$merge[i, 1]; b <- hc$merge[i, 2]
    xa <- getx(a); xb <- getx(b); h <- node_h[i]
    node_x[i] <- (xa + xb) / 2
    segs[[i]] <- data.frame(x = c(xa, xb, xa), y = c(geth(a), geth(b), h), xend = c(xa, xb, xb), yend = c(h, h, h), node = i)
  }
  list(segments = do.call(rbind, segs), xpos = xpos)
}

#' @rdname caagro_plots
#' @param horizontal Draw the dendrogram horizontally.
#' @export
plot.caagro_cluster <- function(x, type = "dendrogram", style = caagro_style(), horizontal = FALSE, ...) {
  hc <- x$model; g <- style$geoms; pal <- caagro_palette()
  ds <- dendro_segments(hc)
  seg <- ds$segments[, c("x", "y", "xend", "yend", "node")]
  k <- x$settings$k; h <- x$settings$h
  cl <- if (!is.null(k) || !is.null(h)) stats::cutree(hc, k = k, h = h) else NULL
  seg$.col <- "(arvore)"
  groups <- NULL
  if (!is.null(cl)) {
    # members of each node
    n <- length(hc$order); members <- vector("list", n - 1)
    for (i in seq_len(n - 1)) {
      m <- hc$merge[i, ]
      members[[i]] <- unlist(lapply(m, function(z) if (z < 0) -z else members[[z]]))
    }
    node_cl <- vapply(members, function(mm) { u <- unique(cl[mm]); if (length(u) == 1) paste0("Grupo ", u) else "(arvore)" }, character(1))
    seg$.col <- node_cl[seg$node]
    groups <- c(paste0("Grupo ", sort(unique(cl))), "(arvore)")
  }
  labs <- data.frame(x = ds$xpos, label = hc$labels %||% as.character(seq_along(ds$xpos)))
  labs$.col <- if (!is.null(cl)) paste0("Grupo ", cl) else "(arvore)"
  cut_h <- if (!is.null(h)) h else if (!is.null(k) && k > 1) mean(rev(hc$height)[c(k - 1, k)]) else NULL
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = seg, ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend, colour = .data$.col),
                          linewidth = g$line_width * 0.8, lineend = "round")
  off <- -0.02 * max(hc$height)
  p <- p + ggplot2::geom_text(data = labs, ggplot2::aes(x = .data$x, y = off, label = .data$label, colour = .data$.col),
                              angle = if (horizontal) 0 else 90, hjust = 1, size = g$label_size * 0.8, show.legend = FALSE)
  if (!is.null(cut_h)) p <- p + ggplot2::geom_hline(yintercept = cut_h, linetype = "dashed", colour = pal[["laranja"]], linewidth = 0.5)
  p <- p + ggplot2::scale_x_continuous(breaks = NULL) + ggplot2::expand_limits(y = 8 * off)
  if (horizontal) p <- p + ggplot2::coord_flip() + ggplot2::scale_y_reverse()
  cols <- if (!is.null(groups)) c(stats::setNames(caagro_palette(length(groups) - 1), groups[-length(groups)]), "(arvore)" = "#64748B") else c("(arvore)" = "#315775")
  st <- style; if (is.null(st$geoms$colors)) st$geoms$colors <- cols
  attr_groups <- names(cols)
  p <- p + ggplot2::guides(fill = "none")
  out <- apply_style(finish_plot(p, c(x = "none", y = if (horizontal) "none" else "continuous"), attr_groups,
                                 list(title = "Dendrograma", subtitle = x$method, x = "", y = "Altura (dist\u00e2ncia de fus\u00e3o)", legend = "Grupo")), st)
  out
}

#' @rdname caagro_plots
#' @export
plot.caagro_kmeans <- function(x, type = c("clusters", "elbow", "silhouette"), style = caagro_style(), ...) {
  type <- match.arg(type)
  g <- style$geoms; pal <- caagro_palette()
  if (type == "elbow") {
    sel <- x$tables$selection
    p <- ggplot2::ggplot(sel, ggplot2::aes(.data$k, .data$within_ss)) + ggplot2::geom_line(colour = pal[["azul_secundario"]], linewidth = g$line_width) +
      ggplot2::geom_point(size = g$point_size + 0.5, colour = pal[["azul_secundario"]]) +
      ggplot2::geom_vline(xintercept = x$settings$k, linetype = "dashed", colour = pal[["laranja"]])
    return(apply_style(finish_plot(p, defaults = list(title = "M\u00e9todo do cotovelo", subtitle = "Soma de quadrados dentro dos grupos", x = "k", y = "SQ dentro")), style))
  }
  cl <- x$tables$clusters
  cl$.g <- paste0("Grupo ", cl$cluster)
  if (type == "silhouette") {
    cl <- cl[order(cl$cluster, -cl$silhouette), ]; cl$.i <- seq_len(nrow(cl))
    p <- ggplot2::ggplot(cl, ggplot2::aes(.data$.i, .data$silhouette, fill = .data$.g)) + ggplot2::geom_col(width = 1) +
      ggplot2::geom_hline(yintercept = mean(cl$silhouette), linetype = "dashed", colour = pal[["cinza_tecnico"]])
    return(apply_style(finish_plot(p, groups = sort(unique(cl$.g)), defaults = list(title = "Silhueta", subtitle = sprintf("M\u00e9dia = %.2f", mean(cl$silhouette)), x = "Indiv\u00edduos", y = "Largura da silhueta", legend = "Grupo")), style))
  }
  M <- as.matrix(x$data_used); if (x$settings$scale) M <- scale(M)
  pc <- stats::prcomp(M)
  ve <- 100 * pc$sdev^2 / sum(pc$sdev^2)
  sc <- data.frame(PC1 = pc$x[, 1], PC2 = pc$x[, 2], .g = cl$.g, label = cl$label)
  p <- ggplot2::ggplot(sc, ggplot2::aes(.data$PC1, .data$PC2, fill = .data$.g, colour = .data$.g)) +
    ggplot2::stat_ellipse(geom = "polygon", alpha = g$band_alpha * 0.6, level = 0.95, linewidth = 0.3) +
    ggplot2::geom_point(shape = 21, colour = "white", size = g$point_size + 0.5)
  if (isTRUE(g$show_values)) p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$label), size = g$label_size * 0.75, vjust = -0.8, colour = "#64748B")
  apply_style(finish_plot(p, groups = sort(unique(sc$.g)), defaults = list(title = "Grupos k-means", subtitle = "Proje\u00e7\u00e3o nos dois primeiros componentes principais",
                                                                     x = sprintf("PC1 (%.1f%%)", ve[1]), y = sprintf("PC2 (%.1f%%)", ve[2]), legend = "Grupo")), style)
}

#' @rdname caagro_plots
#' @export
plot.caagro_cor <- function(x, type = "heatmap", style = caagro_style(), ...) {
  g <- style$geoms; pal <- caagro_palette()
  pr <- x$tables$pairs
  v <- x$settings$variables
  hm <- rbind(data.frame(a = pr$var1, b = pr$var2, r = pr$r, s = pr$signif),
              data.frame(a = pr$var2, b = pr$var1, r = pr$r, s = pr$signif),
              data.frame(a = v, b = v, r = 1, s = ""))
  hm$a <- factor(hm$a, levels = v); hm$b <- factor(hm$b, levels = rev(v))
  p <- ggplot2::ggplot(hm, ggplot2::aes(.data$a, .data$b, fill = .data$r)) + ggplot2::geom_tile(colour = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(formatC(.data$r, format = "f", digits = 2), ifelse(.data$s %in% c("ns", "."), "", .data$s))),
                       size = g$label_size * 0.85, colour = "#14283F") +
    ggplot2::scale_fill_gradientn(colours = caagro_palette(7, "diverging"), limits = c(-1, 1), name = "r")
  apply_style(finish_plot(p, c(x = "discrete", y = "discrete"),
                          defaults = list(title = "Matriz de correla\u00e7\u00e3o", subtitle = paste0(x$method, " - * p ajustado (", x$settings$p_adjust, ") < 0,05"), x = "", y = "")),
              style) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
