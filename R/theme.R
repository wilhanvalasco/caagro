#' caagro colour palette
#'
#' Institutional colours of caagro. Orange is an accent colour and is only
#' used after the blue/green tones.
#'
#' @param n Number of colours. If `NULL`, the named institutional colours
#'   are returned.
#' @param type `"qualitative"` (groups), `"sequential"` or `"diverging"`.
#' @return A character vector of hex colours.
#' @examples
#' caagro_palette()
#' caagro_palette(4)
#' @export
caagro_palette <- function(n = NULL, type = c("qualitative", "sequential", "diverging")) {
  type <- match.arg(type)
  brand <- c(azul_profundo = "#14283F", azul_secundario = "#315775", verde_cientifico = "#218568",
             verde_suave = "#75AE97", cinza_tecnico = "#64748B", cinza_claro = "#E7ECF0",
             fundo = "#F5F7FA", branco = "#FFFFFF", laranja = "#F28C28")
  if (is.null(n)) return(brand)
  if (type == "sequential") return(grDevices::colorRampPalette(c("#E7ECF0", "#75AE97", "#218568", "#14283F"))(n))
  if (type == "diverging") return(grDevices::colorRampPalette(c("#315775", "#A9BCCB", "#F5F7FA", "#F6C08A", "#F28C28"))(n))
  qual <- c("#315775", "#218568", "#F28C28", "#14283F", "#75AE97", "#64748B", "#8E6C8A", "#C9A227",
            "#7FA7C9", "#B5543C")
  if (n <= length(qual)) qual[seq_len(n)] else grDevices::colorRampPalette(qual)(n)
}

#' ggplot2 themes of caagro
#'
#' Five editorial themes sharing the same typographic hierarchy:
#' `"light"` (Scientific Light, default), `"minimal"` (Scientific Minimal),
#' `"bw"` (Publication Black & White), `"dark"` (Scientific Dark) and
#' `"corporate"` (caagro Corporate).
#'
#' @param style Theme name.
#' @param base_size Base font size (pt).
#' @param base_family Font family (`""` = device default sans serif).
#' @return A ggplot2 theme.
#' @examples
#' library(ggplot2)
#' ggplot(PlantGrowth, aes(group, weight)) + geom_boxplot() + theme_caagro()
#' @export
theme_caagro <- function(style = c("light", "minimal", "bw", "dark", "corporate"),
                         base_size = 11, base_family = "") {
  style <- match.arg(style)
  pal <- caagro_palette()
  ink <- switch(style, dark = "#E7ECF0", bw = "#000000", "#14283F")
  sub <- switch(style, dark = "#A9B4C2", bw = "#333333", pal[["cinza_tecnico"]])
  bg <- switch(style, dark = "#14283F", corporate = "#FFFFFF", "#FFFFFF")
  grid <- switch(style, dark = "#2A3F57", bw = NA, minimal = "#EEF1F4", "#E7ECF0")
  axis_line <- switch(style, minimal = NA, dark = "#64748B", bw = "#000000", "#64748B")
  th <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = ink),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.25), colour = ink,
                                         margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.95), colour = sub,
                                            margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(size = ggplot2::rel(0.75), colour = sub, hjust = 0),
      plot.title.position = "plot", plot.caption.position = "plot",
      axis.title = ggplot2::element_text(size = ggplot2::rel(0.95), colour = ink),
      axis.text = ggplot2::element_text(size = ggplot2::rel(0.85), colour = sub),
      axis.line = if (is.na(axis_line)) ggplot2::element_blank() else ggplot2::element_line(colour = axis_line, linewidth = 0.4),
      axis.ticks = if (is.na(axis_line)) ggplot2::element_blank() else ggplot2::element_line(colour = axis_line, linewidth = 0.4),
      axis.ticks.length = ggplot2::unit(3, "pt"),
      panel.grid.major = if (is.na(grid)) ggplot2::element_blank() else ggplot2::element_line(colour = grid, linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(size = ggplot2::rel(0.85), face = "bold"),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
      legend.key = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", colour = ink, size = ggplot2::rel(0.9), hjust = 0),
      plot.background = ggplot2::element_rect(fill = bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = bg, colour = NA),
      plot.margin = ggplot2::margin(12, 14, 10, 10)
    )
  if (style == "bw") {
    th <- th + ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.5),
                              axis.line = ggplot2::element_blank(),
                              strip.background = ggplot2::element_rect(fill = "grey92", colour = "black"))
  }
  if (style == "corporate") {
    th <- th + ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = pal[["azul_profundo"]], size = ggplot2::rel(1.3)),
      strip.background = ggplot2::element_rect(fill = pal[["azul_profundo"]], colour = NA),
      strip.text = ggplot2::element_text(colour = "white", face = "bold", hjust = 0.02),
      panel.background = ggplot2::element_rect(fill = "#FBFCFD", colour = NA))
  }
  th
}

#' Universal graphic style specification
#'
#' A single object describing every visual aspect of a caagro plot. All
#' applications use the same editor, which reads and writes this object, and
#' every `plot()` method accepts it. Styles can be saved to and loaded from
#' JSON with [style_to_json()] / [style_from_json()].
#'
#' @param theme Theme name (see [theme_caagro()]).
#' @param base_size Base font size.
#' @param font_family Font family.
#' @param title,subtitle,caption Plot texts (`NULL` keeps the default
#'   produced by the analysis; `""` removes).
#' @param show_title Show title and subtitle.
#' @param title_size,title_color,title_align Title size multiplier, colour
#'   and alignment (`0` left, `0.5` centre, `1` right).
#' @param x,y Axis specifications created with [axis_spec()].
#' @param legend Legend specification created with [legend_spec()].
#' @param geoms Geometry specification created with [geom_spec()].
#' @return An object of class `caagro_style`.
#' @examples
#' st <- caagro_style(title = "Produtividade", theme = "bw",
#'                    y = axis_spec(title = "Produtividade", unit = "t/ha", decimals = 1))
#' fit <- anova_exp(PlantGrowth, "weight", "group")
#' plot(compare_means(fit), style = st)
#' @export
caagro_style <- function(theme = "light", base_size = 11, font_family = "",
                         title = NULL, subtitle = NULL, caption = NULL, show_title = TRUE,
                         title_size = 1, title_color = NULL, title_align = 0,
                         x = axis_spec(), y = axis_spec(), legend = legend_spec(),
                         geoms = geom_spec()) {
  structure(list(theme = theme, base_size = base_size, font_family = font_family,
                 title = title, subtitle = subtitle, caption = caption, show_title = show_title,
                 title_size = title_size, title_color = title_color, title_align = title_align,
                 x = x, y = y, legend = legend, geoms = geoms),
            class = "caagro_style")
}

#' @rdname caagro_style
#' @param unit Unit appended to the axis title in parentheses.
#' @param min,max Axis limits (`NA` = automatic).
#' @param breaks_by Interval between tick marks (`NA` = automatic).
#' @param decimals Decimal places of tick labels (`NA` = automatic).
#' @param angle Rotation of tick labels (degrees).
#' @param text_size Tick label size multiplier.
#' @param trans Scale transformation: `"identity"`, `"log10"`, `"sqrt"`.
#' @export
axis_spec <- function(title = NULL, unit = NULL, min = NA, max = NA, breaks_by = NA,
                      decimals = NA, angle = 0, text_size = 1, trans = "identity") {
  list(title = title, unit = unit, min = min, max = max, breaks_by = breaks_by, decimals = decimals,
       angle = angle, text_size = text_size, trans = trans)
}

#' @rdname caagro_style
#' @param show Show legend.
#' @param position `"right"`, `"bottom"`, `"top"`, `"left"`, `"inside"`.
#' @param ncol Number of legend columns (`NA` = automatic).
#' @param size Legend text size multiplier.
#' @param order Character vector with the desired order of groups.
#' @param labels Named character vector: new labels for groups.
#' @export
legend_spec <- function(show = TRUE, position = "right", ncol = NA, size = 1, title = NULL,
                        order = NULL, labels = NULL) {
  list(show = show, position = position, ncol = ncol, size = size, title = title,
       order = order, labels = labels)
}

#' @rdname caagro_style
#' @param point_size,point_shape Point size and shape (0-25).
#' @param line_width,line_type Line width and type.
#' @param alpha Transparency of filled elements and raw points.
#' @param colors Character vector of colours (named by group or in order);
#'   `NULL` = caagro palette.
#' @param fill Single fill colour for bars when there are no groups.
#' @param bar_width,errorbar_width Widths of bars and error bar caps.
#' @param band_alpha Transparency of confidence bands.
#' @param label_size Size of value/letter labels.
#' @param label_decimals Decimal places of value labels.
#' @param show_letters Show grouping letters.
#' @param show_values Show mean values.
#' @param show_points Show raw observations.
#' @param show_band Show confidence/prediction bands (regression).
#' @param show_means Show means per x level (regression).
#' @param error_type Error bars for means: `"se"`, `"sd"`, `"ci"` or `"none"`.
#' @param eq_position Position of equations: `"topleft"`, `"topright"`,
#'   `"bottomleft"`, `"bottomright"` or `"none"`.
#' @param eq_metrics Metrics printed with equations: any of `"r2"`, `"p"`,
#'   `"aic"`, `"rmse"`, `"n"`.
#' @param show_optimum Mark the dose of maximum response (when available).
#' @export
geom_spec <- function(point_size = 2.2, point_shape = 21, line_width = 0.8, line_type = "solid",
                      alpha = 0.55, colors = NULL, fill = "#315775", bar_width = 0.62,
                      errorbar_width = 0.18, band_alpha = 0.18, label_size = 3.6,
                      label_decimals = 2, show_letters = TRUE, show_values = FALSE,
                      show_points = TRUE, show_band = TRUE, show_means = TRUE, error_type = "se",
                      eq_position = "topleft", eq_metrics = c("r2", "n"), show_optimum = FALSE) {
  list(point_size = point_size, point_shape = point_shape, line_width = line_width,
       line_type = line_type, alpha = alpha, colors = colors, fill = fill, bar_width = bar_width,
       errorbar_width = errorbar_width, band_alpha = band_alpha, label_size = label_size,
       label_decimals = label_decimals, show_letters = show_letters, show_values = show_values,
       show_points = show_points, show_band = show_band, show_means = show_means,
       error_type = error_type, eq_position = eq_position, eq_metrics = eq_metrics,
       show_optimum = show_optimum)
}

#' Save and load styles as JSON
#' @param style A `caagro_style`.
#' @param file Path of a `.json` file. If `NULL`, the JSON string is returned.
#' @param json A JSON string or a path to a JSON file.
#' @return `style_to_json()` returns JSON text (invisibly when writing a
#'   file); `style_from_json()` returns a `caagro_style`.
#' @examples
#' js <- style_to_json(caagro_style(theme = "bw"))
#' style_from_json(js)$theme
#' @export
style_to_json <- function(style, file = NULL) {
  txt <- jsonlite::toJSON(unclass(style), auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE)
  if (is.null(file)) return(txt)
  writeLines(txt, file); invisible(txt)
}

#' @rdname style_to_json
#' @export
style_from_json <- function(json) {
  if (length(json) == 1 && file.exists(json)) json <- paste(readLines(json, warn = FALSE), collapse = "\n")
  lst <- jsonlite::fromJSON(json, simplifyVector = TRUE)
  base <- unclass(caagro_style())
  fill_na <- function(z) { if (is.list(z)) lapply(z, fill_na) else if (is.null(z)) NA else z }
  merge_list <- function(a, b) {
    for (nm in names(b)) {
      if (is.list(a[[nm]]) && is.list(b[[nm]])) a[[nm]] <- merge_list(a[[nm]], b[[nm]])
      else a[nm] <- list(b[[nm]])
    }
    a
  }
  out <- merge_list(base, lst)
  for (ax in c("x", "y")) for (k in c("min", "max", "breaks_by", "decimals")) if (is.null(out[[ax]][[k]])) out[[ax]][[k]] <- NA
  structure(out, class = "caagro_style")
}

# R code that reproduces a style (internal; used by the graphics editor)
style_code <- function(style) {
  dflt <- unclass(caagro_style())
  s <- unclass(style)
  diffs <- function(a, b) names(a)[!vapply(names(a), function(n) identical(a[[n]], b[[n]]), logical(1))]
  top <- setdiff(diffs(s, dflt), c("x", "y", "legend", "geoms"))
  args <- lapply(top, function(n) paste0(n, " = ", paste(deparse(s[[n]]), collapse = "")))
  for (comp in c("x", "y", "legend", "geoms")) {
    dd <- diffs(s[[comp]], dflt[[comp]])
    if (length(dd)) {
      fn <- c(x = "axis_spec", y = "axis_spec", legend = "legend_spec", geoms = "geom_spec")[[comp]]
      inner <- paste(vapply(dd, function(n) paste0(n, " = ", paste(deparse(s[[comp]][[n]]), collapse = "")), character(1)), collapse = ", ")
      args[[length(args) + 1]] <- paste0(comp, " = caagro::", fn, "(", inner, ")")
    }
  }
  paste0("estilo <- caagro::caagro_style(", if (length(args)) paste0("\n  ", paste(unlist(args), collapse = ",\n  "), "\n") else "", ")")
}

axis_title <- function(spec, default) {
  t <- spec$title %||% default
  if (!is.null(spec$unit) && nzchar(spec$unit %||% "")) t <- paste0(t, " (", spec$unit, ")")
  if (is.character(t) && identical(t, "")) return(NULL)
  t
}

num_labeller <- function(decimals) {
  if (is.null(decimals) || is.na(decimals)) return(ggplot2::waiver())
  function(v) formatC(v, format = "f", digits = decimals)
}

# Apply a caagro style to a ggplot produced by caagro (internal)
apply_style <- function(p, style = caagro_style(), defaults = list()) {
  if (!inherits(style, "caagro_style")) style <- do.call(caagro_style, style)
  axes <- attr(p, "caagro_axes") %||% c(x = "continuous", y = "continuous")
  groups <- attr(p, "caagro_groups")
  defaults <- utils::modifyList(attr(p, "caagro_defaults") %||% list(), defaults)
  th <- theme_caagro(style$theme, style$base_size, style$font_family)
  title <- if (isTRUE(style$show_title)) style$title %||% defaults$title else NULL
  subtitle <- if (isTRUE(style$show_title)) style$subtitle %||% defaults$subtitle else NULL
  if (identical(title, "")) title <- NULL
  if (identical(subtitle, "")) subtitle <- NULL
  p <- p + th + ggplot2::labs(title = title, subtitle = subtitle, caption = style$caption %||% defaults$caption,
                              x = axis_title(style$x, defaults$x %||% ggplot2::waiver()),
                              y = axis_title(style$y, defaults$y %||% ggplot2::waiver()))
  tt <- ggplot2::theme(
    plot.title = ggplot2::element_text(size = ggplot2::rel(1.25 * style$title_size), hjust = style$title_align,
                                       colour = style$title_color),
    plot.subtitle = ggplot2::element_text(hjust = style$title_align),
    axis.text.x = ggplot2::element_text(angle = style$x$angle, size = ggplot2::rel(0.85 * style$x$text_size),
                                        hjust = if (style$x$angle > 0) 1 else 0.5,
                                        vjust = if (style$x$angle > 0) 1 else 0.5),
    axis.text.y = ggplot2::element_text(angle = style$y$angle, size = ggplot2::rel(0.85 * style$y$text_size)),
    legend.position = if (isTRUE(style$legend$show)) (if (style$legend$position == "inside") c(0.98, 0.98) else style$legend$position) else "none",
    legend.justification = if (identical(style$legend$position, "inside")) c(1, 1) else "center",
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.8 * style$legend$size)),
    legend.title = ggplot2::element_text(size = ggplot2::rel(0.85 * style$legend$size), face = "bold")
  )
  p <- p + tt
  lim_list <- list()
  for (ax in c("x", "y")) {
    spec <- style[[ax]]
    if (axes[[ax]] == "continuous") {
      lims <- c(spec$min %||% NA, spec$max %||% NA)
      brk <- if (!is.na(spec$breaks_by %||% NA)) {
        bb <- spec$breaks_by
        function(r) { r <- range(r, na.rm = TRUE); seq(floor(r[1] / bb) * bb, ceiling(r[2] / bb) * bb, by = bb) }
      } else ggplot2::waiver()
      trans <- spec$trans %||% "identity"
      p <- suppressMessages(p + cont_scale(ax, labels = num_labeller(spec$decimals), breaks = brk, trans = trans))
      if (any(!is.na(lims))) lim_list[[paste0(ax, "lim")]] <- lims
    }
  }
  if (length(lim_list)) p <- suppressMessages(p + do.call(ggplot2::coord_cartesian, lim_list))
  # colour scales
  if (!is.null(groups)) {
    ord <- style$legend$order %||% groups
    ord <- c(intersect(ord, groups), setdiff(groups, ord))
    cols <- style$geoms$colors
    if (is.null(cols) || !length(cols)) cols <- caagro_palette(length(ord))
    if (is.null(names(cols))) cols <- stats::setNames(rep_len(cols, length(ord)), ord)
    cols <- cols[ord]; cols[is.na(cols)] <- caagro_palette(length(ord))[is.na(cols)]
    names(cols) <- ord
    labs <- style$legend$labels
    lab_vec <- stats::setNames(ord, ord)
    if (!is.null(labs)) lab_vec[names(labs)[names(labs) %in% ord]] <- labs[names(labs) %in% ord]
    ltitle <- style$legend$title %||% defaults$legend %||% ggplot2::waiver()
    guide <- if (!is.na(style$legend$ncol %||% NA)) ggplot2::guide_legend(ncol = style$legend$ncol) else "legend"
    p <- suppressMessages(p +
      ggplot2::scale_colour_manual(values = cols, limits = ord, labels = lab_vec, name = ltitle, guide = guide, aesthetics = c("colour", "fill")))
  }
  p
}

# Continuous scale compatible with ggplot2 < 3.5 (trans) and >= 3.5 (transform)
cont_scale <- function(ax, labels, breaks, trans) {
  fn <- if (ax == "x") ggplot2::scale_x_continuous else ggplot2::scale_y_continuous
  args <- list(labels = labels, breaks = breaks)
  if (utils::packageVersion("ggplot2") >= "3.5.0") args$transform <- trans else args$trans <- trans
  do.call(fn, args)
}
