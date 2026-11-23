# Shiny module: universal graphics editor ----------------------------------
# Every application uses this same module. It edits a caagro_style object,
# renders a preview and exports the plot re-rendered at the final size.

style_clipboard <- function(session) {
  ud <- session$userData
  if (is.null(ud$caagro_style_clip)) ud$caagro_style_clip <- shiny::reactiveVal(NULL)
  ud$caagro_style_clip
}

mod_plot_editor_ui <- function(id, height = "560px") {
  ns <- shiny::NS(id)
  acc <- bslib::accordion(
    open = "Tipo e tema", multiple = FALSE,
    bslib::accordion_panel("Tipo e tema", icon = shiny::icon("palette"),
      shiny::uiOutput(ns("type_ui")),
      shiny::selectInput(ns("theme"), "Tema", c("Scientific Light" = "light", "Scientific Minimal" = "minimal",
                                                 "Publication Black & White" = "bw", "Scientific Dark" = "dark", "caagro Corporate" = "corporate")),
      shiny::sliderInput(ns("base_size"), "Tamanho base da fonte", 7, 22, 11, step = 0.5)),
    bslib::accordion_panel("T\u00edtulo e subt\u00edtulo", icon = shiny::icon("heading"),
      shiny::checkboxInput(ns("show_title"), "Exibir t\u00edtulo e subt\u00edtulo", TRUE),
      shiny::textInput(ns("title"), "T\u00edtulo", placeholder = "(autom\u00e1tico)"),
      shiny::textInput(ns("subtitle"), "Subt\u00edtulo", placeholder = "(autom\u00e1tico)"),
      shiny::textInput(ns("caption"), "Nota de rodap\u00e9", placeholder = "(autom\u00e1tica)"),
      shiny::fluidRow(shiny::column(4, shiny::numericInput(ns("title_size"), "Tamanho", 1, 0.5, 3, 0.1)),
                      shiny::column(4, shiny::textInput(ns("title_color"), "Cor", "#14283F")),
                      shiny::column(4, shiny::selectInput(ns("title_align"), "Alinhar", c("Esquerda" = 0, "Centro" = 0.5, "Direita" = 1))))),
    bslib::accordion_panel("Eixo X", icon = shiny::icon("arrows-left-right"), axis_controls(ns, "x")),
    bslib::accordion_panel("Eixo Y", icon = shiny::icon("arrows-up-down"), axis_controls(ns, "y")),
    bslib::accordion_panel("Legenda", icon = shiny::icon("list"),
      shiny::checkboxInput(ns("leg_show"), "Exibir legenda", TRUE),
      shiny::fluidRow(shiny::column(6, shiny::selectInput(ns("leg_pos"), "Posi\u00e7\u00e3o", c("Direita" = "right", "Abaixo" = "bottom", "Acima" = "top", "Esquerda" = "left", "Interna" = "inside"))),
                      shiny::column(6, shiny::numericInput(ns("leg_ncol"), "Colunas", NA, 1, 10, 1))),
      shiny::textInput(ns("leg_title"), "T\u00edtulo da legenda", placeholder = "(autom\u00e1tico)"),
      shiny::numericInput(ns("leg_size"), "Tamanho do texto", 1, 0.5, 3, 0.1),
      shiny::textInput(ns("leg_order"), "Ordem dos grupos (separados por ;)", placeholder = "A; B; C"),
      shiny::textInput(ns("leg_labels"), "Nomes personalizados (grupo=nome; ...)", placeholder = "T1=Testemunha; T2=Produto")),
    bslib::accordion_panel("Elementos gr\u00e1ficos", icon = shiny::icon("shapes"),
      shiny::fluidRow(shiny::column(6, shiny::numericInput(ns("point_size"), "Tamanho dos pontos", 2.2, 0.2, 10, 0.2)),
                      shiny::column(6, shiny::selectInput(ns("point_shape"), "Formato", c("Circulo" = 21, "Quadrado" = 22, "Losango" = 23, "Triangulo" = 24)))),
      shiny::fluidRow(shiny::column(6, shiny::numericInput(ns("line_width"), "Espessura das linhas", 0.8, 0.1, 4, 0.1)),
                      shiny::column(6, shiny::selectInput(ns("line_type"), "Tipo de linha", c("Cont\u00ednua" = "solid", "Tracejada" = "dashed", "Pontilhada" = "dotted", "Traco-ponto" = "dotdash")))),
      shiny::sliderInput(ns("alpha"), "Transpar\u00eancia dos pontos", 0, 1, 0.55, 0.05),
      shiny::sliderInput(ns("band_alpha"), "Transpar\u00eancia de bandas/elipses", 0, 1, 0.18, 0.02),
      shiny::fluidRow(shiny::column(6, shiny::numericInput(ns("bar_width"), "Largura das barras", 0.62, 0.1, 1, 0.02)),
                      shiny::column(6, shiny::numericInput(ns("errorbar_width"), "Largura das barras de erro", 0.18, 0, 1, 0.02))),
      shiny::fluidRow(shiny::column(6, shiny::numericInput(ns("label_size"), "Tamanho dos r\u00f3tulos", 3.6, 1, 10, 0.2)),
                      shiny::column(6, shiny::numericInput(ns("label_decimals"), "Casas decimais (r\u00f3tulos)", 2, 0, 6, 1))),
      shiny::textInput(ns("colors"), "Cores (hex, separadas por ;)", placeholder = "#315775; #218568; #F28C28"),
      shiny::checkboxInput(ns("show_points"), "Mostrar observa\u00e7\u00f5es individuais", TRUE),
      shiny::checkboxInput(ns("show_letters"), "Mostrar letras dos testes", TRUE),
      shiny::checkboxInput(ns("show_values"), "Mostrar valores / r\u00f3tulos", FALSE)),
    bslib::accordion_panel("Curvas (regress\u00e3o)", icon = shiny::icon("chart-line"),
      shiny::checkboxInput(ns("show_band"), "Mostrar bandas", TRUE),
      shiny::checkboxInput(ns("show_means"), "Mostrar m\u00e9dias por dose", TRUE),
      shiny::selectInput(ns("error_type"), "Barras de erro das m\u00e9dias", c("Erro-padr\u00e3o" = "se", "Desvio-padr\u00e3o" = "sd", "IC 95%" = "ci", "Nenhuma" = "none")),
      shiny::selectInput(ns("eq_position"), "Posi\u00e7\u00e3o das equa\u00e7\u00f5es", c("Superior esquerda" = "topleft", "Superior direita" = "topright", "Inferior esquerda" = "bottomleft", "Inferior direita" = "bottomright", "Ocultar" = "none")),
      shiny::checkboxGroupInput(ns("eq_metrics"), "M\u00e9tricas junto as equa\u00e7\u00f5es", c("R2" = "r2", "p" = "p", "AIC" = "aic", "RMSE" = "rmse", "n" = "n"), c("r2", "n"), inline = TRUE),
      shiny::checkboxInput(ns("show_optimum"), "Marcar dose \u00f3tima / ED50", FALSE)),
    bslib::accordion_panel("Estilos", icon = shiny::icon("floppy-disk"),
      shiny::div(class = "caagro-btn-row",
        shiny::actionButton(ns("reset"), "Restaurar padr\u00e3o", class = "btn-sm btn-outline-secondary"),
        shiny::actionButton(ns("copy_style"), "Copiar estilo", class = "btn-sm btn-outline-primary"),
        shiny::actionButton(ns("paste_style"), "Aplicar estilo copiado", class = "btn-sm btn-outline-primary"),
        shiny::actionButton(ns("show_code"), "C\u00f3digo R", class = "btn-sm btn-outline-primary")),
      shiny::downloadButton(ns("save_style"), "Salvar estilo (.json)", class = "btn-sm"),
      shiny::fileInput(ns("load_style"), "Carregar estilo (.json)", accept = ".json"))
  )
  bslib::layout_sidebar(
    class = "caagro-editor-layout",
    sidebar = bslib::sidebar(acc, width = 330, position = "right", open = sidebar_open(), title = "Editor gr\u00e1fico"),
    bslib::card(full_screen = TRUE, class = "caagro-plot-card",
                shiny::plotOutput(ns("plot"), height = height)),
    bslib::card(bslib::card_header("Exportar gr\u00e1fico"),
      bslib::layout_columns(col_widths = bslib::breakpoints(sm = 6, lg = 4),
        shiny::selectInput(ns("fmt"), "Formato", c("PNG" = "png", "TIFF" = "tiff", "SVG" = "svg", "PDF" = "pdf"), width = "100%"),
        shiny::selectInput(ns("dpi"), "Resolu\u00e7\u00e3o", c("300 dpi" = 300, "600 dpi" = 600, "1200 dpi" = 1200, "150 dpi" = 150), width = "100%"),
        shiny::selectInput(ns("bg"), "Fundo", c("Transparente" = "transparent", "Branco" = "white"), width = "100%"),
        shiny::numericInput(ns("width"), "Largura", 17, 1, 200, width = "100%"),
        shiny::numericInput(ns("height"), "Altura", 12, 1, 200, width = "100%"),
        shiny::selectInput(ns("units"), "Unidade", c("cm", "mm", "in"), width = "100%")),
      shiny::textInput(ns("fname"), "Nome do arquivo", "grafico_caagro", width = "100%"),
      shiny::downloadButton(ns("download"), "Baixar gr\u00e1fico", class = "btn-primary"))
  )
}

axis_controls <- function(ns, ax) {
  p <- function(n) ns(paste0(ax, "_", n))
  shiny::tagList(
    shiny::fluidRow(shiny::column(7, shiny::textInput(p("title"), "T\u00edtulo", placeholder = "(autom\u00e1tico)")),
                    shiny::column(5, shiny::textInput(p("unit"), "Unidade", placeholder = "ex.: kg/ha"))),
    shiny::fluidRow(shiny::column(4, shiny::numericInput(p("min"), "M\u00ednimo", NA)),
                    shiny::column(4, shiny::numericInput(p("max"), "M\u00e1ximo", NA)),
                    shiny::column(4, shiny::numericInput(p("by"), "Intervalo", NA, min = 0))),
    shiny::fluidRow(shiny::column(4, shiny::numericInput(p("dec"), "Decimais", NA, 0, 6, 1)),
                    shiny::column(4, shiny::numericInput(p("angle"), "Rota\u00e7\u00e3o", 0, 0, 90, 15)),
                    shiny::column(4, shiny::numericInput(p("tsize"), "Fonte", 1, 0.5, 3, 0.1))),
    shiny::selectInput(p("trans"), "Escala", c("Linear" = "identity", "Log10" = "log10", "Raiz quadrada" = "sqrt"))
  )
}

parse_kv <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
  parts <- trimws(strsplit(txt, ";", fixed = TRUE)[[1]])
  parts <- parts[grepl("=", parts, fixed = TRUE)]
  if (!length(parts)) return(NULL)
  kv <- strsplit(parts, "=", fixed = TRUE)
  stats::setNames(trimws(vapply(kv, `[`, "", 2)), trimws(vapply(kv, `[`, "", 1)))
}
parse_list <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
  v <- trimws(strsplit(txt, ";", fixed = TRUE)[[1]]); v[nzchar(v)]
}
blank_null <- function(x) if (is.null(x) || !nzchar(trimws(x))) NULL else x
num_na <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) NA else x

style_from_inputs <- function(input) {
  ax <- function(a) axis_spec(title = blank_null(input[[paste0(a, "_title")]]), unit = blank_null(input[[paste0(a, "_unit")]]),
                              min = num_na(input[[paste0(a, "_min")]]), max = num_na(input[[paste0(a, "_max")]]),
                              breaks_by = num_na(input[[paste0(a, "_by")]]), decimals = num_na(input[[paste0(a, "_dec")]]),
                              angle = input[[paste0(a, "_angle")]] %||% 0, text_size = input[[paste0(a, "_tsize")]] %||% 1,
                              trans = input[[paste0(a, "_trans")]] %||% "identity")
  cols <- parse_list(input$colors)
  if (!is.null(cols)) cols <- cols[grepl("^#[0-9A-Fa-f]{6}$|^[a-z]+$", cols)]
  caagro_style(
    theme = input$theme %||% "light", base_size = input$base_size %||% 11,
    title = blank_null(input$title), subtitle = blank_null(input$subtitle), caption = blank_null(input$caption),
    show_title = isTRUE(input$show_title %||% TRUE), title_size = input$title_size %||% 1,
    title_color = blank_null(input$title_color), title_align = as.numeric(input$title_align %||% 0),
    x = ax("x"), y = ax("y"),
    legend = legend_spec(show = isTRUE(input$leg_show %||% TRUE), position = input$leg_pos %||% "right",
                         ncol = num_na(input$leg_ncol), size = input$leg_size %||% 1, title = blank_null(input$leg_title),
                         order = parse_list(input$leg_order), labels = parse_kv(input$leg_labels)),
    geoms = geom_spec(point_size = input$point_size %||% 2.2, point_shape = as.numeric(input$point_shape %||% 21),
                      line_width = input$line_width %||% 0.8, line_type = input$line_type %||% "solid",
                      alpha = input$alpha %||% 0.55, colors = if (length(cols)) cols else NULL,
                      bar_width = input$bar_width %||% 0.62, errorbar_width = input$errorbar_width %||% 0.18,
                      band_alpha = input$band_alpha %||% 0.18, label_size = input$label_size %||% 3.6,
                      label_decimals = input$label_decimals %||% 2, show_letters = isTRUE(input$show_letters %||% TRUE),
                      show_values = isTRUE(input$show_values), show_points = isTRUE(input$show_points %||% TRUE),
                      show_band = isTRUE(input$show_band %||% TRUE), show_means = isTRUE(input$show_means %||% TRUE),
                      error_type = input$error_type %||% "se", eq_position = input$eq_position %||% "topleft",
                      eq_metrics = input$eq_metrics %||% character(), show_optimum = isTRUE(input$show_optimum))
  )
}

update_inputs_from_style <- function(session, st) {
  u_txt <- function(id, v) shiny::updateTextInput(session, id, value = if (is.null(v) || (length(v) == 1 && is.na(v))) "" else paste(v, collapse = "; "))
  u_num <- function(id, v) shiny::updateNumericInput(session, id, value = if (is.null(v)) NA else v)
  shiny::updateSelectInput(session, "theme", selected = st$theme)
  shiny::updateSliderInput(session, "base_size", value = st$base_size)
  shiny::updateCheckboxInput(session, "show_title", value = isTRUE(st$show_title))
  u_txt("title", st$title); u_txt("subtitle", st$subtitle); u_txt("caption", st$caption)
  u_num("title_size", st$title_size); u_txt("title_color", st$title_color %||% "#14283F")
  shiny::updateSelectInput(session, "title_align", selected = as.character(st$title_align))
  for (a in c("x", "y")) {
    s <- st[[a]]
    u_txt(paste0(a, "_title"), s$title); u_txt(paste0(a, "_unit"), s$unit)
    u_num(paste0(a, "_min"), s$min); u_num(paste0(a, "_max"), s$max); u_num(paste0(a, "_by"), s$breaks_by)
    u_num(paste0(a, "_dec"), s$decimals); u_num(paste0(a, "_angle"), s$angle); u_num(paste0(a, "_tsize"), s$text_size)
    shiny::updateSelectInput(session, paste0(a, "_trans"), selected = s$trans)
  }
  l <- st$legend
  shiny::updateCheckboxInput(session, "leg_show", value = isTRUE(l$show)); shiny::updateSelectInput(session, "leg_pos", selected = l$position)
  u_num("leg_ncol", l$ncol); u_txt("leg_title", l$title); u_num("leg_size", l$size); u_txt("leg_order", l$order)
  u_txt("leg_labels", if (length(l$labels)) paste(names(l$labels), l$labels, sep = "=") else NULL)
  g <- st$geoms
  for (n in c("point_size", "line_width", "bar_width", "errorbar_width", "label_size", "label_decimals")) u_num(n, g[[n]])
  shiny::updateSelectInput(session, "point_shape", selected = as.character(g$point_shape))
  shiny::updateSelectInput(session, "line_type", selected = g$line_type)
  shiny::updateSliderInput(session, "alpha", value = g$alpha); shiny::updateSliderInput(session, "band_alpha", value = g$band_alpha)
  u_txt("colors", g$colors)
  for (n in c("show_points", "show_letters", "show_values", "show_band", "show_means", "show_optimum")) shiny::updateCheckboxInput(session, n, value = isTRUE(g[[n]]))
  shiny::updateSelectInput(session, "error_type", selected = g$error_type); shiny::updateSelectInput(session, "eq_position", selected = g$eq_position)
  shiny::updateCheckboxGroupInput(session, "eq_metrics", selected = g$eq_metrics)
}

# result: reactive returning a caagro_result (or NULL)
# extra: optional reactive list of extra args to plot()
mod_plot_editor_server <- function(id, result, extra = shiny::reactive(list())) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    clip <- style_clipboard(session)
    output$type_ui <- shiny::renderUI({
      r <- result(); shiny::req(r)
      shiny::selectInput(ns("type"), "Tipo de gr\u00e1fico", plot_types(r))
    })
    style <- shiny::debounce(shiny::reactive(style_from_inputs(input)), 350)
    plot_obj <- shiny::reactive({
      r <- result(); shiny::req(r)
      tp <- input$type %||% plot_types(r)[[1]]
      if (!tp %in% plot_types(r)) tp <- plot_types(r)[[1]]
      tryCatch(do.call(plot, c(list(r, type = tp, style = style()), extra())),
               error = function(e) {
                 ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0, label = paste("Gr\u00e1fico indispon\u00edvel:", conditionMessage(e)), colour = "#64748B") + ggplot2::theme_void()
               })
    })
    output$plot <- shiny::renderPlot(plot_obj(), res = 110, bg = "transparent")
    shiny::observeEvent(input$reset, update_inputs_from_style(session, caagro_style()))
    shiny::observeEvent(input$copy_style, { clip(style()); shiny::showNotification("Estilo copiado. Use 'Aplicar estilo copiado' em outro gr\u00e1fico.", type = "message") })
    shiny::observeEvent(input$paste_style, {
      st <- clip()
      if (is.null(st)) shiny::showNotification("Nenhum estilo copiado ainda.", type = "warning") else update_inputs_from_style(session, st)
    })
    shiny::observeEvent(input$load_style, {
      st <- safe_run(style_from_json(input$load_style$datapath))
      if (!is.null(st)) update_inputs_from_style(session, st)
    })
    output$save_style <- shiny::downloadHandler(filename = function() "estilo_caagro.json",
                                                content = function(file) style_to_json(style(), file))
    shiny::observeEvent(input$show_code, {
      r <- result(); shiny::req(r)
      code <- paste0(style_code(style()), "\n\ngrafico <- plot(resultado, type = ", deparse(input$type %||% plot_types(r)[[1]]), ", style = estilo)\n",
                     "caagro:::save_plot(grafico, \"", input$fname, ".", input$fmt, "\", width = ", input$width, ", height = ", input$height,
                     ", units = \"", input$units, "\", dpi = ", input$dpi, ", bg = \"", input$bg, "\")")
      shiny::showModal(shiny::modalDialog(title = "C\u00f3digo R do gr\u00e1fico", size = "l", easyClose = TRUE,
                                          shiny::tags$pre(class = "caagro-code", code),
                                          shiny::div(class = "caagro-muted", "Substitua 'resultado' pelo objeto retornado pela fun\u00e7\u00e3o de an\u00e1lise (veja a aba Exporta\u00e7\u00e3o > Script R).")))
    })
    output$download <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_.-]", "_", input$fname %||% "gr\u00e1fico"), ".", input$fmt),
      content = function(file) save_plot(plot_obj(), file, width = input$width, height = input$height, units = input$units,
                                         dpi = as.numeric(input$dpi), bg = input$bg)
    )
    list(style = style, plot = plot_obj, type = shiny::reactive(input$type))
  })
}
