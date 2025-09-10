# Plataforma Validação

# Função para instalar e carregar pacotes
use_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# Lista de pacotes (sem pacotes de imagem: jpeg, pixmap, EBImage)
pacotes <- c(
  "shinycssloaders",
  "xfun",
  "highr",
  "renv",
  "shiny",
  "shinyWidgets",
  "shinythemes",
  "shinydashboard",
  "shinyauthr",
  "tibble",
  "sodium",
  "plotly",
  "dplyr",
  "ggplot2",
  "DT",
  "httr",
  "curl",
  "openxlsx",
  "stringr",
  "broom",
  "agricolae",
  "progress",
  "ScottKnott",
  "writexl",
  "readxl",
  "bslib"
)

# Instala e carrega todos
invisible(lapply(pacotes, use_package))


# Funções estatisticas

# Estatistica Quanti
anQuant <- function(resp, resp_exp,
                    medida = c("me", "md"),
                    m = c("linear", "linear2", "Exp", "Log"),
                    y_min = NULL, y_max = NULL,
                    conf.level = 0.95,
                    nsim = 1000,
                    shade_alpha = 0.2,
                    rotulo = 0.5,
                    fator = NULL,
                    PM = TRUE) {

  suppressWarnings({
    library(dplyr)
    library(tibble)
  })

  medida <- match.arg(medida)
  m <- match.arg(m)

  # --- helper para ajuste ---
  .fit_model <- function(df_modelo, m, conf.level, nsim) {
    y_var <- df_modelo$y
    if (m == "linear") {
      modelo <- lm(y ~ resp_exp, data = df_modelo)
    } else if (m == "linear2") {
      modelo <- lm(y ~ resp_exp + I(resp_exp^2), data = df_modelo)
    } else if (m == "Exp") {
      start_list <- list(a = 1, b = 0.1)
      if (all(y_var > 0)) {
        lin0 <- lm(log(y) ~ resp_exp, data = df_modelo)
        start_list$b <- unname(coef(lin0)[2])
        start_list$a <- exp(unname(coef(lin0)[1]))
      }
      modelo <- nls(y ~ a * exp(b * resp_exp),
                    data = df_modelo, start = start_list,
                    control = nls.control(warnOnly = TRUE))
    } else if (m == "Log") {
      df_modelo$y_adj <- ifelse(df_modelo$y == 0, 0.01, df_modelo$y)
      start_list <- list(A = max(df_modelo$y), x0 = median(df_modelo$resp_exp), s = 1)
      if (all(df_modelo$y_adj > 0 & df_modelo$y_adj < max(df_modelo$y))) {
        y_logit <- log(start_list$A / df_modelo$y_adj - 1)
        lin0 <- lm(y_logit ~ resp_exp, data = df_modelo)
        start_list$s  <- -1 / coef(lin0)[2]
        start_list$x0 <- -coef(lin0)[1] * start_list$s
      }
      modelo <- nls(y ~ A / (1 + exp(-(resp_exp - x0) / s)),
                    data = df_modelo, start = start_list,
                    control = nls.control(warnOnly = TRUE))
    }

    # Métricas
    if (inherits(modelo, "lm")) {
      smry <- summary(modelo)
      R2  <- unname(smry$r.squared)
      R2a <- unname(smry$adj.r.squared)
    } else {
      y_obs <- df_modelo$y
      rss <- sum(residuals(modelo)^2)
      tss <- sum((y_obs - mean(y_obs))^2)
      n   <- length(y_obs)
      p   <- length(coef(modelo))
      R2  <- if (tss > 0) 1 - rss/tss else NA_real_
      R2a <- if (tss > 0 && n > p) 1 - (rss/(n - p)) / (tss/(n - 1)) else NA_real_
    }

    # Ponto máximo
    dentro <- function(x, lo, hi) pmin(pmax(x, lo), hi)
    x_lo <- min(df_modelo$resp_exp); x_hi <- max(df_modelo$resp_exp)

    if (m == "linear") {
      b1 <- coef(modelo)["resp_exp"]
      x_max <- if (is.na(b1) || b1 >= 0) x_hi else x_lo
      y_max <- as.numeric(predict(modelo, newdata = data.frame(resp_exp = x_max)))
    } else if (m == "linear2") {
      cf <- coef(modelo)
      a0 <- cf["(Intercept)"]; b1 <- cf["resp_exp"]; c2 <- cf["I(resp_exp^2)"]
      if (!is.na(c2) && c2 < 0) {
        xv <- -b1 / (2 * c2)
        xv <- dentro(xv, x_lo, x_hi)
        x_max <- xv
        y_max <- a0 + b1 * xv + c2 * xv^2
      } else {
        cand_x <- c(x_lo, x_hi)
        cand_y <- a0 + b1 * cand_x + c2 * cand_x^2
        idx <- which.max(cand_y)
        x_max <- cand_x[idx]; y_max <- cand_y[idx]
      }
    } else {
      x_max <- x_hi
      y_max <- as.numeric(predict(modelo, newdata = data.frame(resp_exp = x_max)))
    }

    list(modelo = modelo, R2 = R2, R2a = R2a,
         x_max = as.numeric(x_max), y_max = as.numeric(y_max))
  }

  # --- caso com fator ---
  if (!is.null(fator)) {
    df <- data.frame(resp = resp, resp_exp = resp_exp, fator = factor(fator))
    resumo <- df %>%
      group_by(fator, resp_exp) %>%
      summarise(
        media   = mean(resp),
        mediana = median(resp),
        sd      = sd(resp),
        n       = n(),
        se      = sd / sqrt(n),
        cv      = (sd / mean(resp)) * 100,
        .groups = "drop"
      )

    lista_niveis <- split(resumo, resumo$fator)
    y_ref <- if (medida == "me") resumo$media else resumo$mediana
    y_lim_inf <- if (!is.null(y_min)) y_min else min(y_ref, na.rm = TRUE) * 0.95
    y_lim_sup <- if (!is.null(y_max)) y_max else max(y_ref, na.rm = TRUE) * 1.05
    x_all <- sort(unique(resumo$resp_exp))

    cores <- grDevices::rainbow(length(lista_niveis))
    names(cores) <- names(lista_niveis)

    plot(NA, NA,
         xlim = range(x_all),
         ylim = c(y_lim_inf, y_lim_sup),
         xlab = "Tempo",
         ylab = ifelse(medida == "me", "Média", "Mediana"),
         main = paste0("Modelo: ", m, " | Curvas por ", deparse(substitute(fator))))

    params_list <- list()
    leg_text <- character(0); leg_col <- character(0)

    for (nm in names(lista_niveis)) {
      dat_n <- lista_niveis[[nm]]
      y_var <- if (medida == "me") dat_n$media else dat_n$mediana
      df_modelo <- tibble(resp_exp = dat_n$resp_exp, y = y_var)

      fit <- try(.fit_model(df_modelo, m, conf.level, nsim), silent = TRUE)

      if (inherits(fit, "try-error")) {
        warning("Não foi possível ajustar o modelo para o nível: ", nm)
        params_list[[nm]] <- tibble(
          nivel = nm, modelo = m,
          coef = NA, valor = NA,
          R2 = NA, R2_ajustado = NA,
          x_max = NA, y_max = NA,
          Modelo = list(NA)
        )
        next
      }

      modelo <- fit$modelo
      points(df_modelo$resp_exp, df_modelo$y, pch = 20, col = cores[nm])
      x_seq <- seq(min(df_modelo$resp_exp), max(df_modelo$resp_exp), length.out = 300)
      pred  <- suppressWarnings(predict(modelo, newdata = data.frame(resp_exp = x_seq)))
      lines(x_seq, pred, col = cores[nm], lwd = 2)

      x_max <- fit$x_max; y_max <- fit$y_max
      if (!is.null(PM) && isTRUE(PM)) {
        abline(v = x_max, col = cores[nm], lwd = 2, lty = 3)
        points(x_max, y_max, col = cores[nm], pch = 1, lwd = 2, cex = 1.8)

        # Rótulo em duas linhas
        label_txt <- c(
          paste0("PM = ", formatC(y_max, format = "f", digits = 2)),
          paste0("X  = ", formatC(x_max, format = "f", digits = 2))
        )
        y_lab <- min(y_max + 0.07 * (y_lim_sup - y_lim_inf),
                     y_lim_sup - 0.02 * (y_lim_sup - y_lim_inf))

        cex_lab <- 0.65
        line_h  <- strheight("A", cex = cex_lab) * 1.2
        tw <- max(strwidth(label_txt, cex = cex_lab))

        rect(x_max - tw/2 - 0.2, y_lab - line_h,
             x_max + tw/2 + 0.2, y_lab + line_h,
             col = "white", border = "gray80")

        text(x_max, y_lab,         labels = label_txt[1], col = cores[nm],
             cex = cex_lab, font = 2)
        text(x_max, y_lab - line_h, labels = label_txt[2], col = cores[nm],
             cex = cex_lab, font = 2)
      }

      co <- coef(modelo)
      params_list[[nm]] <- tibble(
        nivel = nm,
        modelo = m,
        coef  = names(co),
        valor = as.numeric(co),
        R2 = fit$R2,
        R2_ajustado = fit$R2a,
        x_max = x_max,
        y_max = y_max,
        Modelo = list(modelo)
      )

      leg_text <- c(leg_text, nm)
      leg_col  <- c(leg_col, cores[nm])
    }

    box(); grid(col = "gray80")
    if (length(leg_text) > 0) {
      legend("topleft", legend = leg_text, col = leg_col, lwd = 2, bty = "n")
    }

    grafico_record <- recordPlot()
    Tabela <- resumo %>% mutate(valor = if (medida == "me") media else mediana)
    Parametros <- dplyr::bind_rows(params_list)

    return(list(
      Tabela      = Tabela,
      Parametros  = Parametros,
      Grafico     = grafico_record
    ))
  }

  # --- caso sem fator ---
  df <- data.frame(resp = resp, resp_exp = resp_exp)
  resumo <- df %>%
    group_by(resp_exp) %>%
    summarise(
      media   = mean(resp),
      mediana = median(resp),
      sd      = sd(resp),
      n       = n(),
      se      = sd / sqrt(n),
      cv      = (sd / mean(resp)) * 100,
      .groups = "drop"
    )

  y_var <- if (medida == "me") resumo$media else resumo$mediana
  df_modelo <- tibble(resp_exp = resumo$resp_exp, y = y_var)

  fit <- .fit_model(df_modelo, m, conf.level, nsim)
  modelo <- fit$modelo

  x_vals <- df_modelo$resp_exp
  y_vals <- y_var
  y_lim_inf <- if (!is.null(y_min)) y_min else min(y_vals) * 0.95
  y_lim_sup <- if (!is.null(y_max)) y_max else max(y_vals) * 1.05

  plot(x_vals, y_vals, pch = 20,
       xlab = "Tempo", ylab = ifelse(medida == "me", "Média", "Mediana"),
       main = paste("Modelo:", m), ylim = c(y_lim_inf, y_lim_sup))
  x_seq <- seq(min(x_vals), max(x_vals), length.out = 300)
  pred  <- suppressWarnings(predict(modelo, newdata = data.frame(resp_exp = x_seq)))
  lines(x_seq, pred, col = "black", lwd = 2)

  if (!is.null(PM) && isTRUE(PM)) {
    x_max <- fit$x_max; y_max <- fit$y_max
    abline(v = x_max, col = "black", lwd = 2, lty = 3)
    points(x_max, y_max, col = "black", pch = 1, lwd = 2, cex = 1.8)

    label_txt <- c(
      paste0("PM = ", formatC(y_max, format = "f", digits = 2)),
      paste0("X  = ", formatC(x_max, format = "f", digits = 2))
    )
    y_lab <- min(y_max + 0.07 * (y_lim_sup - y_lim_inf),
                 y_lim_sup - 0.02 * (y_lim_sup - y_lim_inf))

    cex_lab <- 0.65
    line_h  <- strheight("A", cex = cex_lab) * 1.2
    tw <- max(strwidth(label_txt, cex = cex_lab))

    rect(x_max - tw/2 - 0.2, y_lab - line_h,
         x_max + tw/2 + 0.2, y_lab + line_h,
         col = "white", border = "gray80")

    text(x_max, y_lab,         labels = label_txt[1], col = "black",
         cex = cex_lab, font = 2)
    text(x_max, y_lab - line_h, labels = label_txt[2], col = "black",
         cex = cex_lab, font = 2)
  }

  grafico_record <- recordPlot()
  resumo <- resumo %>% mutate(valor = y_var)
  co <- coef(modelo)
  params <- tibble(
    nivel = NA_character_, modelo = m,
    coef  = names(co), valor = as.numeric(co),
    R2 = fit$R2, R2_ajustado = fit$R2a,
    x_max = fit$x_max, y_max = fit$y_max,
    Modelo = list(modelo)
  )

  list(
    Tabela      = resumo,
    Parametros  = params,
    Grafico     = grafico_record
  )
}


# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  withMathJax(),  # Habilita renderização de LaTeX
  uiOutput("app_ui")
)

# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  withMathJax(),  # Habilita renderização de LaTeX
  uiOutput("app_ui")
)

# SERVER -----------------------------------------------------------------------
server <- function(input, output, session) {

  # UI Dinâmico com abas
  output$app_ui <- renderUI({
    tabs <- list(
      tabPanel(
        "Quantitativo", icon = icon("chart-bar"),
        sidebarLayout(
          sidebarPanel(
            fileInput("file1", label = NULL, accept = c(".csv", ".xlsx")),
            uiOutput("column1"),
            uiOutput("column2"),
            uiOutput("column3"),
            selectInput("medida", "Tipo de medida:",
                        choices = c("Média" = "me", "Mediana" = "md")),

            selectInput("modelo", "Modelo:",
                        choices = c("Linear" = "linear",
                                    "Quadrático" = "linear2",
                                    "Exponencial" = "Exp",
                                    "Logístico" = "Log")),

            selectInput("alfa", "Nível de confiança:",
                        choices = c("1%" = 0.01, "5%" = 0.05,
                                    "10%" = 0.10, "15%" = 0.15),
                        selected = 0.05),
            checkboxInput("show_max", "Exibir ponto de máximo", value = FALSE),

            numericInput("y_min", "Limite mínimo do eixo y:", value = 0, step = 1),
            numericInput("y_max", "Limite máximo do eixo y:", value = 100, step = 1),

            sliderInput("rotulo", "Posição vertical do rótulo (proporção):",
                        min = 0, max = 10, value = 0.5, step = 0.01),

            fluidRow(
              column(4,
                     downloadButton("downloadPlot", "Gráfico",
                                    style = "padding:2px 6px; font-size:12px;")
              ),
              column(4,
                     downloadButton("baixar_excel_st", "Resumo",
                                    style = "padding:2px 6px; font-size:12px;")
              ),
              column(4,
                     downloadButton("baixar_excel_param", "Parâmetros",
                                    style = "padding:2px 6px; font-size:12px;")
              )
            )



          ),
          mainPanel(
            plotOutput("plot_statis", height = "600px"),
            hr(),
            # Estilos das info-boxes
            tags$head(
              tags$style(HTML("
      .info-box {
        background-color: #f9f9f9;
        border-radius: 8px;
        padding: 15px;
        text-align: center;
        margin-bottom: 15px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        height: auto;
      }
      .info-box h3 { font-size: 16px; margin: 0; font-weight: bold; }
      .info-box p { font-size: 14px; color: #666; margin: 5px 0 0; }
      .info-box .separator { border-top: 1px solid #ccc; margin: 10px 0; }
      .info-box .number { font-size: calc(1vw + 8px); font-weight: bold; }
    "))
            ),

            fluidRow(
              column(12, div(class = "info-box", uiOutput("Coeficientes"),
                             div(class="separator"), p("Equação Ajustada")))
            ),
            hr(),
            DTOutput("table_results"),
            hr(),
            DTOutput("table_parametros"),
            br(), br()  # espaço extra abaixo da segunda tabela
          )

        )
      ),
      tabPanel(
        "Sobre", icon = icon("info-circle"),
        tags$div(
          style = "text-align:center; margin-top:20px;",
          tags$h3("Estatística de Forma Simples", style="color:black; font-weight:bold;")
        ),
        tags$div(
          style = "text-align:justify; margin:20px; font-size:16px; color:black;",
          HTML("O <strong>ESTATÍSTICAS</strong> fornece análises quantitativas
               com ajuste de modelos (linear, quadrático, exponencial e logístico),
               cálculo de intervalos de confiança e visualização dos resultados
               de forma simples e direta.")
        ),
        tags$hr(style = "border-top: 2px solid black;"),
        tags$div(
          style = "text-align:center; margin-top:50px;",
          tags$style(HTML("
            .foto-zoom { transition: transform 0.3s ease; border-radius: 100%; }
            .foto-zoom:hover { transform: scale(1.8); }
          ")),
          tags$img(src = "wilhan.png", height = "150px", width = "150px", class = "foto-zoom")
        ),
        tags$div(
          style = "text-align:center; margin-top:10px;",
          tags$h4("Wilhan Valasco", style="color:black; font-weight:bold;")
        ),
        tags$div(
          style = "text-align:center; margin-top:15px; font-size:15px; font-family:'Courier New', monospace;",
          tags$p(HTML("<b>Email:</b> wilhanvalasco@hotmail.com"), style="color:black;"),
          tags$p(HTML("<b>Site:</b> <a href='https://wilhanvalasco.github.io/meu-site/' target='_blank'>Meu Site</a>"), style="color:black;"),
          tags$p(HTML("<b>LinkedIn:</b> <a href='https://www.linkedin.com/in/wilhan-valasco/' target='_blank'>linkedin.com/in/wilhan-valasco</a>"), style="color:black;"),
          tags$a(
            href = "https://wa.me/64992862039", target="_blank",
            tags$img(
              src = "https://png.pngtree.com/png-vector/20221018/ourmid/pngtree-whatsapp-mobile-software-icon-png-image_6315991.png",
              height = "30px", width = "30px", style="margin-top:10px;"
            )
          )
        ),
        tags$hr()
      )
    )
    do.call(tabsetPanel, tabs)
  })

  # --- Parte de Dados ---
  data <- reactive({
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    switch(ext,
           csv = read.csv2(input$file1$datapath, stringsAsFactors = FALSE),
           xlsx = readxl::read_excel(input$file1$datapath),
           validate("Formato de arquivo não suportado."))
  })

  observe({
    req(data())
    cols <- colnames(data())
    output$column1 <- renderUI(selectInput("col1", "Selecionar Variável Explicativa", choices = cols))
    output$column2 <- renderUI(selectInput("col2", "Selecionar Variável Resposta", choices = cols))
    output$column3 <- renderUI({
      selectInput("col3", "Selecionar Fator", choices = c("", cols))
    })
  })

  df_proc <- reactive({
    req(data(), input$col1, input$col2)
    df <- data()

    out <- data.frame(
      x = as.numeric(df[[input$col1]]),
      y = as.numeric(df[[input$col2]])
    )

    if (!is.null(input$col3) && input$col3 != "") {
      out$f <- df[[input$col3]]
    } else {
      out$f <- NULL
    }

    out
  })

  resultado <- reactive({
    df <- df_proc()
    anQuant(
      resp = df$y,
      resp_exp = df$x,
      medida = input$medida,
      m = input$modelo,
      y_min = input$y_min,
      y_max = input$y_max,
      conf.level = 1 - as.numeric(input$alfa),
      nsim = 5000,
      rotulo = input$rotulo,
      fator = df$f,
      PM = input$show_max
    )
  })

  # --- Renderização do gráfico ---
  output$plot_statis <- renderPlot({
    req(resultado())
    if ("Grafico" %in% names(resultado())) {
      print(resultado()$Grafico)
    } else {
      resultado()
    }
  })

  # --- Download do gráfico ---
  output$downloadPlot <- downloadHandler(
    filename = function() paste0("grafico_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1600, height = 1200, res = 300, bg = "transparent")
      on.exit(dev.off())
      if ("Grafico" %in% names(resultado())) {
        print(resultado()$Grafico)
      } else {
        resultado()
      }
    }
  )

  # --- Tabela Resumo ---
  output$table_results <- renderDT({
    req(resultado())
    tabela <- resultado()$Tabela
    tabela[] <- lapply(tabela, function(col) {
      if (is.numeric(col)) round(col, 2) else col
    })
    datatable(tabela, options =  list(
      searching = FALSE, paging = FALSE, info = FALSE, lengthChange = FALSE,
      columnDefs = list(list(className = 'dt-center', targets = '_all'))
    ), rownames = FALSE, class = 'cell-border stripe')
  })

  # --- Tabela Parametros ---
  output$table_parametros <- renderDT({
    req(resultado())
    tabela <- resultado()$Parametros
    tabela[] <- lapply(tabela, function(col) {
      if (is.numeric(col)) round(col, 3) else col
    })
    datatable(tabela, options = list(
      searching = FALSE, paging = FALSE, info = FALSE, lengthChange = FALSE,
      scrollX = TRUE,
      scrollY = "400px",   # altura fixa
      dom = 't',
      columnDefs = list(list(className = 'dt-center', targets = '_all'))
    ), rownames = FALSE, class = 'cell-border stripe')
  })

  # --- Info box ---
  output$R2 <- renderText({ req(resultado()); formatC(resultado()$R2, 3, format = "f") })
  output$R2adj <- renderText({ req(resultado()); formatC(resultado()$R2_ajustado, 3, format = "f") })

  output$ANOVA_F <- renderText({
    mod <- resultado()$Modelo
    if (inherits(mod, "lm")) formatC(anova(mod)$`F value`[1], 3) else "-"
  })
  output$ANOVA_p <- renderText({
    mod <- resultado()$Modelo
    if (inherits(mod, "lm")) formatC(anova(mod)$`Pr(>F)`[1], 3) else "-"
  })

  # --- Coeficientes como equação em LaTeX ---
  # --- Equações genuínas em LaTeX ---
  output$Coeficientes <- renderUI({
    eq <- NULL

    if (input$modelo == "linear") {
      eq <- "$$y = \\beta_0 + \\beta_1 x$$"

    } else if (input$modelo == "linear2") {
      eq <- "$$y = \\beta_0 + \\beta_1 x + \\beta_2 x^2$$"

    } else if (input$modelo == "Exp") {
      eq <- "$$y = \\alpha \\cdot e^{\\beta x}$$"

    } else if (input$modelo == "Log") {
      eq <- "$$y = \\frac{A}{1 + e^{-\\frac{(x - x_0)}{s}}}$$"
    }

    withMathJax(HTML(eq))
  })


  # --- Download Excel Resumo ---
  output$baixar_excel_st <- downloadHandler(
    filename = function() paste0("tabela_resumo_", Sys.Date(), ".xlsx"),
    content = function(file) { writexl::write_xlsx(resultado()$Tabela, file) }
  )

  # --- Download Excel Parametros ---
  output$baixar_excel_param <- downloadHandler(
    filename = function() paste0("tabela_parametros_", Sys.Date(), ".xlsx"),
    content = function(file) { writexl::write_xlsx(resultado()$Parametros, file) }
  )
}

# RUN --------------------------------------------------------------------------
shinyApp(ui, server)
