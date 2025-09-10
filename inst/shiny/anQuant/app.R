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
{
  anQuant <- function(resp, resp_exp,
                      medida = c("me", "md"),
                      m = c("linear", "linear2", "Exp", "Log"),
                      y_min = NULL, y_max = NULL,
                      conf.level = 0.95,
                      nsim = 1000,
                      shade_alpha = 0.2,
                      rotulo = 0.5) {

    suppressWarnings({
      library(dplyr)
      library(tibble)
    })

    medida <- match.arg(medida)
    m <- match.arg(m)

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
                    data = df_modelo,
                    start = start_list,
                    control = nls.control(warnOnly = TRUE))
    } else if (m == "Log") {
      df_modelo$y_adj <- ifelse(df_modelo$y == 0, 0.01, df_modelo$y)
      start_list <- list(A = max(df_modelo$y), x0 = median(df_modelo$resp_exp), s = 1)
      if (all(df_modelo$y_adj > 0 & df_modelo$y_adj < max(df_modelo$y))) {
        y_logit <- log(start_list$A / df_modelo$y_adj - 1)
        lin0 <- lm(y_logit ~ resp_exp, data = df_modelo)
        start_list$s <- -1 / coef(lin0)[2]
        start_list$x0 <- -coef(lin0)[1] * start_list$s
      }
      modelo <- nls(y ~ A / (1 + exp(-(resp_exp - x0) / s)),
                    data = df_modelo,
                    start = start_list,
                    control = nls.control(warnOnly = TRUE))
    }

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

    x_vals <- df_modelo$resp_exp
    y_vals <- y_var
    y_lim_inf <- if (!is.null(y_min)) y_min else min(y_vals) * 0.95
    y_lim_sup <- if (!is.null(y_max)) y_max else max(y_vals) * 1.05

    # gráfico base com fundo transparente
    plot(x_vals, y_vals,
         pch = 20,
         xlab = "",
         ylab = ifelse(medida == "me", "Média", "Mediana"),
         main = paste("Modelo:", m),
         ylim = c(y_lim_inf, y_lim_sup),
         bg = "transparent")   # <<< fundo transparente


    x_seq <- seq(min(x_vals), max(x_vals), length.out = 300)
    pred  <- suppressWarnings(predict(modelo, newdata = data.frame(resp_exp = x_seq)))
    lines(x_seq, pred, col = "black", lwd = 2)

    alpha <- 1 - conf.level
    draw_polygon_band <- function(x, lower, upper) {
      col_band <- rgb(0, 0, 0, alpha = shade_alpha)
      polygon(c(x, rev(x)), c(lower, rev(upper)),
              border = NA, col = col_band)
    }

    if (inherits(modelo, "lm")) {
      # Tenta usar confbands::confbands() se o pacote existir; caso contrário, fallback manual
      if (requireNamespace("confbands", quietly = TRUE)) {
        ok <- isTRUE(try({
          confbands(
            modelo,
            newdata = data.frame(resp_exp = seq(min(x_vals), max(x_vals), length.out = 200)),
            add = TRUE, conf.level = conf.level, col = "gray40"
          )
          TRUE
        }, silent = TRUE))
        if (!ok) {
          sefit <- predict(modelo, newdata = data.frame(resp_exp = x_seq), se.fit = TRUE)
          crit  <- qt(1 - alpha/2, df = df.residual(modelo))
          upper <- sefit$fit + crit * sefit$se.fit
          lower <- sefit$fit - crit * sefit$se.fit
          draw_polygon_band(x_seq, lower, upper)
        }
      } else {
        sefit <- predict(modelo, newdata = data.frame(resp_exp = x_seq), se.fit = TRUE)
        crit  <- qt(1 - alpha/2, df = df.residual(modelo))
        upper <- sefit$fit + crit * sefit$se.fit
        lower <- sefit$fit - crit * sefit$se.fit
        draw_polygon_band(x_seq, lower, upper)
      }
    } else {
      co <- coef(modelo)
      V  <- try(vcov(modelo), silent = TRUE)
      if (inherits(V, "try-error")) {
        res_sd <- sd(residuals(modelo))
        crit   <- qnorm(1 - alpha/2)
        upper  <- pred + crit * res_sd
        lower  <- pred - crit * res_sd
        draw_polygon_band(x_seq, lower, upper)
      } else {
        k <- length(co)
        L <- try(chol(V), silent = TRUE)
        if (inherits(L, "try-error")) {
          eig <- eigen((V + t(V)) / 2, symmetric = TRUE)
          eig$values[eig$values < 0] <- 0
          L <- t(eig$vectors %*% diag(sqrt(eig$values)))
        }
        z <- matrix(rnorm(k * nsim), nrow = k, ncol = nsim)
        sim_par <- matrix(co, nrow = k, ncol = nsim)
        sim_par <- sim_par + L %*% z
        rownames(sim_par) <- names(co)
        nx <- length(x_seq)
        sim_mat <- matrix(NA_real_, nrow = nx, ncol = nsim)
        for (j in seq_len(nsim)) {
          if (m == "Exp") {
            a <- sim_par["a", j]; b <- sim_par["b", j]
            sim_mat[, j] <- a * exp(b * x_seq)
          } else if (m == "Log") {
            A <- sim_par["A", j]; x0 <- sim_par["x0", j]; s <- sim_par["s", j]
            sim_mat[, j] <- A / (1 + exp(-(x_seq - x0) / s))
          }
        }
        lower <- apply(sim_mat, 1, quantile, probs = alpha/2, na.rm = TRUE)
        upper <- apply(sim_mat, 1, quantile, probs = 1 - alpha/2, na.rm = TRUE)
        draw_polygon_band(x_seq, lower, upper)
      }
    }

    box()
    grid(col = "gray80")
    resumo <- resumo %>% mutate(valor = y_var)

    dentro <- function(x, lo, hi) pmin(pmax(x, lo), hi)
    x_lo <- min(x_vals); x_hi <- max(x_vals)

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

    abline(v = as.numeric(x_max), col = "darkorange", lwd = 2, lty = 3)
    points(as.numeric(x_max), as.numeric(y_max),
           col = "darkorange", lwd = 2, pch = 1, cex = 2.5)

    label_txt <- paste0("PM = ", formatC(y_max, format = "f", digits = 2))
    xr <- range(x_vals); yr <- c(y_lim_inf, y_lim_sup)
    dx <- 0.00 * diff(xr)
    dy <- 0.1 * diff(yr)

    x_lab <- as.numeric(x_max) + dx
    y_lab <- as.numeric(y_max) + dy
    y_lab <- min(y_lab, y_lim_sup - rotulo * diff(yr))

    cex_lab <- 0.7
    tw <- strwidth(label_txt, cex = cex_lab)
    th <- strheight(label_txt, cex = cex_lab)
    pad_x <- 0.4 * th
    pad_y <- 0.35 * th

    x0 <- x_lab - tw/2 - pad_x
    x1 <- x_lab + tw/2 + pad_x
    y0 <- y_lab - th/2 - pad_y
    y1 <- y_lab + th/2 + pad_y

    op <- par(xpd = NA); on.exit(par(op), add = TRUE)
    segments(as.numeric(x_max), as.numeric(y_max),
             x_lab, y_lab, col = "gray90", lty = 3)
    rect(x0, y0, x1, y1, col = "white", border = "gray90")
    text(x_lab, y_lab, labels = label_txt, cex = cex_lab, col = "gray20")

    # >>> grava o gráfico para poder redesenhar no download
    grafico_record <- recordPlot()

    return(list(
      Tabela       = resumo,
      R2           = R2,
      R2_ajustado  = R2a,
      Maximo       = list(x = as.numeric(x_max), y = as.numeric(y_max), modelo = m),
      Modelo       = modelo,
      Grafico      = grafico_record
    ))
  }



}



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

            numericInput("y_min", "Limite mínimo do eixo y:", value = 0, step = 1),
            numericInput("y_max", "Limite máximo do eixo y:", value = 100, step = 1),

            sliderInput("rotulo", "Posição vertical do rótulo (proporção):",
                        min = 0, max = 1, value = 0.5, step = 0.01),

            downloadButton("downloadPlot", "Salvar Gráfico"),
            tags$hr(),
            downloadButton("baixar_excel_st", "Salvar Excel")
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
              column(3, div(class = "info-box", h3(textOutput("R2")), div(class="separator"), p("R²"))),
              column(3, div(class = "info-box", h3(textOutput("R2adj")), div(class="separator"), p("R² Ajustado"))),
              column(3, div(class = "info-box", h3(textOutput("ANOVA_F")), div(class="separator"), p("F-ANOVA"))),
              column(3, div(class = "info-box", h3(textOutput("ANOVA_p")), div(class="separator"), p("p-Valor ANOVA")))
            ),
            hr(),
            fluidRow(
              column(12, div(class = "info-box", uiOutput("Coeficientes"),
                             div(class="separator"), p("Equação Ajustada")))
            ),
            hr(),
            DTOutput("table_results")
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
           xlsx = read_excel(input$file1$datapath),
           validate("Formato de arquivo não suportado."))
  })

  observe({
    req(data())
    cols <- colnames(data())
    output$column1 <- renderUI(selectInput("col1", "Selecionar Variável Explicativa", choices = cols))
    output$column2 <- renderUI(selectInput("col2", "Selecionar Variável Resposta", choices = cols))
  })

  df_proc <- reactive({
    req(data(), input$col1, input$col2)
    df <- data()
    data.frame(
      x = as.numeric(df[[input$col1]]),
      y = as.numeric(df[[input$col2]])
    )
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
      rotulo = input$rotulo
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
      # fundo transparente
      png(file, width = 1600, height = 1200, res = 300, bg = "transparent")
      on.exit(dev.off())
      if ("Grafico" %in% names(resultado())) {
        print(resultado()$Grafico)
      } else {
        resultado()
      }
    }
  )


  # --- Tabela ---
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
  output$Coeficientes <- renderUI({
    req(resultado())
    mod <- resultado()$Modelo
    cf <- coef(mod)
    eq <- NULL

    if (input$modelo == "linear") {
      a <- formatC(cf[1], digits = 3, format = "f")
      b <- formatC(cf[2], digits = 3, format = "f")
      eq <- paste0("$$y = ", a, " + ", b, " \\cdot x$$")

    } else if (input$modelo == "linear2") {
      a <- formatC(cf[1], digits = 3, format = "f")
      b <- formatC(cf[2], digits = 3, format = "f")
      c <- formatC(cf[3], digits = 3, format = "f")
      eq <- paste0("$$y = ", a, " + ", b, " \\cdot x + ", c, " \\cdot x^{2}$$")

    } else if (input$modelo == "Exp") {
      a <- formatC(cf[1], digits = 3, format = "f")
      b <- formatC(cf[2], digits = 3, format = "f")
      eq <- paste0("$$y = ", a, " \\cdot e^{", b, " \\cdot x}$$")

    } else if (input$modelo == "Log") {
      A  <- formatC(cf["A"],  digits = 3, format = "f")
      x0 <- formatC(cf["x0"], digits = 3, format = "f")
      s  <- formatC(cf["s"],  digits = 3, format = "f")
      eq <- paste0("$$y = \\frac{", A, "}{1 + e^{-(x - ", x0, ")/", s, "}}$$")
    }

    withMathJax(HTML(eq))
  })

  # --- Download Excel ---
  output$baixar_excel_st <- downloadHandler(
    filename = function() paste0("tabela_resultado_", Sys.Date(), ".xlsx"),
    content = function(file) { writexl::write_xlsx(resultado()$Tabela, file) }
  )
}

# RUN --------------------------------------------------------------------------
shinyApp(ui, server)
