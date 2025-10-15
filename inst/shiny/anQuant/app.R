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
  "ragg",
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
# ============================================================
# Estatística Quanti - Função anQuant
# ============================================================
# Estatistica Quanti
{
  anQuant <- function(resp, resp_exp,
                      medida = c("me", "md"),
                      m = c("linear", "linear2", "Exp", "Log"),
                      y_min = NULL, y_max = NULL,
                      conf.level = 0.95,
                      nsim = 1000,
                      shade_alpha = 0.2,
                      rotulo = 0.5,          # 0..1 controla quão alto fica o rótulo acima do PM
                      fator = NULL,          # vetor/coluna opcional de fator
                      PM = TRUE,             # exibir ponto de máximo
                      fonte = 1,             # << controla tamanhos de texto
                      ...) {

    suppressWarnings({
      library(dplyr)
      library(tibble)
    })

    # garante valor válido p/ fonte e define cex dos pontos
    fonte   <- as.numeric(fonte)
    if (!is.finite(fonte) || fonte <= 0) fonte <- 1
    .cex_pt <- fonte        # pontos dos dados
    .cex_pm <- 1.2 * fonte  # marcador do ponto de máximo (um pouquinho maior)


    medida <- match.arg(medida)
    m      <- match.arg(m)

    # -------------------------------
    # Helpers
    # -------------------------------
    .fit_model <- function(df_modelo, modelo_tipo) {
      # df_modelo: cols .exp, .y
      if (modelo_tipo == "linear") {
        modelo <- lm(.y ~ .exp, data = df_modelo)

      } else if (modelo_tipo == "linear2") {
        modelo <- lm(.y ~ .exp + I(.exp^2), data = df_modelo)

      } else if (modelo_tipo == "Exp") {
        start_list <- list(a = 1, b = 0.1)
        if (all(df_modelo$.y > 0, na.rm = TRUE)) {
          lin0 <- lm(log(.y) ~ .exp, data = df_modelo)
          start_list$b <- unname(coef(lin0)[2])
          start_list$a <- exp(unname(coef(lin0)[1]))
        }
        modelo <- nls(.y ~ a * exp(b * .exp),
                      data = df_modelo, start = start_list,
                      control = nls.control(warnOnly = TRUE))

      } else if (modelo_tipo == "Log") {
        df_modelo$y_adj <- ifelse(df_modelo$.y == 0, 0.01, df_modelo$.y)
        A0  <- max(df_modelo$.y, na.rm = TRUE)
        x00 <- stats::median(df_modelo$.exp, na.rm = TRUE)
        s0  <- 1
        if (all(df_modelo$y_adj > 0 & df_modelo$y_adj < A0, na.rm = TRUE)) {
          y_logit <- log(A0 / df_modelo$y_adj - 1)
          lin0 <- lm(y_logit ~ .exp, data = df_modelo)
          s0  <- -1 / coef(lin0)[2]
          x00 <- -coef(lin0)[1] * s0
        }
        modelo <- nls(.y ~ A / (1 + exp(-( .exp - x0) / s)),
                      data = df_modelo,
                      start = list(A = A0, x0 = x00, s = s0),
                      control = nls.control(warnOnly = TRUE))
      }
      modelo
    }

    .r2s <- function(modelo, y_obs) {
      if (inherits(modelo, "lm")) {
        smry <- summary(modelo)
        c(R2 = unname(smry$r.squared), R2a = unname(smry$adj.r.squared))
      } else {
        rss <- sum(residuals(modelo)^2)
        tss <- sum((y_obs - mean(y_obs))^2)
        n   <- length(y_obs); p <- length(coef(modelo))
        R2  <- if (tss > 0) 1 - rss/tss else NA_real_
        R2a <- if (tss > 0 && n > p) 1 - (rss/(n - p)) / (tss/(n - 1)) else NA_real_
        c(R2 = R2, R2a = R2a)
      }
    }

    .pm <- function(modelo, modelo_tipo, x_lo, x_hi) {
      dentro <- function(x, lo, hi) pmin(pmax(x, lo), hi)

      if (modelo_tipo == "linear") {
        b1 <- unname(coef(modelo)[".exp"])
        x_max <- if (is.na(b1) || b1 >= 0) x_hi else x_lo
        y_max <- as.numeric(predict(modelo, newdata = data.frame(.exp = x_max)))

      } else if (modelo_tipo == "linear2") {
        cf <- coef(modelo)
        a0 <- cf["(Intercept)"]; b1 <- cf[".exp"]; c2 <- cf["I(.exp^2)"]
        if (!is.na(c2) && c2 < 0) {
          xv <- -b1 / (2 * c2)
          x_max <- dentro(xv, x_lo, x_hi)
          y_max <- a0 + b1 * x_max + c2 * x_max^2
        } else {
          cand_x <- c(x_lo, x_hi)
          cand_y <- a0 + b1 * cand_x + c2 * cand_x^2
          idx <- which.max(cand_y)
          x_max <- cand_x[idx]; y_max <- cand_y[idx]
        }

      } else {
        # curvas crescentes típicas (Exp/Logístico): máximo no limite direito
        x_max <- x_hi
        y_max <- as.numeric(predict(modelo, newdata = data.frame(.exp = x_max)))
      }
      list(x_max = as.numeric(x_max), y_max = as.numeric(y_max))
    }

    .ci_bands <- function(modelo, modelo_tipo, x_seq, conf.level, nsim) {
      alpha <- 1 - conf.level
      if (inherits(modelo, "lm")) {
        sefit <- predict(modelo, newdata = data.frame(.exp = x_seq), se.fit = TRUE)
        crit  <- qt(1 - alpha/2, df = df.residual(modelo))
        upper <- sefit$fit + crit * sefit$se.fit
        lower <- sefit$fit - crit * sefit$se.fit
        fit   <- sefit$fit
        list(fit = fit, lower = lower, upper = upper)

      } else {
        # simulação paramétrica via vcov
        co <- coef(modelo)
        V  <- try(vcov(modelo), silent = TRUE)
        if (inherits(V, "try-error")) {
          # fallback simples: usa desvio dos resíduos
          pred <- as.numeric(predict(modelo, newdata = data.frame(.exp = x_seq)))
          res_sd <- stats::sd(residuals(modelo))
          crit   <- qnorm(1 - alpha/2)
          upper  <- pred + crit * res_sd
          lower  <- pred - crit * res_sd
          return(list(fit = pred, lower = lower, upper = upper))
        }
        # garantir matriz positiva
        k <- length(co)
        L <- try(chol(V), silent = TRUE)
        if (inherits(L, "try-error")) {
          eig <- eigen((V + t(V))/2, symmetric = TRUE)
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
          if (modelo_tipo == "Exp") {
            a <- sim_par["a", j]; b <- sim_par["b", j]
            sim_mat[, j] <- a * exp(b * x_seq)
          } else if (modelo_tipo == "Log") {
            A <- sim_par["A", j]; x0 <- sim_par["x0", j]; s <- sim_par["s", j]
            sim_mat[, j] <- A / (1 + exp(-(x_seq - x0) / s))
          }
        }
        pred  <- rowMeans(sim_mat, na.rm = TRUE)
        lower <- apply(sim_mat, 1, quantile, probs = alpha/2, na.rm = TRUE)
        upper <- apply(sim_mat, 1, quantile, probs = 1 - alpha/2, na.rm = TRUE)
        list(fit = pred, lower = lower, upper = upper)
      }
    }

    .draw_band <- function(x, lower, upper, alpha_band) {
      col_band <- rgb(0, 0, 0, alpha = alpha_band)
      polygon(c(x, rev(x)), c(lower, rev(upper)), border = NA, col = col_band)
    }

    # -------------------------------
    # Pré-processamento (com fator)
    # -------------------------------
    if (!is.null(fator)) {
      df <- data.frame(.y = resp, .exp = resp_exp, .fac = factor(fator))
      resumo <- df %>%
        group_by(.fac, .exp) %>%
        summarise(
          media = mean(.y), mediana = median(.y),
          sd = sd(.y), n = n(), se = sd/sqrt(n),
          cv = (sd/mean(.y)) * 100, .groups = "drop"
        )
      y_ref <- if (medida == "me") resumo$media else resumo$mediana

      y_lim_inf <- if (!is.null(y_min)) y_min else min(y_ref, na.rm = TRUE) * 0.95
      y_lim_sup <- if (!is.null(y_max)) y_max else max(y_ref, na.rm = TRUE) * 1.05

      # Paleta e layout
      cores <- grDevices::rainbow(length(unique(resumo$.fac)))
      names(cores) <- levels(resumo$.fac)
      par(mar = c(5, 4, 4, 2) + 0.1)
      opbg <- par(bg = NA); on.exit(par(opbg), add = TRUE)  # permite transparência

      plot(NA, NA,
           xlim = range(resumo$.exp, na.rm = TRUE),
           ylim = c(y_lim_inf, y_lim_sup),
           xlab = "",
           ylab = ifelse(medida == "me", "Média", "Mediana"),
           main = paste("Modelo:", m),
           xaxt = "n",
           cex.axis = fonte,   # <<< tamanhos dos ticks dos eixos
           cex.lab  = fonte,   # <<< rótulos dos eixos
           cex.main = fonte)   # <<< título

      axis(1, at = sort(unique(resumo$.exp)),
           labels = sort(unique(resumo$.exp)),
           cex.axis = fonte)

      params_list <- list()
      pred_list   <- list()

      for (lv in levels(resumo$.fac)) {
        dat_n <- resumo[resumo$.fac == lv, , drop = FALSE]
        if (nrow(dat_n) < 2) next

        y_var   <- if (medida == "me") dat_n$media else dat_n$mediana
        df_mod  <- tibble(.exp = dat_n$.exp, .y = y_var)
        modelo  <- try(.fit_model(df_mod, m), silent = TRUE)
        if (inherits(modelo, "try-error")) next

        # pontos
        points(df_mod$.exp, df_mod$.y, pch = 20, col = cores[lv], cex = .cex_pt)


        # seq + bandas
        x_seq <- seq(min(df_mod$.exp), max(df_mod$.exp), length.out = 300)
        cb <- .ci_bands(modelo, m, x_seq, conf.level, nsim)
        .draw_band(x_seq, cb$lower, cb$upper, shade_alpha)
        lines(x_seq, cb$fit, col = cores[lv], lwd = 2)


        # ponto de máximo
        pm <- .pm(modelo, m, min(df_mod$.exp), max(df_mod$.exp))
        if (isTRUE(PM)) {
          abline(v = pm$x_max, col = cores[lv], lty = 3)
          points(pm$x_max, pm$y_max, col = cores[lv], pch = 21, bg = "white", cex = 1.8)

          # rótulo
          xr <- range(resumo$.exp, na.rm = TRUE); yr <- c(y_lim_inf, y_lim_sup)
          y_lab <- min(pm$y_max + rotulo * diff(yr), y_lim_sup)
          lab <- sprintf("PM=%.2f | X=%.2f", pm$y_max, pm$x_max)
          cex_lab <- 0.8 * fonte   # <<< tamanho do label proporcional à fonte
          tw <- strwidth(lab, cex = cex_lab); th <- strheight(lab, cex = cex_lab)
          pad <- 0.3 * th
          rect(pm$x_max - tw/2 - pad, y_lab - th/2 - pad,
               pm$x_max + tw/2 + pad, y_lab + th/2 + pad,
               col = "gray90", border = "gray70")
          text(pm$x_max, y_lab, labels = lab, cex = cex_lab, col = cores[lv], font = 2)
          segments(pm$x_max, pm$y_max, pm$x_max, y_lab, col = "gray80", lty = 3)
        }

        # acumula saídas
        r2s <- .r2s(modelo, y_var)
        co  <- coef(modelo)
        params_list[[lv]] <- tibble(
          nivel = lv, modelo = m,
          coef = names(co), valor = as.numeric(co),
          R2 = unname(r2s["R2"]), R2_ajustado = unname(r2s["R2a"]),
          x_max = pm$x_max, y_max = pm$y_max,
          Modelo = list(modelo)
        )
        pred_list[[lv]] <- tibble(nivel = lv, .exp = x_seq,
                                  fit = cb$fit, lower = cb$lower, upper = cb$upper)
      }

      box(); grid(col = "gray85")
      legend("topleft", legend = names(cores), col = cores, lwd = 2, bty = "n",
             cex = fonte)  # <<< legenda acompanha fonte

      grafico_record <- recordPlot()
      Tabela     <- resumo %>% mutate(valor = if (medida == "me") media else mediana)
      Parametros <- dplyr::bind_rows(params_list)
      Predicoes  <- dplyr::bind_rows(pred_list)

      return(list(
        Tabela      = Tabela,         # cols: .fac, .exp, valor, ...
        Parametros  = Parametros,     # inclui lista Modelo por nível
        Predicoes   = Predicoes,      # para reconstruir no ggplot
        Grafico     = grafico_record, # recordedplot
        R2          = NA_real_,       # (global não faz sentido com fator)
        R2_ajustado = NA_real_
      ))
    }

    # -------------------------------
    # Sem fator
    # -------------------------------
    df <- data.frame(.y = resp, .exp = resp_exp)
    resumo <- df %>%
      group_by(.exp) %>%
      summarise(
        media = mean(.y), mediana = median(.y),
        sd = sd(.y), n = n(), se = sd/sqrt(n),
        cv = (sd/mean(.y)) * 100, .groups = "drop"
      )
    y_var <- if (medida == "me") resumo$media else resumo$mediana
    df_modelo <- tibble(.exp = resumo$.exp, .y = y_var)

    modelo <- .fit_model(df_modelo, m)
    r2s    <- .r2s(modelo, y_var)

    x_vals <- df_modelo$.exp
    y_vals <- y_var

    y_lim_inf <- if (!is.null(y_min)) y_min else min(y_vals, na.rm = TRUE) * 0.95
    y_lim_sup <- if (!is.null(y_max)) y_max else max(y_vals, na.rm = TRUE) * 1.05

    par(mar = c(5, 4, 4, 2) + 0.1)
    opbg <- par(bg = NA); on.exit(par(opbg), add = TRUE)  # permite transparência

    plot(x_vals, y_vals, pch = 20,
         xlab = "",
         ylab = ifelse(medida == "me", "Média", "Mediana"),
         main = paste("Modelo:", m),
         ylim = c(y_lim_inf, y_lim_sup),
         xaxt = "n",
         cex = .cex_pt,
         cex.axis = fonte,   # <<< eixos
         cex.lab  = fonte,   # <<< rótulos dos eixos
         cex.main = fonte)   # <<< título

    axis(1, at = sort(unique(x_vals)), labels = sort(unique(x_vals)),
         cex.axis = fonte)

    x_seq <- seq(min(x_vals), max(x_vals), length.out = 300)
    cb <- .ci_bands(modelo, m, x_seq, conf.level, nsim)
    .draw_band(x_seq, cb$lower, cb$upper, shade_alpha)
    lines(x_seq, cb$fit, col = "black", lwd = 2)

    pm <- .pm(modelo, m, min(x_vals), max(x_vals))
    if (isTRUE(PM)) {
      abline(v = pm$x_max, col = "darkorange", lty = 3)
      points(pm$x_max, pm$y_max, col = "darkorange", pch = 21, bg = "white", cex = 1.8)

      # rótulo
      xr <- range(x_vals, na.rm = TRUE); yr <- c(y_lim_inf, y_lim_sup)
      y_lab <- min(pm$y_max + rotulo * diff(yr), y_lim_sup)
      lab <- sprintf("PM=%.2f | X=%.2f", pm$y_max, pm$x_max)
      cex_lab <- 0.8 * fonte     # <<< texto interno proporcional à fonte
      tw <- strwidth(lab, cex = cex_lab); th <- strheight(lab, cex = cex_lab)
      pad <- 0.3 * th
      rect(pm$x_max - tw/2 - pad, y_lab - th/2 - pad,
           pm$x_max + tw/2 + pad, y_lab + th/2 + pad,
           col = "gray90", border = "gray70")
      text(pm$x_max, y_lab, labels = lab, cex = cex_lab, col = "gray20", font = 2)
      segments(pm$x_max, pm$y_max, pm$x_max, y_lab, col = "gray80", lty = 3)
    }

    box(); grid(col = "gray85")

    grafico_record <- recordPlot()
    Tabela <- resumo %>% mutate(valor = y_var)
    co <- coef(modelo)
    Parametros <- tibble(
      nivel = NA_character_, modelo = m,
      coef  = names(co), valor = as.numeric(co),
      R2 = unname(r2s["R2"]), R2_ajustado = unname(r2s["R2a"]),
      x_max = pm$x_max, y_max = pm$y_max,
      Modelo = list(modelo)
    )
    Predicoes <- tibble(nivel = NA_character_, .exp = x_seq,
                        fit = cb$fit, lower = cb$lower, upper = cb$upper)

    list(
      Tabela      = Tabela,         # cols: .exp, valor, ...
      Parametros  = Parametros,     # inclui Modelo
      Predicoes   = Predicoes,      # para ggplot
      Grafico     = grafico_record, # recordedplot
      R2          = unname(r2s["R2"]),
      R2_ajustado = unname(r2s["R2a"])
    )
  }




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

            sliderInput("alfa", "Nível de confiança:",
                        min = 0.1, max = 1, step = 0.01,
                        value = 0.95,   # valor inicial
                        ticks = TRUE),   # mostra as marcações
            sliderInput(
              "fonte", "Tamanho da fonte:",
              min = 0, max = 10,
              value = 1.5, step = 0.1
            ),
            checkboxInput("show_max", "Exibir ponto de máximo", value = FALSE),

            numericInput("y_min", "Limite mínimo do eixo y:", value = 0, step = 1),
            numericInput("y_max", "Limite máximo do eixo y:", value = 100, step = 1),

            sliderInput("rotulo", "Posição vertical do rótulo (proporção):",
                        min = 0, max = 10, value = 0.5, step = 0.01),

            fluidRow(
              column(
                4,
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
      PM = input$show_max,
      fonte = input$fonte
    )
  })

  # --- Renderização do gráfico ---
  output$plot_statis <- renderPlot({
    req(resultado())
    obj <- resultado()
    if (is.list(obj) && "Grafico" %in% names(obj)) {
      if (inherits(obj$Grafico, "recordedplot")) {
        grDevices::replayPlot(obj$Grafico)
        return(invisible())
      }
      if (inherits(obj$Grafico, "ggplot")) {
        print(obj$Grafico)
        return(invisible())
      }
    }
    obj
  }, bg = "transparent")  # <- aqui!



  ########
  # --- Download PNG transparente (1000 dpi) ---
  output$downloadPlot <- downloadHandler(
    filename = function() paste0("grafico_", Sys.Date(), ".png"),
    content  = function(file) {

      obj <- try(resultado(), silent = TRUE)

      ragg::agg_png(file, width = 8, height = 6, units = "in",
                    res = 1000, background = "transparent")
      on.exit(dev.off(), add = TRUE)

      if (!inherits(obj, "try-error") && is.list(obj) && "Grafico" %in% names(obj)) {

        if (inherits(obj$Grafico, "recordedplot")) {
          op <- par(bg = NA)              # <- força fundo transparente no base R
          on.exit(par(op), add = TRUE)
          grDevices::replayPlot(obj$Grafico)
          return(invisible())
        }

        if (inherits(obj$Grafico, "ggplot")) {
          gp <- obj$Grafico +
            ggplot2::theme(
              plot.background  = ggplot2::element_rect(fill = NA, colour = NA),
              panel.background = ggplot2::element_rect(fill = NA, colour = NA)
            )
          print(gp)
          return(invisible())
        }
      }

      if (inherits(obj, "ggplot")) {
        gp <- obj +
          ggplot2::theme(
            plot.background  = ggplot2::element_rect(fill = NA, colour = NA),
            panel.background = ggplot2::element_rect(fill = NA, colour = NA)
          )
        print(gp)
        return(invisible())
      }

      plot.new()
      text(0.5, 0.5, "Modelo não disponível ou inválido para plotagem.", cex = 1.1)
    }
  )




  #######










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
