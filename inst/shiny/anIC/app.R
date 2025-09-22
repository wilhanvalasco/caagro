# ============================================================
# App Shiny - Gráfico com plotIC e UI dinâmica
# ============================================================
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
{
  # ---- Remover out
  remove_outliers <- function(data, var_exp, var_resp, bloco = NULL) {
    require(dplyr); require(rlang)
    if (!is.character(var_exp))  var_exp  <- deparse(substitute(var_exp))
    if (!is.character(var_resp)) var_resp <- deparse(substitute(var_resp))
    if (!is.null(bloco) && !is.character(bloco)) bloco <- deparse(substitute(bloco))

    if (is.null(bloco)) {
      limites <- data %>%
        group_by(!!sym(var_exp)) %>%
        summarise(Q1 = quantile(.data[[var_resp]], 0.25, na.rm = TRUE),
                  Q3 = quantile(.data[[var_resp]], 0.75, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(IQR = Q3 - Q1, L = Q1 - 1.5*IQR, U = Q3 + 1.5*IQR)

      df <- data %>%
        left_join(limites, by = var_exp) %>%
        filter(.data[[var_resp]] >= L, .data[[var_resp]] <= U) %>%
        select(-Q1, -Q3, -IQR, -L, -U)
    } else {
      limites <- data %>%
        group_by(!!sym(var_exp), !!sym(bloco)) %>%
        summarise(Q1 = quantile(.data[[var_resp]], 0.25, na.rm = TRUE),
                  Q3 = quantile(.data[[var_resp]], 0.75, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(IQR = Q3 - Q1, L = Q1 - 1.5*IQR, U = Q3 + 1.5*IQR)

      df <- data %>%
        left_join(limites, by = c(var_exp, bloco)) %>%
        filter(.data[[var_resp]] >= L, .data[[var_resp]] <= U) %>%
        select(-Q1, -Q3, -IQR, -L, -U)
    }
    return(df)
  }

  # ---- Plotar IC
  plotIC <- function(data,
                     variavel_exp,
                     variavel_resp,
                     bloco = NULL,
                     conf.level = 0.95,
                     ordered = TRUE, var.equal = TRUE,
                     col = "skyblue", trans = 0.2,
                     ymin = NULL, ymax = NULL,
                     angulo = 0,
                     font = "serif",
                     salve = FALSE,
                     file = "grafico.png",
                     outlier = TRUE) {

    # --------------------------
    # 0) Preparos e validações
    # --------------------------
    variavel_exp  <- trimws(variavel_exp)
    variavel_resp <- trimws(variavel_resp)
    if (!is.null(bloco)) bloco <- trimws(bloco)

    if (!(variavel_exp %in% names(data)))  stop("❌ Coluna experimental não encontrada: ", variavel_exp)
    if (!(variavel_resp %in% names(data))) stop("❌ Coluna resposta não encontrada: ", variavel_resp)
    if (!is.null(bloco) && !(bloco %in% names(data))) stop("❌ Coluna bloco não encontrada: ", bloco)

    df <- as.data.frame(data, check.names = FALSE)

    # --------------------------
    # 1) Renomear internamente (robusto a nomes 'feios')
    # --------------------------
    df$.resp <- suppressWarnings(as.numeric(df[[variavel_resp]]))
    df <- df[is.finite(df$.resp), , drop = FALSE]
    df$.trat <- as.factor(df[[variavel_exp]])
    tem_bloco <- !is.null(bloco)
    if (tem_bloco) {
      df$.block <- as.factor(df[[bloco]])
      if (nlevels(df$.block) < 2) tem_bloco <- FALSE
    }

    if (nlevels(df$.trat) < 2) stop("❌ Variável experimental precisa ter pelo menos 2 níveis.")

    # --------------------------
    # 2) Outliers (se houver função disponível)
    # --------------------------
    if (!outlier && exists("remove_outliers", mode = "function")) {
      df <- tryCatch(
        remove_outliers(df, variavel_exp = ".trat", variavel_resp = ".resp", bloco = if (tem_bloco) ".block" else NULL),
        error = function(e) df
      )
    }

    # --------------------------
    # 3) ANOVA e indicadores
    # --------------------------
    modelo <- if (tem_bloco) {
      aov(.resp ~ .block + .trat, data = df)
    } else {
      aov(.resp ~ .trat, data = df)
    }
    resumo_anova <- summary(modelo)

    QMres <- resumo_anova[[1]]["Residuals", "Mean Sq"]
    media_geral <- mean(df$.resp, na.rm = TRUE)
    CV <- 100 * sqrt(QMres) / media_geral

    bartlett <- tryCatch(
      bartlett.test(x = df$.resp, g = df$.trat),
      error = function(e) NULL
    )

    # --------------------------
    # 4) Médias e IC por tratamento
    # --------------------------
    groups <- split(df$.resp, df$.trat)
    level_names <- names(groups)
    out <- data.frame(
      levels  = level_names,
      LSmeans = sapply(groups, function(x) mean(x, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
    n <- sapply(groups, function(x) sum(!is.na(x)))
    alpha <- 1 - conf.level
    out$LCL <- NA_real_
    out$UCL <- NA_real_

    if (isTRUE(var.equal)) {
      se <- sqrt(QMres / n)
      tval <- qt(1 - alpha/2, df.residual(modelo))
      ok <- n >= 2
      out$LCL[ok] <- out$LSmeans[ok] - tval * se[ok]
      out$UCL[ok] <- out$LSmeans[ok] + tval * se[ok]
    } else {
      s2 <- sapply(groups, stats::var, na.rm = TRUE)
      se <- sqrt(s2 / n)
      tval <- qt(1 - alpha/2, n - 1)   # Welch por grupo (aproxima)
      ok <- n >= 2 & is.finite(se)
      out$LCL[ok] <- out$LSmeans[ok] - tval[ok] * se[ok]
      out$UCL[ok] <- out$LSmeans[ok] + tval[ok] * se[ok]
    }

    # Ordenação dos níveis no gráfico
    if (isTRUE(ordered)) {
      ord <- order(out$LSmeans)
      out$levels <- factor(out$levels, levels = out$levels[ord])
    } else {
      out$levels <- factor(out$levels, levels = levels(df$.trat))
    }
    df$.trat <- factor(df$.trat, levels = levels(out$levels))
    out$posx <- as.numeric(out$levels)

    # Ajustes de texto
    hj <- ifelse(angulo == 0, 0.5, 1)
    vj <- 1

    # --------------------------
    # 5) Gráfico
    # --------------------------
    p <- ggplot2::ggplot(out, ggplot2::aes(x = levels, y = LSmeans)) +
      ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = posx, ymin = LCL, ymax = UCL),
                         fill = col, alpha = trans, color = NA) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = LCL, ymax = UCL), width = 0.15, color = "black") +
      # pontos brutos (observações)
      ggplot2::geom_point(data = df, ggplot2::aes(x = .trat, y = .resp),
                          position = ggplot2::position_jitter(width = 0), alpha = 0.6, color = "black") +
      # ponto da média
      ggplot2::geom_point(size = 5, color = "white") +
      ggplot2::geom_point(size = 3, color = "gray30") +
      ggplot2::geom_label(ggplot2::aes(label = round(LSmeans, 1)), size = 3, fill = "gray90") +
      ggplot2::theme_minimal(base_size = 14, base_family = font) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angulo, hjust = hj, vjust = vj),
                     panel.border = ggplot2::element_rect(color = "black", fill = NA)) +
      ggplot2::labs(x = "", y = "")

    if (!is.null(ymin) || !is.null(ymax)) {
      ylow  <- if (is.null(ymin)) min(out$LCL, out$UCL, out$LSmeans, na.rm = TRUE) else ymin
      yhigh <- if (is.null(ymax)) max(out$LCL, out$UCL, out$LSmeans, na.rm = TRUE) else ymax
      p <- p + ggplot2::coord_cartesian(ylim = c(ylow, yhigh))
    }

    if (isTRUE(salve)) ggplot2::ggsave(filename = file, plot = p, dpi = 300, bg = "transparent")

    # --------------------------
    # 6) Retorno
    # --------------------------
    list(
      grafico   = p,
      anova     = resumo_anova,
      cv        = CV,
      bartlett  = bartlett
    )
  }





}

# ============================================================
# App Shiny - plotIC + ANOVA + Indicadores (CV, Bartlett, Alerta)
# ============================================================

ui <- fluidPage(
  titlePanel(""),
  theme = shinytheme("flatly"),
  withMathJax(),

  # --- CSS para caixas modernas ---
  tags$head(
    tags$style(HTML("
      .info-box {
        background-color: #f9f9f9;
        border-radius: 12px;
        padding: 15px;
        text-align: center;
        margin: 10px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        transition: transform 0.2s;
      }
      .info-box:hover {
        transform: scale(1.05);
      }
      .info-box h3 {
        font-size: 18px;
        font-weight: bold;
        margin-bottom: 10px;
      }
      .info-box p {
        font-size: 16px;
        margin: 0;
      }
      .info-box.alerta {
        background-color: #fff3cd;
        color: #856404;
        border-left: 6px solid #ffcc00;
      }
      .info-box.positivo {
        background-color: #d4edda;
        color: #155724;
        border-left: 6px solid #28a745;
      }
    "))
  ),

  tabsetPanel(
    tabPanel(
      "Intervalos de Confiança", icon = icon("chart-bar"),
      sidebarLayout(
        sidebarPanel(
          fileInput("file1", label = "Carregar arquivo", accept = c(".csv", ".xlsx")),

          uiOutput("column_treat"),
          uiOutput("column_resp"),
          uiOutput("column_block"),

          tags$hr(),
          numericInput("conf_level", "Nível de confiança:", value = 0.95, step = 0.01, min = 0, max = 1),
          checkboxInput("outlier", "Remover outliers", value = FALSE),
          checkboxInput("ordered", "Ordenar tratamentos", value = TRUE),
          checkboxInput("var_equal", "Assumir variâncias iguais", value = TRUE),
          selectInput("col", "Cor:",
                      choices = c("Azul céu" = "skyblue", "Vermelho" = "red",
                                  "Verde" = "green", "Azul" = "blue", "Laranja" = "orange"),
                      selected = "skyblue"),
          sliderInput("trans", "Transparência:", min = 0, max = 1, value = 0.2, step = 0.05),
          numericInput("ymin", "Limite inferior do eixo Y:", value = 0),
          numericInput("ymax", "Limite superior do eixo Y:", value = 100),
          sliderInput("angulo", "Ângulo do texto:", min = 0, max = 360, value = 0, step = 5),
          textInput("file", "Nome do arquivo:", value = "grafico.png"),

          tags$hr(),
          downloadButton("downloadPlot", label = NULL, icon = icon("download", lib = "glyphicon"))
        ),

        mainPanel(
          uiOutput("plot_or_msg"),
          hr(),
          uiOutput("anova_section")   # <<-- seção dinâmica com tabela + indicadores
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
        HTML("
      O <strong>Estatística de Forma Simples</strong> é uma aplicação desenvolvida
      para auxiliar pesquisadores, estudantes e profissionais na realização de análises estatísticas
      de forma prática, interativa e intuitiva.<br><br>

      A ferramenta permite importar dados em formato <i>CSV</i> ou <i>Excel</i>,
      selecionar variáveis de interesse e obter análises completas, incluindo:<br>
      • Análise de variância (ANOVA)<br>
      • Intervalos de confiança para médias<br>
      • Teste de homocedasticidade (Bartlett)<br>
      • Indicadores estatísticos como o coeficiente de variação (CV%)<br><br>

      Além dos resultados numéricos, o aplicativo gera gráficos dinâmicos e exportáveis,
      facilitando a interpretação e comunicação dos resultados.
      O objetivo é tornar o processo estatístico mais acessível, confiável e visual,
      sem perder o rigor científico.
    ")
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
)

server <- function(input, output, session) {

  # --- Leitura do dataset ---
  dados <- reactive({
    req(input$file1)
    ext <- tolower(tools::file_ext(input$file1$name))

    df <- switch(ext,
                 "csv" = {
                   primeira_linha <- readLines(input$file1$datapath, n = 1)
                   sep <- if (grepl(";", primeira_linha)) ";" else ","
                   readr::read_delim(
                     input$file1$datapath,
                     delim = sep,
                     show_col_types = FALSE,
                     name_repair = "minimal"
                   )
                 },
                 "xlsx" = readxl::read_excel(
                   input$file1$datapath,
                   .name_repair = "minimal"
                 ),
                 validate("❌ Formato não suportado. Use CSV ou XLSX.")
    )

    as.data.frame(df, check.names = FALSE)  # <- mantém "Prod (sc-ha)"
  })



  # --- Inputs dinâmicos ---
  output$column_treat <- renderUI({
    req(dados())
    selectInput("treat", "Variável Explicativa:", choices = names(dados()))
  })
  output$column_resp <- renderUI({
    req(dados())
    selectInput("resp", "Variável Resposta:", choices = names(dados()))
  })
  output$column_block <- renderUI({
    req(dados())
    selectInput("bloco", "Controle Local (opcional):", choices = c("Nenhum", names(dados())))
  })

  # --- Dataset com fatores ---
  dat <- reactive({
    req(dados())
    df <- dados()
    if (!is.null(input$treat) && input$treat %in% names(df)) df[[input$treat]] <- as.factor(df[[input$treat]])
    if (!is.null(input$bloco) && input$bloco != "Nenhum" && input$bloco %in% names(df)) df[[input$bloco]] <- as.factor(df[[input$bloco]])
    df
  })

  # --- Resultado da análise (usa nomes seguros) ---
  resultado <- reactive({
    req(dados(), input$treat, input$resp)

    # cria um data.frame com nomes seguros para evitar erro com parênteses/hífen
    df <- dados()
    # renomeia temporariamente as colunas escolhidas
    df$.resp <- df[[input$resp]]
    df$.trat <- as.factor(df[[input$treat]])

    tem_bloco <- !is.null(input$bloco) && input$bloco != "Nenhum" && input$bloco %in% names(df)
    if (tem_bloco) {
      df$.block <- as.factor(df[[input$bloco]])
    }

    # checagem de numérico da resposta
    validate(need(is.numeric(df$.resp), ""))

    # modelo ANOVA com nomes seguros
    modelo <- if (tem_bloco) {
      aov(.resp ~ .block + .trat, data = df)
    } else {
      aov(.resp ~ .trat, data = df)
    }

    # CV%
    qm_res <- summary(modelo)[[1]]["Residuals", "Mean Sq"]
    media_geral <- mean(df$.resp, na.rm = TRUE)
    CV <- 100 * sqrt(qm_res) / media_geral

    # Bartlett: use interface por vetores (evita fórmulas com nomes ruins)
    bartlett <- tryCatch(
      bartlett.test(x = df$.resp, g = df$.trat),
      error = function(e) NULL
    )

    list(Modelo = modelo, CV = CV, Bartlett = bartlett)
  })



  # --- Gráfico ou aviso ---
  output$plot_or_msg <- renderUI({
    if (!is.numeric(dat()[[input$resp]])) {
      tags$h4("⚠️ Selecione uma variável numérica para resposta.")
    } else {
      plotOutput("plot_statis")
    }
  })

  grafico_plotIC <- reactive({
    req(dat())
    validate(need(is.numeric(dat()[[input$resp]]), NULL))
    bloco <- if (!is.null(input$bloco) && input$bloco != "Nenhum") input$bloco else NULL
    plotIC(
      data          = dat(),
      variavel_exp  = input$treat,
      variavel_resp = input$resp,
      bloco         = bloco,
      conf.level    = input$conf_level,
      ordered       = input$ordered,
      var.equal     = input$var_equal,
      col           = input$col,
      trans         = input$trans,
      ymin          = input$ymin,
      ymax          = input$ymax,
      angulo        = input$angulo,
      font          = "serif",
      salve         = FALSE,
      outlier       = input$outlier,
      file          = input$file
    )
  })
  output$plot_statis <- renderPlot({ grafico_plotIC()$grafico })

  # --- Download ---
  output$downloadPlot <- downloadHandler(
    filename = function() { input$file },
    content = function(file) {
      ggsave(file, grafico_plotIC()$grafico, dpi=1000, width=8, height=6, units="in", bg="transparent")
    }
  )

  # --- Seção ANOVA dinâmica com indicadores ---
  output$anova_section <- renderUI({
    if (!is.numeric(dat()[[input$resp]])) return(NULL)

    bart <- resultado()$Bartlett
    if (is.null(bart)) return(NULL)
    pval <- bart$p.value

    alerta_txt <- ifelse(pval > 0.05, "Variâncias homogêneas", "Variâncias heterogêneas")
    alerta_icon <- ifelse(pval > 0.05, "✔️", "⚠️")
    alerta_class <- ifelse(pval > 0.05, "positivo", "alerta")

    tagList(
      h4("Análise de Variância"),
      DTOutput("table_results"),
      br(),
      h4("Indicadores"),
      fluidRow(
        # Caixa CV%
        column(4,
               div(class = "info-box",
                   h3("CV%"),
                   p(paste0(formatC(resultado()$CV, 2, format="f"), " %"))
               )
        ),
        # Caixa Bartlett
        column(4,
               div(class = "info-box",
                   h3("Homocedasticidade"),
                   p(paste0("p = ", formatC(pval, 4, format="f")))
               )
        ),
        # Caixa Alerta
        column(4,
               div(class = paste("info-box", alerta_class),
                   h3("Alerta"),
                   p(paste(alerta_icon, alerta_txt))
               )
        )
      )
    )
  })

  # --- Tabela ANOVA ---
  output$table_results <- renderDT({
    if (!is.numeric(dat()[[input$resp]])) return(NULL)
    tab <- broom::tidy(resultado()$Modelo)
    tab <- tab[, c("term","df","sumsq","meansq","statistic","p.value")]
    names(tab) <- c("FV","GL","SQ","QM","Fcalc","Valor-p")
    tab[] <- lapply(tab, function(col) if(is.numeric(col)) round(col, 3) else col)
    datatable(
      tab,
      options = list(searching=FALSE, paging=FALSE, info=FALSE, scrollX=TRUE, dom='t',
                     columnDefs=list(list(className='dt-center', targets="_all"))),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })
}

shinyApp(ui, server)
