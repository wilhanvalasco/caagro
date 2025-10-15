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


# Estatistica Quali
{

  rm.out.trt <- function(trat, resp, remove_outliers = TRUE) {
    # Combine as variáveis em um data frame
    data <- data.frame(Resp = as.numeric(resp), Trat = as.factor(trat))

    if (!remove_outliers) {
      # Se remove_outliers é FALSE, retorna os dados originais
      return(data)
    }

    # Função para identificar e substituir outliers pela mediana do grupo
    replace_outliers <- function(group) {
      # Verifica se o grupo é numérico
      if (!is.numeric(group)) {
        stop("O grupo deve ser numérico.")
      }

      q1 <- quantile(group, 0.25, na.rm = TRUE)
      q3 <- quantile(group, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr

      # Substitui outliers pela mediana do grupo
      mediana <- median(group, na.rm = TRUE)
      group[group < lower_bound | group > upper_bound] <- mediana
      return(group)
    }

    # Aplica a função em cada grupo
    data$Resp <- ave(data$Resp, data$Trat, FUN = replace_outliers)

    return(data)
  }



  calcular_limites_bigode <- function(Resp, Trat)
  {
    # Criar um data frame com Trat e Resp
    dat <- data.frame(Trat = Trat, Resp = Resp)

    # Aplicar a função usando group_by e summarise do dplyr
    resultados <- dat %>%
      group_by(Trat) %>%
      summarise(Q1 = quantile(Resp, probs = 0.25, na.rm = TRUE),
                Q3 = quantile(Resp, probs = 0.75, na.rm = TRUE),
                IQR = Q3 - Q1,
                limite_inf = max(min(Resp), Q1 - 1.5 * IQR), # Garantir que limite_inf não seja menor que o valor mínimo observado
                limite_sup = min(max(Resp), Q3 + 1.5 * IQR), # Garantir que limite_sup não seja maior que o valor máximo observado
                .groups = 'drop') %>%
      as.data.frame()

    # Definir o nome das linhas como o nome dos tratamentos
    rownames(resultados) <- resultados$Trat

    # Manter apenas as colunas limite_inf e limite_sup
    resultados <- resultados %>%
      select(Trat, limite_inf, limite_sup)

    return(resultados)
  }

  # Define a função grafics
  plot_Data <- function(Trat, Bloco, Resp,
                        ylim_min = 0, ylim_max = 100, test, conf.level = 0.95, rm = FALSE,
                        angle.x = 0, salve = FALSE, quali_graf = 30, nome_graf = NULL,
                        fonte = 1) {

    alfa <- conf.level

    # Definindo estrutura dos dados com lógica simplificada
    if (exists("Bloco") && length(Bloco) > 0) {
      dat <- data.frame(
        Trat = as.factor(Trat),
        Bloco = as.factor(Bloco),
        Resp = as.numeric(Resp)
      )
      dat <- na.omit(dat)
    } else {
      dat <- data.frame(
        Trat = as.factor(Trat),
        Resp = as.numeric(Resp)
      )
      dat <- na.omit(dat)
    }

    # Remover valores ausentes (NA)
    dat <- na.omit(dat)

    # Remover outliers se 'rm' for TRUE
    if (rm == TRUE) {
      dat <- rm.out.trt(dat$Trat, dat$Resp, remove_outliers = rm)
      dat <- na.omit(dat)
      dat <- data.frame(Trat = dat[, 2], Resp = dat[, 1])
    }

    # Escolher o modelo apropriado baseado na presença de 'Bloco'
    if (!"Bloco" %in% colnames(dat)) {
      mod <- aov(Resp ~ Trat, data = dat)
    } else {
      mod <- aov(Resp ~ Trat + Bloco, data = dat)
    }

    # Teste de comparação múltipla
    comp <- switch(test,
                   "tk"  = { HSD.test(mod, "Trat", alpha = alfa) },
                   "snk" = { SNK.test(mod, "Trat", alpha = alfa, console = FALSE) },
                   "lsd" = { LSD.test(mod, "Trat", p.adj = "bonferroni", alpha = alfa) },
                   "md"  = {
                     dat <- na.omit(dat)
                     if (length(unique(dat$Trat)) > 1 && all(table(dat$Trat) > 0)) {
                       mediana <- Median.test(dat$Resp, dat$Trat, alpha = alfa, console = FALSE)
                       colnames(mediana$groups) <- c("Resp", "groups")
                       med <- mediana$groups
                       result <- list(groups = med)
                     } else {
                       stop("Um ou mais grupos não têm observações suficientes.")
                     }
                   },
                   "sk"  = {
                     sk_result <- SK(mod, sig.level = alfa)
                     combine_groups <- function(sk_result) {
                       media <- sk_result$out$Result
                       group_columns <- grep("^G", names(media), value = TRUE)
                       media <- media %>%
                         mutate(G_combined = apply(select(media, all_of(group_columns)), 1,
                                                   function(x) paste(x, collapse = " ")))
                       media1 <- media %>%
                         select(Resp = 1, groups = G_combined)
                       media1$Resp <- as.numeric(media1$Resp)
                       media1$groups <- str_squish(media1$groups)
                       list(groups = media1)
                     }
                     combine_groups(sk_result)
                   }
    )

    # Prepara o dataframe para o ggplot
    df <- comp$groups %>%
      rownames_to_column(var = "trt") %>%
      mutate(trt = reorder(trt, -Resp)) %>%
      arrange(desc(Resp))

    # Bigodes
    conf_ints <- calcular_limites_bigode(dat$Resp, dat$Trat)
    conf_ints_df <- data.frame(trt = conf_ints[, 1],
                               upr = conf_ints[, 2],
                               lwr = conf_ints[, 3])

    df <- df %>%
      left_join(conf_ints_df, by = "trt")

    # Construção do gráfico (aplicando 'fonte' nos tamanhos)
    grafico <- ggplot(df, aes(x = reorder(trt, -Resp), y = Resp, fill = groups)) +
      geom_bar(stat = "identity") +
      geom_point(data = dat, aes(x = Trat, y = Resp),
                 color = "black", shape = 19, fill = "blue", size = 1.5,
                 position = position_dodge(width = 0.9)) +
      geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, color = "black") +
      geom_label(aes(label = round(Resp, 1)),
                 vjust = 5, color = "black", size = 3.5 * fonte, fontface = "plain",
                 label.size = 0, label.padding = unit(0.2, "lines"), fill = "white") +
      geom_label(aes(label = groups), vjust = -1.5, color = "black", size = 5 * fonte,
                 fontface = "plain", label.size = 0.00002,
                 label.padding = unit(0.2, "lines"), fill = "white") +
      geom_point(aes(y = Resp), color = "black", fill = "black", shape = 19, size = 1.5,
                 position = position_dodge(width = 0.9)) +
      scale_fill_manual(values = c("a" = "#E97132", "b" = "#156082",
                                   "c" = "#25874E", "d" = "#A4BAD2",
                                   "e" = "#73C884", "f" = "#43627F",
                                   "g" = "#FF4F4F", "h" = "#B5B5B7",
                                   "i" = "#FE9088", "j" = "#0C8C34",
                                   "k" = "#0C8C34", "l" = "#620496")) +
      labs(x = "", y = "") +
      coord_cartesian(ylim = c(ylim_min, ylim_max)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_classic() +
      theme(
        axis.text.x  = element_text(
          angle = angle.x,
          hjust = ifelse(angle.x == 90, 1, 0.5),
          vjust = ifelse(angle.x == 90, 0.5, 1),
          face  = "bold", color = "black", size = 12 * fonte
        ),
        axis.text.y  = element_text(color = "black", size = 11 * fonte),
        axis.title.x = element_text(face = "bold", color = "black", size = 14 * fonte),
        axis.title.y = element_text(face = "bold", color = "black", size = 14 * fonte),
        plot.title   = element_text(face = "bold", color = "black", size = 16 * fonte),
        legend.position   = "none",
        panel.border = element_rect(color = "black", fill = NA, size = 0.8),
        panel.background  = element_rect(fill = "transparent"),
        plot.background   = element_rect(fill = "transparent", color = NA)
      ) +
      scale_x_discrete(guide = guide_axis(n.dodge = 1)) +
      geom_hline(yintercept = 0, color = "black", size = 0.5) +
      geom_vline(xintercept = 0, color = "black", size = 0.5)

    # Verifica a condição e executa a ação correspondente
    if (salve) {
      if (is.null(nome_graf) || nome_graf == "") {
        stop("Preencha o nome do gráfico.")
      }
      ggsave(paste0(nome_graf, ".png"),
             plot = grafico, width = 8, height = 6, dpi = quali_graf, bg = "transparent")
      cat("Gráfico salvo como:", paste0(nome_graf, ".png"), "\n")
    } else {
      print(grafico)
    }

    return(grafico)
  }

  # Formatar data
  formatar_colunas <- function(df) {
    # Verifica se as colunas existem no dataframe
    if (all(c("Resp", "upr", "lwr") %in% colnames(df))) {
      df$Resp <- round(df$Resp, 2)
      df$upr <- round(df$upr, 2)
      df$lwr <- round(df$lwr, 2)
    } else {
      warning("As colunas 'Resp', 'upr' e 'lwr' não estão presentes no dataframe.")
    }

    return(df)  # Retorna o dataframe completo, incluindo todas as colunas
  }

  # Define a função com extração de ANOVA incluída
  plot_Data_info <- function(Trat, Bloco, Resp, rm = FALSE)
  {
    # Definindo estrutura dos dados com lógica simplificada
    if (exists("Bloco") && length(Bloco) > 0) {
      dat <- data.frame(
        Trat = as.factor(Trat),
        Bloco = as.factor(Bloco),
        Resp = as.numeric(Resp)
      )
      dat <- na.omit(dat)
    } else {
      dat <- data.frame(
        Trat = as.factor(Trat),
        Resp = as.numeric(Resp)
      )
      dat <- na.omit(dat)
    }

    # Remover valores ausentes (NA)
    dat <- na.omit(dat)

    # Remover outliers se 'rm' for TRUE
    if (rm == TRUE) {
      dat <- rm.out.trt(dat$Trat,dat$Resp, remove_outliers = rm)
      dat <- data.frame(Trat = dat[,2], Resp = dat[,1])
      dat <- na.omit(dat)
    }

    # Escolher o modelo apropriado baseado na presença de 'Bloco'
    if (!"Bloco" %in% colnames(dat)) {
      mod <- aov(Resp ~ Trat, data = dat)
    } else {
      mod <- aov(Resp ~ Bloco + Trat, data = dat)
    }

    # Função interna para extrair valores da ANOVA e calcular o CV%
    extrai_anova <- function(mod, dat) {
      # Executa a ANOVA
      anova_res <- anova(mod)

      # Extrai valores da linha Residuals
      df_residuo <- anova_res["Residuals", "Df"]
      sq_residuo <- anova_res["Residuals", "Sum Sq"]
      qm_trat <- anova_res["Trat", "Mean Sq"]
      qm_residuo <- anova_res["Residuals", "Mean Sq"]

      # Extrai o p-valor do fator de interesse (Trat, por exemplo)
      p_valor <- anova_res["Trat", "Pr(>F)"]

      # Calcula a média de Resp
      media_resp <- mean(dat$Resp, na.rm = TRUE)

      # Calcula o Coeficiente de Variação (CV%)
      cv_percent <- (sqrt(qm_residuo) / media_resp) * 100

      # Retorna uma lista com os valores
      return(list(
        df_residuo = df_residuo,
        sq_residuo = sq_residuo,
        qm_residuo = qm_residuo,
        qm_trat = qm_trat,
        p_valor = round(p_valor, 6),
        cv_percent = round(cv_percent, 2)
      ))
    }

    # Chamar a função para extrair a ANOVA e calcular o CV%
    anova_info <- extrai_anova(mod, dat)

    # Teste de Shapiro-Wilk para normalidade
    shapiro_test <- shapiro.test(dat$Resp)
    shapiro_p <- shapiro_test$p.value

    # Teste de Bartlett para homocedasticidade
    bartlett_test <- bartlett.test(Resp ~ Trat, data = dat)
    bartlett_p <- bartlett_test$p.value

    # Retorna o modelo, os dados e a informação extraída da ANOVA
    return(list(
      mod = mod,
      dat = dat,
      anova_info = anova_info,
      shapiro_p = round(shapiro_p, 10),
      bartlett_p = round(bartlett_p, 10)
    ))
  }


}




# ──────────────────────────────────────────────────────────────────────────────
# App Shiny – STATISTICS
# ──────────────────────────────────────────────────────────────────────────────
library(shiny)
library(shinythemes)
library(DT)
library(readxl)
library(writexl)
library(ggplot2)

# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  uiOutput("app_ui")
)

# SERVER -----------------------------------------------------------------------
server <- function(input, output, session) {

  output$app_ui <- renderUI({
    tabs <- list(
      tabPanel(
        "Qualitativo", icon = icon("chart-bar"),
        sidebarLayout(
          sidebarPanel(
            fileInput("file1", label = NULL, accept = c(".csv", ".xlsx")),
            uiOutput("column1"),
            uiOutput("column2"),
            uiOutput("column3"),

            div(
              style = "text-align: center; border:1px solid #e0e0e0; padding: 5px; border-radius: 10px; background-color: #fff;",
              tags$style(HTML("
                #rm_out {
                  padding: 2px 2px;
                  font-size: 12px;
                  background-color: transparent;
                  border: none;
                  color: black;
                  cursor: pointer;
                  transition: background-color 0.3s, color 0.3s;
                }
                #rm_out:hover { background-color: #e0e0e0; color: black; }
                #rm_out:active { border-color: black; }
                .fa-exclamation-triangle { color: green !important; }
              ")),
              actionButton("rm_out", label = "Remover outliers", icon = icon("thumbs-up"))
            ),

            selectInput(
              "test", "Escolha o teste:",
              choices = c(
                "Tukey" = "tk",
                "Student–Newman–Keuls" = "snk",
                "LSD" = "lsd",
                "Scott–Knott" = "sk",
                "Median test" = "md"
              )
            ),

            selectInput(
              "angle_x", "Ângulo dos rótulos do eixo x:",
              choices = c("0 graus" = 0, "90 graus" = 90)
            ),

            sliderInput(
              "alfa", "Nível de confiança:",
              min = 0, max = 1,
              value = 0.05, step = 0.01,
              post = ""
            ),
            sliderInput(
              "fonte", "Tamanho da fonte:",
              min = 0, max = 10,
              value = 1.5, step = 0.1
            ),
            numericInput("ylim_min", "Limite mínimo do eixo y:", value = 0, step = 1),
            numericInput("ylim_max", "Limite máximo do eixo y:", value = 100, step = 1),
            div(
              style = "display: flex; gap: 10px; margin-top: 10px;",
              downloadButton("downloadPlot", "Salvar Gráfico"),
              downloadButton("baixar_excel_st", "Salvar Excel")
            ),

            tags$hr(),

          ),
          mainPanel(
            plotOutput("plot_statis"),
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
                  height: 90px;
                }
                .info-box h3 { font-size: 16px; margin: 0; font-weight: bold; }
                .info-box p { font-size: 14px; color: #666; margin: 5px 0 0; }
                .info-box .separator { border-top: 1px solid #ccc; margin: 10px 0; }
                .info-box .number { font-size: calc(2vw + 10px); font-weight: bold; }
              "))
            ),
            fluidRow(
              column(width = 3, div(class = "info-box", h3(textOutput("df_residuo")), div(class = "separator"), p("GL Resíduo"))),
              column(width = 3, div(class = "info-box", h3(textOutput("sq_residuo")), div(class = "separator"), p("SQ Resíduo"))),
              column(width = 3, div(class = "info-box", h3(textOutput("qm_residuo")), div(class = "separator"), p("QM Resíduo"))),
              column(width = 3, div(class = "info-box", h3(textOutput("qm_trat")), div(class = "separator"), p("QM Tratamento"))),
              column(width = 3, div(class = "info-box", h3(textOutput("p_valor")), div(class = "separator"), p("Valor-p"))),
              column(width = 3, div(class = "info-box", h3(textOutput("shapiro_p")), div(class = "separator"), p("Teste Shapiro-Wilk"))),
              column(width = 3, div(class = "info-box", h3(textOutput("bartlett_p")), div(class = "separator"), p("Teste Bartlett"))),
              column(width = 3, div(class = "info-box", h3(textOutput("cv_percent")), div(class = "separator"), p("CV (%)")))
            ),
            hr(),
            DTOutput("Table_st")
          )
        )
      ),
      tabPanel(
        "Sobre", icon = icon("info-circle"),

        # Slogan
        tags$div(
          style = "text-align:center; margin-top:20px;",
          tags$h3("Estatística de Forma Simples", style="color:black; font-weight:bold;")
        ),

        # Texto do que faz
        tags$div(
          style = "text-align:justify; margin:20px; font-size:16px; color:black;",
          HTML("O <strong>ESTATÍSTICAS</strong> fornece análises que incluem a comparação de médias e medianas para distribuições Gaussianas e não Gaussianas.
         Além disso, oferece a visualização dos quartis (Q1 e Q3) e a distribuição dos dados por meio de gráficos de dispersão.
         Também inclui o tratamento de valores atípicos (outliers).")
        ),
        tags$hr(style = "border-top: 2px solid black;"),

        # Foto com efeito de zoom
        tags$div(
          style = "text-align:center; margin-top:50px;",
          tags$style(HTML("
      .foto-zoom {
        transition: transform 0.3s ease;
        border-radius: 100%;
      }
      .foto-zoom:hover {
        transform: scale(1.8);
      }
    ")),
          tags$img(src = "wilhan.png", height = "150px", width = "150px", class = "foto-zoom")
        ),

        # Nome
        tags$div(
          style = "text-align:center; margin-top:10px;",
          tags$h4("Wilhan Valasco", style="color:black; font-weight:bold;")
        ),

        # Contatos em Courier New
        tags$div(
          style = "text-align:center; margin-top:15px; font-size:15px; font-family:'Courier New', monospace;",
          tags$p(HTML("<b>Email:</b> wilhanvalasco@hotmail.com"), style="color:black;"),
          tags$p(HTML("<b>Site:</b> <a href='https://wilhanvalasco.github.io/meu-site/' target='_blank'>Meu Site</a>"), style="color:black;"),
          tags$p(HTML("<b>LinkedIn:</b> <a href='https://www.linkedin.com/in/wilhan-valasco/' target='_blank'>linkedin.com/in/wilhan-valasco</a>"), style="color:black;"),

          # WhatsApp
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

  # Dados ----------------------------------------------------------------------
  data <- reactive({
    req(input$file1)
    ext <- tolower(tools::file_ext(input$file1$name))

    out <- switch(
      ext,
      "csv"  = read.csv2(input$file1$datapath, stringsAsFactors = FALSE),
      "xlsx" = read_excel(input$file1$datapath),
      {
        validate("Unsupported file format. Use .csv or .xlsx.")
        NULL
      }
    )
    out
  })

  # Atualiza selects conforme colunas
  observe({
    req(data())
    columns <- colnames(data())

    output$column1 <- renderUI(
      selectInput("col1", "Selecionar Tratamento", choices = c("", columns))
    )
    output$column2 <- renderUI(
      selectInput("col2", "Selecionar Controle Local", choices = c("NULL" = "", columns))
    )
    output$column3 <- renderUI(
      selectInput("col3", "Selecionar Variável Resposta", choices = c("", columns))
    )
  })

  # Toggle "Remove/Add outliers"
  rm_out_value <- reactiveVal(FALSE)
  observeEvent(input$rm_out, {
    rm_out_value(!rm_out_value())
    if (rm_out_value()) {
      updateActionButton(session, "rm_out", label = "Adicionar outliers", icon = icon("exclamation-triangle"))
    } else {
      updateActionButton(session, "rm_out", label = "Remover outliers", icon = icon("check-circle"))
    }
  })

  # Data frame já processado (Trat/Resp/Bloco)
  df_proc <- reactive({
    req(data(), input$col1, input$col3, input$col1 != "", input$col3 != "")
    df <- data()

    validate(
      need(input$col1 %in% names(df), "Coluna de Tratamento inválida."),
      need(input$col3 %in% names(df), "Coluna de Resposta inválida.")
    )

    out <- data.frame(
      Trat = as.factor(df[[input$col1]]),
      Resp = suppressWarnings(as.numeric(df[[input$col3]])),
      stringsAsFactors = FALSE
    )

    if (!is.null(input$col2) && nzchar(input$col2)) {
      out$Bloco <- as.factor(df[[input$col2]])
    } else {
      out$Bloco <- NULL
    }
    out
  })

  # Gráfico (usa sua função plot_Data)
  grafico <- reactive({
    dfp <- df_proc()
    angle_x <- as.numeric(input$angle_x)
    alfa    <- as.numeric(input$alfa)
    rm_     <- rm_out_value()

    plot_Data(
      Trat      = dfp$Trat,
      Bloco     = if (!is.null(dfp$Bloco)) dfp$Bloco else NULL,
      Resp      = dfp$Resp,
      ylim_min  = input$ylim_min,
      ylim_max  = input$ylim_max,
      test      = input$test,
      conf.level= alfa,
      rm        = rm_,
      angle.x   = angle_x,
      salve     = FALSE,
      quali_graf= 500,
      nome_graf = NULL,
      fonte = input$fonte
    )
  })

  output$plot_statis <- renderPlot({
    req(grafico())
    print(grafico())
  })

  # Dados do gráfico para Tabela/Excel (com fallback)
  plot_data_reactive <- reactive({
    g <- grafico()
    pd <- tryCatch({
      if (is.list(g) && !is.null(g$data))      g$data
      else if (inherits(g, "ggplot") && !is.null(g$data)) g$data
      else NULL
    }, error = function(e) NULL)

    if (is.null(pd)) {
      # Fallback: tabela básica com Trat/Resp
      dfp <- df_proc()
      dfp[, c("Trat", "Resp")]
    } else {
      pd
    }
  })

  # Tabela
  output$Table_st <- renderDT({
    req(plot_data_reactive())
    dados_tab <- plot_data_reactive()
    if (exists("formatar_colunas")) {
      dados_tab <- tryCatch(formatar_colunas(dados_tab), error = function(e) dados_tab)
    }
    datatable(
      dados_tab,
      rownames = FALSE,
      options = list(
        searching = FALSE, paging = FALSE, info = FALSE, lengthChange = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      class = 'cell-border stripe'
    )
  })

  # Download do gráfico
  output$downloadPlot <- downloadHandler(
    filename = function() sprintf("output_%s.png", format(Sys.time(), "%Y%m%d_%H%M%S")),
    content = function(file) {
      g <- grafico()   # ou grafico_plotIC()$grafico, dependendo de como está sua reatividade
      ggplot2::ggsave(
        filename = file,
        plot     = g,
        width    = 8,
        height   = 6,
        units    = "in",
        dpi      = 1000,          # alta qualidade (600–1000 já é excelente)
        bg       = "transparent"  # fundo transparente
      )
    }
  )

  # Download Excel
  output$baixar_excel_st <- downloadHandler(
    filename = function() sprintf("resul_statistics_%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M%S")),
    content = function(file) {
      writexl::write_xlsx(plot_data_reactive(), path = file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  # Info ANOVA (usa sua função plot_Data_info)
  anova_res <- reactive({
    dfp <- df_proc()
    plot_Data_info(
      dfp$Trat,
      if (!is.null(dfp$Bloco)) dfp$Bloco else NULL,
      dfp$Resp,
      rm = rm_out_value()
    )
  })

  output$df_residuo <- renderText({ req(anova_res()); anova_res()$anova_info$df_residuo })
  output$sq_residuo <- renderText({ req(anova_res()); anova_res()$anova_info$sq_residuo })
  output$qm_residuo <- renderText({ req(anova_res()); anova_res()$anova_info$qm_residuo })
  output$qm_trat     <- renderText({ req(anova_res()); anova_res()$anova_info$qm_trat })

  output$p_valor <- renderText({
    req(anova_res())
    paste0(round(anova_res()$anova_info$p_valor * 100, 2), "%")
  })
  output$cv_percent <- renderText({
    req(anova_res())
    paste0(round(anova_res()$anova_info$cv_percent, 2), "%")
  })
  output$bartlett_p <- renderText({
    req(anova_res())
    paste0(round(anova_res()$bartlett_p * 100, 2), "%")
  })
  output$shapiro_p <- renderText({
    req(anova_res())
    paste0(round(anova_res()$shapiro_p * 100, 2), "%")
  })
}

# RUN --------------------------------------------------------------------------
shinyApp(ui, server)
