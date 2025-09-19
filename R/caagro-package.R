#' caagro: Ciência Aplicada em Análises Agro
#'
#' O pacote \strong{caagro} (Ciência Aplicada em Análises do Agro)
#' fornece funções e aplicações interativas em Shiny para análises
#' estatísticas aplicadas a experimentos agrícolas. O pacote integra
#' diferentes abordagens estatísticas, contemplando fatores qualitativos,
#' quantitativos e intervalos de confiança.
#'
#' \itemize{
#'   \item Para fatores qualitativos: oferece testes de comparações múltiplas
#'         paramétricos (ex.: Tukey, Scott-Knott, SNK) e não-paramétricos
#'         (ex.: Kruskal-Wallis), permitindo identificar diferenças entre
#'         tratamentos.
#'   \item Para fatores quantitativos: disponibiliza ajuste de modelos lineares
#'         e não lineares (linear, quadrático, exponencial, logístico), com
#'         estimação de parâmetros, ANOVA de regressão e medidas de ajuste
#'         (R² e R² ajustado).
#'   \item Para análise de intervalos de confiança: implementa métodos que
#'         consideram tanto a hipótese de variâncias homogêneas quanto
#'         heterogêneas, retornando estimativas de médias, limites inferiores
#'         e superiores (LCL/UCL), coeficiente de variação (CV\%) e teste de
#'         homocedasticidade de Bartlett.
#' }
#'
#' Os módulos permitem também visualizações gráficas, exportação de tabelas
#' (Excel) e gráficos (PNG), facilitando a interpretação e a comunicação dos
#' resultados experimentais.
#'
#' Interfaces principais:
#' \itemize{
#'   \item \code{anQuali()} – análises de fatores qualitativos.
#'   \item \code{anQuant()} – análises de fatores quantitativos.
#'   \item \code{anIC()} – análises baseadas em intervalos de confiança.
#' }
#'
#' @keywords internal
"_PACKAGE"
