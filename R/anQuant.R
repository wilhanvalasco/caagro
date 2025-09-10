#' Executa o aplicativo Shiny anQuant
#'
#' A função \code{anQuant()} inicializa o aplicativo Shiny integrado ao pacote
#' \strong{caagro}, desenvolvido para análise estatística de fatores quantitativos
#' em experimentos agrícolas. O aplicativo realiza automaticamente:
#'
#' \itemize{
#'   \item Ajuste de modelos de regressão (linear, quadrático, exponencial e logístico).
#'   \item Cálculo de métricas estatísticas (R², R² ajustado, ANOVA, coeficientes).
#'   \item Geração de tabelas de resultados resumidos (médias, medianas, desvios).
#'   \item Exportação dos resultados em \strong{Excel (.xlsx)}.
#'   \item Exportação dos gráficos em \strong{PNG}.
#' }
#'
#' Esses recursos permitem maior eficiência, acurácia, precisão e agilidade
#' nas análises, fornecendo resultados claros e prontos para uso em relatórios
#' e tomadas de decisão.
#'
#' @param iniciar Lógico. Se \code{TRUE}, executa o ShinyApp no navegador.
#' @return Nenhum valor é retornado. O aplicativo Shiny é iniciado no navegador.
#' @export
#'
#' @examples
#' \dontrun{
#' # Inicia o aplicativo anQuant
#' anQuant(iniciar = TRUE)
#' }
anQuant <- function(iniciar = TRUE) {
  if (isTRUE(iniciar)) {
    app_dir <- system.file("shiny", "anQuant", package = "caagro")
    if (app_dir == "") {
      stop("O diretório do app não foi encontrado.
           Verifique se app.R está em inst/shiny/anQuant")
    }
    shiny::runApp(app_dir, launch.browser = TRUE)
  } else {
    message("Defina iniciar = TRUE para rodar o aplicativo Shiny.")
  }
}
