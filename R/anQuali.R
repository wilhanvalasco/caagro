#' Executa o aplicativo Shiny anQuali
#'
#' A função \code{anQuali()} inicializa o aplicativo Shiny integrado ao pacote
#' \strong{caagro}, desenvolvido para análise estatística de fatores qualitativos
#' em experimentos agrícolas. O aplicativo realiza automaticamente:
#'
#' \itemize{
#'   \item Análises gráficas interativas dos tratamentos.
#'   \item Testes de ANOVA aplicados a fatores qualitativos.
#'   \item Geração de tabelas de resultados (com médias, erros-padrão,
#'         valores de F e p-valor).
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
#' # Inicia o aplicativo anQuali
#' anQuali(iniciar = TRUE)
#' }
anQuali <- function(iniciar = TRUE) {
  if (isTRUE(iniciar)) {
    app_dir <- system.file("shiny", "anQuali", package = "caagro")
    if (app_dir == "") {
      stop("O diretório do app não foi encontrado.
           Verifique se app.R está em inst/shiny/anQuali")
    }
    shiny::runApp(app_dir, launch.browser = TRUE)
  } else {
    message("Defina iniciar = TRUE para rodar o aplicativo Shiny.")
  }
}
