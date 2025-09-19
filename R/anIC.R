#' Executa o aplicativo Shiny anIC
#'
#' A função \code{anIC()} inicializa o aplicativo Shiny integrado ao pacote
#' \strong{caagro}, desenvolvido para construção de gráficos com intervalos de confiança
#' e análises associadas em experimentos agrícolas. O aplicativo realiza automaticamente:
#'
#' \itemize{
#'   \item Construção de gráficos de intervalos de confiança para médias de tratamentos.
#'   \item Cálculo e apresentação da análise de variância (ANOVA).
#'   \item Cálculo do coeficiente de variação (CV\%).
#'   \item Execução do teste de homocedasticidade de Bartlett.
#'   \item Exportação dos resultados em \strong{Excel (.xlsx)}.
#'   \item Exportação dos gráficos em \strong{PNG}.
#' }
#'
#' Esses recursos permitem interpretar rapidamente a consistência das variâncias,
#' a significância dos tratamentos e a precisão experimental, fornecendo resultados
#' claros e de fácil aplicação em relatórios e tomadas de decisão.
#'
#' @param iniciar Lógico. Se \code{TRUE}, executa o ShinyApp no navegador.
#' @return Nenhum valor é retornado. O aplicativo Shiny é iniciado no navegador.
#' @export
#'
#' @examples
#' \dontrun{
#' # Exemplo: iniciar o aplicativo anIC
#' anIC(iniciar = TRUE)
#'
#' # Exemplo: mensagem quando iniciar = FALSE
#' anIC(iniciar = FALSE)
#' }
anIC <- function(iniciar = TRUE) {
  if (isTRUE(iniciar)) {
    app_dir <- system.file("shiny", "anIC", package = "caagro")
    if (app_dir == "") {
      stop("O diretório do app não foi encontrado.
           Verifique se app.R está em inst/shiny/anIC")
    }
    shiny::runApp(app_dir, launch.browser = TRUE)
  } else {
    message("Defina iniciar = TRUE para rodar o aplicativo Shiny.")
  }
}
