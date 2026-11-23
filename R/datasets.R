#' Simulated example datasets
#'
#' **All datasets below are simulated** (see `data-raw/make_data.R` in the
#' package sources) to illustrate the methods. They must not be cited as
#' experimental results. Real classic datasets shipped with R
#' (`datasets::PlantGrowth`, `datasets::npk`, `datasets::warpbreaks`,
#' `datasets::sleep`) are also used in the examples.
#'
#' * `sim_dic`: completely randomised design, 5 cultivars x 4 replicates;
#'   `cultivar`, `rep`, `produtividade` (t/ha).
#' * `sim_dbc`: randomised complete blocks, 6 treatments x 4 blocks;
#'   `bloco`, `tratamento`, `produtividade`.
#' * `sim_fatorial`: 3 x 3 factorial (cultivar x adubacao) in 4 blocks;
#'   `cultivar`, `adubacao`, `bloco`, `altura` (cm), with an interaction.
#' * `sim_dose`: quadratic response to doses (0-200) for two cultivars, 4
#'   replicates; `cultivar`, `dose`, `rep`, `produtividade`.
#' * `sim_germinacao`: binomial dose-response (germinated seeds out of 50)
#'   for two products; `produto`, `dose`, `rep`, `germinadas`, `total`.
#' * `sim_multivariado`: 36 genotypes from 3 groups, 6 traits; `genotipo`,
#'   `grupo`, `altura`, `diam_colmo`, `massa_100`, `n_fileiras`, `ciclo`,
#'   `produtividade`.
#'
#' The file `system.file("extdata", "severidade.xlsx", package = "caagro")`
#' is the example spreadsheet distributed with caagro 0.1 (disease severity
#' over time for several treatments). Its origin is not documented; it
#' contains negative severity values and should be treated as illustrative
#' only.
#'
#' @format Data frames.
#' @source Simulated with `set.seed(20251123)`.
#' @name caagro-datasets
#' @aliases sim_dic sim_dbc sim_fatorial sim_dose sim_germinacao sim_multivariado
#' @keywords datasets
#' @examples
#' head(sim_dbc)
#' anova_exp(sim_dbc, "produtividade", "tratamento", block = "bloco")
"sim_dic"

#' @rdname caagro-datasets
"sim_dbc"
#' @rdname caagro-datasets
"sim_fatorial"
#' @rdname caagro-datasets
"sim_dose"
#' @rdname caagro-datasets
"sim_germinacao"
#' @rdname caagro-datasets
"sim_multivariado"
