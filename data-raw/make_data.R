# Script that generates the SIMULATED example datasets of caagro.
# All datasets below are simulated (set.seed) and must not be presented as
# real experimental results. Run from the package root:
#   Rscript data-raw/make_data.R

set.seed(20251123)

# DIC: 5 cultivars x 4 replicates (yield, t/ha)
sim_dic <- data.frame(
  cultivar = factor(rep(paste0("C", 1:5), each = 4)),
  rep = rep(1:4, 5)
)
mu <- c(C1 = 3.2, C2 = 3.6, C3 = 4.1, C4 = 3.5, C5 = 4.4)
sim_dic$produtividade <- round(mu[as.character(sim_dic$cultivar)] + rnorm(20, 0, 0.3), 2)

# DBC: 6 treatments x 4 blocks
trt <- paste0("T", 1:6)
sim_dbc <- expand.grid(tratamento = trt, bloco = paste0("B", 1:4), stringsAsFactors = TRUE)
tef <- c(T1 = 0, T2 = 0.4, T3 = 0.9, T4 = 1.0, T5 = 1.6, T6 = 0.3)
bef <- c(B1 = -0.3, B2 = 0.1, B3 = 0.25, B4 = -0.05)
sim_dbc$produtividade <- round(5 + tef[as.character(sim_dbc$tratamento)] +
                                 bef[as.character(sim_dbc$bloco)] + rnorm(24, 0, 0.35), 2)
sim_dbc <- sim_dbc[order(sim_dbc$bloco, sim_dbc$tratamento), c("bloco", "tratamento", "produtividade")]
rownames(sim_dbc) <- NULL

# Factorial 3 (cultivar) x 3 (adubacao) in 4 blocks
sim_fatorial <- expand.grid(cultivar = c("A", "B", "C"), adubacao = c("N0", "N1", "N2"),
                            bloco = paste0("B", 1:4), stringsAsFactors = TRUE)
eff <- with(sim_fatorial, 40 + c(A = 0, B = 4, C = 2)[as.character(cultivar)] +
              c(N0 = 0, N1 = 6, N2 = 9)[as.character(adubacao)] +
              ifelse(cultivar == "B" & adubacao == "N2", 4, 0) +
              c(B1 = -1, B2 = 0, B3 = 1, B4 = 0)[as.character(bloco)])
sim_fatorial$altura <- round(eff + rnorm(nrow(sim_fatorial), 0, 2), 1)

# Dose-response (quadratic) with two cultivars, 4 replicates
doses <- c(0, 40, 80, 120, 160, 200)
sim_dose <- expand.grid(dose = doses, rep = 1:4, cultivar = c("Cv1", "Cv2"), stringsAsFactors = TRUE)
b <- ifelse(sim_dose$cultivar == "Cv1", 0.030, 0.024)
c2 <- ifelse(sim_dose$cultivar == "Cv1", -0.00011, -0.00008)
sim_dose$produtividade <- round(3 + b * sim_dose$dose + c2 * sim_dose$dose^2 + rnorm(nrow(sim_dose), 0, 0.25), 3)
sim_dose <- sim_dose[, c("cultivar", "dose", "rep", "produtividade")]

# Binomial dose-response (germination of seeds under a herbicide)
dose_b <- c(0, 0.5, 1, 2, 4, 8, 16)
sim_germinacao <- expand.grid(dose = dose_b, rep = 1:4, produto = c("P1", "P2"), stringsAsFactors = TRUE)
ed50 <- ifelse(sim_germinacao$produto == "P1", 2, 5)
pr <- 0.95 / (1 + exp(1.8 * (log(pmax(sim_germinacao$dose, 1e-3)) - log(ed50))))
sim_germinacao$total <- 50L
sim_germinacao$germinadas <- rbinom(nrow(sim_germinacao), 50, pr)
sim_germinacao <- sim_germinacao[, c("produto", "dose", "rep", "germinadas", "total")]

# Multivariate: 36 genotypes from 3 groups, 6 traits
grp <- rep(c("G1", "G2", "G3"), each = 12)
Mu <- rbind(G1 = c(60, 25, 4.0, 14, 120, 3.2), G2 = c(70, 30, 4.6, 16, 110, 3.8), G3 = c(55, 22, 3.6, 13, 135, 2.9))
Sd <- c(4, 2, 0.3, 1, 8, 0.25)
X <- t(vapply(grp, function(g) Mu[g, ] + rnorm(6, 0, Sd), numeric(6)))
colnames(X) <- c("altura", "diam_colmo", "massa_100", "n_fileiras", "ciclo", "produtividade")
sim_multivariado <- data.frame(genotipo = sprintf("Gen%02d", 1:36), grupo = factor(grp), round(X, 2))
rownames(sim_multivariado) <- NULL

save(sim_dic, file = "data/sim_dic.rda", compress = "xz")
save(sim_dbc, file = "data/sim_dbc.rda", compress = "xz")
save(sim_fatorial, file = "data/sim_fatorial.rda", compress = "xz")
save(sim_dose, file = "data/sim_dose.rda", compress = "xz")
save(sim_germinacao, file = "data/sim_germinacao.rda", compress = "xz")
save(sim_multivariado, file = "data/sim_multivariado.rda", compress = "xz")

# Example files for the importer
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
utils::write.table(sim_dose, "inst/extdata/dose_resposta.csv", sep = ";", dec = ",", row.names = FALSE)
utils::write.table(sim_dbc, "inst/extdata/dbc.txt", sep = "\t", row.names = FALSE)
