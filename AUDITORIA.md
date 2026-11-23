# Auditoria do caagro 0.x e proposta arquitetural (Fase 1)

Data: 23/11/2026. Base auditada: `caagro.zip` (versão 0.1.1, tags git v0.1.1, v0.1.2, v0.2.0).

## 1. Inventário de arquivos

| Arquivo | Situação | Decisão |
|---|---|---|
| `R/anQuali.R`, `R/anQuant.R`, `R/anIC.R` | Apenas lançadores `runApp()` | Mantidos (compatibilidade), agora abrem os novos módulos |
| `R/caagro-package.R` | Documentação do pacote | Reescrito |
| `inst/shiny/anQuali/app.R` (803 linhas) | Funções estatísticas + UI no mesmo arquivo | Lógica migrada para `anova_exp()`, `compare_means()`, `check_assumptions()` |
| `inst/shiny/anQuant/app.R` (835 linhas) | `anQuant()` estatístico interno + UI | Lógica migrada e ampliada em `fit_regression()`, `optimum_dose()` |
| `inst/shiny/anIC/app.R` (585 linhas) | `plotIC()` + UI | Migrado para `conf_int_groups()` |
| `inst/shiny/*/www/Wilhan.png` (3 × 11,7 MB) | Foto pessoal, repetida | Removida (autoria vai para README/DESCRIPTION) |
| `.RData`, `.RData 2`, `.Rhistory`, `.DS_Store`, `.Rproj.user/`, `~$xto_caagro.docx`, `.git/` | Arquivos de sessão/sistema | Removidos da distribuição |
| `Artigo/texto_caagro.docx` | Manuscrito (Valasco e Baida) | Não distribuído no pacote (material do autor) |
| `ex.csv` | Curva x;y (9 linhas) | Movido para `data-raw/` |
| `dadosteste_curva.xlsx` | Severidade × tempo, 4 tratamentos | Mantido como `inst/extdata/severidade.xlsx`, com nota sobre procedência não documentada e valores negativos |
| `man/*.Rd`, `NAMESPACE` | Gerados por roxygen | Regenerados |

Tamanho: 34 MB (versão 0.x) → 1,3 MB (tarball 1.0.0).

## 2. Dependências da versão 0.x

`DESCRIPTION` declarava 26 pacotes em Imports, vários sem uso (shinyauthr, sodium,
httr, curl, progress, xfun, highr, renv, writexl, shinydashboard, shinythemes,
shinyWidgets, stringr, tibble, broom, plotly). Cada `app.R` **instalava pacotes
automaticamente** ao abrir (`install.packages()` dentro de `use_package()`) e
carregava tudo com `library()`.

Versão 1.0.0 — Imports: bslib, cluster, DT, ggplot2, jsonlite, mvtnorm, openxlsx,
readxl, rlang, shiny (+ pacotes base). Nenhuma instalação automática.

## 3. Problemas estatísticos e computacionais identificados

### anQuant (regressão)
1. **Nível de confiança invertido**: o slider "Nível de confiança" (padrão 0,95) era
   passado como `conf.level = 1 - 0.95`, produzindo bandas de 5%.
2. Curvas ajustadas **somente às médias** (ou mediana/mín/máx) por dose; R², erros-padrão
   e bandas ignoravam a variabilidade entre repetições.
3. Modelo chamado "Logístico" com código `Log` (confusão com logarítmico).
4. Bandas de modelos não lineares por simulação Monte Carlo sem semente e com
   *fallback* para `pred ± z·sd(resíduos)` quando `vcov` falhava (resultado aparentemente
   válido para um ajuste problemático).
5. `nls(..., warnOnly = TRUE)`: ajustes não convergentes eram apresentados como válidos.
6. Ponto de máximo para modelos não quadráticos era sempre a maior dose.
7. Ajustes por fator descartados silenciosamente (`try(...); next`).

### anQuali (fatores qualitativos)
8. "Barras de erro" eram os limites dos bigodes do boxplot (Q1 − 1,5·IQR; Q3 + 1,5·IQR),
   não intervalos de confiança.
9. Shapiro-Wilk aplicado à **resposta bruta**, não aos resíduos do modelo.
10. LSD sempre com ajuste de Bonferroni, sem informar.
11. Remoção de outliers por **substituição pela mediana** do grupo (altera os dados).
12. Teste da mediana com letras de grupos apresentadas no mesmo gráfico de médias.
13. Rótulo "Nível de confiança" para o que era o nível de significância.
14. Sem verificação de interação ou de estrutura do DBC; letras sempre exibidas.

### anIC
15. **Caixa "Remover outliers" invertida**: desmarcada (padrão) ⇒ outliers removidos;
    marcada ⇒ mantidos.
16. Remoção de outliers dentro de cada célula tratamento × bloco (com 1 observação por
    célula, IQR = 0).
17. Falhas na remoção capturadas por `tryCatch` retornando os dados sem aviso.

### Gerais
18. Interface com aba pessoal (foto, e-mail, WhatsApp) e imagens carregadas de site externo.
19. Funções estatísticas duplicadas dentro dos apps, não testáveis isoladamente; nenhum teste.
20. Gráficos em base R (`recordPlot`) e ggplot2 misturados; exportação apenas PNG.

## 4. Recursos reaproveitados

* Ideia de bandas e ponto de máximo do `anQuant` (MET do quadrático) — reescrita com
  método delta e intervalos.
* Testes oferecidos no `anQuali` (Tukey, SNK, LSD, Scott-Knott, mediana) — reimplementados
  e validados; acrescidos Dunnett, Games-Howell, Holm/Bonferroni.
* Opção de variâncias combinadas × separadas do `anIC` — preservada em `conf_int_groups()`.
* Indicadores CV%, Bartlett, QM do resíduo — mantidos e ampliados.
* Comportamento legado de ajuste às médias — disponível em `aggregate = "mean"`, com advertência.

## 5. Arquitetura adotada

```
R/
  data.R              importação, validação, outliers, formato longo
  result.R            classe caagro_result (print/summary/as.data.frame)
  emm.R, anova.R      modelos lineares, médias marginais, Welch, contrastes
  multcomp.R          comparações múltiplas + letras (Piepho)
  diagnostics.R       pressupostos e influência
  ttest.R, confint.R  testes t, IC clássicos e bootstrap
  regression*.R       registro de modelos, ajuste, bandas, grandezas agronômicas
  nonparametric.R     testes de postos e post-hoc
  multivariate.R      PCA, hclust, k-means, correlação
  descriptive.R       estatística descritiva
  theme.R, plots*.R   design system, estilo universal, métodos plot()
  export.R            figuras, Excel, relatório, script, JSON
  assistant.R         assistente baseado em regras
  app*.R, mod_*.R     Shiny modular (bslib) — apenas chamam as funções acima
inst/app/www          CSS e logotipo
inst/extdata          arquivos de exemplo para importação
data/                 conjuntos simulados (data-raw/make_data.R)
tests/testthat        validação contra implementações de referência
vignettes/            7 vinhetas em português
```
