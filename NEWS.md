# caagro 1.0.0

Reestruturação completa do pacote.

## Novidades

* Arquitetura modular: motores estatísticos independentes do Shiny, objeto
  padronizado `caagro_result` com métodos `print()`, `summary()`, `plot()` e
  `as.data.frame()`.
* Aplicação principal `run_app()` e aplicativos `run_anova()`,
  `run_regression()`, `run_nonparametric()`, `run_confidence()`,
  `run_multivariate()` e `run_exploratory()` com identidade visual comum
  (bslib), modo claro/escuro e "modo científico".
* Importação de `.xlsx`, `.xls`, `.csv`, `.txt`; validação (`check_data()`),
  conversão de tipos, formato largo para longo, sinalização de valores
  atípicos (`flag_outliers()`) e exclusões explícitas registradas.
* ANOVA em DIC, DBC e fatoriais (`anova_exp()`) com SQ tipo I/II/III, médias
  marginais estimadas, CV%, tamanhos de efeito; Welch, testes t e contrastes.
* Comparações múltiplas (`compare_means()`): Tukey-Kramer, SNK, LSD
  (protegido ou não), Scott-Knott, Dunnett, Bonferroni, Holm, Games-Howell;
  efeitos simples em fatoriais; letras pelo algoritmo de Piepho (2004).
* Diagnósticos (`check_assumptions()`): Shapiro-Wilk nos resíduos, Bartlett,
  Levene, Brown-Forsythe, Fligner-Killeen, Cook, alavancagem.
* Regressão (`fit_regression()`) com 16 modelos, bandas de confiança e
  predição (exatas, método delta ou bootstrap), comparação por AICc/BIC,
  múltiplas curvas, teste de igualdade de curvas (`compare_curves()`),
  MET, dose econômica e ED50 (`optimum_dose()`).
* Não paramétricos (`np_test()`, `np_posthoc()`): Kruskal-Wallis, Friedman,
  Mood, Mann-Whitney, Wilcoxon pareado, sinais, permutação; Dunn e Conover.
* Intervalos de confiança clássicos e bootstrap (percentil e BCa).
* Multivariada: PCA, agrupamento hierárquico, k-means, matrizes de correlação.
* Editor gráfico universal (`caagro_style()`), cinco temas, exportação
  PNG/TIFF/SVG/PDF re-renderizada (`save_plot()`).
* Exportação: Excel estruturado, relatório HTML/Word/PDF, script R e
  configuração JSON.
* Assistente de análise baseado em regras (`suggest_method()`).

## Correções em relação à versão 0.x

* `anIC`: a caixa "Remover outliers" estava invertida (com a opção
  desmarcada, os outliers eram removidos).
* `anQuant`: o nível de confiança era usado como `1 - nível` (bandas de 5%
  em vez de 95%); as curvas eram ajustadas apenas às médias; o modelo
  "Logístico" estava identificado como `Log`.
* `anQuali`: barras de erro eram limites de bigode (IQR) e não intervalos de
  confiança; Shapiro-Wilk era aplicado à resposta bruta; o LSD aplicava
  sempre Bonferroni; outliers eram substituídos pela mediana.
* Removidos: instalação automática de pacotes ao abrir os apps,
  dependências não usadas (shinyauthr, sodium, httr, curl, etc.), aba com
  dados pessoais do autor, arquivos de sessão (`.RData`, `.Rhistory`,
  `.DS_Store`) e imagens de 11 MB repetidas.

## Compatibilidade

* `anQuali()`, `anQuant()` e `anIC()` continuam disponíveis e abrem os novos
  módulos equivalentes.
