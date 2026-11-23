# caagro 1.2.1

* Todos os gráficos têm fundo transparente, no aplicativo e na exportação
  (o fundo transparente passou a ser o padrão do editor; branco continua
  disponível).
* DESCRIPTION com `URL` apontando para o README no GitHub e `BugReports`.
* README reescrito como manual de uso completo em português (instalação,
  fluxo de análise, organização da planilha, cada módulo, editor gráfico,
  exportação, reprodutibilidade, interpretação e problemas comuns).

# caagro 1.2.0

* Autoria: Walter Baida Garcia Coutinho (Universidade Federal de Viçosa)
  incluído como autor; afiliações e ORCID dos autores no DESCRIPTION,
  CITATION e README.
* Interface de programação reduzida a uma única função exportada:
  `run_app()`. O módulo pode ser escolhido com
  `run_app(modulo = "anova")` (também `"regressao"`, `"nao_parametrica"`,
  `"intervalos"`, `"multivariada"`, `"exploratoria"`).
* Removidos da exportação: `run_anova()`, `run_regression()`,
  `run_nonparametric()`, `run_confidence()`, `run_multivariate()`,
  `run_exploratory()`, os lançadores antigos `anQuali()`, `anQuant()` e
  `anIC()` e todas as funções de análise, gráficos e exportação. Elas
  continuam no pacote como funções internas, usadas pelo aplicativo e pelos
  scripts reproduzíveis exportados (`caagro:::anova_exp()` etc.).
* Vinhetas substituídas por um único guia do aplicativo.

# caagro 1.1.0

Auditoria de usabilidade, interface e estatística.

## Estatística

* A ANOVA convencional de Fisher (somas de quadrados sequenciais, como
  `aov()`) passou a ser o padrão de `anova_exp()`; tipos II e III continuam
  disponíveis. A ANOVA de Welch virou alternativa (`method = "welch"`).
* `check_assumptions()` apresenta Shapiro-Wilk (resíduos) e Bartlett como
  testes principais, com interpretação em linguagem simples; Levene,
  Brown-Forsythe e Fligner-Killeen ficam na tabela `robust`. Em fatoriais,
  Bartlett passou a usar as combinações de tratamentos (antes usava apenas o
  primeiro fator).
* Dunnett deixou de exibir "letras" (`controle`, `*`, `ns`) na coluna de
  grupos; agora há a coluna `vs_controle`.
* Nova tabela `dms` (diferença mínima significativa) para Tukey e LSD.
* Corrigido erro na ANOVA tipo I com termos confundidos (ex.: `npk`).
* Em fatoriais com interação significativa, o aplicativo compara os níveis
  de um fator dentro dos níveis do outro automaticamente.

## API (menos funções)

* 53 → 41 funções exportadas. Removidas da exportação (continuam internas):
  `welch_anova()` → `anova_exp(method = "welch")`; `contrast_test()` →
  `compare_means(contrasts = )`; `conf_int_groups()` →
  `conf_int(parameter = "group_means")`; `conf_int_boot()` →
  `conf_int(method = "bootstrap")`; `kmeans_exp()` →
  `cluster_exp(method = "kmeans")`; `compare_curves()` → tabela
  `curve_test` de `fit_regression()`; `export_config()` →
  `export_script(config = TRUE)`; `apply_style()`, `style_code()`,
  `plot_types()`, `regression_models()`, `data_sheets()`.

## Interface

* Português brasileiro com acentuação em toda a interface, mensagens,
  tabelas, relatórios e planilhas exportadas; números com vírgula decimal.
* Botão "Carregar exemplo" em todos os módulos, com descrição do
  experimento, variáveis, configuração e o que observar; os campos são
  preenchidos automaticamente.
* Resultados reorganizados: resumo compacto → tabela principal (Fonte de
  variação | GL | SQ | QM | F | Valor-p) → comparação de médias
  (Tratamento | Média | Grupo) → Pressupostos da ANOVA. Textos explicativos e
  detalhes em painéis recolhidos; opções avançadas recolhidas.
* Corrigido: "Executar análise" não mostrava resultados até o usuário abrir a
  aba Análise; tabelas com grande área vazia; menu de navegação quebrando em
  1024 px; menu móvel que não fechava; controles escondidos em celulares;
  campos de exportação comprimidos.
* Removido o interruptor "Modo científico" (as explicações já ficam
  recolhidas).
* Novos conjuntos simulados: `sim_soja` e `sim_notas`.

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
