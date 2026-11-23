# Auditoria crítica do caagro 1.0.0 → versão 1.1.0

Data: 23/11/2026. Base: `caagro-1.0.0-fonte.zip` (idêntico à versão entregue anteriormente).

**Método da auditoria:**

- Leitura do código.
- Execução das funções contra referências do R.
- Navegação real do aplicativo num navegador automatizado (Playwright/Chromium), com cliques como os de um usuário, nas larguras 375, 768, 1024, 1366 e 1920 px.
- Detecção automática de textos cortados, elementos sobrepostos e rolagem horizontal.

Prioridades: **Crítico** (compromete cálculo, interpretação ou funcionamento), **Alto** (usabilidade/leitura), **Médio** (eficiência/consistência), **Baixo** (refinamento).

## A. Problemas encontrados e corrigidos

Cada problema segue o formato Problema → Causa → Consequência → Correção.

### Críticos

**C1. "Executar análise" não mostrava resultado**
- **Onde:** `mod_*.R`
- **Causa:** o cálculo usava `eventReactive`, que só é avaliado quando uma saída visível o solicita. As saídas ficavam na aba Análise, que estava oculta.
- **Consequência:** o usuário clicava em Executar e nada acontecia até abrir a aba Análise.
- **Correção:** `observeEvent` + `reactiveVal`. O cálculo é imediato e a aba Análise é aberta automaticamente.

**C2. Celular (375 px) sem resultados**
- **Onde:** `mod_*.R`, `caagro.css`
- **Causa:** os controles ficavam numa barra lateral recolhida. O Shiny suspende saídas ocultas, então os campos de variáveis não eram criados.
- **Consequência:** não era possível executar análises no celular.
- **Correção:**
  - campos de variáveis com `suspendWhenHidden = FALSE`;
  - barra lateral sempre visível e empilhada acima dos resultados (CSS para menos de 768 px);
  - rolagem automática até os resultados.

**C3. Bartlett errado em experimentos fatoriais**
- **Onde:** `diagnostics.R`
- **Causa:** os grupos eram definidos só pelo 1º fator.
- **Consequência:** variâncias de combinações diferentes eram misturadas, e o teste avaliava a hipótese errada.
- **Correção:** os grupos passam a ser as combinações de tratamentos (células), sobre os resíduos do modelo. Validado contra `bartlett.test` (gl = 8 no fatorial 3 × 3).

**C4. ANOVA tipo I falhava com termos confundidos**
- **Onde:** `anova.R`
- **Causa:** o código pressupunha uma linha da `anova()` para cada termo.
- **Consequência:** erro em delineamentos com confundimento (ex.: `npk`).
- **Correção:** os termos passam a ser pareados pelo nome, e os confundidos são relatados.

**C5. Dunnett exibia "letras" (`controle`, `*`, `ns`) na coluna de grupos**
- **Onde:** `multcomp.R`
- **Causa:** reaproveitamento da coluna de letras.
- **Consequência:** sugeria um agrupamento que o teste não produz.
- **Correção:** a coluna de letras fica vazia, e foi criada a coluna `vs_controle` ("difere / não difere do controle").

### Altos

**A1. ANOVA padrão era a de somas de quadrados tipo II**
- **Onde:** `anova.R`
- **Causa:** escolha de projeto.
- **Consequência:** divergia da ANOVA convencional (`aov`, ExpDes) em fatoriais desbalanceados.
- **Correção:** padrão = ANOVA de Fisher sequencial, idêntica a `aov`. Tipos II e III continuam disponíveis nas opções avançadas.

**A2. Welch competia com a ANOVA convencional**
- **Onde:** `mod_anova.R`
- **Causa:** a seleção "Análise" mostrava ANOVA, Welch e teste t lado a lado.
- **Consequência:** risco de o iniciante escolher Welch sem motivo.
- **Correção:** Welch e teste t foram movidos para "Opções avançadas". A ANOVA de Fisher é o padrão.

**A3. Interface sem acentuação**
- **Onde:** todos os arquivos `R/`
- **Causa:** strings em ASCII puro, exigência do CRAN para código R.
- **Consequência:** "Analise", "Diagnosticos", "Exportacao" etc. em toda a interface, nas mensagens e nos relatórios.
- **Correção:** revisão de todas as strings com dicionário (~300 palavras) e conversão automática para escapes `\uXXXX`. O código continua ASCII e o texto exibido fica acentuado. O `R CMD check` confirma: "non-ASCII characters ... OK".

**A4. Exemplos fracos**
- **Onde:** `mod_data.R`
- **Causa:** a opção "Exemplo" era uma lista de conjuntos sem descrição.
- **Consequência:** os campos ficavam com variáveis adivinhadas (resposta = grupo), o que gerava erros de "need numeric data".
- **Correção:**
  - botão **Carregar exemplo** em todos os módulos (8 exemplos);
  - cada exemplo traz objetivo, variáveis, configuração e o que observar;
  - preenchimento automático dos campos e download dos dados.

**A5. Excesso de informação na tela**
- **Onde:** `mod_*.R`
- **Causa:** 6 cartões de indicadores, 3 notas longas abertas e 5 tabelas no mesmo nível.
- **Consequência:** poluição visual, pouca hierarquia e o resultado principal escondido.
- **Correção:** ordem fixa: resumo compacto (5 números + 1 frase) → quadro da ANOVA → Tratamento | Média | Grupo → Pressupostos. Os detalhes ficam em painéis recolhidos.

**A6. Shapiro-Wilk e Bartlett misturados a 3 outros testes**
- **Onde:** `diagnostics.R`
- **Causa:** tabela única com 5 testes.
- **Consequência:** os dois testes pedidos não se destacavam.
- **Correção:**
  - seção própria "Pressupostos da ANOVA" com W/K², GL, valor-p e as frases de interpretação pedidas;
  - testes robustos na tabela `robust` (aba Diagnósticos);
  - quando um teste não se aplica, isso é informado (n < 3 ou n > 5000; grupo com uma observação).

**A7. Tabelas com cabeçalhos em inglês e ponto decimal**
- **Onde:** `app_helpers.R`, `export.R`
- **Causa:** exibição direta dos nomes internos (`term`, `df`, `ss`...).
- **Consequência:** "term / df / ss / ms" e "Residuals" apareciam na tela, no Excel e no relatório.
- **Correção:** camada de exibição "Fonte de variação | GL | SQ | QM | F | Valor-p", "Resíduo", vírgula decimal e valor-p "< 0,001". A API em R mantém os nomes originais (compatibilidade).

**A8. Tabelas com ~300 px de espaço vazio**
- **Onde:** `app_helpers.R`
- **Causa:** o widget DT preenchia a altura do cartão (`fill = TRUE`).
- **Consequência:** páginas longas e resultados distantes uns dos outros.
- **Correção:** `dt_out()` com `fill = FALSE` e altura automática.

**A9. Menu quebrava em 1024 px**
- **Onde:** `app.R`, `caagro.css`
- **Causa:** 7 itens com ícones + interruptor "Modo científico" + tema.
- **Consequência:** a barra ficava com duas linhas e "Intervalos" era cortado.
- **Correção:**
  - interruptor removido;
  - entre 992 e 1279 px, os ícones do menu somem e o espaçamento é reduzido;
  - `white-space: nowrap` nos itens.

**A10. Menu móvel não fechava após escolher um módulo**
- **Onde:** `app_helpers.R`
- **Causa:** comportamento padrão do Bootstrap.
- **Consequência:** o menu ficava aberto e cobria o conteúdo.
- **Correção:** script que fecha o menu ao clicar num item.

**A11. Fatorial com interação: médias marginais comparadas**
- **Onde:** `mod_anova.R`
- **Causa:** o app só avisava sobre a interação.
- **Consequência:** comparações de efeitos principais inadequadas.
- **Correção:** com interação significativa, o app compara os tratamentos dentro de cada nível do outro fator e explica isso no resumo.

### Médios

**M1. Funções demais (53 exportadas)**
- **Causa:** uma função para cada variante.
- **Consequência:** API difícil de aprender.
- **Correção:** 41 exportadas, com variantes viradas argumentos (lista abaixo). As funções antigas continuam internas.

**M2. Campos de exportação do gráfico espremidos (6 colunas)**
- **Onde:** `mod_plot_editor.R`
- **Consequência:** "300 dpi" quebrava em duas linhas e "Branco" era cortado.
- **Correção:** grade responsiva (3 colunas em telas largas, 2 em telas médias).

**M3. Abas de seção quebravam em 3 linhas no celular**
- **Onde:** CSS
- **Correção:** uma linha com rolagem horizontal.

**M4. DMS não era informada**
- **Onde:** `multcomp.R`
- **Correção:** tabela `dms` para Tukey ($q\sqrt{QM_{res}/r}$) e LSD ($t\sqrt{2QM_{res}/r}$), exibida no rodapé da comparação. Quando as repetições são desiguais, avisa que a DMS varia por par.

**M5. Rótulos de modelos em ASCII nos gráficos**
- **Onde:** `regression_models.R`
- **Consequência:** "Polinomial de 2o grau".
- **Correção:** "Quadrático", "Cúbico", "Linear-platô" etc.

**M6. Opções estatísticas avançadas visíveis antes de serem necessárias**
- **Onde:** todos os módulos
- **Correção:** cada módulo mostra 3 passos numerados (Dados → Variáveis → Método). O resto fica em "Opções avançadas", recolhido.

### Baixos

**B1. "observação(oes)", "apos"**
- **Correção:** grafias corrigidas ("observação(ões)", "após").

**B2. Mensagem de validação verde "Estrutura validada" sempre visível**
- **Correção:** só aparecem erros e alertas.

## B. Redução de funções (53 → 41)

| Antes | Agora |
|---|---|
| `welch_anova()` | `anova_exp(..., method = "welch")` |
| `contrast_test()` | `compare_means(x, contrasts = list(...))` |
| `conf_int_groups()` | `conf_int(..., parameter = "group_means", group = ...)` |
| `conf_int_boot()` | `conf_int(..., method = "bootstrap")` |
| `kmeans_exp()` | `cluster_exp(..., method = "kmeans")` |
| `compare_curves()` | tabela `curve_test` de `fit_regression()` (automática) |
| `export_config()` | `export_script(..., config = TRUE)` |
| `apply_style()`, `style_code()`, `plot_types()`, `regression_models()`, `data_sheets()` | internas (o editor gráfico usa; os tipos de gráfico estão em `?caagro_plots`, os modelos em `?fit_regression`) |

`anQuali()`, `anQuant()`, `anIC()`, os `run_*()` e as funções de análise principais foram mantidos.

## C. Evidências de validação

**Testes automatizados (testthat):** todos aprovados, 8 arquivos.
- **Novos testes estatísticos:**
  - ANOVA padrão = `anova(aov())` em fatorial desbalanceado (SQ e valores-p);
  - Bartlett com grupos = células no fatorial (= `bartlett.test`, gl 8);
  - Shapiro-Wilk = `shapiro.test(resid)`;
  - frases de interpretação;
  - DMS de Tukey e LSD = fórmulas de livro-texto;
  - Dunnett sem letras.
- **Testes de usabilidade** (`test-app.R`, `shiny::testServer`): para cada um dos 8 exemplos, simulam Carregar exemplo → preencher campos → Executar e conferem o resultado:
  - ANOVA soja: DIC + Tukey + pressupostos;
  - DBC: Scott-Knott;
  - Fatorial: interação → comparação dentro de adubação;
  - Regressão dose: quadrático com o menor AICc;
  - Germinação: ED50 dentro da faixa;
  - Notas: Kruskal-Wallis significativo + Dunn;
  - Intervalos;
  - PCA;
  - Exploratória.
- **Mantidos da versão 1.0:** Tukey = `TukeyHSD`, tipos II/III = `car::Anova`, médias ajustadas e Dunnett = `emmeans`, Games-Howell e Dunn = `rstatix`, Levene = `car`, regressões = `lm`/`glm`, BCa ≈ `boot`, PCA/hclust = `stats`.

**Inspeção visual automatizada:** 5 larguras × 7 telas (início + 6 módulos, com exemplo carregado e análise executada), mais Gráficos e Diagnósticos em 375 e 1366 px.
- 0 textos cortados;
- 0 sobreposições entre rótulos, botões, cabeçalhos e indicadores;
- 0 rolagem horizontal da página.
- As capturas reais estão em `man/figures/app-*.jpg`, incluindo `app-mobile.jpg`.

**R CMD check** (sem manual): **Status: OK**, 0 erros, 0 avisos, 0 notas. Inclui exemplos, testes, vinhetas e "non-ASCII characters ... OK".

## D. O que ainda precisa de atenção

1. **Locale UTF-8:**
   - O texto acentuado está em escapes `\uXXXX`. Se o pacote for instalado numa sessão R com locale C/POSIX (alguns servidores Docker), os rótulos de listas de opções aparecem como `<U+00E1>`.
   - macOS, Windows (R ≥ 4.2) e Linux com UTF-8 funcionam normalmente.
   - Em servidor, use `LANG=pt_BR.UTF-8` ou `C.UTF-8`.
2. **Ambiente de teste:**
   - Os testes rodaram com R 4.3.3, bslib 0.6.1 e DT 0.31 (pacotes do Ubuntu).
   - Com bslib ≥ 0.7 a barra lateral usa o modo nativo "always-above" no celular; esse caminho não foi testado aqui.
   - O 404 de `strftime-min.js` vem do empacotamento do DT no Ubuntu, não do caagro.
3. **SNK e Scott-Knott:** continuam validados contra o cálculo manual das fórmulas. Os pacotes agricolae e ScottKnott não puderam ser baixados neste ambiente; recomenda-se uma comparação cruzada antes de publicar.
4. **Gráficos:** os eixos usam ponto decimal (padrão do ggplot2); tabelas e textos usam vírgula. As unidades dos eixos precisam ser informadas no editor (o pacote não as conhece).
5. **Prioridades futuras:**
   - modelos mistos e parcelas subdivididas;
   - Skillings-Mack;
   - testes de ponta a ponta com shinytest2 em integração contínua.
