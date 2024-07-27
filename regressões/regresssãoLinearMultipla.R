# Carregar pacotes necessários
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(purrr)
library(stargazer)
library(gridExtra) # Para criar imagens de tabelas

# Definir o diretório do script atual
script_dir <- dirname(normalizePath(sys.frame(1)$ofile))

# Caminho relativo aos dados
dados_dir <- file.path(dirname(script_dir), "dados")

# Caminho para os arquivos
caminho_sem_abrigo <- file.path(dados_dir, "Inquérito de caracterização das pessoas em situação de sem abrigo", "resultados_sem_abrigo.rds")
caminho_populacao <- file.path(dados_dir, "População Maior de Idade", "resultados_populacao_maior_idade.rds")
caminho_taxa_desemprego <- file.path(dados_dir, "Taxa Desemprego", "resultados_taxa_desemprego.csv")
caminho_valor_renda <- file.path(dados_dir, "Valor Renda m2 Habitação", "resultados_valor_renda.csv")
caminho_valor_venda <- file.path(dados_dir, "Valor Transcções m2 Habitação", "resultados_valor_venda.csv")

# Carregar dados dos diferentes scripts
dados_sem_abrigo <- readRDS(caminho_sem_abrigo)
dados_populacao <- readRDS(caminho_populacao)
taxa_desemprego <- read_csv(caminho_taxa_desemprego)
valor_renda <- read_csv(caminho_valor_renda)
valor_venda <- read_csv(caminho_valor_venda)

# Verificar a estrutura dos dados carregados
print("Estrutura dos dados carregados:")
str(dados_sem_abrigo)
str(dados_populacao)
str(taxa_desemprego)
str(valor_renda)
str(valor_venda)

# Renomear colunas para garantir consistência
if ("Total" %in% colnames(dados_populacao)) {
  dados_populacao <- dados_populacao %>% rename(Populacao = Total)
}
if ("Total" %in% colnames(dados_sem_abrigo)) {
  dados_sem_abrigo <- dados_sem_abrigo %>% rename(PSSA = Total)
}

taxa_desemprego <- taxa_desemprego %>% rename(Desemprego = Taxa)
valor_renda <- valor_renda %>% rename(Renda_m2 = Valor)
valor_venda <- valor_venda %>% rename(Venda_m2 = Valor)

# Selecionar colunas necessárias e unir os dados em um único dataframe
dados_consolidados <- list(dados_sem_abrigo, dados_populacao, taxa_desemprego, valor_renda, valor_venda) %>%
  reduce(full_join, by = "Ano")

# Verificar a estrutura e o resumo dos dados consolidados
print("Estrutura dos dados consolidados:")
str(dados_consolidados)

print("Resumo dos dados consolidados:")
summary(dados_consolidados)

# Ajustar modelos de regressão linear múltipla com 2 variáveis
modelos_2vars <- list(
  "Populacao + Desemprego" = lm(PSSA ~ Populacao + Desemprego, data = dados_consolidados),
  "Populacao + Renda_m2" = lm(PSSA ~ Populacao + Renda_m2, data = dados_consolidados),
  "Populacao + Venda_m2" = lm(PSSA ~ Populacao + Venda_m2, data = dados_consolidados),
  "Desemprego + Renda_m2" = lm(PSSA ~ Desemprego + Renda_m2, data = dados_consolidados),
  "Desemprego + Venda_m2" = lm(PSSA ~ Desemprego + Venda_m2, data = dados_consolidados),
  "Renda_m2 + Venda_m2" = lm(PSSA ~ Renda_m2 + Venda_m2, data = dados_consolidados)
)

resultados_2vars <- lapply(modelos_2vars, summary)

# Ajustar modelos de regressão linear múltipla com 3 variáveis
modelos_3vars <- list(
  "Populacao + Desemprego + Renda_m2" = lm(PSSA ~ Populacao + Desemprego + Renda_m2, data = dados_consolidados),
  "Populacao + Desemprego + Venda_m2" = lm(PSSA ~ Populacao + Desemprego + Venda_m2, data = dados_consolidados),
  "Populacao + Renda_m2 + Venda_m2" = lm(PSSA ~ Populacao + Renda_m2 + Venda_m2, data = dados_consolidados),
  "Desemprego + Renda_m2 + Venda_m2" = lm(PSSA ~ Desemprego + Renda_m2 + Venda_m2, data = dados_consolidados)
)

resultados_3vars <- lapply(modelos_3vars, summary)

# Ajustar modelo de regressão linear múltipla com 4 variáveis
modelo_4vars <- lm(PSSA ~ Populacao + Desemprego + Renda_m2 + Venda_m2, data = dados_consolidados)
resultado_4vars <- summary(modelo_4vars)

# Função para salvar os resultados em um arquivo
salvar_resultados <- function(resultados, nome_arquivo) {
  sink(nome_arquivo)
  print(resultados)
  sink()
}

# Salvar resultados dos modelos
salvar_resultados(resultados_2vars, file.path(script_dir, "resultados_modelos_2vars.txt"))
salvar_resultados(resultados_3vars, file.path(script_dir, "resultados_modelos_3vars.txt"))
salvar_resultados(resultado_4vars, file.path(script_dir, "resultado_modelo_4vars.txt"))

# Criar tabelas de resultados com stargazer
stargazer(modelos_2vars, title="Resultados das Regressões Lineares Múltiplas com 2 Variáveis", type="text", out=file.path(script_dir, "tabela_modelos_2vars.txt"))
stargazer(modelos_3vars, title="Resultados das Regressões Lineares Múltiplas com 3 Variáveis", type="text", out=file.path(script_dir, "tabela_modelos_3vars.txt"))
stargazer(modelo_4vars, title="Resultado da Regressão Linear Múltipla com 4 Variáveis", type="text", out=file.path(script_dir, "tabela_modelo_4vars.txt"))

# Função para salvar tabelas como imagens
salvar_tabela_imagem <- function(titulo, caminho) {
  tabela <- readLines(caminho)
  tabela <- matrix(tabela, ncol = 1)
  tabela_grob <- tableGrob(tabela, theme = ttheme_default(core = list(fg_params = list(hjust = 0.5, x = 0.5),
                                                                      bg_params = list(fill = c("white", "lightblue"), col = "black")),
                                                         colhead = list(fg_params = list(hjust = 0.5, x = 0.5, fontface = "bold")),
                                                         rowhead = list(fg_params = list(hjust = 0.5, x = 0.5))))
  p <- ggplot() + 
    theme_void() + 
    annotation_custom(grob = tabela_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    ggtitle(titulo) +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  ggsave(paste0(caminho, ".png"), plot = p, width = 12, height = 6)
}

# Salvar tabelas de resultados como imagens
salvar_tabela_imagem("Resultados das Regressões Lineares Múltiplas com 2 Variáveis", file.path(script_dir, "tabela_modelos_2vars.txt"))
salvar_tabela_imagem("Resultados das Regressões Lineares Múltiplas com 3 Variáveis", file.path(script_dir, "tabela_modelos_3vars.txt"))
salvar_tabela_imagem("Resultado da Regressão Linear Múltipla com 4 Variáveis", file.path(script_dir, "tabela_modelo_4vars.txt"))

# Exibir mensagens de conclusão
print("Modelos de regressão ajustados e resultados salvos com sucesso.")
