library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(purrr)

# Obter o diretório do script atual
script_dir <- dirname(normalizePath(sys.frame(1)$ofile))

# Caminho relativo ao diretório dos objetos salvos
dados_dir <- file.path(dirname(script_dir), "dados")

# Caminhos dos arquivos RDS e CSV
caminho_sem_abrigo <- file.path(dados_dir, "Inquérito de caracterização das pessoas em situação de sem abrigo", "resultados_sem_abrigo.rds")
caminho_populacao <- file.path(dados_dir, "População Maior de Idade", "resultados_populacao_maior_idade.rds")
caminho_taxa_desemprego <- file.path(dados_dir, "Taxa Desemprego", "resultados_taxa_desemprego_anual.csv")
caminho_valor_renda <- file.path(dados_dir, "Valor Renda m2 Habitação", "resultados_valor_renda_anual.csv")
caminho_valor_venda <- file.path(dados_dir, "Valor Transcções m2 Habitação", "resultados_valor_venda_anual.csv")

# Carregar dados dos diferentes scripts
dados_sem_abrigo <- readRDS(caminho_sem_abrigo)
dados_populacao <- readRDS(caminho_populacao)
taxa_desemprego <- read_csv(caminho_taxa_desemprego)
valor_renda <- read_csv(caminho_valor_renda)
valor_venda <- read_csv(caminho_valor_venda)

# Renomear colunas para garantir consistência
taxa_desemprego <- taxa_desemprego %>% rename(Desemprego = Valor_Anual)
valor_renda <- valor_renda %>% rename(Renda_m2 = Valor_Anual)
valor_venda <- valor_venda %>% rename(Venda_m2 = Valor_Anual)
dados_populacao <- dados_populacao %>% rename(Populacao = Total)

# Selecionar apenas as colunas necessárias
dados_sem_abrigo <- dados_sem_abrigo %>% select(Ano, PSSA)
dados_populacao <- dados_populacao %>% select(Ano, Populacao)
taxa_desemprego <- taxa_desemprego %>% select(Ano, Desemprego)
valor_renda <- valor_renda %>% select(Ano, Renda_m2)
valor_venda <- valor_venda %>% select(Ano, Venda_m2)

# Unir os dados em um único dataframe por ano
dados_consolidados <- list(dados_sem_abrigo, dados_populacao, taxa_desemprego, valor_renda, valor_venda) %>%
  reduce(full_join, by = "Ano")

# Calcular a correlação de Pearson entre PSSA e cada variável individualmente
correlacoes_individuais <- data.frame(
  Variavel = c("Populacao", "Desemprego", "Renda_m2", "Venda_m2"),
  Correlacao = c(
    cor(dados_consolidados$PSSA, dados_consolidados$Populacao, use = "complete.obs"),
    cor(dados_consolidados$PSSA, dados_consolidados$Desemprego, use = "complete.obs"),
    cor(dados_consolidados$PSSA, dados_consolidados$Renda_m2, use = "complete.obs"),
    cor(dados_consolidados$PSSA, dados_consolidados$Venda_m2, use = "complete.obs")
  )
)

# Calcular a correlação de Pearson entre PSSA e combinações de duas variáveis
correlacoes_duplas <- data.frame(
  Combinacao = 1:6,
  Variavel1 = c("Populacao", "Populacao", "Populacao", "Desemprego", "Desemprego", "Renda_m2"),
  Variavel2 = c("Desemprego", "Renda_m2", "Venda_m2", "Renda_m2", "Venda_m2", "Venda_m2"),
  Correlacao = c(
    cor(dados_consolidados$PSSA, dados_consolidados$Populacao + dados_consolidados$Desemprego, use = "complete.obs"),
    cor(dados_consolidados$PSSA, dados_consolidados$Populacao + dados_consolidados$Renda_m2, use = "complete.obs"),
    cor(dados_consolidados$PSSA, dados_consolidados$Populacao + dados_consolidados$Venda_m2, use = "complete.obs"),
    cor(dados_consolidados$PSSA, dados_consolidados$Desemprego + dados_consolidados$Renda_m2, use = "complete.obs"),
    cor(dados_consolidados$PSSA, dados_consolidados$Desemprego + dados_consolidados$Venda_m2, use = "complete.obs"),
    cor(dados_consolidados$PSSA, dados_consolidados$Renda_m2 + dados_consolidados$Venda_m2, use = "complete.obs")
  )
)

# Calcular a correlação de Pearson entre PSSA e combinações de três variáveis
correlacoes_triplas <- data.frame(
  Combinacao = 1:4,
  Variavel1 = c("Populacao", "Populacao", "Populacao", "Desemprego"),
  Variavel2 = c("Desemprego", "Desemprego", "Renda_m2", "Renda_m2"),
  Variavel3 = c("Renda_m2", "Venda_m2", "Venda_m2", "Venda_m2"),
  Correlacao = c(
    cor(dados_consolidados$PSSA, dados_consolidados$Populacao + dados_consolidados$Desemprego + dados_consolidados$Renda_m2, use = "complete.obs"),
    cor(dados_consolidados$PSSA, dados_consolidados$Populacao + dados_consolidados$Desemprego + dados_consolidados$Venda_m2, use = "complete.obs"),
    cor(dados_consolidados$PSSA, dados_consolidados$Populacao + dados_consolidados$Renda_m2 + dados_consolidados$Venda_m2, use = "complete.obs"),
    cor(dados_consolidados$PSSA, dados_consolidados$Desemprego + dados_consolidados$Renda_m2 + dados_consolidados$Venda_m2, use = "complete.obs")
  )
)

# Calcular a correlação de Pearson entre PSSA e combinações de quatro variáveis
correlacoes_quadruplas <- data.frame(
  Combinacao = 1,
  Variavel1 = "Populacao",
  Variavel2 = "Desemprego",
  Variavel3 = "Renda_m2",
  Variavel4 = "Venda_m2",
  Correlacao = cor(dados_consolidados$PSSA, dados_consolidados$Populacao + dados_consolidados$Desemprego + dados_consolidados$Renda_m2 + dados_consolidados$Venda_m2, use = "complete.obs")
)

# Imprimir tabelas de correlações
print("Correlação Individual:")
print(correlacoes_individuais)

print("Correlação Dupla:")
print(correlacoes_duplas)

print("Correlação Tripla:")
print(correlacoes_triplas)

print("Correlação Quadrupla:")
print(correlacoes_quadruplas)

# Criar imagens das tabelas de correlações
p1 <- tableGrob(correlacoes_individuais)
p2 <- tableGrob(correlacoes_duplas)
p3 <- tableGrob(correlacoes_triplas)
p4 <- tableGrob(correlacoes_quadruplas)

# Salvar as tabelas de correlações como imagens no mesmo diretório dos dados
ggsave(file.path(dados_dir, "correlacoes_individuais.png"), p1, width = 6, height = 4)
ggsave(file.path(dados_dir, "correlacoes_duplas.png"), p2, width = 8, height = 6)
ggsave(file.path(dados_dir, "correlacoes_triplas.png"), p3, width = 10, height = 8)
ggsave(file.path(dados_dir, "correlacoes_quadruplas.png"), p4, width = 12, height = 6)
