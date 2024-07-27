library(readr)
library(dplyr)

# Caminhos dos arquivos CSV e RDS
dados_dir <- "C:/SAD/dados"
caminho_taxa_desemprego <- file.path(dados_dir, "Taxa Desemprego", "resultados_taxa_desemprego.csv")
caminho_valor_renda <- file.path(dados_dir, "Valor Renda m2 Habitação", "resultados_valor_renda.csv")
caminho_valor_venda <- file.path(dados_dir, "Valor Transcções m2 Habitação", "resultados_valor_venda.csv")

# Carregar dados dos diferentes arquivos
taxa_desemprego <- read_csv(caminho_taxa_desemprego)
valor_renda <- read_csv(caminho_valor_renda)
valor_venda <- read_csv(caminho_valor_venda)

# Verificar a estrutura dos dados
print("Estrutura da taxa de desemprego:")
print(str(taxa_desemprego))

print("Estrutura do valor da renda:")
print(str(valor_renda))

print("Estrutura do valor da venda:")
print(str(valor_venda))

# Normalizar a taxa de desemprego para valores anuais
taxa_desemprego_anual <- taxa_desemprego %>%
  group_by(Ano) %>%
  summarize(Valor_Anual = mean(Taxa, na.rm = TRUE))

print("Taxa de desemprego anual:")
print(taxa_desemprego_anual)

# Normalizar o valor da renda para valores anuais
valor_renda_anual <- valor_renda %>%
  group_by(Ano) %>%
  summarize(Valor_Anual = mean(Valor, na.rm = TRUE))

print("Valor da renda anual:")
print(valor_renda_anual)

# Normalizar o valor da venda para valores anuais
valor_venda_anual <- valor_venda %>%
  group_by(Ano) %>%
  summarize(Valor_Anual = mean(Valor, na.rm = TRUE))

print("Valor da venda anual:")
print(valor_venda_anual)

# Salvar os dados normalizados como arquivos RDS
saveRDS(taxa_desemprego_anual, file.path(dados_dir, "Taxa Desemprego", "resultados_taxa_desemprego_anual.rds"))
saveRDS(valor_renda_anual, file.path(dados_dir, "Valor Renda m2 Habitação", "resultados_valor_renda_anual.rds"))
saveRDS(valor_venda_anual, file.path(dados_dir, "Valor Transcções m2 Habitação", "resultados_valor_venda_anual.rds"))
