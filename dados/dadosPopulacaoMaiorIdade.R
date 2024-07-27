library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Obter o diretório do script atual
script_dir <- dirname(normalizePath(sys.frame(1)$ofile))

# Caminho relativo ao diretório do script
caminho_csv <- file.path(script_dir, "População Maior de Idade", "População Maior de Idade - 2018 a 2023 - Editado.csv")

# Obter o diretório do arquivo CSV
csv_dir <- dirname(caminho_csv)

# Ler os dados do arquivo CSV usando o delimitador correto
dados_csv <- read_delim(caminho_csv, delim = ";", col_types = cols(.default = "c"))

# Verificar a estrutura dos dados lidos
print(colnames(dados_csv))
print(head(dados_csv))

# Garantir que todas as colunas numéricas estão no formato correto
dados_csv <- dados_csv %>%
  mutate(across(where(is.character), ~ as.numeric(gsub("[^0-9.]", "", .))))

# Verificar a presença de NA após a conversão
print(colSums(is.na(dados_csv)))

# Substituir NA por 0 nas colunas numéricas
dados_csv <- dados_csv %>%
  replace_na(list(Total = 0))

# Calcular a soma das linhas por ano
somas_por_ano <- dados_csv %>%
  group_by(Ano) %>%
  summarise(Total = sum(across(where(is.numeric)), na.rm = TRUE))

# Remover quaisquer linhas onde 'Ano' seja NA
somas_por_ano <- somas_por_ano %>%
  drop_na(Ano)

# Exibir os resultados completos
print(somas_por_ano)

# Salvar os dados processados como CSV
write_csv(somas_por_ano, file.path(csv_dir, "resultados_populacao_maior_idade.csv"))

# Salvar como objeto RDS
saveRDS(somas_por_ano, file.path(csv_dir, "resultados_populacao_maior_idade.rds"))

# Criar o gráfico de evolução com os dados atuais
ggplot() +
  geom_line(data = somas_por_ano, aes(x = Ano, y = Total), color = "blue") +
  geom_point(data = somas_por_ano, aes(x = Ano, y = Total), color = "orange") +
  geom_text(data = somas_por_ano, aes(x = Ano, y = Total, label = Total), vjust = -0.5, hjust = 0.5, color = "blue") +
  labs(title = "Evolução da População Maior de Idade",
       x = "Ano",
       y = "Total de População Maior de Idade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Salvar o gráfico em um arquivo PNG
ggsave(file.path(csv_dir, "evolucao_populacao_maior_idade.png"))
