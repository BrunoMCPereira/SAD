library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Configurar a opção para exibir gráficos no VSCode
options(vsc.plot = TRUE)

# Obter o diretório do script atual
script_dir <- dirname(normalizePath(sys.frame(1)$ofile))

# Caminho relativo ao diretório dos arquivos Excel
excel_dir <- file.path(script_dir, "Inquérito de caracterização das pessoas em situação de sem abrigo")

# Construir os caminhos dos arquivos Excel
caminhos <- file.path(excel_dir, c(
  "Situação de sem abrigo – 31 de dezembro 2022 – Dados.xlsx",
  "Situação de sem abrigo - 31 dezembro 2018 - Dados.xlsx",
  "Situação de sem abrigo - 31 dezembro 2019 - Dados.xlsx",
  "Situação de sem abrigo – 31 de dezembro 2020 – Dados.xlsx",
  "Situação de sem abrigo - 31 dezembro 2021 - Dados.xlsx"
))

resultados <- lapply(caminhos, function(caminho) {
  ano <- sub(".*?(\\d{4}).*", "\\1", basename(caminho))
  sheet_name <- paste(ano, "Concelhos", sep = "_")

  dados <- suppressMessages(read_excel(caminho, sheet = sheet_name))
  
  # Selecionar apenas as colunas de interesse para evitar colunas extras sem nome
  dados <- dados %>% select(`NUT II`, Concelho, PSSA, PST, PSC)

  # Verificar presença de "Área Metropolitana de Lisboa"
  if (!any(grepl("Área Metropolitana de Lisboa", dados$`NUT II`))) {
    cat("Área Metropolitana de Lisboa não encontrada no ano:", ano, "\n")
    return(NULL)
  }

  # Filtrar linhas da Área Metropolitana de Lisboa pela coluna "NUT II"
  dados_lisboa <- filter(dados, grepl("Área Metropolitana de Lisboa", dados$`NUT II`))

  # Selecionar colunas de interesse e substituir valores não numéricos por 0
  dados_interesse <- select(dados_lisboa, Concelho, PSSA) %>%
    mutate(PSSA = as.numeric(gsub("[^0-9]", "", PSSA))) %>%
    replace_na(list(PSSA = 0))

  # Calcular a soma das colunas de interesse
  soma_valores <- sum(dados_interesse$PSSA, na.rm = TRUE)

  return(data.frame(Ano = as.numeric(ano), PSSA = soma_valores))
})

# Combinar todos os resultados em um único dataframe e ordenar por ano
resultados_finais <- bind_rows(resultados) %>%
  arrange(Ano)

# Exibir os resultados completos
print(resultados_finais)

# Salvar os dados processados como CSV
write.csv(resultados_finais, file.path(excel_dir, "resultados_sem_abrigo.csv"), row.names = FALSE)

# Salvar como objeto RDS
saveRDS(resultados_finais, file.path(excel_dir, "resultados_sem_abrigo.rds"))

# Criar o gráfico de evolução com os dados atuais
ggplot() +
  geom_line(data = resultados_finais, aes(x = Ano, y = PSSA), color = "blue") +
  geom_point(data = resultados_finais, aes(x = Ano, y = PSSA), color = "red") +
  geom_text(data = resultados_finais, aes(x = Ano, y = PSSA, label = PSSA), vjust = -0.5, hjust = 0.5) +
  labs(title = "Evolução do Total de Pessoas em Situação de Sem-Abrigo na Área Metropolitana de Lisboa",
       x = "Ano",
       y = "Pessoas em Situação de Sem-Abrigo (PSSA)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Salvar o gráfico em um arquivo PNG
ggsave(file.path(excel_dir, "evolucao_pssa.png"))
