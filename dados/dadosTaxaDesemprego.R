library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Obter o diretório do script atual
script_dir <- dirname(normalizePath(sys.frame(1)$ofile))

# Caminho relativo ao diretório do script
caminho_excel <- file.path(script_dir, "Taxa Desemprego", "Taxa Desemprego Trimestral - 2018 a 1T2024 - Editado.xls")

# Obter o diretório do arquivo Excel
excel_dir <- dirname(caminho_excel)

# Ler os dados da folha 'Quadro' a partir da linha 4
print("Lendo os dados da folha 'Quadro'...")
dados_excel <- read_excel(caminho_excel, sheet = "Quadro", range = "B4:Z5")

# Verificar a estrutura dos dados lidos
print("Estrutura dos dados lidos:")
print(str(dados_excel))

# Transformar os dados para um formato longo
print("Transformando os dados para formato longo...")
dados_longos <- dados_excel %>%
  pivot_longer(cols = everything(), names_to = "Trimestre", values_to = "Taxa") %>%
  mutate(
    Ano = as.numeric(sub(".*de (\\d{4}).*", "\\1", Trimestre)),
    Trimestre_Num = as.numeric(sub(".*(\\d).º Trimestre.*", "\\1", Trimestre)),
    Trimestre_Label = paste0(Trimestre_Num, "T ", Ano),
    Taxa = as.numeric(gsub(",", ".", Taxa))  # Converte a coluna Taxa para numérico
  ) %>%
  arrange(Ano, Trimestre_Num) %>%
  filter(!is.na(Taxa))  # Remove linhas com Taxa NA

# Garantir que os trimestres estejam em ordem correta no eixo x
dados_longos$Trimestre_Label <- factor(dados_longos$Trimestre_Label, levels = unique(dados_longos$Trimestre_Label))

# Adicionar uma coluna de Taxa formatada para exibição no gráfico
dados_longos <- dados_longos %>%
  mutate(Taxa_Percentual = paste0(format(Taxa * 100, nsmall = 2), "%"))

print("Estrutura dos dados transformados:")
print(str(dados_longos))

print("Primeiras linhas dos dados transformados:")
print(head(dados_longos))

# Salvar os dados processados como CSV
write.csv(dados_longos, file.path(excel_dir, "resultados_taxa_desemprego.csv"), row.names = FALSE)

# Salvar como objeto RDS
saveRDS(dados_longos, file.path(excel_dir, "resultados_taxa_desemprego.rds"))

# Exibir os resultados completos
print("Resultados completos:")
print(dados_longos)

# Criar o gráfico de evolução
print("Criando gráfico de evolução...")
# Cria o gráfico de evolução com posicionamento e formato dos rótulos aprimorados
ggplot(dados_longos, aes(x = Trimestre_Label, y = Taxa, group = 1)) +
  geom_line(color = "darkred") +
  geom_point(color = "blue") +
  geom_text(aes(label = ifelse(Taxa < 0.71, Taxa_Percentual, paste0(format(Taxa * 100, nsmall = 2), "%")),
                vjust = ifelse(Taxa >= 0.77, -1.5, 1.5)),
            color = "blue", size = 3) +
  labs(title = "Evolução da Taxa de Desemprego Trimestral",
       x = "Ano e Trimestre",
       y = "Taxa de Desemprego (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Salvar o gráfico em um arquivo PNG
ggsave(file.path(excel_dir, "evolucao_taxa_desemprego.png"))

print("Gráfico salvo com sucesso!")
