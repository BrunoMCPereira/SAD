library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Obter o diretório do script atual
script_dir <- dirname(normalizePath(sys.frame(1)$ofile))

# Caminho relativo ao diretório do script
caminho_excel <- file.path(script_dir, "Valor Renda m2 Habitação", "Valor Renda m2 Habitação - Anual - 2020 a 2024 - Editado.xls")

# Obter o diretório do arquivo Excel
excel_dir <- dirname(caminho_excel)

# Ler os títulos dos trimestres da linha 12
print("Lendo os títulos dos trimestres da linha 12...")
titulos <- read_excel(caminho_excel, sheet = "Quadro", range = "C12:R12", col_names = FALSE)

# Ler os valores dos trimestres da linha 13
print("Lendo os valores dos trimestres da linha 13...")
valores <- read_excel(caminho_excel, sheet = "Quadro", range = "C13:R13", col_names = FALSE)

# Verificar a estrutura dos títulos lidos
print("Estrutura dos títulos lidos:")
print(titulos)

# Verificar a estrutura dos valores lidos
print("Estrutura dos valores lidos:")
print(valores)

# Renomear as colunas dos valores com os títulos lidos
colnames(valores) <- as.character(titulos[1, ])

# Transformar os dados para um formato longo
print("Transformando os dados para formato longo...")
dados_longos <- valores %>%
  pivot_longer(cols = everything(), names_to = "Trimestre", values_to = "Valor") %>%
  mutate(
    Valor = as.numeric(Valor),  # Converter para numérico
    Ano = as.numeric(sub(".*de (\\d{4}).*", "\\1", Trimestre)),
    Trimestre_Num = as.numeric(sub(".*(\\d).º Trimestre.*", "\\1", Trimestre)),
    Trimestre_Label = paste0(Trimestre_Num, "T ", Ano)
  ) %>%
  arrange(Ano, Trimestre_Num)

# Remover linhas onde a conversão para numérico falhou
dados_longos <- dados_longos %>% filter(!is.na(Valor))

# Garantir que os trimestres estejam em ordem correta no eixo x
dados_longos$Trimestre_Label <- factor(dados_longos$Trimestre_Label, levels = unique(dados_longos$Trimestre_Label))

print("Trimestres e seus valores:")
print(dados_longos %>% select(Trimestre_Label, Valor))

# Salvar os dados processados como CSV
write.csv(dados_longos, file.path(excel_dir, "resultados_valor_renda.csv"), row.names = FALSE)

# Salvar como objeto RDS
saveRDS(dados_longos, file.path(excel_dir, "resultados_valor_renda.rds"))

# Exibir os resultados completos
print("Resultados completos:")
print(dados_longos)

# Criar o gráfico de evolução
print("Criando gráfico de evolução...")
ggplot(data = dados_longos, aes(x = Trimestre_Label, y = Valor, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  geom_text(aes(label = scales::number(Valor, accuracy = 0.01)), vjust = ifelse(dados_longos$Valor >= 0.077, -0.5, 1.5), color = "blue") +
  labs(title = "Evolução do Valor da Renda por m2 de Habitação",
       x = "Ano e Trimestre",
       y = "Valor da Renda (€)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Salvar o gráfico em um arquivo PNG
ggsave(file.path(excel_dir, "evolucao_valor_renda.png"))
