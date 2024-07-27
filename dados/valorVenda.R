library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Obter o diretório do script atual
script_dir <- dirname(normalizePath(sys.frame(1)$ofile))

# Caminho relativo ao diretório do script
caminho_excel <- file.path(script_dir, "Valor Transcções m2 Habitação", "Valor Transacções m2 Habitação - Trimestral - 2020 a 2023 - Editado.xls")

# Obter o diretório do arquivo Excel
excel_dir <- dirname(caminho_excel)

# Ler os dados da folha "Quadro" a partir da linha 9
print("Lendo os dados da folha 'Quadro'...")
dados_excel <- read_excel(caminho_excel, sheet = "Quadro", range = "C9:R10")

# Verificar a estrutura dos dados lidos
print("Estrutura dos dados lidos:")
print(str(dados_excel))

# Renomear as colunas para facilitar a manipulação
colnames(dados_excel) <- c(paste0("Trimestre_", 1:16))

# Transformar os dados para um formato longo
print("Transformando os dados para formato longo...")
dados_longos <- dados_excel %>%
  pivot_longer(cols = everything(), names_to = "Trimestre", values_to = "Valor") %>%
  mutate(
    Valor = as.numeric(gsub(",", ".", Valor)),  # Converter para numérico
    Ano = rep(2020:2023, each = 4, length.out = n()),
    Trimestre_Num = rep(1:4, times = ceiling(n() / 4), length.out = n()),
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
write.csv(dados_longos, file.path(excel_dir, "resultados_valor_venda.csv"), row.names = FALSE)

# Salvar como objeto RDS
saveRDS(dados_longos, file.path(excel_dir, "resultados_valor_venda.rds"))

# Exibir os resultados completos
print("Resultados completos:")
print(dados_longos)

# Criar o gráfico de evolução
print("Criando gráfico de evolução...")
ggplot(dados_longos, aes(x = Trimestre_Label, y = Valor, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Evolução do Valor da Venda por m2 de Habitação",
       x = "Ano e Trimestre",
       y = "Valor da Venda (€)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Salvar o gráfico em um arquivo PNG
ggsave(file.path(excel_dir, "evolucao_valor_venda.png"))
