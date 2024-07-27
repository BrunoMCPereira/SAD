# Carregar pacotes necessários
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(gtable)
library(purrr)
library(car)
library(stargazer)

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

# Criar modelos de regressão linear múltipla com combinações de variáveis
modelo_populacao_desemprego <- lm(PSSA ~ Populacao + Desemprego, data = dados_consolidados)
modelo_populacao_renda <- lm(PSSA ~ Populacao + Renda_m2, data = dados_consolidados)
modelo_populacao_venda <- lm(PSSA ~ Populacao + Venda_m2, data = dados_consolidados)
modelo_desemprego_renda <- lm(PSSA ~ Desemprego + Renda_m2, data = dados_consolidados)
modelo_desemprego_venda <- lm(PSSA ~ Desemprego + Venda_m2, data = dados_consolidados)
modelo_renda_venda <- lm(PSSA ~ Renda_m2 + Venda_m2, data = dados_consolidados)

modelo_populacao_desemprego_renda <- lm(PSSA ~ Populacao + Desemprego + Renda_m2, data = dados_consolidados)
modelo_populacao_desemprego_venda <- lm(PSSA ~ Populacao + Desemprego + Venda_m2, data = dados_consolidados)
modelo_populacao_renda_venda <- lm(PSSA ~ Populacao + Renda_m2 + Venda_m2, data = dados_consolidados)
modelo_desemprego_renda_venda <- lm(PSSA ~ Desemprego + Renda_m2 + Venda_m2, data = dados_consolidados)

modelo_quatro_variaveis <- lm(PSSA ~ Populacao + Desemprego + Renda_m2 + Venda_m2, data = dados_consolidados)

# Preparar dados para previsão
previsoes <- data.frame(
  Ano = c(2024, 2025),
  Populacao = c(4850000, 4900000),
  Desemprego = c(0.07, 0.065),
  Renda_m2 = c(10, 10.5),
  Venda_m2 = c(2400, 2450)
)

# Prever PSSA para 2024 e 2025 usando cada modelo
previsoes$PSSA_populacao_desemprego <- round(predict(modelo_populacao_desemprego, newdata = previsoes))
previsoes$PSSA_populacao_renda <- round(predict(modelo_populacao_renda, newdata = previsoes))
previsoes$PSSA_populacao_venda <- round(predict(modelo_populacao_venda, newdata = previsoes))
previsoes$PSSA_desemprego_renda <- round(predict(modelo_desemprego_renda, newdata = previsoes))
previsoes$PSSA_desemprego_venda <- round(predict(modelo_desemprego_venda, newdata = previsoes))
previsoes$PSSA_renda_venda <- round(predict(modelo_renda_venda, newdata = previsoes))

previsoes$PSSA_populacao_desemprego_renda <- round(predict(modelo_populacao_desemprego_renda, newdata = previsoes))
previsoes$PSSA_populacao_desemprego_venda <- round(predict(modelo_populacao_desemprego_venda, newdata = previsoes))
previsoes$PSSA_populacao_renda_venda <- round(predict(modelo_populacao_renda_venda, newdata = previsoes))
previsoes$PSSA_desemprego_renda_venda <- round(predict(modelo_desemprego_renda_venda, newdata = previsoes))

previsoes$PSSA_quatro_variaveis <- round(predict(modelo_quatro_variaveis, newdata = previsoes))

# Imprimir as previsões
print("Previsões para 2024 e 2025 com combinações de variáveis:")
print(previsoes)

# Salvar previsões em arquivos CSV
write_csv(previsoes, file.path(dados_dir, "previsoes_PSSA_combinacoes_2024_2025.csv"))

# Função para salvar tabelas como imagens
salvar_tabela_imagem <- function(tabela, titulo, caminho, largura = 10, altura = 6) {
  tabela_grob <- tableGrob(tabela)
  titulo_grob <- textGrob(titulo, gp = gpar(fontsize = 20, fontface = "bold"))

  # Adicionar título à tabela
  tabela_grob <- gtable_add_rows(tabela_grob, heights = grobHeight(titulo_grob) + unit(5, "mm"), pos = 0)
  tabela_grob <- gtable_add_grob(tabela_grob, titulo_grob, 1, 1, 1, ncol(tabela_grob))

  # Centralizar o texto nas células
  tabela_grob$grobs <- lapply(tabela_grob$grobs, function(g) {
    if (inherits(g, "text")) {
      g$just <- "center"
    }
    return(g)
  })

  # Adicionar bordas às células
  for (j in seq_len(ncol(tabela_grob))) {
    for (i in seq_len(nrow(tabela_grob))) {
      tabela_grob <- gtable_add_grob(
        tabela_grob,
        grob = segmentsGrob(x0 = 0, x1 = 1, y0 = 0, y1 = 0, gp = gpar(lwd = 1)),
        t = i, l = j, b = i, r = j
      )
      tabela_grob <- gtable_add_grob(
        tabela_grob,
        grob = segmentsGrob(x0 = 0, x1 = 0, y0 = 0, y1 = 1, gp = gpar(lwd = 1)),
        t = i, l = j, b = i, r = j
      )
    }
  }

  # Salvar a tabela como imagem
  ggsave(caminho, plot = grid.draw(tabela_grob), width = largura, height = altura)
}

# Salvar tabelas de previsões como imagens com título, ajuste de células e bordas
salvar_tabela_imagem(previsoes[, c("Ano", "PSSA_populacao_desemprego", "PSSA_populacao_renda", "PSSA_populacao_venda", "PSSA_desemprego_renda", "PSSA_desemprego_venda", "PSSA_renda_venda")],
                     "Previsões PSSA com 2 Variáveis (2024-2025)", file.path(dados_dir, "previsoes_PSSA_combinacoes_2_variaveis.png"))
salvar_tabela_imagem(previsoes[, c("Ano", "PSSA_populacao_desemprego_renda", "PSSA_populacao_desemprego_venda", "PSSA_populacao_renda_venda", "PSSA_desemprego_renda_venda")],
                     "Previsões PSSA com 3 Variáveis (2024-2025)", file.path(dados_dir, "previsoes_PSSA_combinacoes_3_variaveis.png"))
salvar_tabela_imagem(previsoes[, c("Ano", "PSSA_quatro_variaveis")],
                     "Previsões PSSA com 4 Variáveis (2024-2025)", file.path(dados_dir, "previsoes_PSSA_combinacoes_4_variaveis.png"))

# Exibir mensagens de conclusão
print("Previsões para 2024 e 2025 salvas com sucesso.")
