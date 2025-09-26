# ============================
# 1. Carregamento de Pacotes
# ============================
# Instale os pacotes se ainda não tiver:
# install.packages("tidyverse")
# install.packages("janitor")

library(tidyverse)
library(tidyr)
library(janitor)

# ============================
# 2. Carregamento e Preparação dos Dados
# ============================
caminho_base <- "/home/diego/Documentos/Semestre 2024.2/Nova_Analise/tabelas"
arquivo_alunos <- file.path(caminho_base, "alunos-final.csv")

dados <- read_delim(arquivo_alunos, delim = ",", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# Limpeza e padronização dos nomes das colunas
dados_clean <- dados %>%
  clean_names()

# Assegurar que 'periodo_de_ingresso' é numérico
dados_clean$periodo_de_ingresso <- as.numeric(dados_clean$periodo_de_ingresso)

# Filtrar estudantes ingressantes entre 2011 e 2023
dados_filtrados <- dados_clean %>%
  filter(periodo_de_ingresso >= 2011 & periodo_de_ingresso <= 2023)

# ============================
# 3. Cálculo das Taxas de Evasão Cumulativa
# ============================

# Definir os grupos de currículos com base nos períodos de ingresso
dados_curriculo_1999 <- dados_filtrados %>%
  filter(curriculo == "Currículo 1999") %>%
  filter(periodo_de_ingresso >= 2011.1 & periodo_de_ingresso <= 2017.2)

dados_curriculo_2017 <- dados_filtrados %>%
  filter(curriculo == "Currículo 2017") %>%
  filter(periodo_de_ingresso >= 2018.1 & periodo_de_ingresso <= 2022.2)

# Calcular N0 (Total de Ingressantes) para cada currículo
n0_1999 <- dados_curriculo_1999 %>%
  distinct(matricula) %>%
  nrow()

n0_2017 <- dados_curriculo_2017 %>%
  distinct(matricula) %>%
  nrow()

# Função revisada para calcular a evasão com a nova lógica (grafia corrigida)
calcular_evasao_cumulativa <- function(dados, n0, nome_curriculo) {
  dados %>%
    # Filtro corrigido para 'GRADUADO' em maiúsculas
    filter(status == "INATIVO" & tipo_de_evasao != "GRADUADO") %>%
    mutate(periodo_evasao_num = as.numeric(str_extract(periodo_de_evasao, "\\d+"))) %>%
    group_by(periodo_evasao_num) %>%
    summarise(evadidos = n(), .groups = 'drop') %>%
    arrange(periodo_evasao_num) %>%
    mutate(evadidos_acumulados = cumsum(evadidos)) %>%
    mutate(taxa_cumulativa = (evadidos_acumulados / n0) * 100) %>%
    select(periodo = periodo_evasao_num, taxa_cumulativa) %>%
    mutate(curriculo = nome_curriculo)
}

evasao_1999 <- calcular_evasao_cumulativa(dados_curriculo_1999, n0_1999, "Currículo 1999")
evasao_2017 <- calcular_evasao_cumulativa(dados_curriculo_2017, n0_2017, "Currículo 2017")

# Unir os resultados para comparação
dados_comparacao <- bind_rows(evasao_1999, evasao_2017)

# ============================
# 4. Geração da Tabela de Comparação
# ============================
cat("Tabela de Evasão Cumulativa por Período e Currículo:\n")
tabela_evasao <- dados_comparacao %>%
  pivot_wider(names_from = curriculo, values_from = taxa_cumulativa) %>%
  rename(Periodo = periodo) %>%
  knitr::kable(digits = 2, caption = "Taxas de Evasão Cumulativa (em %)", format = "simple")

print(tabela_evasao)

# ============================
# 5. Geração do Gráfico de Comparação
# ============================
cat("\nGráfico de Linhas da Evasão Cumulativa:\n")
grafico_evasao_cumulativa <- ggplot(dados_comparacao, aes(x = periodo, y = taxa_cumulativa, color = curriculo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Comparativo da Evasão Cumulativa (Currículo 1999 vs. 2017)",
    subtitle = "Taxa acumulada de evasão ao longo dos 4 primeiros períodos",
    x = "Período Letivo",
    y = "Taxa de Evasão Cumulativa (%)",
    color = "Currículo"
  ) +
  scale_x_continuous(breaks = 1:4) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(grafico_evasao_cumulativa)
