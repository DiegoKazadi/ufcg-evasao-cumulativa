# ============================
# 1. Carregamento de Pacotes
# ============================
library(tidyverse)
library(janitor)

# ============================
# 2. Carregamento e Preparação dos Dados
# ============================
caminho_base <- "/home/diego/Documentos/Semestre 2024.2/Nova_Analise/tabelas"
arquivo_alunos <- file.path(caminho_base, "alunos-final.csv")

dados <- read_delim(arquivo_alunos, delim = ",", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# Limpeza e padronização dos nomes das colunas
dados_clean <- dados %>%
  clean_names() %>%
  # Converter colunas críticas para formato adequado
  mutate(
    periodo_de_ingresso = as.numeric(periodo_de_ingresso),
    status = toupper(status),  # Padronizar para maiúsculas
    tipo_de_evasao = toupper(tipo_de_evasao)  # Padronizar para maiúsculas
  ) %>%
  # Filtrar períodos válidos e remover NA
  filter(
    periodo_de_ingresso >= 2011 & periodo_de_ingresso <= 2023,
    !is.na(periodo_de_ingresso)
  )

# ============================
# 3. Separar Currículos com Filtros Corretos
# ============================
dados_curriculo_1999 <- dados_clean %>%
  filter(
    curriculo == "Currículo 1999",
    periodo_de_ingresso >= 2011,
    periodo_de_ingresso <= 2017.2
  )

dados_curriculo_2017 <- dados_clean %>%
  filter(
    curriculo == "Currículo 2017",
    periodo_de_ingresso >= 2018,
    periodo_de_ingresso <= 2022.2
  )

# ============================
# 4. Cálculo dos Totais de Ingressantes (N0)
# ============================
n0_1999 <- dados_curriculo_1999 %>%
  summarise(n = n_distinct(matricula)) %>%
  pull(n)

n0_2017 <- dados_curriculo_2017 %>%
  summarise(n = n_distinct(matricula)) %>%
  pull(n)

# ============================
# 5. Função de Cálculo de Evasão Corrigida
# ============================
calcular_evasao_cumulativa <- function(dados, n0, nome_curriculo) {
  # Extrair número do período de evasão (assumindo formato ANO.SEMESTRE)
  dados_evasao <- dados %>%
    mutate(
      periodo_evasao_num = as.numeric(str_replace(periodo_de_evasao, "\\.", ""))
    ) %>%
    filter(
      status == "INATIVO",
      tipo_de_evasao != "GRADUADO",
      !is.na(periodo_evasao_num)
    )
  
  if (nrow(dados_evasao) == 0) {
    return(tibble(periodo = numeric(), taxa_cumulativa = numeric(), curriculo = character()))
  }
  
  dados_evasao %>%
    group_by(periodo_evasao_num) %>%
    summarise(evadidos = n(), .groups = 'drop') %>%
    arrange(periodo_evasao_num) %>%
    mutate(evadidos_acumulados = cumsum(evadidos)) %>%
    mutate(taxa_cumulativa = (evadidos_acumulados / n0) * 100) %>%
    select(periodo = periodo_evasao_num, taxa_cumulativa) %>%
    mutate(curriculo = nome_curriculo)
}

# Aplicar função
evasao_1999 <- calcular_evasao_cumulativa(dados_curriculo_1999, n0_1999, "Currículo 1999")
evasao_2017 <- calcular_evasao_cumulativa(dados_curriculo_2017, n0_2017, "Currículo 2017")

# ============================
# 6. Unir Resultados e Gerar Tabela
# ============================
dados_comparacao <- bind_rows(evasao_1999, evasao_2017)

if (nrow(dados_comparacao) > 0) {
  tabela_evasao <- dados_comparacao %>%
    pivot_wider(
      names_from = curriculo,
      values_from = taxa_cumulativa,
      values_fill = 0
    ) %>%
    rename(Periodo = periodo)
  
  print(knitr::kable(tabela_evasao, digits = 2, caption = "Taxas de Evasão Cumulativa (em %)"))
} else {
  message("Nenhum dado de evasão encontrado para os filtros aplicados.")
}

# ============================
# 7. Gráfico com Verificação de Dados
# ============================
if (nrow(dados_comparacao) > 0) {
  grafico <- ggplot(dados_comparacao, aes(x = periodo, y = taxa_cumulativa, color = curriculo)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    labs(
      title = "Comparativo da Evasão Cumulativa",
      x = "Período Letivo",
      y = "Taxa de Evasão Cumulativa (%)",
      color = "Currículo"
    ) +
    theme_minimal()
  
  print(grafico)
} else {
  message("Dados insuficientes para gerar o gráfico.")
}

