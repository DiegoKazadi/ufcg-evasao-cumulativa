# =====================================================
# Pacotes
# =====================================================
library(readr)   # leitura de CSV
library(dplyr)   # manipulação
library(tibble)  # melhor visualização
library(janitor) # limpeza de nomes

# =====================================================
# 1. Função para carregar e visualizar tabelas
# =====================================================
carregar_tabelas <- function(pasta) {
  arquivos <- list.files(path = pasta, pattern = "\\.csv$", full.names = TRUE)
  if (length(arquivos) == 0) stop("Nenhum arquivo CSV encontrado na pasta.")
  
  cat("Arquivos encontrados:\n")
  print(basename(arquivos))
  
  tabelas <- list()
  
  for (arq in arquivos) {
    nome <- tools::file_path_sans_ext(basename(arq))
    df <- read_delim(
      arq, delim = ";", show_col_types = FALSE,
      locale = locale(decimal_mark = ".", grouping_mark = ",")
    )
    
    tabelas[[nome]] <- df
    
    cat("\n=============================\n")
    cat("Tabela:", nome, "\n")
    cat("Dimensões:", nrow(df), "linhas x", ncol(df), "colunas\n")
    cat("Colunas:\n")
    print(colnames(df))
    cat("Visualização inicial:\n")
    print(head(df, 5))
    
    problemas <- problems(df)
    if (nrow(problemas) > 0) {
      cat("\n⚠️ Problemas encontrados (mostrando até 5):\n")
      print(head(problemas, 5))
    }
    cat("=============================\n")
  }
  
  return(tabelas)
}

# =====================================================
# Caminhos possíveis
# =====================================================
# Windows
pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas"

# Linux
pasta_dados <- "/home/diego/Documentos/Tabelas"

tabelas <- carregar_tabelas(pasta_dados)

# =====================================================
# 2. Pré-processamento coletivo
# =====================================================
preprocess_coletivo <- function(df) {
  df %>%
    clean_names() %>%
    mutate(across(
      where(is.character),
      ~ iconv(.x, from = "", to = "UTF-8", sub = "byte") %>% trimws()
    ))
}

tabelas <- lapply(tabelas, preprocess_coletivo)

print(names(tabelas))

# =====================================================
# 3. Selecionar tabela principal
# =====================================================
alunos_final <- tabelas[["alunos-final"]]

# =====================================================
# 4. Enriquecimento dos dados
# =====================================================
alunos_final <- alunos_final %>%
  mutate(
    ano_ingresso = floor(periodo_de_ingresso),
    semestre_ingresso = ifelse(
      (periodo_de_ingresso - ano_ingresso) < 0.2, 1, 2
    ),
    periodo_label = paste0(ano_ingresso, ".", semestre_ingresso)
  )

# =====================================================
# 5. Verificação de integridade
# =====================================================
check_inconsistencias <- alunos_final %>%
  filter((ano_ingresso < 2018 & curriculo == 2017) |
           (ano_ingresso >= 2018 & curriculo == 1999)) %>%
  distinct(matricula, ano_ingresso, curriculo)

cat("\n==============================\n")
if (nrow(check_inconsistencias) > 0) {
  cat("⚠️ Inconsistências encontradas:\n")
  print(check_inconsistencias)
} else {
  cat("✅ Nenhuma inconsistência encontrada.\n")
}
cat("==============================\n")

# =====================================================
# 6. Funções auxiliares
# =====================================================# ============================
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

filtrar_evasao <- function(df) {
  df %>%
    filter(
      status == "INATIVO",
      tolower(tipo_de_evasao) != "graduado"
    )
}

calcular_taxas_cumulativas <- function(df) {
  
  # Filtrar apenas períodos válidos
  df <- df %>%
    filter(
      (curriculo == 1999 & periodo_de_ingresso >= 2011.1 & periodo_de_ingresso <= 2017.2) |
        (curriculo == 2017 & periodo_de_ingresso >= 2018.1 & periodo_de_ingresso <= 2022.2)
    )
  
  totais <- df %>%
    group_by(curriculo, periodo_label) %>%
    summarise(total_ingressantes = n(), .groups = "drop")
  
  evasoes <- df %>%
    filtrar_evasao() %>%
    group_by(curriculo, periodo_label) %>%
    summarise(total_evasoes = n(), .groups = "drop")
  
  dados <- totais %>%
    left_join(evasoes, by = c("curriculo", "periodo_label")) %>%
    mutate(total_evasoes = ifelse(is.na(total_evasoes), 0, total_evasoes)) %>%
    arrange(curriculo, periodo_label) %>%
    group_by(curriculo) %>%
    mutate(
      evasoes_acumuladas = cumsum(total_evasoes),
      taxa_cumulativa = round((evasoes_acumuladas / sum(total_ingressantes)) * 100, 2)
    ) %>%
    ungroup()
  
  return(dados)
}

# =====================================================
# 7. Execução principal
# =====================================================
taxas_evasao <- calcular_taxas_cumulativas(alunos_final)

cat("\n📊 Taxas Cumulativas de Evasão por Currículo e Período:\n")
print(taxas_evasao, n = nrow(taxas_evasao))

# =====================================================
# 8. Diagnóstico dos períodos de ingresso
# =====================================================
cat("\n📅 Diagnóstico dos períodos distintos:\n")
print(sort(unique(alunos_final$periodo_label)))

cat("\n📈 Frequência de ingressantes por período:\n")
print(table(alunos_final$periodo_label))

# ===================================================
# 9. Grafico
# ===================================================

library(ggplot2)

ggplot(taxas_evasao, aes(x = periodo_label, y = taxa_cumulativa, group = curriculo, color = factor(curriculo))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("1999" = "#1f77b4", "2017" = "#ff7f0e")) +
  labs(
    title = "Evolução da Taxa Cumulativa de Evasão por Currículo",
    x = "Período de Ingresso",
    y = "Taxa Cumulativa (%)",
    color = "Currículo"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =========================================================


library(ggplot2)
library(scales)

ggplot(taxas_evasao, aes(
  x = periodo_label,
  y = taxa_cumulativa,
  group = factor(curriculo),
  color = factor(curriculo),
  shape = factor(curriculo)
)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(size = 3.5) +
  geom_text(
    aes(label = sprintf("%.1f", taxa_cumulativa)),
    color = "black",
    vjust = -1.2,   # acima do ponto
    size = 3.8,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("1999" = "#003366", "2017" = "#FF8C00"),
    labels = c("1999", "2017")
  ) +
  scale_shape_manual(
    values = c("1999" = 15, "2017" = 17),
    labels = c("1999", "2017")
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Evolução da Taxa Cumulativa de Evasão",
    subtitle = "Comparativo entre Currículos 1999 e 2017",
    x = "Período de Ingresso",
    y = "Taxa Cumulativa (%)",
    color = "Currículo",
    shape = "Currículo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",        # legenda abaixo do eixo X
    legend.direction = "horizontal",   # horizontal
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    legend.box.spacing = unit(0.5, "lines")
  )

