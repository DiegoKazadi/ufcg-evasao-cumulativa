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
# =====================================================
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


# =====================================================
# 10. Análise por Faixa Etária
# =====================================================

# Criar faixas etárias
alunos_final <- alunos_final %>%
  mutate(
    faixa_etaria = case_when(
      `idade_aproximada_no_ingresso` < 20 ~ "< 20",
      `idade_aproximada_no_ingresso` >= 20 & `idade_aproximada_no_ingresso` <= 24 ~ "20-24",
      `idade_aproximada_no_ingresso` >= 25 & `idade_aproximada_no_ingresso` <= 29 ~ "25-29",
      `idade_aproximada_no_ingresso` >= 30 & `idade_aproximada_no_ingresso` <= 34 ~ "30-34",
      `idade_aproximada_no_ingresso` >= 35 ~ "35+",
      TRUE ~ NA_character_
    )
  )

# Filtrar apenas períodos válidos (como antes)
dados_validos <- alunos_final %>%
  filter(
    (curriculo == 1999 & periodo_de_ingresso >= 2011.1 & periodo_de_ingresso <= 2017.2) |
      (curriculo == 2017 & periodo_de_ingresso >= 2018.1 & periodo_de_ingresso <= 2022.2)
  )

# Contar ingressantes por faixa etária e período
totais_idade <- dados_validos %>%
  group_by(curriculo, periodo_label, faixa_etaria) %>%
  summarise(total_ingressantes = n(), .groups = "drop")

# Contar evadidos (exclui graduados)
evasoes_idade <- dados_validos %>%
  filter(status == "INATIVO", tolower(tipo_de_evasao) != "graduado") %>%
  group_by(curriculo, periodo_label, faixa_etaria) %>%
  summarise(evadidos = n(), .groups = "drop")

# Juntar e calcular taxas
taxas_idade <- totais_idade %>%
  left_join(evasoes_idade, by = c("curriculo", "periodo_label", "faixa_etaria")) %>%
  mutate(
    evadidos = ifelse(is.na(evadidos), 0, evadidos),
    taxa_evasao_acumulada = round((evadidos / total_ingressantes) * 100, 2)
  ) %>%
  arrange(curriculo, periodo_label, faixa_etaria)

# Visualizar tabela consolidada
cat("\n📊 Tabela de Evasão Cumulativa por Faixa Etária:\n")
print(taxas_idade, n = 40)

# =====================================================
# 11. Gráfico – Evasão por Faixa Etária
# =====================================================

library(ggplot2)

ggplot(taxas_idade, aes(
  x = faixa_etaria,
  y = taxa_evasao_acumulada,
  fill = faixa_etaria
)) +
  geom_bar(stat = "summary", fun = mean, position = "dodge") +
  facet_wrap(~ curriculo, ncol = 1) +
  labs(
    title = "Taxa Média de Evasão Cumulativa por Faixa Etária",
    subtitle = "Comparativo entre Currículos 1999 e 2017",
    x = "Faixa Etária no Ingresso",
    y = "Taxa de Evasão Média (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "none"
  )


# =====================================================
# 10. Análise por Faixa Etária (Melhorada)
# =====================================================

# Criar faixas etárias
alunos_final <- alunos_final %>%
  mutate(
    faixa_etaria = case_when(
      `idade_aproximada_no_ingresso` < 20 ~ "< 20",
      `idade_aproximada_no_ingresso` >= 20 & `idade_aproximada_no_ingresso` <= 24 ~ "20-24",
      `idade_aproximada_no_ingresso` >= 25 & `idade_aproximada_no_ingresso` <= 29 ~ "25-29",
      `idade_aproximada_no_ingresso` >= 30 & `idade_aproximada_no_ingresso` <= 34 ~ "30-34",
      `idade_aproximada_no_ingresso` >= 35 ~ "35+",
      TRUE ~ NA_character_
    )
  )

# Filtrar apenas períodos válidos
dados_validos <- alunos_final %>%
  filter(
    (curriculo == 1999 & periodo_de_ingresso >= 2011.1 & periodo_de_ingresso <= 2017.2) |
      (curriculo == 2017 & periodo_de_ingresso >= 2018.1 & periodo_de_ingresso <= 2022.2)
  )

# Calcular taxa de evasão cumulativa por faixa etária e período
taxas_idade <- dados_validos %>%
  group_by(curriculo, periodo_label, faixa_etaria) %>%
  summarise(
    total_ingressantes = n(),
    evadidos = sum(status == "INATIVO" & tolower(tipo_de_evasao) != "graduado"),
    .groups = "drop"
  ) %>%
  mutate(
    taxa_evasao_acumulada = round((evadidos / total_ingressantes) * 100, 2)
  ) %>%
  arrange(curriculo, periodo_label, faixa_etaria)

# Visualizar tabela consolidada (agrupada por faixa e currículo)
cat("\n📊 Tabela de Evasão Cumulativa por Faixa Etária e Currículo:\n")
print(taxas_idade, n = 60)

# =====================================================
# 11. Gráfico Comparativo – Curvas de Evasão por Currículo
# =====================================================

library(ggplot2)
library(scales)

ggplot(taxas_idade, aes(
  x = periodo_label,
  y = taxa_evasao_acumulada,
  group = factor(curriculo),
  color = factor(curriculo),
  shape = factor(curriculo)
)) +
  geom_line(size = 1.3) +
  geom_point(size = 3.5) +
  geom_text(
    aes(label = sprintf("%.1f", taxa_evasao_acumulada)),
    color = "black",
    vjust = -1.2,  # acima do ponto
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("1999" = "#003366", "2017" = "#FF8C00"),
    labels = c("1999", "2017")
  ) +
  scale_shape_manual(
    values = c("1999" = 15, "2017" = 17),  # quadrado e triângulo
    labels = c("1999", "2017")
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Evolução da Taxa Cumulativa de Evasão por Faixa Etária",
    subtitle = "Comparativo entre Currículos 1999 e 2017",
    x = "Período de Ingresso",
    y = "Taxa Cumulativa (%)",
    color = "Currículo",
    shape = "Currículo"
  ) +
  facet_wrap(~ faixa_etaria, ncol = 2) +  # agrupar por faixa etária
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    legend.box.spacing = unit(0.5, "lines")
  )

