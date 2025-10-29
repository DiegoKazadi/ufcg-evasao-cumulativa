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
# 10b. Tratamento robusto da variável idade e análise
# =====================================================

library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

# --- 1) localizar a coluna de idade automaticamente ---
idade_col_candidates <- names(alunos_final)[grepl("idade", names(alunos_final), ignore.case = TRUE)]
if(length(idade_col_candidates) == 0) {
  cat("⚠️ Não encontrei nenhuma coluna com 'idade'. Colunas disponíveis:\n")
  print(names(alunos_final))
  stop("Por favor, informe qual coluna representa a idade ou renomeie-a para algo contendo 'idade'.")
}
# usar a primeira candidata encontrada (ajuste se quiser outra)
idade_col <- idade_col_candidates[1]
cat("➡️ Usando a coluna de idade:", idade_col, "\n")

# --- 2) criar coluna numérica 'idade' -- limpa textos como '25 anos', 'aprox 24' ---
alunos_final <- alunos_final %>%
  mutate(
    idade_raw = .data[[idade_col]],
    # transformar em character antes de extrair dígitos
    idade_raw = as.character(idade_raw),
    idade = str_extract(idade_raw, "\\d{1,3}"),    # pega os dígitos
    idade = as.numeric(idade)
  )

# verificar quantos NA depois da conversão:
na_idade <- sum(is.na(alunos_final$idade))
cat("Número de registros sem idade numérica:", na_idade, "\n")

# --- 3) criar faixa etária (ajuste os cortes se quiser) ---
alunos_final <- alunos_final %>%
  mutate(faixa_etaria = case_when(
    !is.na(idade) & idade < 20 ~ "<20",
    !is.na(idade) & idade >= 20 & idade < 25 ~ "20-24",
    !is.na(idade) & idade >= 25 & idade < 30 ~ "25-29",
    !is.na(idade) & idade >= 30 & idade < 35 ~ "30-34",
    !is.na(idade) & idade >= 35 ~ ">=35",
    TRUE ~ "Sem dado"
  ))

# --- 4) preparar coluna numérica para ordenar períodos (assumindo periodo_label como 'YYYY.S') ---
# Se ainda não existir periodo_label, tenta reconstruir a partir do campo original
if(!"periodo_label" %in% names(alunos_final)) {
  # tenta usar 'periodo_de_ingresso' se existir
  if("periodo_de_ingresso" %in% names(alunos_final)) {
    alunos_final <- alunos_final %>%
      mutate(
        ano_ing = floor(periodo_de_ingresso),
        semestre_ing = ifelse((periodo_de_ingresso - ano_ing) < 0.2, 1, 2),
        periodo_label = paste0(ano_ing, ".", semestre_ing)
      )
  } else {
    stop("Não encontrei 'periodo_label' nem 'periodo_de_ingresso'.")
  }
}

# criar periodo_numeric para ordenar (ex: "2011.1" -> 2011.1 numeric)
alunos_final <- alunos_final %>%
  mutate(periodo_numeric = as.numeric(periodo_label))

# --- 5) marcar evadido (1/0) ---
alunos_final <- alunos_final %>%
  mutate(
    evadido = ifelse(
      toupper(trimws(status)) == "INATIVO" & tolower(tipo_de_evasao) != "graduado",
      1, 0
    )
  )

# --- 6) calcular totais por periodo x faixa etaria ---
tabela_base <- alunos_final %>%
  filter(!is.na(faixa_etaria)) %>%   # opcional: remover "Sem dado" se preferir
  group_by(periodo_label, periodo_numeric, faixa_etaria) %>%
  summarise(
    total_ingressantes = n(),
    total_evadidos = sum(evadido, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(faixa_etaria, periodo_numeric)

# --- 7) calcular evasões acumuladas por faixa (ordenadas por periodo_numeric) e taxa relativa ao total de ingressantes daquela faixa ---
tabela_idade <- tabela_base %>%
  group_by(faixa_etaria) %>%
  arrange(periodo_numeric, .by_group = TRUE) %>%
  mutate(
    evasoes_acumuladas = cumsum(total_evadidos),
    total_ingressantes_faixa = sum(total_ingressantes, na.rm = TRUE),
    taxa_evasao_acumulada = round((evasoes_acumuladas / total_ingressantes_faixa) * 100, 2)
  ) %>%
  ungroup() %>%
  # selecionar colunas pedidas e renomear para português limpo
  select(
    Periodo = periodo_label,
    Faixa_Etaria = faixa_etaria,
    Total_Ingressantes = total_ingressantes,
    Evadidos = total_evadidos,
    Evasoes_Acumuladas = evasoes_acumuladas,
    Taxa_Evasao_Acumulada = taxa_evasao_acumulada
  )

# --- 8) mostrar a tabela final ---
cat("\n📊 Tabela: Evasão Cumulativa por Faixa Etária e Período\n")
print(tabela_idade, n = nrow(tabela_idade))

# --- 9) Gráfico para visualização ---
ggplot(tabela_idade, aes(x = Periodo, y = Taxa_Evasao_Acumulada, group = Faixa_Etaria, color = Faixa_Etaria)) +
  geom_line(size = 1.1) +
  geom_point(size = 2.5) +
  geom_text(aes(label = sprintf("%.1f", Taxa_Evasao_Acumulada)), vjust = -0.8, size = 3, show.legend = FALSE) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Evasão Cumulativa por Faixa Etária",
    x = "Período de Ingresso",
    y = "Taxa Cumulativa (%)",
    color = "Faixa Etária"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
