# Pacotes
library(readr)   # leitura de CSV
library(dplyr)   # manipulação
library(tibble)  # melhor visualização

# Função para carregar e visualizar tabelas
carregar_tabelas <- function(pasta) {
  
  # Lista todos os arquivos CSV na pasta
  arquivos <- list.files(path = pasta, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(arquivos) == 0) {
    stop("Nenhum arquivo CSV encontrado na pasta.")
  }
  
  cat("Arquivos encontrados:\n")
  print(basename(arquivos))
  
  # Cria lista vazia para armazenar as tabelas
  tabelas <- list()
  
  # Carrega cada arquivo
  for (arq in arquivos) {
    nome <- tools::file_path_sans_ext(basename(arq)) # nome do arquivo sem extensão
    
    # Ler CSV com separador ";"
    df <- read_delim(arq, delim = ";", show_col_types = FALSE,
                     locale = locale(decimal_mark = ".", grouping_mark = ",")) 
    
    # Guardar na lista
    tabelas[[nome]] <- df
    
    # Exibir resumo no console
    cat("\n=============================\n")
    cat("Tabela:", nome, "\n")
    cat("Dimensões:", dim(df)[1], "linhas x", dim(df)[2], "colunas\n")
    cat("Colunas:\n")
    print(colnames(df))
    cat("Visualização inicial:\n")
    print(head(df, 5))  # mostra as 5 primeiras linhas
    
    # Mostrar possíveis problemas de parsing
    problemas <- problems(df)
    if (nrow(problemas) > 0) {
      cat("\n⚠️ Problemas encontrados (mostrando até 5):\n")
      print(head(problemas, 5))
    }
    
    cat("=============================\n")
  }
  
  return(tabelas)
}

# Usando a função no seu caminho (Windows)
pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025/Tabelas"

# Usando a função no seu caminho (linux)
pasta_dados <- "/home/diego/Documentos/Tabelas"

tabelas <- carregar_tabelas(pasta_dados)

# Função de pré-processamento coletivo
preprocess_coletivo <- function(df) {
  df %>%
    janitor::clean_names() %>%
    mutate(across(
      where(is.character),
      ~ iconv(.x, from = "", to = "UTF-8", sub = "byte") %>% trimws()
    ))
}

# =====================================================
# 1. Carregamento e pré-processamento das tabelas
# =====================================================

# Pré-processamento coletivo (já implementado)
tabelas <- lapply(tabelas, preprocess_coletivo)

# Mostrar nomes das tabelas carregadas
print(names(tabelas))

# Pré-processamento individual (se necessário em casos específicos)
preprocess_individual <- function(df) {
  df %>%
    janitor::clean_names() %>% 
    mutate(across(
      where(is.character),
      ~ iconv(.x, from = "", to = "UTF-8", sub = "byte") %>% trimws()
    ))
}

# Carregar apenas a tabela de interesse
alunos_final <- tabelas[["alunos-final"]]

# =====================================================
# 2. Enriquecimento dos dados
# =====================================================

# Garantir ano e semestre separados
alunos_final <- alunos_final %>%
  mutate(
    ano_ingresso = floor(periodo_de_ingresso),
    semestre_ingresso = ifelse(periodo_de_ingresso %% 1 == 0.1, 1, 2)
  )

# =====================================================
# 3. Verificação de integridade
# =====================================================

# Verificar se há currículos em anos incompatíveis
check_inconsistencias <- alunos_final %>%
  filter((ano_ingresso < 2018 & curriculo == 2017) |
           (ano_ingresso >= 2018 & curriculo == 1999)) %>%
  distinct(matricula, ano_ingresso, curriculo)

# Mostrar inconsistências encontradas
if (nrow(check_inconsistencias) > 0) {
  cat("️ Inconsistências encontradas:\n")
  print(check_inconsistencias)
} else {
  cat(" Nenhuma inconsistência encontrada.\n")
}

# =====================================================
# 4. Funções auxiliares
# =====================================================

# Função para filtrar evasão real (ignorar graduados)
filtrar_evasao <- function(df) {
  df %>%
    filter(
      status == "INATIVO",
      tolower(tipo_de_evasao) != "graduado"
    )
}

# Função para calcular taxas cumulativas de evasão
calcular_taxas_cumulativas <- function(df) {
  
  # Filtrar períodos válidos por currículo
  df <- df %>%
    filter(
      (curriculo == 1999 & periodo_de_ingresso >= 2011.1 & periodo_de_ingresso <= 2017.2) |
        (curriculo == 2017 & periodo_de_ingresso >= 2018.1 & periodo_de_ingresso <= 2022.2)
    )
  
  # Totais de ingressantes
  totais <- df %>%
    group_by(curriculo, periodo_de_ingresso) %>%
    summarise(total_ingressantes = n(), .groups = "drop")
  
  # Totais de evasões reais
  evasoes <- df %>%
    filtrar_evasao() %>%
    group_by(curriculo, periodo_de_ingresso) %>%
    summarise(total_evasoes = n(), .groups = "drop")
  
  # Juntar bases e calcular acumulado
  dados <- totais %>%
    left_join(evasoes, by = c("curriculo", "periodo_de_ingresso")) %>%
    mutate(total_evasoes = ifelse(is.na(total_evasoes), 0, total_evasoes)) %>%
    arrange(curriculo, periodo_de_ingresso) %>%
    group_by(curriculo) %>%
    mutate(
      evasoes_acumuladas = cumsum(total_evasoes),
      taxa_cumulativa = round((evasoes_acumuladas / total_ingressantes) * 100, 2)
    ) %>%
    ungroup()
  
  return(dados)
}

# =====================================================
# 5. Execução
# =====================================================

# Calcular taxas cumulativas de evasão
taxas_evasao <- calcular_taxas_cumulativas(alunos_final)

# Visualizar no terminal (sem gráficos)
print(head(taxas_evasao, 20))

# =====================================================
# 6. Diagnóstico de período de ingresso
# =====================================================

# Valores únicos de período
print(sort(unique(alunos_final$periodo_de_ingresso)))

# Frequência por período
print(table(alunos_final$periodo_de_ingresso))
