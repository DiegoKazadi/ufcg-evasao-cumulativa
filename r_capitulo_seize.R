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

# Usando a função pre-processamento
tabelas <- lapply(tabelas, preprocess_coletivo)

# Mostrar nomes das tabelas carregadas
names(tabelas)

# Olhar algumas colunas depois do pré-processamento
head(tabelas[[1]])
glimpse(tabelas[[1]])

# Carregar apenas a tabela de interesse (alunos-final.csv)
alunos_final <- tabelas[["alunos-final"]]

# Função de pré-processamento específica
preprocess_individual <- function(df) {
  df %>%
    janitor::clean_names() %>% 
    mutate(across(
      where(is.character),
      ~ iconv(.x, from = "", to = "UTF-8", sub = "byte") %>% trimws()
    ))
}

# Função para filtrar evasão real (ignorar graduados)
filtrar_evasao <- function(df) {
  df %>%
    filter(
      status == "INATIVO",
      tolower(tipo_de_evasao) != "graduado"
    )
}

# Calcular taxas cumulativas de evasão com filtro por currículo e período
calcular_taxas_cumulativas <- function(df) {
  
  # Filtrar currículos corretamente
  df <- df %>%
    filter(
      (curriculo == 1999 & periodo_de_ingresso >= 2011 & periodo_de_ingresso <= 2017) |
        (curriculo == 2017 & periodo_de_ingresso >= 2018 & periodo_de_ingresso <= 2022)
    )
  
  # total ingressantes por currículo e período
  totais <- df %>%
    group_by(curriculo, periodo_de_ingresso) %>%
    summarise(total_ingressantes = n(), .groups = "drop")
  
  # evasões reais
  evasoes <- df %>%
    filtrar_evasao() %>%
    group_by(curriculo, periodo_de_ingresso) %>%
    summarise(total_evasoes = n(), .groups = "drop")
  
  # juntar bases
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
