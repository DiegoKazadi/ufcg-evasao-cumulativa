# Pacotes
library(readr)   # para ler CSV
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
    
    # Ler CSV
    df <- read_csv(arq, show_col_types = FALSE)
    
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
    cat("=============================\n")
  }
  
  return(tabelas)
}

# Usando a função no seu caminho (Windows)
pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas"

tabelas <- carregar_tabelas(pasta_dados)
