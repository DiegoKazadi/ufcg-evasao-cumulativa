# Pacotes necessários
library(dplyr)
library(ggplot2)

# --- 0. Preparação: garantir que alunos_final existe e tem periodo_de_ingresso ---
if (!exists("alunos_final")) stop("Objeto 'alunos_final' não encontrado. Carregue e pré-processe antes.")

# Converter periodo_de_ingresso para string e extrair ano/semestre com robustez
df <- alunos_final %>%
  mutate(
    periodo_str = as.character(periodo_de_ingresso),
    ano_ingresso = as.integer(sub("\\..*$", "", periodo_str)),
    semestre_ingresso = as.integer(sub("^.*\\.", "", periodo_str))
  )

# Checagens rápidas
cat("Valores únicos de periodo_de_ingresso:\n")
print(sort(unique(df$periodo_de_ingresso)))

cat("\nNúmero de NAs em periodo_de_ingresso:\n")
print(sum(is.na(df$periodo_de_ingresso)))

# --- 1) Contagem por período (ex.: 2011.1, 2011.2, ...) ---
ingressantes_periodo <- df %>%
  group_by(periodo_de_ingresso) %>%
  summarise(
    n_ingressantes = n(),
    curriculos = paste(sort(unique(curriculo)), collapse = ","),
    .groups = "drop"
  ) %>%
  arrange(periodo_de_ingresso)

cat("\nIngressantes por PERÍODO (todas as linhas):\n")
print(ingressantes_periodo, n = nrow(ingressantes_periodo))

# --- 2) Contagem por semestre (ano + semestre) ---
ingressantes_semestre <- df %>%
  group_by(ano_ingresso, semestre_ingresso) %>%
  summarise(n_ingressantes = n(), .groups = "drop") %>%
  arrange(ano_ingresso, semestre_ingresso)

cat("\nIngressantes por ANO e SEMESTRE (todas as linhas):\n")
print(ingressantes_semestre, n = nrow(ingressantes_semestre))

# --- 3) Contagem por ANO (soma semestres) ---
ingressantes_ano <- ingressantes_semestre %>%
  group_by(ano_ingresso) %>%
  summarise(total_ano = sum(n_ingressantes), .groups = "drop") %>%
  arrange(ano_ingresso)

cat("\nIngressantes por ANO (2011-2023):\n")
print(ingressantes_ano, n = nrow(ingressantes_ano))

# --- 4) Ver quem aparece em 2017.2 (detalhe para investigação) ---
cat("\nDetalhe dos registros em 2017.2:\n")
detalhe_2017_2 <- df %>%
  filter(ano_ingresso == 2017, semestre_ingresso == 2) %>%
  select(matricula, cpf, curriculo, periodo_de_ingresso, status, tipo_de_evasao, forma_de_ingresso) %>%
  arrange(matricula)

print(detalhe_2017_2, n = nrow(detalhe_2017_2))

cat("\nContagem por curriculo em 2017.2:\n")
print(detalhe_2017_2 %>% count(curriculo))

cat("\nContagem por forma_de_ingresso em 2017.2:\n")
print(detalhe_2017_2 %>% count(forma_de_ingresso))

# --- 5) Checar duplicatas por matricula em 2017.2 (para ver se há múltiplos registros) ---
dups_2017_2 <- detalhe_2017_2 %>%
  group_by(matricula) %>%
  filter(n() > 1) %>%
  ungroup()

cat("\nRegistros duplicados (mesma matricula) em 2017.2 — se aparecer, investigar):\n")
print(dups_2017_2, n = nrow(dups_2017_2))

# --- 6) Tabela cruzada: curriculo x periodo (para detectar associações inesperadas) ---
cat("\nTabela cruzada curriculo x periodo_de_ingresso:\n")
cross_tab <- df %>%
  count(curriculo, periodo_de_ingresso) %>%
  arrange(curriculo, periodo_de_ingresso)

print(cross_tab, n = nrow(cross_tab))

# --- 7) Plots para visualização ---
# 7a) Curva anual total
p1 <- ggplot(ingressantes_ano, aes(x = ano_ingresso, y = total_ano)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 2011:2023) +
  labs(title = "Ingressantes por ano (2011-2023)", x = "Ano de ingresso", y = "Total ingressantes") +
  theme_minimal()

print(p1)  # abre em Plots do RStudio

# 7b) Curva por período (semestre) separada por currículo
ingressantes_period_curriculo <- df %>%
  group_by(curriculo, periodo_de_ingresso) %>%
  summarise(n = n(), .groups = "drop")

p2 <- ggplot(ingressantes_period_curriculo, aes(x = periodo_de_ingresso, y = n, color = factor(curriculo))) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = sort(unique(ingressantes_period_curriculo$periodo_de_ingresso))) +
  labs(title = "Ingressantes por período (semestre) por currículo", x = "Período (ano.semestre)", y = "Ingressantes", color = "Currículo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)

# --- 8) Recomendações imediatas após execução ---
cat("\nRecomendações:\n")
cat("1) Verifique o resultado de detalhe_2017_2 — se houver poucos registros, confirme se isso é real (poucos ingressantes) ou se houve perda/filtragem de linhas.\n")
cat("2) Se detectar matriculas duplicadas ou ausência de registros esperados, verifique o pré-processamento e eventuais filtros aplicados anteriormente.\n")
cat("3) Para mais investigação, cole aqui a saída de 'detalhe_2017_2' se parecer inesperada.\n")
