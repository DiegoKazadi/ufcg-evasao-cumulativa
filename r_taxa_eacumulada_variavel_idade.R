# =====================================================
# Capítulo 6.1 — Variável Demográfica: Idade
# Análise Cumulativa do Impacto da Reforma Curricular
# =====================================================

library(dplyr)
library(ggplot2)
library(scales)
library(knitr)

# =====================================================
# 1. Preparação dos dados
# =====================================================

# Criar faixas etárias no momento do ingresso
alunos_final <- alunos_final %>%
  mutate(
    faixa_etaria = case_when(
      idade_aproximada_no_ingresso < 20 ~ "< 20",
      idade_aproximada_no_ingresso >= 20 & idade_aproximada_no_ingresso <= 24 ~ "20-24",
      idade_aproximada_no_ingresso >= 25 & idade_aproximada_no_ingresso <= 29 ~ "25-29",
      idade_aproximada_no_ingresso >= 30 & idade_aproximada_no_ingresso <= 34 ~ "30-34",
      idade_aproximada_no_ingresso >= 35 ~ "35+",
      TRUE ~ NA_character_
    ),
    curriculo = as.factor(curriculo) # 🔹 Corrige erro da escala discreta
  )

# =====================================================
# 2. Cálculo da evasão cumulativa
# =====================================================

# Considerar apenas os 4 primeiros períodos
periodos_limite <- sort(unique(alunos_final$periodo_label))[1:4]

taxa_cumulativa_idade <- alunos_final %>%
  filter(periodo_label %in% periodos_limite) %>%
  group_by(curriculo, periodo_label, faixa_etaria) %>%
  summarise(
    total_ingressantes = n(),
    evadidos_cumulativos = sum(status == "INATIVO" & tolower(tipo_de_evasao) != "graduado"),
    .groups = "drop"
  ) %>%
  group_by(curriculo, faixa_etaria) %>%
  arrange(periodo_label) %>%
  mutate(
    evasao_cumulativa = cumsum(evadidos_cumulativos),
    taxa_cumulativa = round((evasao_cumulativa / total_ingressantes[1]) * 100, 2)
  ) %>%
  ungroup()

# =====================================================
# 3. Visualização gráfica
# =====================================================

ggplot(
  taxa_cumulativa_idade,
  aes(
    x = periodo_label,
    y = taxa_cumulativa,
    group = curriculo,
    color = curriculo,
    shape = curriculo
  )
) +
  geom_line(linewidth = 1.3) + # atualizado (substitui 'size')
  geom_point(size = 4) +
  geom_text(
    aes(label = paste0(taxa_cumulativa, "%")),
    vjust = -1,
    size = 3.5,
    color = "black",
    show.legend = FALSE
  ) +
  facet_wrap(~faixa_etaria) +
  scale_color_manual(
    values = c("1999" = "#003366", "2017" = "#FF8C00"),
    labels = c("Currículo 1999", "Currículo 2017")
  ) +
  scale_shape_manual(
    values = c("1999" = 15, "2017" = 17),
    labels = c("Currículo 1999", "Currículo 2017")
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Evasão Cumulativa por Faixa Etária e Período",
    subtitle = "Comparativo entre Currículos 1999 (azul) e 2017 (amarelo escuro)",
    x = "Período Letivo",
    y = "Taxa Cumulativa de Evasão (%)",
    color = "Currículo",
    shape = "Currículo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.direction = "horizontal",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# =====================================================
# 4. Impressão da Tabela 6.5 — Taxa de Evasão Acumulada
# =====================================================

tabela_6_5 <- taxa_cumulativa_idade %>%
  select(curriculo, faixa_etaria, periodo_label, taxa_cumulativa) %>%
  tidyr::pivot_wider(
    names_from = periodo_label,
    values_from = taxa_cumulativa
  ) %>%
  arrange(faixa_etaria, curriculo)

cat("\n==============================\n")
cat("📘 Tabela 6.5 — Taxa de Evasão Acumulada por Faixa Etária\n")
cat("==============================\n")

# Impressão formatada
print(
  knitr::kable(
    tabela_6_5,
    caption = "Tabela 6.5 — Taxa de Evasão Acumulada por Faixa Etária e Currículo",
    align = "c",
    digits = 2
  )
)
# =====================================================
# Capítulo 6.1 — Variável Demográfica: Idade
# Análise Cumulativa do Impacto da Reforma Curricular
# =====================================================

library(dplyr)
library(ggplot2)
library(scales)
library(knitr)

# =====================================================
# 1. Preparação dos dados
# =====================================================

# Criar faixas etárias no momento do ingresso
alunos_final <- alunos_final %>%
  mutate(
    faixa_etaria = case_when(
      idade_aproximada_no_ingresso < 20 ~ "< 20",
      idade_aproximada_no_ingresso >= 20 & idade_aproximada_no_ingresso <= 24 ~ "20-24",
      idade_aproximada_no_ingresso >= 25 & idade_aproximada_no_ingresso <= 29 ~ "25-29",
      idade_aproximada_no_ingresso >= 30 & idade_aproximada_no_ingresso <= 34 ~ "30-34",
      idade_aproximada_no_ingresso >= 35 ~ "35+",
      TRUE ~ NA_character_
    ),
    curriculo = as.factor(curriculo) # 🔹 Corrige erro da escala discreta
  )

# =====================================================
# 2. Cálculo da evasão cumulativa
# =====================================================

# Considerar apenas os 4 primeiros períodos
periodos_limite <- sort(unique(alunos_final$periodo_label))[1:4]

taxa_cumulativa_idade <- alunos_final %>%
  filter(periodo_label %in% periodos_limite) %>%
  group_by(curriculo, periodo_label, faixa_etaria) %>%
  summarise(
    total_ingressantes = n(),
    evadidos_cumulativos = sum(status == "INATIVO" & tolower(tipo_de_evasao) != "graduado"),
    .groups = "drop"
  ) %>%
  group_by(curriculo, faixa_etaria) %>%
  arrange(periodo_label) %>%
  mutate(
    evasao_cumulativa = cumsum(evadidos_cumulativos),
    taxa_cumulativa = round((evasao_cumulativa / total_ingressantes[1]) * 100, 2)
  ) %>%
  ungroup()

# =====================================================
# 3. Visualização gráfica
# =====================================================

ggplot(
  taxa_cumulativa_idade,
  aes(
    x = periodo_label,
    y = taxa_cumulativa,
    group = curriculo,
    color = curriculo,
    shape = curriculo
  )
) +
  geom_line(linewidth = 1.3) + # atualizado (substitui 'size')
  geom_point(size = 4) +
  geom_text(
    aes(label = paste0(taxa_cumulativa, "%")),
    vjust = -1,
    size = 3.5,
    color = "black",
    show.legend = FALSE
  ) +
  facet_wrap(~faixa_etaria) +
  scale_color_manual(
    values = c("1999" = "#003366", "2017" = "#FF8C00"),
    labels = c("Currículo 1999", "Currículo 2017")
  ) +
  scale_shape_manual(
    values = c("1999" = 15, "2017" = 17),
    labels = c("Currículo 1999", "Currículo 2017")
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Evasão Cumulativa por Faixa Etária e Período",
    subtitle = "Comparativo entre Currículos 1999 (azul) e 2017 (amarelo escuro)",
    x = "Período Letivo",
    y = "Taxa Cumulativa de Evasão (%)",
    color = "Currículo",
    shape = "Currículo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.direction = "horizontal",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# =====================================================
# 4. Impressão da Tabela 6.5 — Taxa de Evasão Acumulada
# =====================================================

tabela_6_5 <- taxa_cumulativa_idade %>%
  select(curriculo, faixa_etaria, periodo_label, taxa_cumulativa) %>%
  tidyr::pivot_wider(
    names_from = periodo_label,
    values_from = taxa_cumulativa
  ) %>%
  arrange(faixa_etaria, curriculo)

cat("\n==============================\n")
cat("📘 Tabela 6.5 — Taxa de Evasão Acumulada por Faixa Etária\n")
cat("==============================\n")

# Impressão formatada
print(
  knitr::kable(
    tabela_6_5,
    caption = "Tabela 6.5 — Taxa de Evasão Acumulada por Faixa Etária e Currículo",
    align = "c",
    digits = 2
  )
)
# =====================================================
# Capítulo 6.1 — Variável Demográfica: Idade
# Análise Cumulativa do Impacto da Reforma Curricular
# =====================================================

library(dplyr)
library(ggplot2)
library(scales)
library(knitr)

# =====================================================
# 1. Preparação dos dados
# =====================================================

# Criar faixas etárias no momento do ingresso
alunos_final <- alunos_final %>%
  mutate(
    faixa_etaria = case_when(
      idade_aproximada_no_ingresso < 20 ~ "< 20",
      idade_aproximada_no_ingresso >= 20 & idade_aproximada_no_ingresso <= 24 ~ "20-24",
      idade_aproximada_no_ingresso >= 25 & idade_aproximada_no_ingresso <= 29 ~ "25-29",
      idade_aproximada_no_ingresso >= 30 & idade_aproximada_no_ingresso <= 34 ~ "30-34",
      idade_aproximada_no_ingresso >= 35 ~ "35+",
      TRUE ~ NA_character_
    ),
    curriculo = as.factor(curriculo) # 🔹 Corrige erro da escala discreta
  )

# =====================================================
# 2. Cálculo da evasão cumulativa
# =====================================================

# Considerar apenas os 4 primeiros períodos
periodos_limite <- sort(unique(alunos_final$periodo_label))[1:4]

taxa_cumulativa_idade <- alunos_final %>%
  filter(periodo_label %in% periodos_limite) %>%
  group_by(curriculo, periodo_label, faixa_etaria) %>%
  summarise(
    total_ingressantes = n(),
    evadidos_cumulativos = sum(status == "INATIVO" & tolower(tipo_de_evasao) != "graduado"),
    .groups = "drop"
  ) %>%
  group_by(curriculo, faixa_etaria) %>%
  arrange(periodo_label) %>%
  mutate(
    evasao_cumulativa = cumsum(evadidos_cumulativos),
    taxa_cumulativa = round((evasao_cumulativa / total_ingressantes[1]) * 100, 2)
  ) %>%
  ungroup()

# =====================================================
# 3. Visualização gráfica
# =====================================================

ggplot(
  taxa_cumulativa_idade,
  aes(
    x = periodo_label,
    y = taxa_cumulativa,
    group = curriculo,
    color = curriculo,
    shape = curriculo
  )
) +
  geom_line(linewidth = 1.3) + # atualizado (substitui 'size')
  geom_point(size = 4) +
  geom_text(
    aes(label = paste0(taxa_cumulativa, "%")),
    vjust = -1,
    size = 3.5,
    color = "black",
    show.legend = FALSE
  ) +
  facet_wrap(~faixa_etaria) +
  scale_color_manual(
    values = c("1999" = "#003366", "2017" = "#FF8C00"),
    labels = c("Currículo 1999", "Currículo 2017")
  ) +
  scale_shape_manual(
    values = c("1999" = 15, "2017" = 17),
    labels = c("Currículo 1999", "Currículo 2017")
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Evasão Cumulativa por Faixa Etária e Período",
    subtitle = "Comparativo entre Currículos 1999 (azul) e 2017 (amarelo escuro)",
    x = "Período Letivo",
    y = "Taxa Cumulativa de Evasão (%)",
    color = "Currículo",
    shape = "Currículo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.direction = "horizontal",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# =====================================================
# 4. Impressão da Tabela 6.5 — Taxa de Evasão Acumulada
# =====================================================

tabela_6_5 <- taxa_cumulativa_idade %>%
  select(curriculo, faixa_etaria, periodo_label, taxa_cumulativa) %>%
  tidyr::pivot_wider(
    names_from = periodo_label,
    values_from = taxa_cumulativa
  ) %>%
  arrange(faixa_etaria, curriculo)

cat("\n==============================\n")
cat("📘 Tabela 6.5 — Taxa de Evasão Acumulada por Faixa Etária\n")
cat("==============================\n")

# Impressão formatada
print(
  knitr::kable(
    tabela_6_5,
    caption = "Tabela 6.5 — Taxa de Evasão Acumulada por Faixa Etária e Currículo",
    align = "c",
    digits = 2
  )
)
