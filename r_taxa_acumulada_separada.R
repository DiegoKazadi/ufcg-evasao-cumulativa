# =====================================================
# Capítulo 6.1 — Variável Demográfica: Idade
# Gráficos Separados por Período (1º ao 4º)
# =====================================================

library(dplyr)
library(ggplot2)
library(scales)

# =====================================================
# 1. Filtrar e preparar dados
# =====================================================

# Considerar apenas os 4 primeiros períodos
periodos_limite <- sort(unique(alunos_final$periodo_label))[1:4]

taxa_cumulativa_idade <- alunos_final %>%
  filter(periodo_label %in% periodos_limite) %>%
  mutate(
    faixa_etaria = case_when(
      idade_aproximada_no_ingresso < 20 ~ "< 20",
      idade_aproximada_no_ingresso >= 20 & idade_aproximada_no_ingresso <= 24 ~ "20-24",
      idade_aproximada_no_ingresso >= 25 & idade_aproximada_no_ingresso <= 29 ~ "25-29",
      idade_aproximada_no_ingresso >= 30 & idade_aproximada_no_ingresso <= 34 ~ "30-34",
      idade_aproximada_no_ingresso >= 35 ~ "35+",
      TRUE ~ NA_character_
    ),
    curriculo = as.factor(curriculo)
  ) %>%
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
# 2. Função para gerar gráfico de um período específico
# =====================================================

plot_por_periodo <- function(df, periodo_escolhido) {
  df_filtrado <- df %>% filter(periodo_label == periodo_escolhido)
  
  ggplot(
    df_filtrado,
    aes(
      x = faixa_etaria,
      y = taxa_cumulativa,
      group = curriculo,
      color = curriculo,
      shape = curriculo
    )
  ) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 4) +
    geom_text(
      aes(label = paste0(taxa_cumulativa, "%")),
      vjust = -1,
      size = 3.5,
      color = "black",
      show.legend = FALSE
    ) +
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
      title = paste("Taxa de Evasão Cumulativa —", periodo_escolhido),
      subtitle = "Comparativo entre Currículos 1999 (azul) e 2017 (amarelo escuro)",
      x = "Faixa Etária",
      y = "Taxa Cumulativa de Evasão (%)",
      color = "Currículo",
      shape = "Currículo"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

# =====================================================
# 3. Gerar e exibir os quatro gráficos
# =====================================================

grafico_1 <- plot_por_periodo(taxa_cumulativa_idade, periodos_limite[1])
grafico_2 <- plot_por_periodo(taxa_cumulativa_idade, periodos_limite[2])
grafico_3 <- plot_por_periodo(taxa_cumulativa_idade, periodos_limite[3])
grafico_4 <- plot_por_periodo(taxa_cumulativa_idade, periodos_limite[4])

# Visualizar um por vez:
grafico_1
grafico_2
grafico_3
grafico_4
