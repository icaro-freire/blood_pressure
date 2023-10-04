# carregando pacotes ------------------------------------------------------
library(ggplot2)
library(dplyr)

# carregando dados --------------------------------------------------------
dados <- readr::read_csv("data/blood-pressure_icaro.csv")

dados

# escalas pressão A -------------------------------------------------------
dados_pressaoA <- dados |> 
  select(sis, dia) |> 
  mutate(
    classes = case_when(
      sis < 100 & dia < 60 ~ "low", 
      (100 <= sis & sis <= 120) & (60 <= dia & dia <= 80) ~ "normal", 
      (120 <= sis & sis <= 129 ) & (60 <= dia & dia <= 80) ~ "elevated",
      (130 <= sis & sis <= 139) | (80 <= dia & dia <= 89) ~ "hyper1",
      (140 <= sis & sis <= 180) | (90 <= dia & dia <= 120) ~ "hyper2",
      (sis > 180) | (dia > 120) ~ "crisis",
      .default = "other"
    )
  )

# escala pressão B --------------------------------------------------------
dados_pressaoB <- dados |> 
  select(dia, sis) |> 
  mutate(
    classes = case_when(
      sis < 100 & dia < 60 ~ "low", 
      sis < 120 & dia < 80 ~ "normal",
      (120 <= sis & sis <= 129) & (dia < 80) ~ "elevated",
      (130 <= sis & sis <= 139) & (80 <= dia & dia <= 89) ~ "stage1_ALL",
      (130 <= sis & sis <= 139) & (dia < 80) ~ "stage1_ISH",
      (sis < 130) & (80 <= dia & dia <= 89) ~ "stage1_IDH",
      (sis > 140) & (dia > 90) ~ "stage2_ALL",
      (sis > 140) & (dia < 90) ~ "stage2_ISH",
      (sis < 140) & (dia > 90)  ~ "stage2_IDH",
      (sis > 180) | (dia >120)  ~ "crisis",
      .default = "other"
    )
  ) 

# escala de cores ---------------------------------------------------------
cores <- c(
  "#C0C0C0", # low
  "#00FF00", # normal
  "#FFFF00", # elevated
  "#008080", # stage1_ALL
  "#0000FF", # stage1_ISH
  "#000080", # stage1_IDH
  "#808000", # stage2_ALL
  "#800000", # stage2_ISH
  "#FF0000", # stage2_IDH
  "#000000", # crisis
  "#FF00FF" # other
)

# gráfico mapeamento A ----------------------------------------------------

dados_pressaoA |> 
  ggplot() +
  aes(x = dia, y = sis, fill = classes) +
  geom_tile(
    color = "white"
  )

# gráfico mapeamento B ----------------------------------------------------

dados_pressaoB|> 
  ggplot() +
  aes(dia, sis, fill = classes) +
  geom_tile(
    color = "white"
  ) +
  scale_fill_manual(values = cores) +
  theme_minimal()

# linha temporal (geral) --------------------------------------------------

dados_dia <- dados |> 
  mutate(
    id = 1:nrow(dados), 
  ) |> 
  select(id, dia)

dados_dia

dados_sis <- dados |> 
  mutate(
    id = 1:nrow(dados), 
  ) |> 
  select(id, sis)


dados_dia |> 
  ggplot() +
  aes(id, dia) +
  geom_point() +
  geom_line(color = "blue") +
  geom_smooth(method = lm, se = FALSE, color = "red")

dados_sis |> 
  ggplot() +
  aes(id, sis) +
  geom_point() +
  geom_line(color = "blue") +
  geom_smooth(method = lm, se = FALSE, color = "red")

# densidade ---------------------------------------------------------------

dados_pressaoA |> 
  ggplot() +
  aes(x = dia, fill = classes) +
  geom_density()

dados_pressaoB |> 
  ggplot() +
  aes(x = dia, fill = classes) +
  geom_density()
