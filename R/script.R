# carregando pacotes
library(ggplot2)

# carregando dados
dados <- readr::read_csv("data/blood-pressure_icaro.csv")

dados |> 
  ggplot2::ggplot() +
  ggplot2::aes(y = sis, x = dia) +
  ggplot2::geom_point()

dados |> 
  ggplot() + 
  aes(x = date, y = sis) +
  geom_point() +
  geom_line(color = "blue") +
  geom_smooth(method = lm, se = FALSE, color = "red")
