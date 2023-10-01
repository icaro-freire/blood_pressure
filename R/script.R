# carregando dados
dados <- readr::read_csv("../data/blood-pressure_icaro.csv")

novo_dados <- dados |> 
  dplyr::relocate(time, .after = date)

readr::write_csv(novo_dados, "../data/blood-pressure_icaro-novo.csv")
