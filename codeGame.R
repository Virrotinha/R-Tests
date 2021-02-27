library(highcharter)
library(dplyr)   
library(ggplot2)
library(janitor)

game <- videogamesdata
filtrados <- janitor::clean_names(game) %>%
  filter(platform %in% c('Wii', 'DS'))  
# %>% filter(year == 2005)

filtrados <- janitor::clean_names(game) %>%
  filter(platform %in% c('Wii', 'DS'))  
  
  table(game$Platform) # %>% as.numeric(game$Platform)

  tabyl(game$Platform %in% c('Wii', 'DS')) #frequencia em %


sumarizados <- filtrados %>% 
  group_by(platform) %>% 
  summarise(total=n()) #quantidade 

sumarizados <- game %>% group_by(Platform) %>% summarise(total=n())

grafico <-
  hchart(sumarizados, type = "column", 
         hcaes(x = Platform, y = total)) %>%
  hc_colors("#440154FF") %>%
  hc_title(text = "Plataformas de Jogos") %>%
  hc_subtitle(text = "Plataformas em que os jogos são disponibilizados") %>%
  hc_xAxis(title = list(text = "Plataformas")) %>%
  hc_yAxis(title = list(text = "Número de jogos")) %>%
  hc_credits(
    enabled = TRUE,
    text = "Fonte: Painel COVID-19 Espírito Santo",
    href = "https://coronavirus.es.gov.br/painel-covid-19-es")
grafico

filtrados2 <- janitor::clean_names(game) %>%
  filter(Publisher %>% sumarizados2$total > 500)
  
sumarizados2 <- game %>% group_by(Publisher) %>% summarise(total=n())
filtrados2 <- janitor::clean_names(sumarizados2) %>%
  filter(total > 500)

grafico2 <-
  hchart(filtrados2, type = "column", 
         hcaes(x = publisher, y = total)) %>%
  hc_colors("#CC0000") %>%
  hc_title(text = "Empresas de Jogos") %>%
  hc_subtitle(text = "Empresas que publicaram os jogos") %>%
  hc_xAxis(title = list(text = "Empresas")) %>%
  hc_yAxis(title = list(text = "Número de jogos")) %>%
  hc_credits(
    enabled = TRUE,
    text = "",
    href = "") %>%
  hc_add_theme(hc_theme_chalk())


grafico2
