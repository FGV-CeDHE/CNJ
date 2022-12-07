## TITULO: Ranking Incremento de Desmatamento
## DATA: 16/07/2022
## AUTORA: Mônica Rocabado (FGV CeDHE)

## PACOTES UTIIZADOS
library(dplyr)

## OBJETIVO

# - Obter um ranking dos municípios com maior soma por ano de incremento de desmatamento
# - Datos da Comissão Pastoral da Terra


# SUBIR A BASE
terra_brasilis_estado <- readr::read_csv2("bases_originais/terrabrasilis_amazon_12_8_2022_1662994454033.csv")

terra_brasilis_mun <- readr::read_csv2("bases_originais/terrabrasilis_amazon_12_8_2022_1662994469220.csv")

# TRANSFORMAÇÃO
terrabrasilis_mun_18_21_agregado <- terra_brasilis_mun %>% 
  filter(year >= 2018) %>% 
  group_by(mun, geocode_ibge) %>% 
  summarize(Soma_area = sum(`area km²`)) %>% 
  ungroup()


terrabrasilis_mun_18_21_agregado_top_10 <- terrabrasilis_mun_18_20_agregado %>% 
  filter(Soma_area >= 781.67)

# SALVAR O DADO
write.csv(terrabrasilis_mun_18_20_agregado, "terrabrasilis_mun_18_21_agregado.csv")
