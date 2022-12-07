## TITULO: Ranking Ajuizamento
## DATA: 16/07/2022
## AUTORA: Mônica Rocabado (FGV CeDHE)

## PACOTES UTIIZADOS
library(dplyr)

## OBJETIVO

# - Obter um ranking dos municípios com maior soma por ano de ajuizamentos 

# SUBIR A BASE
sirenejud_allvars_filt_v05092022 <- readRDS("~/4. CNJ/datajud_v3_amlegal/bases_originais/sirenejud_allvars_filt_v05092022.rds")

# TRANSFORMAÇÃO
sirenejud_18_21_agregado <- sirenejud_allvars_filt_v05092022 %>% 
  mutate(ano_v2 = as.double(ano_v2)) %>% 
  filter(ano_v2 >= 2018,
         ano_v2 < 2022) %>% 
  group_by(municipio, uf) %>% 
  summarize(Soma_ajuizamento = n()) %>% 
  ungroup()

sirenejud_18_21_agregado_top_10 <- sirenejud_18_21_agregado %>%
  filter(Soma_ajuizamento >= 610)

#SALVAR O DADO
write.csv(sirenejud_18_21_agregado, "Sirenejud_18a21.csv")
