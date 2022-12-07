## TITULO: Ranking Conflitos
## DATA: 16/07/2022
## AUTORA: Mônica Rocabado (FGV CeDHE)

## PACOTES UTIIZADOS
library(dplyr)

## OBJETIVO

# - Obter um ranking dos municípios com maior soma por ano de conflitos
# - Datos da Comissão Pastoral da Terra

# SUBIR A BASE
cpt_conflitos <- readxl::read_excel("bases_originais/CPT_Conflitos_AMLegal.xlsx")

# TRANSFORMAÇÃO
cpt_18_20_agregado <- cpt_conflitos %>% 
  filter(ANO >= 2018) %>% 
  group_by(`Nome Município`, UF, CD_MUN) %>% 
  summarize(Soma_Conflitos = sum(VALOR)) %>% 
  ungroup()

cpt_18_20_agregado_top_10 <- cpt_18_20_agregado %>%
  filter(Soma_Conflitos >= 45)

#SALVAR O DADO
write.csv(cpt_18_20_agregado, "CPT_18a20.csv")
