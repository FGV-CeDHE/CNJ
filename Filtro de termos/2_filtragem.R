library(stringr)
library(dplyr)

#FILTRO DE TERMOS
#Criar colunas de acordo com os termos pré estabelecidos para as próprias pesquisadoras manusearem a base

decisoes_filtros <- decisoes %>% 
  #deixar pronto para análise, tudo em minúscula e sem espaços extras
  mutate(coluna_texto_tratada = str_to_lower(coluna_texto),
         coluna_texto_tratada = str_squish(coluna_texto_tratada)
         ) %>% 
  #criando colunas para os termos
  mutate(coluna_termo = str_detect(coluna_texto_tratada, 'mineração|garimpo|minério|mineral(?<!água)|mineradora|agricultura|cultivo|plantação|pecuária|gado|pasto|madeira|madeireiro|rodovia|ferrovia|hidrovia|hidrelétricas|energia|UHE|usina|empreendimento|risco|dano|ilegal|contaminação|ameaça|vulnerabilidade; exploração ilegal; demarcação; desafetação; alteração dos limites; revisão dos limites; desmatamento; queimada; invasão; conflito fundiário; conflito agrário; disputa por terra; grilagem; impacto; degradação;  ambiental; indígena; tradicionais; quilombola; meio ambiente; unidade de conservação; Amazônia; floresta; socioambiental'),
         )