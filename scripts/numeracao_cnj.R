
## TÍTULO: NUMERAÇÃO ÚNICA CNJ | CAMADA SIRENEJUD
## DATA: 14/11/2022
## AUTOR: REBECA CARVALHO

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(tm)

## OBJETIVOS

#'         - Coletar informações sobre as unidades de origem
#'           das ações ambientais obtidas via web scraping.
#'         - Preparar os dados para a Camada do SireneJud.

## AMBIENTE

setwd("CNJ")

# 1. Data -----------------------------------------------------------------

## Carregando os dados de referência

df <- readRDS("~/CeDHE/CNJ/data/output/recorte_processos ambientais_14112022.rds")

cod_unjud <- readxl::read_xlsx("data/input/Módulo de Produtividade Mensal/código_unidade de origem_14112022.xlsx")

## Criando um banco com os códigos de cada justiça estadual

triborigem <- data.frame(SigTribOrig = c("AC", "AL", "AP", "AM", "BA",
                                         "CE", "DF", "ES","GO", "MA", "MT",
                                         "MS", "MG", "PA", "PB", "PR", "PE", "PI",
                                         "RJ", "RN", "RS", "RO", "RR", 
                                         "SC", "SP", "SE", "TO"),
                         TribunalOrigem = 1:27) %>% 
  mutate(TribunalOrigem = str_pad(TribunalOrigem,
                                  width = 2,
                                  side = "left",
                                  pad = 0),
         SigTribOrig = paste0("TJ",
                              SigTribOrig))

## 1.1. Limpeza ------------------------------------------------------------

## Organizando os dados das unidades de origem

cod_unjud <- cod_unjud %>% 
  rename("Justica" = "Justiça",
         "SigTribOrig" = "Tribunal sigla",
         "UF" = "Tribunal UF",
         "Municipio" = "Tribunal município",
         "TipoUnidade" = "Tipo Unidade",
         "ClassUnidade" = "Classificação unidade",
         "CodMunicipio" = "Código IBGE - Município sede",
         "UnidadeOrigem" = "Código da unidade de origem do processo",
         "lat" = "Latitude",
         "lon" = "Longitude") %>% 
  select(lat,
         lon,
         Justica,
         SigTribOrig,
         UF,
         CodMunicipio,
         Municipio,
         UnidadeOrigem) %>% 
  filter(UnidadeOrigem != "-") %>% 
  mutate(lat = gsub("\\,",
                    ".",
                    lat),
         lon = gsub("\\,",
                    ".",
                    lon),
         UnidadeOrigem = str_pad(UnidadeOrigem,
                                 width = 4,
                                 side = "left",
                                 pad = 0)) %>% 
  group_by(SigTribOrig, Municipio, UnidadeOrigem) %>% 
  slice(1) %>% 
  filter(Municipio != "-") %>% 
  unique()

## Organizando os processos ambientais

df <- df %>% 
  filter(Numero != "SEM NÚMERO ÚNICO") %>% 
  mutate(Numero2 = Numero) %>% 
  separate(Numero2,
           into = c("Sequencial",
                    "DigVerificador",
                    "AnoAjuizamento",
                    "SegmtPoderJudiciario",
                    "TribunalOrigem",
                    "UnidadeOrigem"),
           sep = "[[:punct:]]") %>% 
  mutate(SegmtPoderJudiciario = case_when(SegmtPoderJudiciario == 1 ~ "Supremo Tribunal Federal",
                                          SegmtPoderJudiciario == 2 ~ "Conselho Nacional de Justiça",
                                          SegmtPoderJudiciario == 3 ~ "Superior Tribunal de Justiça",
                                          SegmtPoderJudiciario == 4 ~ "Justiça Federal",
                                          SegmtPoderJudiciario == 5 ~ "Justiça do Trabalho",
                                          SegmtPoderJudiciario == 6 ~ "Justiça Eleitoral",
                                          SegmtPoderJudiciario == 7 ~ "Justiça Militar da União",
                                          SegmtPoderJudiciario == 8 ~ "Justiça dos Estados e do Distrito Federal e Territórios",
                                          SegmtPoderJudiciario == 9 ~ "Justiça Militar Estadual",
                                          TRUE ~ "NA")) 

# 2. Join -----------------------------------------------------------------

## Juntando com as informações das justiças estaduais

df <- left_join(df,
                triborigem) %>% 
  mutate(SigTribOrig = ifelse(is.na(SigTribOrig),
                              SiglaTribunal,
                              SigTribOrig),
         SigTribOrig = ifelse(SegmtPoderJudiciario == "Justiça Federal" &
                                TribunalOrigem == "01",
                              "TRF1",
                              ifelse(SegmtPoderJudiciario == "Justiça Federal" &
                                       TribunalOrigem == "02",
                                     "TRF2",
                                     ifelse(SegmtPoderJudiciario == "Justiça Federal" &
                                              TribunalOrigem == "03",
                                            "TRF3",
                                            ifelse(SegmtPoderJudiciario == "Justiça Federal" &
                                                     TribunalOrigem == "04",
                                                   "TRF4",
                                                   ifelse(SegmtPoderJudiciario == "Justiça Federal" &
                                                            TribunalOrigem == "05",
                                                          "TRF5",
                                                          ifelse(SegmtPoderJudiciario == "Justiça Federal" &
                                                                   TribunalOrigem == "06",
                                                                 "TRF6",
                                                                 SigTribOrig)))))))


## Juntando os dados das unidades de origem

df <- left_join(df,
                cod_unjud) %>% 
  select(Numero,
         Sequencial,
         DigVerificador,
         SegmtPoderJudiciario,
         TribunalOrigem,
         SigTribOrig,
         UnidadeOrigem,
         SiglaTribunal,
         UF,
         CodMunicipio,
         Municipio,
         AnoAjuizamento,
         DataJulgamento,
         DataPublicacao,
         Ementa,
         Infraestrutura:`Exploração florestal`,
         lat,
         lon) %>% 
  rename("SigTribJulg" = "SiglaTribunal",
         "CodTribOrigem" = "TribunalOrigem") %>% 
  mutate(SigTribOrig =  ifelse(SegmtPoderJudiciario == "Superior Tribunal de Justiça",
                               "STJ",
                               SigTribOrig)) %>% 
  filter(SegmtPoderJudiciario != "NA") %>% 
  unique()

## Verificando quais unidades não foram encontradas

naoencontr <- df %>% 
  filter(is.na(Municipio)) %>% 
  select(SegmtPoderJudiciario:UnidadeOrigem) %>% 
  unique() %>% 
  arrange(SegmtPoderJudiciario,
          SigTribOrig,
          UnidadeOrigem)


# 3. Salva ----------------------------------------------------------------

## Salvando o banco final gerado

saveRDS(df,
        "data/output/Camada SireneJud/recorte ambiental_setores_14112022.rds")

write.csv(df,
          "data/output/Camada SireneJud/recorte ambiental_setores_14112022.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

writexl::write_xlsx(naoencontr,
                    "data/output/Camada SireneJud/não encontradas_unidades de origem.xlsx")
