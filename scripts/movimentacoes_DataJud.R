
## TÍTULO: MOVIMENTAÇÕES DATAJUD - NOVA BASE DE 05/09/2022
## AUTORA: REBECA CARVALHO
## DATA: 14/09/2022

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(jsonlite)
library(geobr)
library(data.table)
library(tm)
library(reshape2)

## PREPARANDO O AMBIENTE

setwd("CNJ")

# 1. Data -----------------------------------------------------------------

## Carregando os dados de referência

movimentos <- readRDS("data/output/DataJud/datajud_movimentos_v26102022.rds")

## Selecionando somente os casos que interessam no DataJud

movimentos <- movimentos %>% 
  filter(ano_inicio_situacao_novo >= "2020") %>% 
  unique()

## Reorganizando os dados

movimentos <- movimentos %>% 
  mutate(movimento = str_replace_all(movimento,
                                     "\\[|]",
                                     ""),
         movimento = str_replace_all(movimento,
                                     '"',
                                     ""),
         movimento = str_replace_all(movimento,
                                     "\\}",
                                     "")) %>% 
  separate_rows(movimento,
                sep = "{") %>% 
  separate(movimento,
           sep = "\\,",
           into = c("dt_inicio_situacao",
                    "dt_fim_situacao",
                    "sigla_grau",
                    "id_situacao",
                    "id_situacao_iniciar",
                    "id_situacao_finalizar",
                    "id_fase_processual",
                    "id_tipo_procedimento",
                    "id_natureza_procedimento",
                    "nome_situacao",
                    "nome_fase_processual",
                    "nome_tipo_procedimento",
                    "nome_natureza_procedimento",
                    "id_movimento_origem",
                    "nome_movimento",
                    "movimentacao_complemento",
                    "movimento_nome_completo",
                    "movimento_nivel",
                    "nome_julgador",
                    "criminal")) %>% 
  mutate(across(everything(), 
                gsub, 
                pattern = ".*: ", 
                replacement = ""),
         across(everything(), 
                gsub, 
                pattern = ".*:", 
                replacement = ""),
         across(everything(), 
                str_to_upper)) %>% 
  filter(dt_inicio_situacao != "") %>% 
  mutate(nome_movimento = stripWhitespace(trimws(nome_movimento)), 
         nome_movimento = case_when(nome_movimento == "A DEPENDER DO JULGAMENTO DE OUTRA CAUSA" ~ 
                                      "A DEPENDER DO JULGAMENTO DE OUTRA CAUSA, DE OUTRO JUÍZO OU DECLARAÇÃO INCIDENTE",
                                    nome_movimento == "ANISTIA" ~ "ANISTIA, GRAÇA OU INDULTO",
                                    nome_movimento == "CONHECIMENTO EM PARTE E NÃO-PROVIMENTO" ~ "CONHECIMENTO EM PARTE E NÃO-PROVIMENTO OU DENEGAÇÃO",
                                    nome_movimento == "CONHECIMENTO EM PARTE E PROVIMENTO" ~ "CONHECIMENTO EM PARTE E PROVIMENTO OU CONCESSÃO",
                                    nome_movimento == "CONHECIMENTO EM PARTE E PROVIMENTO EM PARTE" ~ 
                                      "CONHECIMENTO EM PARTE E PROVIMENTO EM PARTE OU CONCESSÃO EM PARTE",
                                    nome_movimento == "CUMPRIMENTO DE LEVANTAMENTO DA SUSPENSÃO OU DESSOBRESTAMENTO" ~ 
                                      "CUMPRIMENTO DE LEVANTAMENTO DA SUSPENSÃO",
                                    nome_movimento == "PRESCRIÇÃO INTERCORRENTE (ART. 921" ~ "PRESCRIÇÃO INTERCORRENTE (ART. 921, § 4º, CPC)",
                                    nome_movimento == "POR DECISÃO DO PRESIDENTE DO STF - IRDR" ~ "POR DECISÃO DO PRESIDENTE DO STF - SIRDR",
                                    nome_movimento == "SUMARÍSSIMO (ART. 852-B" ~ "SUMARÍSSIMO (ART. 852-B, § 1º/CLT)",
                                    T ~ as.character(nome_movimento)))

## Salvando os dados brutos

saveRDS(movimentos,
        "data/output/DataJud/movimentações_v26102022.rds")
