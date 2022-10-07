
## TÍTULO: ESTRUTURAÇÃO DOS DADOS | MÓDULO DE PRODUTIVIDADE
## DATA: 05/10/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(tidyverse)
library(sf)
library(data.table)
library(abjutils)

## PREPARANDO O AMBIENTE

setwd("data/input/Módulo de Produtividade Mensal")

# 1. Dados ----------------------------------------------------------------

## 1.1. STF ----------------------------------------------------------------

infoservent_STF <- read_delim("Dados desagregados/STF/moduloprod_infoservent_STF_03082022.csv", 
                               delim = ";", 
                               escape_double = FALSE, 
                               col_types = cols(`Indicador Valor` = col_number()), 
                               trim_ws = TRUE)

## 1.2. STJ ----------------------------------------------------------------

infomagistr_STJ <- read_delim("Dados desagregados/STJ/moduloprod_infomagistr_STJ_03082022.csv", 
                               delim = ";", 
                               escape_double = FALSE, 
                               col_types = cols(`Indicador Valor` = col_number()), 
                               trim_ws = TRUE)

## 1.3. TJAM ---------------------------------------------------------------

## Carregando as informações do STF

infomagistr_TJAM <- read_delim("Dados desagregados/TJAM/moduloprod_infomagistr_TJAM_03082022.csv", 
                               delim = ";", 
                               escape_double = FALSE, 
                               col_types = cols(`Indicador Valor` = col_number()), 
                               trim_ws = TRUE)

infoservent_TJAM <- read_delim("Dados desagregados/TJAM/moduloprod_infoservent_TJAM_03082022.csv", 
                              delim = ";", 
                              escape_double = FALSE, 
                              col_types = cols(`Indicador Valor` = col_number()), 
                              trim_ws = TRUE)

magistr_TJAM <- read_delim("Dados desagregados/TJAM/moduloprod_magistr_TJAM_03082022.csv", 
                           delim = ";", 
                           escape_double = FALSE, 
                           trim_ws = TRUE)

servent_TJAM <- read_delim("Dados desagregados/TJAM/moduloprod_servent_TJAM_03082022.csv", 
                           delim = ";", 
                           escape_double = FALSE,
                           trim_ws = TRUE)

serventpendt_TJAM <- read_delim("Dados desagregados/TJAM/moduloprod_serventpendt_TJAM_03082022.csv", 
                           delim = ";", 
                           escape_double = FALSE,
                           trim_ws = TRUE)

## 1.4. TJMT ---------------------------------------------------------------

infomagistr_TJMT <- read_delim("Dados desagregados/TJMT/moduloprod_infomagistr_TJMT_03082022.csv", 
                               delim = ";", 
                               escape_double = FALSE, 
                               col_types = cols(`Indicador Valor` = col_number()), 
                               trim_ws = TRUE)

## 1.5. TJPA ---------------------------------------------------------------

infomagistr_TJPA <- read_delim("Dados desagregados/TJPA/moduloprod_infomagistr_TJPA_03082022.csv", 
                               delim = ";", 
                               escape_double = FALSE, 
                               col_types = cols(`Indicador Valor` = col_number()), 
                               trim_ws = TRUE)

## 1.6. TJRO ---------------------------------------------------------------

infomagistr_TJRO <- read_delim("Dados desagregados/TJRO/moduloprod_infomagistr_TJRO_03082022.csv", 
                               delim = ";", 
                               escape_double = FALSE, 
                               col_types = cols(`Indicador Valor` = col_number()), 
                               trim_ws = TRUE)

## 1.7. TRF1 ---------------------------------------------------------------

infomagistr_TRF1 <- read_delim("Dados desagregados/TRF1/moduloprod_infomagistr_TRF1_03082022.csv", 
                               delim = ";", 
                               escape_double = FALSE, 
                               col_types = cols(`Indicador Valor` = col_number()), 
                               trim_ws = TRUE)

