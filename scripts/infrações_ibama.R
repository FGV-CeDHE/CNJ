
## TÍTULO: AUTOS DE INFRAÇÃO | IBAMA
## AUTOR: REBECA CARVALHO
## DATA: 01/11/2022

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(Ibamam)

## AMBIENTE

setwd("CNJ")

# 1. Data -----------------------------------------------------------------

df <- read_delim("data/input/Ibama/auto_infracao.csv", 
                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

