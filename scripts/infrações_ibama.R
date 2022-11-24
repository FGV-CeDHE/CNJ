
## TÍTULO: AUTOS DE INFRAÇÃO | IBAMA
## AUTOR: REBECA CARVALHO
## DATA: 01/11/2022

## PACOTES UTILIZADOS

# remotes::install_github("cccneto/Ibamam", 
#                         force = TRUE)

library(Ibamam)

## AMBIENTE

setwd("CNJ")

# 1. Data -----------------------------------------------------------------

df <- get_dataset_ibamam(dataset = "distribuidas")

