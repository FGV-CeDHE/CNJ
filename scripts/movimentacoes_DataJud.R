
## PROCESSANDO ARQUIVOS DATAJUD
## DATA: 30/05/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(jsonlite)
library(geobr)
library(data.table)

# 1. Data -----------------------------------------------------------------

## Carregando os dados do DataJud

datajud <- readRDS("data/output/DataJud/dataJud_final_17072022.rds")

## Carregando a lista de municípios que integra a Amazônia Legal

municipios <- readxl::read_xls("data/input/SireneJud/lista_de_municipios_Amazonia_Legal_2020.xls")


## Criando uma lista com os arquivos disponíveis

files <- list.files(path = "data/input/DataJud",
                    pattern = "json")

## Cria uma lista onde os dados
## serão armazenados

movimentos <- list()

## For loop que extrai os dados
## de movimentação

for(i in seq_along(files)){
  
  cat("Lendo", i, "\n")
  
  ## Carregando o arquivo
  
  temp <- jsonlite::fromJSON(paste0("data/input/DataJud/",
                                    files[i]),
                             flatten = TRUE)
  
  temp2 <- list()
  
  for(j in 1:nrow(temp)){
    
    cat("Linha", j, "\n")
    
    Numero <- temp$`_source.dadosBasicos.numero`[j]
  
  unlist <- tryCatch({
    suppressMessages({
      
    mov   <- as.data.frame(temp$`_source.movimento`[[j]])
    
    mov$Numero <- Numero
    
    temp2[[j]] <- mov

      
      unlist })}, error = function(e) {print("erro")
        NA})
  }
  
  temp2 <- rbind.fill(temp2)
  
 saveRDS(temp2,
         paste0("data/output/DataJud/movimentos_parte_",
                i,
                ".rds"))
  gc()
}

## Filtrando somente os dados referentes a Amazônia Legal

datajud <- datajud %>% 
  filter(UF %in% municipios$SIGLA) %>% 
  mutate(Numero = str_replace_all(Numero, 
                                  "[[:punct:]]", 
                                  ""))

## Criando uma lista com os arquivos disponíveis

files <- list.files(path = "data/output/DataJud",
                    pattern = "movimentos_parte")

## Cria uma lista onde os dados serão armazenados

movimentos_final <- list()

## For loop que carrega os arquivos

for(i in seq_along(files)){
  
  cat("Lendo", i, "\n")
  
  ## Carregando o arquivo
  
  temp <- readRDS(paste0("data/output/DataJud/",
                                   files[i]))
  
  temp <- temp %>%
    filter(Numero %in% unique(datajud$Numero)) %>% 
    mutate(across(everything(), as.character)) %>% 
    unique()
  
  movimentos_final <- rbind.fill(movimentos_final, 
                                 temp)
  
  gc()
  
}

saveRDS(movimentos_final,
        "data/output/DataJud/movimentações_amazônia legal.rds")

exportJson <- toJSON(datajud$movimento)

write(exportJson, file = "movimentos2.JSON")

temp <- jsonlite::fromJSON("movimentos.JSON",
                           flatten = TRUE)


# 2. Limpeza --------------------------------------------------------------

## Carregando os dados de referência

movimentacoes <- readRDS("data/output/DataJud/movimentações_amazônia legal.rds")

codigo <- readxl::read_xlsx("data/output/DataJud/cod_movimentos.xlsx")

## Alterando o nome da variável

codigo <- codigo %>% 
  dplyr::rename("movimentoNacional.codigoNacional" = "Código") %>% 
  dplyr::select(-Glossário)

## Alterando o tipo da variável

movimentacoes <- movimentacoes %>% 
  mutate(movimentoNacional.codigoNacional = as.character(movimentoNacional.codigoNacional))

## Juntando os dados

movimentacoes <- left_join(movimentacoes,
                           codigo) %>% 
  mutate(Movimento = paste0(`movimentoNacional.codigoNacional`,
                            " - ",
                            Descrição))
