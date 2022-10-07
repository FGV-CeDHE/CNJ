
## TÍTULO: LEITURA E ANÁLISE DE PROCESSOS FAVORÁVEIS E DESFAVORÁVEIS
## A PROTEÇÃO DO MEIO AMBIENTE
## DATA: 28/07/2022
## AUTOR: REBECA CARVALHO


## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(pdftools)
library(readr)
library(readtext)
library(tm)
library(ptstem) 
library(stringi)
library(wordcloud)
library(abjutils)
library(extrafont)
library(geobr)

## PREPARANDO O AMBIENTE

extrafont::loadfonts()

# 1. Data -----------------------------------------------------------------

sirenejud <- readxl::read_xlsx("data/input/8municipios_Sirenejud_.xlsx",
                               sheet = 3)

municipios <- geobr::read_municipality(year = 2020)

## Filtrando os municípios de interesse

municipios <- municipios %>% 
  filter(abbrev_state %in% c("AM",
                             "PA",
                             "MT",
                             "RO"))

## Deixando somente as informações necessárias

municipios <- str_to_lower(municipios$name_muni)

## Cria uma lista com os nomes dos .pdfs que serão lidos

pdf_desfavoravel <- list.files(path = "data/input/SireneJud/FavDesfav_concluídos_8mun/Desfavorável", 
                               pattern = ".pdf",
                               ignore.case=TRUE)

pdf_favoravel <- list.files(path = "data/input/SireneJud/FavDesfav_concluídos_8mun/Favorável", 
                            pattern = ".pdf",
                            ignore.case = TRUE)

## Cria um dataframe vazio em que as decisões
## desfavoráveis serão armazenadas

textos_desfavoravel <- data.frame("Numero" = rep(NA,
                                                 29),
                                  "InteiroTeor" = rep(NA,
                                                      29),
                                  "Decisao" = rep("Desfavorável",
                                                  29))

## For loop que extrai os dados dos PDFs

for(i in seq_along(pdf_desfavoravel)){
  
  cat("Lendo",i,"\n")
  
  textos_desfavoravel$Numero[i] <- str_split(pdf_desfavoravel[i],
                                             "_")[[1]][1]
  
  textos_desfavoravel$InteiroTeor[i]  <- paste(pdftools::pdf_text(paste0("data/input/SireneJud/FavDesfav_concluídos_8mun/Desfavorável/", 
                                          pdf_desfavoravel[i])),
                                          sep = '', collapse = '')

}

## Cria um dataframe vazio em que as decisões
## favoráveis serão armazenadas

textos_favoravel <- data.frame("Numero" = rep(NA,
                                                 53),
                               "InteiroTeor" = rep(NA,
                                                      53),
                               "Decisao" = rep("Favorável",
                                                  53))

## For loop que extrai os dados dos PDFs

for(i in seq_along(pdf_favoravel)){
  
  cat("Lendo",i,"\n")
  
  textos_favoravel$Numero[i] <- str_split(pdf_favoravel[i],
                                             "_")[[1]][1]
  
  textos_favoravel$InteiroTeor[i]  <- paste(pdftools::pdf_text(paste0("data/input/SireneJud/FavDesfav_concluídos_8mun/Favorável/", 
                                                                      pdf_favoravel[i])),
                                               sep = '', 
                                            collapse = '')
  
}

## Empilha os dois arquivos de textos

decisoes <- rbind(textos_favoravel, 
                  textos_desfavoravel)

## Cria uma coluna temporário com o texto tratado

decisoes <- decisoes %>% 
  mutate(InteiroTeor2 = str_to_lower(InteiroTeor))

## Juntando com os dados do SireneJud

decisoes <- left_join(decisoes,
                      sirenejud)

## Verificando em quantas observações as palavras 
## coordenadas e hectares aparecem

decisoes <- decisoes %>% 
  mutate(Hectares = grepl("hectares",
                          InteiroTeor2),
         CoordenadasGeo = grepl("coordenadas geográficas",
                                InteiroTeor2))

## Define qual palavra será buscada

expr <- c("hectares")

## Adequa a lista ao formato regex

regex <- regex(paste0("[^.]+", 
                      expr,
                      ".*",
                      collapse = "|"))

decisoes$Tamanho <-  str_extract(decisoes$InteiroTeor2, 
                                 regex)

## Define quais palavras serão buscada

expr <- c("coordenadas geográficas",
          "coordenadas geograficas",
          "coordenadas")

## Adequa a lista ao formato regex

regex <- regex(paste0("[^.]", 
                      expr,
                      ".*?\\.(.*?\\.){0,1}[^.]+",
                      collapse = "|"))

decisoes$Coordenadas <-  str_extract(decisoes$InteiroTeor2, 
                                     regex)

regex <- regex(paste0("[^.]", 
                      expr,
                      "[^.]+",
                      collapse = "|"))

for(i in 1:nrow(decisoes)){
  
  cat("Lendo", i, "\n")
  
  if(is.na(decisoes$Coordenadas[i])){
    
    decisoes$Coordenadas[i] <-  str_extract_all(decisoes$InteiroTeor2[i], 
                                            regex)
    
  }
  
}

## Adequa o vetor ao formato regex

regex <- regex(paste0("\\b", municipios, "\\b", collapse = "|"))

## Procura no texto de cada TAC o município
## compromissário, anuente ou interveniente

for (i in 1:nrow(decisoes)) {
  
  cat("Lendo", i, "\n")
  
  decisoes$Municipio2[i] <- str_extract_all(stripWhitespace(as.character(decisoes$Coordenadas[i])), 
                                           regex)
  
}

## Padronizando o Tamanho

decisoes <- decisoes %>% 
  mutate(Tamanho = gsub("\n",
                        " ",
                        Tamanho,
                        fixed = TRUE))

## Adequa a lista ao formato regex

regex <- regex(paste0("[^\\s]*[0-9]\\s*(.*?)\\s*hectares",
                      collapse = "|"))

decisoes$Tamanho2 <-  str_extract(decisoes$Tamanho, 
                                 regex)

## Organizando os dados

decisoes <- decisoes %>% 
  select(Numero,
         Data,
         Esfera,
         Tribunal,
         Status,
         InteiroTeor,
         Decisao,
         Hectares,
         CoordenadasGeo,
         Tamanho,
         Tamanho2,
         Coordenadas,
         Municipio,
         Municipio2) %>% 
  mutate(Municipio2 = str_to_upper(as.character(Municipio2)),
         Esfera = str_to_upper(Esfera),
         Status = str_to_upper(Status),
         Decisao = str_to_upper(Decisao))

write.xlsx(decisoes,
           "data/output/SireneJud/decisões_geocodificadas_SireneJud.xlsx")

