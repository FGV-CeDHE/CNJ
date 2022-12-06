
## TÍTULO: NOMES E DESCRIÇÕES DAS CLASSES, ASSUNTOS E MOVIMENTOS (TPU)
## AUTORA: REBECA CARVALHO
## DATA: 05/06/2022

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(tm)

## OBJETIVOS

#'         - Coletar os nomes e as descrições das classes, assuntos e movimentos
#'           presentes na base do DataJud a partir dos códigos.

## FUNÇÕES

source("functions/TPUnamesA.R", 
       encoding = "UTF-8")

source("functions/TPUnamesC.R", 
       encoding = "UTF-8")

source("functions/TPUnamesM.R", 
       encoding = "UTF-8")

source("functions/strdehtml.R", 
       encoding = "UTF-8")

# 1. Data -----------------------------------------------------------------

## Carregando o banco do DatJud

df <- readRDS("data/output/DataJud/dataJud_final_15072022.rds")

## 1.1. Classes Processuais ------------------------------------------------

## Verificando quantos movimentos distintos 
## apareceram nos dados

classes <- unique(df$ClasseProcessual)

## Data frame onde os dados serão
## armazenados posteriormente

cod_classes <- data_frame()

## For loop que coleta as informações

for(i in seq_along(classes)){
  
  cat("Lendo", i, "\n")
  
  
  temp <- retorna_nome_codigo_TPU_C(classes[i],
                                    dt_frame = TRUE)
  
  cod_classes <- rbind.fill(cod_classes,
                            temp)
  
}

## Organizando os dados

cod_classes <- cod_classes %>% 
  rename("Código" = "codigo",
         "Descrição" = "nome",
         "Glossário" = "glossario") %>% 
  arrange(Código) %>% 
  mutate(Glossário = gsub("<[^>]+>", 
                          "",
                          Glossário),
         Glossário = strdehtml(Glossário),
         Glossário = stripWhitespace(Glossário),
         Glossário = str_sub(Glossário,
                             1,
                             32767))

## 1.2. Assuntos -----------------------------------------------------------

## Verificando quantos movimentos distintos 
## apareceram nos dados

assuntos <- unique(c(unlist(str_split(df$CodigoAssunto,
                                     " / "))))

## Data frame onde os dados serão
## armazenados posteriormente

cod_assuntos <- data_frame()

## For loop que coleta as informações

for(i in seq_along(assuntos)){
  
  cat("Lendo", i, "\n")
  
  
  temp <- retorna_nome_codigo_TPU_A(assuntos[i],
                                    dt_frame = TRUE)
  
  cod_assuntos <- rbind.fill(cod_assuntos,
                             temp)
  
}

## Organizando os dados

cod_assuntos <- cod_assuntos %>% 
  rename("Código" = "codigo",
         "Descrição" = "nome",
         "Glossário" = "glossario") %>% 
  arrange(Código) %>% 
  mutate(Glossário = gsub("<[^>]+>", 
                          "",
                          Glossário),
         Glossário = strdehtml(Glossário),
         Glossário = stripWhitespace(Glossário))

## Salvando os dados

saveRDS(cod_assuntos,
        "data/output/DataJud/cod_assuntos.rds")

writexl::write_xlsx(cod_assuntos,
                    "data/output/DataJud/cod_assuntos.xlsx")

## 1.3. Movimentos ---------------------------------------------------------

## Verificando quantos movimentos distintos 
## apareceram nos dados

movimentos <- unique(c(unlist(str_split(df$CodigoNacionalMovimento,
                        " / "))))

## Data frame onde os dados serão
## armazenados posteriormente

cod_movimentos <- data_frame()

## For loop que coleta as informações

for(i in seq_along(movimentos)){
  
  cat("Lendo", i, "\n")


  temp <- retorna_nome_codigo_TPU_M(movimentos[i],
                                    dt_frame = TRUE)
  
  cod_movimentos <- rbind.fill(cod_movimentos,
                               temp)
  
}

## Organizando os dados

cod_movimentos <- cod_movimentos %>% 
  rename("Código" = "codigo",
         "Descrição" = "nome",
         "Glossário" = "glossario") %>% 
  arrange(Código) %>% 
  mutate(Glossário = trimws(gsub("<[^>]+>", 
                          "",
                          Glossário)),
         Glossário = strdehtml(Glossário),
         Glossário = stripWhitespace(Glossário))

# 2. Salva ----------------------------------------------------------------

## Salvando os dados

saveRDS(cod_classes,
        "data/output/DataJud/cod_classes.rds")

writexl::write_xlsx(cod_classes,
                    "data/output/DataJud/cod_classes.xlsx")

saveRDS(cod_assuntos,
        "data/output/DataJud/cod_assuntos.rds")

writexl::write_xlsx(cod_assuntos,
                    "data/output/DataJud/cod_assuntos.xlsx")

saveRDS(cod_movimentos,
        "data/output/DataJud/cod_movimentos.rds")

writexl::write_xlsx(cod_movimentos,
                    "data/output/DataJud/cod_movimentos.xlsx")
