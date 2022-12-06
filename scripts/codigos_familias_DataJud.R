
## TÍTULO: HIERARQUIA DAS CLASSES, ASSUNTOS E MOVIMENTOS (TPU)
## AUTORA: REBECA CARVALHO
## DATA: 20/10/2022

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(tm)

## OBJETIVOS

#'         - Coletar as hierarquias das classes, assuntos e movimentos
#'           presentes na base do DataJud a partir dos códigos.

## FUNÇÕES

source("functions/TPUnamesA.R", 
       encoding = "UTF-8")

source("functions/TPUnamesC.R", 
       encoding = "UTF-8")

source("functions/TPUnamesM.R", 
       encoding = "UTF-8")

source("functions/TPUfamilyA.R", 
       encoding = "UTF-8")

source("functions/TPUfamilyC.R", 
       encoding = "UTF-8")

source("functions/TPUfamilyM.R", 
       encoding = "UTF-8")

source("functions/strdehtml.R", 
       encoding = "UTF-8")

## PREPARANDO O AMBIENTE

setwd("CNJ")

# 1. Data -----------------------------------------------------------------

## Carregando o banco do DatJud

df <- readRDS("data/output/DataJud/dataJud_final_17072022.rds")

## 1.1. Classes Processuais ------------------------------------------------

## Verificando quantos movimentos distintos 
## apareceram nos dados

classes <- unique(parse_number(df$ClasseProcessual))

## Data frame onde os dados serão
## armazenados posteriormente

cod_classes <- data_frame()

## For loop que coleta as informações

for(i in seq_along(classes)){
  
  cat("Lendo", i, "\n")
  
  
  temp <- retorna_familia_TPU_C(classes[i],
                                dt_frame = TRUE)
  
  cod_classes <- rbind.fill(cod_classes,
                            temp)
  
}

## Organizando os dados

cod_classes <- cod_classes %>% 
  rename("Família" = "familia",
         "Código" = "codigo") %>% 
  arrange(Código) 

## 1.2. Assuntos -----------------------------------------------------------

## Verificando quantos movimentos distintos 
## apareceram nos dados

assuntos <- unique(c(unlist(str_split(parse_number(df$Assunto),
                                      " / "))))

assuntos <- unique(cod_assuntos$Código)


## Data frame onde os dados serão
## armazenados posteriormente

cod_assuntos <- data_frame()

## For loop que coleta as informações

for(i in seq_along(assuntos)){
  
  cat("Lendo", i, "\n")
  
  
  temp <- retorna_familia_TPU_A(assuntos[i],
                                dt_frame = TRUE)
  
  cod_assuntos <- rbind.fill(cod_assuntos,
                             temp)
  
}

## Organizando os dados

cod_assuntos <- cod_assuntos %>% 
  rename("Família" = "familia",
         "Código" = "codigo") %>% 
  arrange(Código)

## 1.3. Movimentos ---------------------------------------------------------

## Verificando quantos movimentos distintos 
## apareceram nos dados

movimentos <- unique(c(unlist(str_split(df$CodigoNacionalMovimento,
                                        " / "))))

movimentos <- unique(cod_movimentos$Código)

## Data frame onde os dados serão
## armazenados posteriormente

cod_movimentos <- data_frame()

## For loop que coleta as informações

for(i in seq_along(movimentos)){
  
  cat("Lendo", i, "\n")
  
  
  temp <- retorna_familia_codigo_TPU(movimentos[i],
                                     dt_frame = TRUE)
  
  cod_movimentos <- rbind.fill(cod_movimentos,
                               temp)
  
}

## Organizando os dados

cod_movimentos <- cod_movimentos %>% 
  rename("Família" = "familia",
         "Código" = "codigo") %>% 
  arrange(Código)

# 2. Limpeza --------------------------------------------------------------

## 2.1. Classes Processuais ------------------------------------------------

## Separando os códigos em diferentes colunas

cod_classes <- cod_classes %>% 
  separate(Família,
           into = paste0("codigo_0", 1:5),
           sep = ",") %>% 
  rename("codigo_06" = "Código") %>% 
  mutate(codigo_06 = as.character(codigo_06))

## 2.2. Assuntos -----------------------------------------------------------

## Separando os códigos em diferentes colunas

cod_assuntos <- cod_assuntos %>% 
  separate(Família,
           into = paste0("codigo_0", 1:5),
           sep = ",") %>% 
  rename("codigo_06" = "Código") %>% 
  mutate(codigo_06 = as.character(codigo_06))


## 2.3. Movimentos ---------------------------------------------------------

## Separando os códigos em diferentes colunas

cod_movimentos <- cod_movimentos %>% 
  separate(Família,
           into = paste0("codigo_0", 1:4),
           sep = ",") %>% 
  rename("codigo_05" = "Código") %>% 
  mutate(codigo_05 = as.character(codigo_05)) %>% 
  select(codigo_01:codigo_04,
         codigo_05)

# 3. Join -----------------------------------------------------------------

## 3.1. Classes Processuais ------------------------------------------------

## Preparando o banco

cod_classes <- cod_classes %>% 
  filter(codigo_06 != 0) %>% 
  mutate(descricao_01 = NA,
         glossario_01 = NA,
         descricao_02 = NA,
         glossario_02 = NA,
         descricao_03 = NA,
         glossario_03 = NA,
         descricao_04 = NA,
         glossario_04 = NA,
         descricao_05 = NA,
         glossario_05 = NA,
         descricao_06 = NA,
         glossario_06 = NA) 

## Lista de códigos que serão buscados

classes <- unique(c(cod_classes$codigo_06,
                    cod_classes$codigo_05,
                    cod_classes$codigo_04,
                    cod_classes$codigo_03,
                    cod_classes$codigo_02,
                    cod_classes$codigo_01))

## For loop que coleta as informações

for(i in seq_along(classes)){
  
  cat("Lendo", i, "\n")
  
  
  temp <- retorna_nome_codigo_TPU_C(classes[i],
                                    dt_frame = TRUE)
  
  if(ncol(temp) < 3){
    
    temp$glossario <- NA
  
    }
  
  if(nrow(temp) == 1){
  
  cod_classes <- cod_classes %>% 
    mutate(descricao_01 = ifelse(codigo_01 == classes[i],
                                 temp$nome,
                                 descricao_01),
           glossario_01 = ifelse(codigo_01 == classes[i],
                                 temp$glossario,
                                 glossario_01),
           descricao_02 = ifelse(codigo_02 == classes[i],
                                 temp$nome,
                                 descricao_02),
           glossario_02 = ifelse(codigo_02 == classes[i],
                                 temp$glossario,
                                 glossario_02),
           descricao_03 = ifelse(codigo_03 == classes[i],
                                 temp$nome,
                                 descricao_03),
           glossario_03 = ifelse(codigo_03 == classes[i],
                                 temp$glossario,
                                 glossario_03),
           descricao_04 = ifelse(codigo_04 == classes[i],
                                 temp$nome,
                                 descricao_04),
           glossario_04 = ifelse(codigo_04 == classes[i],
                                 temp$glossario,
                                 glossario_04),
           descricao_05 = ifelse(codigo_05 == classes[i],
                                 temp$nome,
                                 descricao_05),
           glossario_05 = ifelse(codigo_05 == classes[i],
                                 temp$glossario,
                                 glossario_05),
           descricao_06 = ifelse(codigo_06 == classes[i],
                                 temp$nome,
                                 descricao_06),
           glossario_06 = ifelse(codigo_06 == classes[i],
                                 temp$glossario,
                                 glossario_06)) %>% 
    select(codigo_01,
           descricao_01:glossario_01,
           codigo_02,
           descricao_02:glossario_02,
           codigo_03,
           descricao_03:glossario_03,
           codigo_04,
           descricao_04:glossario_04,
           codigo_05,
           descricao_05:glossario_05,
           codigo_06,
           descricao_06:glossario_06)
  }
  
}

## Padronizando os dados

cod_classes <- cod_classes %>% 
  mutate(across(everything(), as.character),
         across(starts_with("glossario"),
                ~ str_remove_all(.,
                                 "<[^>]+>")),
         across(starts_with("glossario"),
                stripWhitespace),
         across(starts_with("glossario"), strdehtml)) %>% 
  filter(!is.na(descricao_06)) %>% 
  mutate(across(starts_with("descricao"),
                str_to_sentence),
         arvore_classe = paste0(codigo_01,
                                 " - ",
                                 descricao_01,
                                 " | ",
                                 codigo_02,
                                 " - ",
                                 descricao_02,
                                 " | ",
                                 codigo_03,
                                 " - ",
                                 descricao_03,
                                 " | ",
                                 codigo_04,
                                 " - ",
                                 descricao_04,
                                 " | ",
                                 codigo_05,
                                 " - ",
                                 descricao_05,
                                 " | ",
                                 codigo_06,
                                 " - ",
                                 descricao_06),
         arvore_classe = gsub("NA - NA |",
                               "",
                               arvore_classe),
         arvore_classe = gsub("| | | | | ",
                               " | ",
                               arvore_classe,
                               fixed = TRUE),
         arvore_classe = gsub("| | | | ",
                               " | ",
                               arvore_classe,
                               fixed = TRUE),
         arvore_classe = gsub("| | | ",
                               " | ",
                               arvore_classe,
                               fixed = TRUE),
         arvore_classe = gsub("| | ",
                               " | ",
                               arvore_classe,
                               fixed = TRUE),
         arvore_classe = gsub(" | 11099 - ",
                               "11099 - ",
                               arvore_classe,
                               fixed = TRUE),
         arvore_classe = gsub(" | 1198 - ",
                               "1198 - ",
                               arvore_classe,
                               fixed = TRUE),
         arvore_classe = gsub(" | 2 - ",
                               "2 - ",
                               arvore_classe,
                               fixed = TRUE),
         arvore_classe = gsub(" | 268 - ",
                               "268 - ",
                               arvore_classe,
                               fixed = TRUE),
         arvore_classe = gsub(" | 385 - ",
                               "385 - ",
                               arvore_classe,
                               fixed = TRUE),
         across(everything(),
                ~ str_replace_all(.,
                                  "NA",
                                  "")),
         across(everything(),
                stripWhitespace),
         across(everything(),
                str_squish)) %>% 
  mutate(glossario_06 = str_sub(glossario_06,
                                1,
                                32767)) %>% 
  select(arvore_classe,
         codigo_01:glossario_06)

## 3.2. Assuntos -----------------------------------------------------------

## Preparando o banco

cod_assuntos <- cod_assuntos %>% 
  filter(codigo_06 != 0) %>% 
  mutate(descricao_01 = NA,
         glossario_01 = NA,
         descricao_02 = NA,
         glossario_02 = NA,
         descricao_03 = NA,
         glossario_03 = NA,
         descricao_04 = NA,
         glossario_04 = NA,
         descricao_05 = NA,
         glossario_05 = NA,
         descricao_06 = NA,
         glossario_06 = NA) 

## Lista de códigos que serão buscados

assuntos <- unique(c(cod_assuntos$codigo_06,
                     cod_assuntos$codigo_05,
                     cod_assuntos$codigo_04,
                     cod_assuntos$codigo_03,
                     cod_assuntos$codigo_02,
                     cod_assuntos$codigo_01))

## For loop que coleta as informações

for(i in seq_along(assuntos)){
  
  cat("Lendo", i, "\n")
  
  
  temp <- retorna_nome_codigo_TPU_A(assuntos[i],
                                    dt_frame = TRUE)
  
  if(ncol(temp) < 3){
    
    temp$glossario <- NA
  
    }
  
  if(nrow(temp) == 1){
    
    cod_assuntos <- cod_assuntos %>% 
      mutate(descricao_01 = ifelse(codigo_01 == assuntos[i],
                                   temp$nome,
                                   descricao_01),
             glossario_01 = ifelse(codigo_01 == assuntos[i],
                                   temp$glossario,
                                   glossario_01),
             descricao_02 = ifelse(codigo_02 == assuntos[i],
                                   temp$nome,
                                   descricao_02),
             glossario_02 = ifelse(codigo_02 == assuntos[i],
                                   temp$glossario,
                                   glossario_02),
             descricao_03 = ifelse(codigo_03 == assuntos[i],
                                   temp$nome,
                                   descricao_03),
             glossario_03 = ifelse(codigo_03 == assuntos[i],
                                   temp$glossario,
                                   glossario_03),
             descricao_04 = ifelse(codigo_04 == assuntos[i],
                                   temp$nome,
                                   descricao_04),
             glossario_04 = ifelse(codigo_04 == assuntos[i],
                                   temp$glossario,
                                   glossario_04),
             descricao_05 = ifelse(codigo_05 == assuntos[i],
                                   temp$nome,
                                   descricao_05),
             glossario_05 = ifelse(codigo_05 == assuntos[i],
                                   temp$glossario,
                                   glossario_05),
             descricao_06 = ifelse(codigo_06 == assuntos[i],
                                   temp$nome,
                                   descricao_06),
             glossario_06 = ifelse(codigo_06 == assuntos[i],
                                   temp$glossario,
                                   glossario_06)) %>% 
      select(codigo_01,
             descricao_01:glossario_01,
             codigo_02,
             descricao_02:glossario_02,
             codigo_03,
             descricao_03:glossario_03,
             codigo_04,
             descricao_04:glossario_04,
             codigo_05,
             descricao_05:glossario_05,
             codigo_06,
             descricao_06:glossario_06)
  }
  
}

## Padronizando os dados

cod_assuntos <- cod_assuntos %>% 
  mutate(across(everything(), as.character),
         across(starts_with("glossario"),
                ~ str_remove_all(.,
                                 "<[^>]+>")),
         across(starts_with("glossario"),
                stripWhitespace),
         across(starts_with("glossario"), strdehtml)) %>% 
  mutate(across(starts_with("descricao"),
                str_to_sentence),
         arvore_assunto = paste0(codigo_01,
                                   " - ",
                                   descricao_01,
                                   " | ",
                                   codigo_02,
                                   " - ",
                                   descricao_02,
                                   " | ",
                                   codigo_03,
                                   " - ",
                                   descricao_03,
                                   " | ",
                                   codigo_04,
                                   " - ",
                                   descricao_04,
                                   " | ",
                                   codigo_05,
                                   " - ",
                                   descricao_05,
                                 " | ",
                                 codigo_06,
                                 " - ",
                                 descricao_06),
         arvore_assunto = gsub("NA - NA |",
                                 "",
                                 arvore_assunto),
         arvore_assunto = gsub("| | | | | ",
                               " | ",
                               arvore_assunto,
                               fixed = TRUE),
         arvore_assunto = gsub("| | | | ",
                                 " | ",
                                 arvore_assunto,
                                 fixed = TRUE),
         arvore_assunto = gsub("| | | ",
                                 " | ",
                                 arvore_assunto,
                                 fixed = TRUE),
         arvore_assunto = gsub("| | ",
                                 " | ",
                                 arvore_assunto,
                                 fixed = TRUE),
         arvore_assunto = gsub(" | 10110 - ",
                                 "10110 - ",
                                 arvore_assunto,
                                 fixed = TRUE),
         arvore_assunto = gsub(" | 1146 - ",
                                 "1146 - ",
                                 arvore_assunto,
                                 fixed = TRUE),
         arvore_assunto = gsub(" | 1156 - ",
                               "1156 - ",
                               arvore_assunto,
                               fixed = TRUE),
         arvore_assunto = gsub(" | 1209 - ",
                               "1209 - ",
                               arvore_assunto,
                               fixed = TRUE),
         arvore_assunto = gsub(" | 12467 - ",
                               "12467 - ",
                               arvore_assunto,
                               fixed = TRUE),
         arvore_assunto = gsub(" | 287 - ",
                               "287 - ",
                               arvore_assunto,
                               fixed = TRUE),
         arvore_assunto = gsub(" | 6191 - ",
                               "6191 - ",
                               arvore_assunto,
                               fixed = TRUE),
         arvore_assunto = gsub(" | 7724 - ",
                               "7724 - ",
                               arvore_assunto,
                               fixed = TRUE),
         arvore_assunto = gsub(" | 8826 - ",
                               "8826 - ",
                               arvore_assunto,
                               fixed = TRUE),
         arvore_assunto = gsub(" | 899 - ",
                               "899 - ",
                               arvore_assunto,
                               fixed = TRUE),
         arvore_assunto = gsub(" | 9633 - ",
                               "9633 - ",
                               arvore_assunto,
                               fixed = TRUE),
         arvore_assunto = gsub(" | 9985 - ",
                               "9985 - ",
                               arvore_assunto,
                               fixed = TRUE),
         arvore_assunto = gsub(" | 14 - ",
                               "14 - ",
                               arvore_assunto,
                               fixed = TRUE),
         across(everything(),
                ~ str_replace_all(.,
                                  "NA",
                                  "")),
         across(everything(),
                stripWhitespace),
         across(everything(),
                str_squish)) %>% 
  select(arvore_assunto,
         codigo_01:glossario_06)

## 3.3. Movimentos ---------------------------------------------------------

## Preparando o banco

cod_movimentos <- cod_movimentos %>% 
  mutate(descricao_01 = NA,
         glossario_01 = NA,
         descricao_02 = NA,
         glossario_02 = NA,
         descricao_03 = NA,
         glossario_03 = NA,
         descricao_04 = NA,
         glossario_04 = NA,
         descricao_05 = NA,
         glossario_05 = NA) 

## Lista de códigos que serão buscados

movimentos <- unique(c(cod_movimentos$codigo_05,
                       cod_movimentos$codigo_04,
                       cod_movimentos$codigo_03,
                       cod_movimentos$codigo_02,
                       cod_movimentos$codigo_01))

## For loop que coleta as informações

for(i in seq_along(movimentos)){
  
  cat("Lendo", i, "\n")
  
  
  temp <- retorna_nome_codigo_TPU_M(movimentos[i],
                                    dt_frame = TRUE)
  
  if(ncol(temp) < 3){
    
    temp$glossario <- NA
  }
  
  if(nrow(temp) == 1){
    
    cod_movimentos <- cod_movimentos %>% 
      mutate(descricao_01 = ifelse(codigo_01 == movimentos[i],
                                   temp$nome,
                                   descricao_01),
             glossario_01 = ifelse(codigo_01 == movimentos[i],
                                   temp$glossario,
                                   glossario_01),
             descricao_02 = ifelse(codigo_02 == movimentos[i],
                                   temp$nome,
                                   descricao_02),
             glossario_02 = ifelse(codigo_02 == movimentos[i],
                                   temp$glossario,
                                   glossario_02),
             descricao_03 = ifelse(codigo_03 == movimentos[i],
                                   temp$nome,
                                   descricao_03),
             glossario_03 = ifelse(codigo_03 == movimentos[i],
                                   temp$glossario,
                                   glossario_03),
             descricao_04 = ifelse(codigo_04 == movimentos[i],
                                   temp$nome,
                                   descricao_04),
             glossario_04 = ifelse(codigo_04 == movimentos[i],
                                   temp$glossario,
                                   glossario_04),
             descricao_05 = ifelse(codigo_05 == movimentos[i],
                                   temp$nome,
                                   descricao_05),
             glossario_05 = ifelse(codigo_05 == movimentos[i],
                                   temp$glossario,
                                   glossario_05)) %>% 
      select(codigo_01,
             descricao_01:glossario_01,
             codigo_02,
             descricao_02:glossario_02,
             codigo_03,
             descricao_03:glossario_03,
             codigo_04,
             descricao_04:glossario_04,
             codigo_05,
             descricao_05:glossario_05)
  
    }
  
}

## Padronizando os dados

cod_movimentos <- cod_movimentos %>% 
  mutate(across(everything(), as.character),
         across(starts_with("glossario"),
                ~ str_remove_all(.,
                                 "<[^>]+>")),
         across(starts_with("glossario"),
                stripWhitespace),
         across(starts_with("glossario"), strdehtml)) %>% 
  mutate(across(starts_with("descricao"),
                str_to_sentence),
         arvore_movimento = paste0(codigo_01,
                                   " - ",
                                   descricao_01,
                                   " | ",
                                   codigo_02,
                                   " - ",
                                   descricao_02,
                                   " | ",
                                   codigo_03,
                                   " - ",
                                   descricao_03,
                                   " | ",
                                   codigo_04,
                                   " - ",
                                   descricao_04,
                                   " | ",
                                   codigo_05,
                                   " - ",
                                   descricao_05),
         arvore_movimento = gsub("NA - NA |",
                                 "",
                                 arvore_movimento),
         arvore_movimento = gsub("| | | | ",
                                 " | ",
                                 arvore_movimento,
                                 fixed = TRUE),
         arvore_movimento = gsub("| | | ",
                                 " | ",
                                 arvore_movimento,
                                 fixed = TRUE),
         arvore_movimento = gsub("| | ",
                                 " | ",
                                 arvore_movimento,
                                 fixed = TRUE),
         arvore_movimento = gsub(" | 1 - ",
                                 "1 - ",
                                 arvore_movimento,
                                 fixed = TRUE),
         arvore_movimento = gsub(" | 14 - ",
                                 "14 - ",
                                 arvore_movimento,
                                 fixed = TRUE),
         across(everything(),
                ~ str_replace_all(.,
                                "NA",
                                ""))) %>% 
  select(arvore_movimento,
         codigo_01:glossario_05)

# 4. Salva ----------------------------------------------------------------

saveRDS(cod_classes,
        "data/output/DataJud/familias_cod_classes.rds")

writexl::write_xlsx(cod_classes,
                    "data/output/DataJud/familias_cod_classes.xlsx")

saveRDS(cod_assuntos,
        "data/output/DataJud/familias_cod_assuntos.rds")

writexl::write_xlsx(cod_assuntos,
                    "data/output/DataJud/familias_cod_assuntos.xlsx")

saveRDS(cod_movimentos,
        "data/output/DataJud/familias_cod_movimentos.rds")

writexl::write_xlsx(cod_movimentos,
                    "data/output/DataJud/familias_cod_movimentos.xlsx")
