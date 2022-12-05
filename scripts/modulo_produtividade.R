
## TÍTULO: ESTRUTURAÇÃO DOS DADOS | MÓDULO DE PRODUTIVIDADE
## DATA: 05/10/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(data.table)
library(abjutils)
library(tm)

## PREPARANDO O AMBIENTE

setwd("CNJ")

# 1. Dados ----------------------------------------------------------------

## Carregando os dados de referência

produtservent <- read_delim("data/input/Módulo de Produtividade Mensal/produtividade_serventias_unidade jud_14112022.csv", 
                            delim = ";", 
                            escape_double = FALSE, 
                            trim_ws = TRUE)

produtmagistr <- read_delim("data/input/Módulo de Produtividade Mensal/produtividade_magistrados_unidade jud_14112022.csv", 
                            delim = ";", 
                            escape_double = FALSE, 
                            trim_ws = TRUE)

codigo <- readxl::read_xlsx("data/input/DataJud/seqorgaos_amazônia legal.xlsx")

municipios <-  readxl::read_xls("data/input/SireneJud/municípios_amazônia legal_2020.xls")

ambiental <- readxl::read_xlsx("data/input/Módulo de Produtividade Mensal/orgãos_competência ambiental_03122022.xlsx")

## 1.1. Limpeza ------------------------------------------------------------

### 1.1.1. Municipios -------------------------------------------------------

## Adequando o formato dos dados

municipios <- municipios %>% 
  mutate(NM_MUN = str_to_upper(rm_accent(NM_MUN)),
         NM_MUN = ifelse(CD_MUN == 1100049,
                         "CACOAL",
                         NM_MUN))

### 1.1.2. Codigos dos Orgaos Julgadores ------------------------------------

## Filtrando somente os orgaos atuantes na
## Amazônia Legal

codigo <- codigo %>%
  mutate(Municipio = rm_accent(Municipio),
         OrgaoJulgador = rm_accent(OrgaoJulgador)) %>% 
  filter(UF %in% municipios$SIGLA &
         Municipio %in% municipios$NM_MUN)

### 1.1.3. Serventias -------------------------------------------------------

## Reorganizando os dados

produtservent <- produtservent %>%
  select(-Tipo...2,
         -Classificação,
         -Ano) %>% 
  pivot_longer(cols = `2015`:`2021`, 
               names_to = "Ano",
               values_to = "Total") %>% 
  rename("SiglaTribunal" = "Sigla",
         "CodigoUnidadeJud" = "Código",
         "UnidadeJudiciaria" = "Unidade Judiciária",
         "Municipio" = "Município",
         "Tipo" = "Tipo...8",
         "Descricao" = "Descrição") %>% 
  mutate(Total = gsub("\\.",
                      "",
                      Total),
         Total = as.numeric(gsub("\\-",
                                 "",
                                 Total)),
         CodigoUnidadeJud = str_pad(CodigoUnidadeJud,
                                    width = 5,
                                    side = "left",
                                    pad = "0")) %>% 
  filter(Tipo %in% c("SENTENÇAS",
                     "SENTENÇAS HOMOLOGATÓRIAS",
                     "OUTRAS SENTENÇAS") &
         Descricao != "PENDENTES") 

## Criando um banco com somente a produtividades
## das serventias da Amazônia Legal

produtservent_ambt <- produtservent %>%
  filter(CodigoUnidadeJud %in% unique(codigo$CodigoOrgao))

### 1.1.4. Magistrados ------------------------------------------------------

## Reorganizando os dados

produtmagistr <- produtmagistr %>% 
  select(-`...8`) %>% 
  pivot_longer(cols = `2015`:`2021`, 
               names_to = "Ano",
               values_to = "Total") %>% 
  rename("SiglaTribunal" = "Tribunal",
         "Municipio" = "Município",
         "UnidadeJudiciaria" = "Unidade Judiciária") %>% 
  mutate(Total = gsub("\\.",
                      "",
                      Total),
         Total = as.numeric(gsub("\\-",
                                 "",
                                 Total)),
         UnidadeJudiciaria = str_to_upper(rm_accent(UnidadeJudiciaria)),
         UnidadeJudiciaria = stripWhitespace(trimws(str_squish(UnidadeJudiciaria))),
         Municipio = str_to_upper(rm_accent(Municipio)),
         Municipio = stripWhitespace(trimws(str_squish(Municipio)))) %>% 
  select(Ano,
         SiglaTribunal,
         UF,
         Municipio,
         UnidadeJudiciaria,
         Magistrado,
         Tipo,
         Detalhamento,
         Total)

## Criando uma versão somente com os dados das unidades judiciárias
## atuantes na Amazônia Legal

produtmagistr_ambt <- produtmagistr %>% 
  filter(UnidadeJudiciaria %in% unique(codigo$OrgaoJulgador) &
         UF %in% unique(codigo$UF) &
         Municipio %in% unique(codigo$Municipio)) 

# 2. Agrupando ------------------------------------------------------------

## 2.1. Sentenças ----------------------------------------------------------

## Verificandoo o total de sentenças dadas por ano nas
## serventias gerais

sentencas <- produtservent %>%
  group_by(Ano,
           SiglaTribunal,
           CodigoUnidadeJud,
           UnidadeJudiciaria) %>% 
  summarise(TotalSentencas = sum(Total,
                        na.rm = TRUE)) %>% 
  unique()

## Verificandoo o total de sentenças dadas por ano nas
## atuantes na amazônia legal

sentencas_ambt <- produtservent_ambt %>%
  group_by(Ano,
           SiglaTribunal,
           CodigoUnidadeJud,
           UnidadeJudiciaria) %>% 
  summarise(TotalSentencas = sum(Total,
                        na.rm = TRUE)) %>% 
  unique()

## 2.2. Magistrados --------------------------------------------------------

## Verificando quantos juízes diferentes passaram pelas unidades 
## judiciárias gerais

magistrados <- produtmagistr %>%
  select(Ano,
         SiglaTribunal,
         UnidadeJudiciaria,
         Magistrado) %>%
  unique() %>% 
  group_by(Ano,
           SiglaTribunal,
           UnidadeJudiciaria) %>% 
  summarise(TotalJuizes = n()) %>% 
  unique()

## Verificando quantos juízes diferentes passaram pelas unidades 
## judiciárias atuantes na Amazônia Legal

magistrados_ambt <- produtmagistr_ambt %>%
  select(Ano,
         SiglaTribunal,
         UnidadeJudiciaria,
         Magistrado) %>%
  unique() %>% 
  group_by(Ano,
           SiglaTribunal,
           UnidadeJudiciaria) %>% 
  summarise(TotalJuizes = n()) %>% 
  unique()

# 3. Join -----------------------------------------------------------------

## Juntando com os dados gerais e ambientais com os
## dados dos magistrados

magistrados <- left_join(magistrados,
                         sentencas) %>% 
  unique() %>% 
  filter(!is.na(TotalSentencas)) %>% 
  group_by(SiglaTribunal,
           CodigoUnidadeJud,
           UnidadeJudiciaria) %>% 
  summarise(TotalSentencas = mean(TotalSentencas,
                                  na.rm = TRUE),
            TotalJuizes = mean(TotalJuizes,
                               na.rm = TRUE)) 

magistrados_ambt <- left_join(magistrados_ambt,
                              sentencas_ambt) %>% 
  unique() %>% 
  filter(!is.na(TotalSentencas)) %>% 
  group_by(SiglaTribunal,
           CodigoUnidadeJud,
           UnidadeJudiciaria) %>% 
  summarise(TotalSentencas = mean(TotalSentencas,
                                  na.rm = TRUE),
            TotalJuizes = mean(TotalJuizes,
                               na.rm = TRUE)) %>% 
  mutate(Tipo = ifelse(CodigoUnidadeJud %in% ambiental$CodigoOrgao,
                       "Competência Ambiental",
                       "Outras Competências"))

# 4. Salva ----------------------------------------------------------------

## 4.1. Geral --------------------------------------------------------------

saveRDS(produtservent,
        "data/output/Módulo de Produtividade/produtividade_unidade jud gerais_05122022.rds")

saveRDS(produtmagistr,
        "data/output/Módulo de Produtividade/produtividade_magistrados_unidade jud gerais_05122022.rds")

saveRDS(magistrados,
        "data/output/Módulo de Produtividade/relação sentenças x juízes_unidade jud gerais_05122022.rds")

## 4.2. Amazônia Legal -----------------------------------------------------

## Salva os dados gerados

saveRDS(produtservent_ambt,
        "data/output/Módulo de Produtividade/produtividade_unidade jud ambientais_05122022.rds")

saveRDS(produtmagistr_ambt,
        "data/output/Módulo de Produtividade/produtividade_magistrados_unidade jud ambientais_05122022.rds")

saveRDS(magistrados_ambt,
        "data/output/Módulo de Produtividade/relação sentenças x juízes_unidade jud ambientais_05122022.rds")
