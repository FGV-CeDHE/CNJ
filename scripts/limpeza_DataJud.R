
## TÍTULO: ESTRUTURAÇÃO DADOS SIRENEJUD
## DATA: 03/09/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(tidyverse)
library(sf)
library(data.table)
library(abjutils)

## PREPARANDO O AMBIENTE

setwd("CNJ")

## FUNÇÕES

source("functions/encoder.R", 
       encoding = "UTF-8")

# 1. Data -----------------------------------------------------------------

## Carregando os dados

municipios <- readxl::read_xls("data/input/SireneJud/municípios_amazônia legal_2020.xls")

# df <- fread("data/input/DataJud/DataJud_26102022.csv",
#             encoding = "UTF-8")

df <- readRDS("data/input/DataJud/DataJud_filt_allvars_26102022.rds")

classes <- readRDS("data/output/DataJud/familias_cod_classes.rds")

assuntos <- readRDS("data/output/DataJud/familias_cod_assuntos.rds")

geom <- readRDS("data/input/DataJud/geom_unidades judiciárias_v10102022.rds")

natureza_classes <- readxl::read_xlsx("data/input/DataJud/natureza_classes.xlsx")

## Padronizando os dados

municipios <- municipios %>% 
  mutate(NM_MUN = str_to_upper(rm_accent(NM_MUN)),
         NM_MUN = ifelse(CD_MUN == 1100049,
                         "CACOAL",
                         NM_MUN))

# 2. Limpeza --------------------------------------------------------------

## Filtrando somente os estados de interesse

df <- df %>%
  filter(uf %in% municipios$SIGLA) %>% 
  select(-geom)

## Atualizando informações das coordenadas geográficas

df <- left_join(df,
                geom) 

## Estruturando as coordenadas geográficas 
## e ajustando outros detalhes

df <- df %>% 
  extract(geom,
          c('lat', 'lon'),
            '\\((.*) (.*)\\)',
            convert = TRUE) %>%
  rename("lat" = "lon",
         "lon" = "lat") %>% 
  mutate(municipio = rm_accent(municipio),
         ano_v2 = str_sub(numprocess,
                          12,
                          15),
         data_ajuizamento = as.Date(data_ajuizamento,
                                    format = "%d/%m/%Y"),
         dt_inicio_situacao_novo = ifelse(is.na(dt_inicio_situacao_novo),
                                             as.character(ano_v2),
                                             as.character(dt_inicio_situacao_novo)),
         dt_inicio_situacao_novo_v2 = str_sub(dt_inicio_situacao_novo,
                                              1,
                                              4),
         dt_inicio_situacao_julgado = as.Date(dt_inicio_situacao_julgado,
                                              format = "%Y-%m-%d"),
         dt_inicio_situacao_baixado = as.Date(dt_inicio_situacao_baixado,
                                            format = "%Y-%m-%d")) %>%
  filter(municipio %in% municipios$NM_MUN) %>%
  mutate(lat = round(lat,6),
         lon = round(lon,6)) %>%
  mutate(across(everything(), ~ str_remove_all(., '"')),
         noassuntos = gsub('"',
                           "",
                           noassuntos),
         noassuntos = str_replace_all(noassuntos,
                                      "\\[|]",
                                      ""),
         noassuntos = gsub(";",
                           "",
                           noassuntos),
         co_assunto = str_replace_all(co_assunto,
                                      "\\[|]",
                                      ""),
         noassuntos = str_replace_all(noassuntos,
                                      "\\[|]",
                                      ""),
         orgaojulgador = gsub("VARA UNICA",
                              "VARA ÚNICA",
                              orgaojulgador)) %>% 
  select(-nome_sistema_origem,
         -porte_tribunal_nome,
         -co_assunto_array,
         -complex_assunto,
         -ano_v2)

## 2.1. Partes -------------------------------------------------------------

### 2.1.1. Polo passivo -----------------------------------------------------

## Criando um banco temporário com apenas as partes do POLO PASSIVO 

partes_pa <- df %>% 
  select(numprocess,
         grau,
         partes_pa_desc,
         partes_pa_list) %>% 
  unique()

## Reorganizando os dados do POLO PASSIVO

partes_pa <- partes_pa %>% 
  mutate(across(everything(), 
                str_to_upper)) %>% 
  mutate(partes_pa_list = str_replace_all(partes_pa_list,
                                     "\\[|]",
                                     ""),
         partes_pa_list = str_replace_all(partes_pa_list,
                                     '"',
                                     ""),
         partes_pa_list = str_replace_all(partes_pa_list,
                                     "\\}",
                                     ""),
         partes_pa_list = gsub(", NA PESSOA DE SEU REPRESENTANTE LEGAL",
                               "",
                               partes_pa_list,
                               perl = TRUE),
         partes_pa_list = gsub("NA PESSOA DE SEU REPRESENTANTE LEGAL",
                               "",
                               partes_pa_list,
                               perl = TRUE),
         partes_pa_list = gsub(", NOME:",
                               "; NOME:",
                               partes_pa_list,
                               perl = TRUE),
         partes_pa_list = gsub(", SEXO:",
                               "; SEXO:",
                               partes_pa_list,
                               perl = TRUE),
         partes_pa_list = gsub(", TIPOPESSOA:",
                               "; TIPOPESSOA:",
                               partes_pa_list,
                               perl = TRUE),
         partes_pa_list = gsub(", NUMERODOCUMENTOPRINCIPAL:",
                               "; NUMERODOCUMENTOPRINCIPAL:",
                               partes_pa_list,
                               perl = TRUE),
         partes_pa_list = gsub(", NACIONALIDADE:",
                               "; NACIONALIDADE:",
                               partes_pa_list,
                               perl = TRUE),
         partes_pa_list = gsub(", ASSISTENCIAJUDICIARIA:",
                               "; ASSISTENCIAJUDICIARIA:",
                               partes_pa_list,
                               perl = TRUE),
         partes_pa_list = gsub(", INTIMACAOPENDENTE:",
                               "; INTIMACAOPENDENTE:",
                               partes_pa_list,
                               perl = TRUE)) %>% 
  separate_rows(partes_pa_list,
                sep = "{") %>% 
  separate(partes_pa_list,
           sep = "; ",
           into = c("polo_pa",
                    "nome_pa",
                    "sexo_pa",
                    "tipoPessoa_pa",
                    "numeroDocumentoPrincipal_pa",
                    "nacionalidade_pa",
                    "assistenciaJudiciaria_pa",
                    "intimacaoPendente_pa")) %>% 
  mutate(across(everything(), 
                gsub, 
                pattern = ".*: ", 
                replacement = ""),
         partes_pa_desc = "POLO PASSIVO") %>% 
  filter((!is.na(nome_pa) &
          nome_pa != "") &
         (!is.na(nome_pa) &
          nome_pa != "")) %>% 
  unique() %>% 
  select(-assistenciaJudiciaria_pa,
         -intimacaoPendente_pa) 

## Agregando os dados por processo e grau

partes_pa_agg <- aggregate(nome_pa ~ numprocess + grau + tipoPessoa_pa, 
                           data = partes_pa,
                           paste,
                           collapse = "\n")

## Preparando os dados para o join

partes_pa <- partes_pa %>% 
  select(-nome_pa) %>% 
  unique()

## Juntando os dados tratados

partes_pa <- left_join(partes_pa,
                       partes_pa_agg) %>% 
  select(numprocess:polo_pa,
         nome_pa,
         sexo_pa:numeroDocumentoPrincipal_pa) %>% 
  mutate(numeroDocumentoPrincipal_pa = ifelse(tipoPessoa_pa == "JURIDICA",
                                              str_pad(numeroDocumentoPrincipal_pa,
                                                      width = 14,
                                                      side = "left",
                                                      pad = "0"),
                                              numeroDocumentoPrincipal_pa))

### 2.1.2. Polo ativo -------------------------------------------------------

## Criando um banco temporário com apenas as partes do POLO ATIVO 

partes_at <- df %>% 
  select(numprocess,
         grau,
         partes_at_desc,
         partes_at_list) %>% 
  unique()

## Reorganizando os dados do POLO ATIVO

partes_at <- partes_at %>% 
  mutate(across(everything(), 
                str_to_upper)) %>% 
  mutate(partes_at_list = str_replace_all(partes_at_list,
                                          "\\[|]",
                                          ""),
         partes_at_list = str_replace_all(partes_at_list,
                                          '"',
                                          ""),
         partes_at_list = str_replace_all(partes_at_list,
                                          "\\}",
                                          ""),
         partes_at_list = gsub(", NA PESSOA DE SEU REPRESENTANTE LEGAL",
                               "",
                               partes_at_list,
                               perl = TRUE),
         partes_at_list = gsub("NA PESSOA DE SEU REPRESENTANTE LEGAL",
                               "",
                               partes_at_list,
                               perl = TRUE),
         partes_at_list = gsub(", NOME:",
                               "; NOME:",
                               partes_at_list,
                               perl = TRUE),
         partes_at_list = gsub(", SEXO:",
                               "; SEXO:",
                               partes_at_list,
                               perl = TRUE),
         partes_at_list = gsub(", TIPOPESSOA:",
                               "; TIPOPESSOA:",
                               partes_at_list,
                               perl = TRUE),
         partes_at_list = gsub(", NUMERODOCUMENTOPRINCIPAL:",
                               "; NUMERODOCUMENTOPRINCIPAL:",
                               partes_at_list,
                               perl = TRUE),
         partes_at_list = gsub(", NACIONALIDADE:",
                               "; NACIONALIDADE:",
                               partes_at_list,
                               perl = TRUE),
         partes_at_list = gsub(", ASSISTENCIAJUDICIARIA:",
                               "; ASSISTENCIAJUDICIARIA:",
                               partes_at_list,
                               perl = TRUE),
         partes_at_list = gsub(", INTIMACAOPENDENTE:",
                               "; INTIMACAOPENDENTE:",
                               partes_at_list,
                               perl = TRUE)) %>% 
  separate_rows(partes_at_list,
                sep = "{") %>% 
  separate(partes_at_list,
           sep = "; ",
           into = c("polo_at",
                    "nome_at",
                    "sexo_at",
                    "tipoPessoa_at",
                    "numeroDocumentoPrincipal_at",
                    "nacionalidade_at",
                    "assistenciaJudiciaria_at",
                    "intimacaoPendente_at")) %>% 
  mutate(across(everything(), 
                gsub, 
                pattern = ".*: ", 
                replacement = ""),
         partes_at_desc = "POLO ATIVO") %>% 
  filter((!is.na(nome_at) &
            nome_at != "") &
           (!is.na(nome_at) &
              nome_at != "")) %>% 
  unique() %>% 
  select(-assistenciaJudiciaria_at,
         -intimacaoPendente_at) 

## Agregando os dados por processo e grau

partes_at_agg <- aggregate(nome_at ~ numprocess + grau + tipoPessoa_at, 
                           data = partes_at,
                           paste,
                           collapse = "\n")

## Preparando os dados para o join

partes_at <- partes_at %>% 
  select(-nome_at) %>% 
  unique()

## Juntando os dados tratados

partes_at <- left_join(partes_at,
                       partes_at_agg) %>% 
  select(numprocess:polo_at,
         nome_at,
         sexo_at:numeroDocumentoPrincipal_at) %>% 
  mutate(numeroDocumentoPrincipal_at = ifelse(tipoPessoa_at == "JURIDICA",
                                              str_pad(numeroDocumentoPrincipal_at,
                                                      width = 14,
                                                      side = "left",
                                                      pad = "0"),
                                              numeroDocumentoPrincipal_at))

### 2.1.3. Terceiros --------------------------------------------------------

## Criando um banco temporário com apenas as partes do POLO PASSIVO 

partes_tc <- df %>% 
  select(numprocess,
         grau,
         partes_tc_desc,
         partes_tc_list) %>% 
  unique()

## Reorganizando os dados do POLO PASSIVO

partes_tc <- partes_tc %>% 
  mutate(across(everything(), 
                str_to_upper)) %>% 
  mutate(partes_tc_list = str_replace_all(partes_tc_list,
                                          "\\[|]",
                                          ""),
         partes_tc_list = str_replace_all(partes_tc_list,
                                          '"',
                                          ""),
         partes_tc_list = str_replace_all(partes_tc_list,
                                          "\\}",
                                          ""),
         partes_tc_list = gsub(", NA PESSOA DE SEU REPRESENTANTE LEGAL",
                               "",
                               partes_tc_list,
                               perl = TRUE),
         partes_tc_list = gsub("NA PESSOA DE SEU REPRESENTANTE LEGAL",
                               "",
                               partes_tc_list,
                               perl = TRUE),
         partes_tc_list = gsub(", NOME:",
                               "; NOME:",
                               partes_tc_list,
                               perl = TRUE),
         partes_tc_list = gsub(", SEXO:",
                               "; SEXO:",
                               partes_tc_list,
                               perl = TRUE),
         partes_tc_list = gsub(", TIPOPESSOA:",
                               "; TIPOPESSOA:",
                               partes_tc_list,
                               perl = TRUE),
         partes_tc_list = gsub(", NUMERODOCUMENTOPRINCIPAL:",
                               "; NUMERODOCUMENTOPRINCIPAL:",
                               partes_tc_list,
                               perl = TRUE),
         partes_tc_list = gsub(", NACIONALIDADE:",
                               "; NACIONALIDADE:",
                               partes_tc_list,
                               perl = TRUE),
         partes_tc_list = gsub(", ASSISTENCIAJUDICIARIA:",
                               "; ASSISTENCIAJUDICIARIA:",
                               partes_tc_list,
                               perl = TRUE),
         partes_tc_list = gsub(", INTIMACAOPENDENTE:",
                               "; INTIMACAOPENDENTE:",
                               partes_tc_list,
                               perl = TRUE)) %>% 
  separate_rows(partes_tc_list,
                sep = "{") %>% 
  separate(partes_tc_list,
           sep = "; ",
           into = c("polo_tc",
                    "nome_tc",
                    "sexo_tc",
                    "tipoPessoa_tc",
                    "numeroDocumentoPrincipal_tc",
                    "nacionalidade_tc",
                    "assistenciaJudiciaria_tc",
                    "intimacaoPendente_tc")) %>% 
  mutate(across(everything(), 
                gsub, 
                pattern = ".*: ", 
                replacement = ""),
         partes_tc_desc = "TERCEIRO") %>% 
  filter((!is.na(nome_tc) &
            nome_tc != "") &
           (!is.na(nome_tc) &
              nome_tc != "")) %>% 
  unique() %>% 
  select(-assistenciaJudiciaria_tc,
         -intimacaoPendente_tc) 

## Agregando os dados por processo e grau

partes_tc_agg <- aggregate(nome_tc ~ numprocess + grau + tipoPessoa_tc, 
                           data = partes_tc,
                           paste,
                           collapse = "\n")

## Preparando os dados para o join

partes_tc <- partes_tc %>% 
  select(-nome_tc,
         -numeroDocumentoPrincipal_tc) %>% 
  unique()

## Juntando os dados tratados

partes_tc <- left_join(partes_tc,
                       partes_tc_agg) %>% 
  select(numprocess:polo_tc,
         nome_tc,
         sexo_tc:tipoPessoa_tc)

### 2.1.4. Fiscal da lei diverso --------------------------------------------

## Criando um banco temporário com apenas as partes do POLO PASSIVO 

partes_fl <- df %>% 
  select(numprocess,
         grau,
         partes_fl_desc,
         partes_fl_list) %>% 
  unique()

## Reorganizando os dados do POLO PASSIVO

partes_fl <- partes_fl %>% 
  mutate(across(everything(), 
                str_to_upper)) %>% 
  mutate(partes_fl_list = str_replace_all(partes_fl_list,
                                          "\\[|]",
                                          ""),
         partes_fl_list = str_replace_all(partes_fl_list,
                                          '"',
                                          ""),
         partes_fl_list = str_replace_all(partes_fl_list,
                                          "\\}",
                                          ""),
         partes_fl_list = gsub(", NA PESSOA DE SEU REPRESENTANTE LEGAL",
                               "",
                               partes_fl_list,
                               perl = TRUE),
         partes_fl_list = gsub("NA PESSOA DE SEU REPRESENTANTE LEGAL",
                               "",
                               partes_fl_list,
                               perl = TRUE),
         partes_fl_list = gsub(", NOME:",
                               "; NOME:",
                               partes_fl_list,
                               perl = TRUE),
         partes_fl_list = gsub(", SEXO:",
                               "; SEXO:",
                               partes_fl_list,
                               perl = TRUE),
         partes_fl_list = gsub(", TIPOPESSOA:",
                               "; TIPOPESSOA:",
                               partes_fl_list,
                               perl = TRUE),
         partes_fl_list = gsub(", NUMERODOCUMENTOPRINCIPAL:",
                               "; NUMERODOCUMENTOPRINCIPAL:",
                               partes_fl_list,
                               perl = TRUE),
         partes_fl_list = gsub(", NACIONALIDADE:",
                               "; NACIONALIDADE:",
                               partes_fl_list,
                               perl = TRUE),
         partes_fl_list = gsub(", ASSISTENCIAJUDICIARIA:",
                               "; ASSISTENCIAJUDICIARIA:",
                               partes_fl_list,
                               perl = TRUE),
         partes_fl_list = gsub(", INTIMACAOPENDENTE:",
                               "; INTIMACAOPENDENTE:",
                               partes_fl_list,
                               perl = TRUE)) %>% 
  separate_rows(partes_fl_list,
                sep = "{") %>% 
  separate(partes_fl_list,
           sep = "; ",
           into = c("polo_fl",
                    "nome_fl",
                    "sexo_fl",
                    "tipoPessoa_fl",
                    "numeroDocumentoPrincipal_fl",
                    "nacionalidade_fl",
                    "assistenciaJudiciaria_fl",
                    "intimacaoPendente_fl")) %>% 
  mutate(across(everything(), 
                gsub, 
                pattern = ".*: ", 
                replacement = ""),
         partes_fl_desc = "FISCAL DA LEI DIVERSO") %>% 
  filter((!is.na(nome_fl) &
            nome_fl != "") &
           (!is.na(nome_fl) &
              nome_fl != "")) %>% 
  unique() %>% 
  select(-assistenciaJudiciaria_fl,
         -intimacaoPendente_fl) 

## Agregando os dados por processo e grau

partes_fl_agg <- aggregate(nome_fl ~ numprocess + grau + tipoPessoa_fl, 
                           data = partes_fl,
                           paste,
                           collapse = "\n")

## Preparando os dados para o join

partes_fl <- partes_fl %>% 
  select(-nome_fl,
         -numeroDocumentoPrincipal_fl) %>% 
  unique()

## Juntando os dados tratados

partes_fl <- left_join(partes_fl,
                       partes_fl_agg) %>% 
  select(numprocess:polo_fl,
         nome_fl,
         sexo_fl:tipoPessoa_fl)

### 2.1.5. Testemunha do juízo ----------------------------------------------

## Criando um banco temporário com apenas as partes da TESTEMUNHA DO JUÍZO 

partes_tj <- df %>% 
  select(numprocess,
         grau,
         partes_tj_desc,
         partes_tj_list) %>% 
  unique()

## Reorganizando os dados da TESTEMUNHA DO JUÍZO

partes_tj <- partes_tj %>% 
  mutate(across(everything(), 
                str_to_upper)) %>% 
  mutate(partes_tj_list = str_replace_all(partes_tj_list,
                                          "\\[|]",
                                          ""),
         partes_tj_list = str_replace_all(partes_tj_list,
                                          '"',
                                          ""),
         partes_tj_list = str_replace_all(partes_tj_list,
                                          "\\}",
                                          ""),
         partes_tj_list = gsub(", NA PESSOA DE SEU REPRESENTANTE LEGAL",
                               "",
                               partes_tj_list,
                               perl = TRUE),
         partes_tj_list = gsub("NA PESSOA DE SEU REPRESENTANTE LEGAL",
                               "",
                               partes_tj_list,
                               perl = TRUE),
         partes_tj_list = gsub(", NOME:",
                               "; NOME:",
                               partes_tj_list,
                               perl = TRUE),
         partes_tj_list = gsub(", SEXO:",
                               "; SEXO:",
                               partes_tj_list,
                               perl = TRUE),
         partes_tj_list = gsub(", TIPOPESSOA:",
                               "; TIPOPESSOA:",
                               partes_tj_list,
                               perl = TRUE),
         partes_tj_list = gsub(", NUMERODOCUMENTOPRINCIPAL:",
                               "; NUMERODOCUMENTOPRINCIPAL:",
                               partes_tj_list,
                               perl = TRUE),
         partes_tj_list = gsub(", NACIONALIDADE:",
                               "; NACIONALIDADE:",
                               partes_tj_list,
                               perl = TRUE),
         partes_tj_list = gsub(", ASSISTENCIAJUDICIARIA:",
                               "; ASSISTENCIAJUDICIARIA:",
                               partes_tj_list,
                               perl = TRUE),
         partes_tj_list = gsub(", INTIMACAOPENDENTE:",
                               "; INTIMACAOPENDENTE:",
                               partes_tj_list,
                               perl = TRUE)) %>% 
  separate_rows(partes_tj_list,
                sep = "{") %>% 
  separate(partes_tj_list,
           sep = "; ",
           into = c("polo_tj",
                    "nome_tj",
                    "sexo_tj",
                    "tipoPessoa_tj",
                    "numeroDocumentoPrincipal_tj",
                    "nacionalidade_tj",
                    "assistenciaJudiciaria_tj",
                    "intimacaoPendente_tj")) %>% 
  mutate(across(everything(), 
                gsub, 
                pattern = ".*: ", 
                replacement = ""),
         partes_tj_desc = "TESTEMUNHA DO JUÍZO") %>% 
  filter((!is.na(nome_tj) &
            nome_tj != "") &
           (!is.na(nome_tj) &
              nome_tj != "")) %>% 
  unique() %>% 
  select(-assistenciaJudiciaria_tj,
         -intimacaoPendente_tj) 

## Agregando os dados por processo e grau

partes_tj_agg <- aggregate(nome_tj ~ numprocess + grau + tipoPessoa_tj, 
                           data = partes_tj,
                           paste,
                           collapse = "\n")

## Preparando os dados para o join

partes_tj <- partes_tj %>% 
  select(-nome_tj,
         -numeroDocumentoPrincipal_tj) %>% 
  unique()

## Juntando os dados tratados

partes_tj <- left_join(partes_tj,
                       partes_tj_agg) %>% 
  select(numprocess:polo_tj,
         nome_tj,
         sexo_tj:tipoPessoa_tj)

### 2.1.6. Assistente simples desinteressado (amicus curie) -----------------

## Criando um banco temporário com apenas as partes do ASSISTENTE SIMPLES DESINTERESSADO

partes_ad <- df %>% 
  select(numprocess,
         grau,
         partes_ad_desc,
         partes_ad_list) %>% 
  unique()

## Reorganizando os dados do ASSISTENTE SIMPLES DESINTERESSADO

partes_ad <- partes_ad %>% 
  mutate(across(everything(), 
                str_to_upper)) %>% 
  mutate(partes_ad_list = str_replace_all(partes_ad_list,
                                          "\\[|]",
                                          ""),
         partes_ad_list = str_replace_all(partes_ad_list,
                                          '"',
                                          ""),
         partes_ad_list = str_replace_all(partes_ad_list,
                                          "\\}",
                                          ""),
         partes_ad_list = gsub(", NA PESSOA DE SEU REPRESENTANTE LEGAL",
                               "",
                               partes_ad_list,
                               perl = TRUE),
         partes_ad_list = gsub("NA PESSOA DE SEU REPRESENTANTE LEGAL",
                               "",
                               partes_ad_list,
                               perl = TRUE),
         partes_ad_list = gsub(", NOME:",
                               "; NOME:",
                               partes_ad_list,
                               perl = TRUE),
         partes_ad_list = gsub(", SEXO:",
                               "; SEXO:",
                               partes_ad_list,
                               perl = TRUE),
         partes_ad_list = gsub(", TIPOPESSOA:",
                               "; TIPOPESSOA:",
                               partes_ad_list,
                               perl = TRUE),
         partes_ad_list = gsub(", NUMERODOCUMENTOPRINCIPAL:",
                               "; NUMERODOCUMENTOPRINCIPAL:",
                               partes_ad_list,
                               perl = TRUE),
         partes_ad_list = gsub(", NACIONALIDADE:",
                               "; NACIONALIDADE:",
                               partes_ad_list,
                               perl = TRUE),
         partes_ad_list = gsub(", ASSISTENCIAJUDICIARIA:",
                               "; ASSISTENCIAJUDICIARIA:",
                               partes_ad_list,
                               perl = TRUE),
         partes_ad_list = gsub(", INTIMACAOPENDENTE:",
                               "; INTIMACAOPENDENTE:",
                               partes_ad_list,
                               perl = TRUE)) %>% 
  separate_rows(partes_ad_list,
                sep = "{") %>% 
  separate(partes_ad_list,
           sep = "; ",
           into = c("polo_ad",
                    "nome_ad",
                    "sexo_ad",
                    "tipoPessoa_ad",
                    "numeroDocumentoPrincipal_ad",
                    "nacionalidade_ad",
                    "assistenciaJudiciaria_ad",
                    "intimacaoPendente_ad")) %>% 
  mutate(across(everything(), 
                gsub, 
                pattern = ".*: ", 
                replacement = ""),
         partes_ad_desc = "ASSISTENTE SIMPLES DESINTERESSADO") %>% 
  filter((!is.na(nome_ad) &
            nome_ad != "") &
           (!is.na(nome_ad) &
              nome_ad != "")) %>% 
  unique() %>% 
  select(-assistenciaJudiciaria_ad,
         -intimacaoPendente_ad) 

## Agregando os dados por processo e grau

partes_ad_agg <- aggregate(nome_ad ~ numprocess + grau + tipoPessoa_ad, 
                           data = partes_ad,
                           paste,
                           collapse = "\n")

## Preparando os dados para o join

partes_ad <- partes_ad %>% 
  select(-nome_ad,
         -numeroDocumentoPrincipal_ad) %>% 
  unique()

## Juntando os dados tratados

partes_ad <- left_join(partes_ad,
                       partes_ad_agg) %>% 
  select(numprocess:polo_ad,
         nome_ad,
         sexo_ad:tipoPessoa_ad)

### 2.1.7. Vítima -----------------------------------------------------------

## Criando um banco temporário com apenas as partes da VÍTIMA 

partes_vi <- df %>% 
  select(numprocess,
         grau,
         partes_vi_desc,
         partes_vi_list) %>% 
  unique()

## Reorganizando os dados da VÍTIMA

partes_vi <- partes_vi %>% 
  mutate(across(everything(), 
                str_to_upper)) %>% 
  mutate(partes_vi_list = str_replace_all(partes_vi_list,
                                          "\\[|]",
                                          ""),
         partes_vi_list = str_replace_all(partes_vi_list,
                                          '"',
                                          ""),
         partes_vi_list = str_replace_all(partes_vi_list,
                                          "\\}",
                                          ""),
         partes_vi_list = gsub(", NA PESSOA DE SEU REPRESENTANTE LEGAL",
                               "",
                               partes_vi_list,
                               perl = TRUE),
         partes_vi_list = gsub("NA PESSOA DE SEU REPRESENTANTE LEGAL",
                               "",
                               partes_vi_list,
                               perl = TRUE),
         partes_vi_list = gsub(", NOME:",
                               "; NOME:",
                               partes_vi_list,
                               perl = TRUE),
         partes_vi_list = gsub(", SEXO:",
                               "; SEXO:",
                               partes_vi_list,
                               perl = TRUE),
         partes_vi_list = gsub(", TIPOPESSOA:",
                               "; TIPOPESSOA:",
                               partes_vi_list,
                               perl = TRUE),
         partes_vi_list = gsub(", NUMERODOCUMENTOPRINCIPAL:",
                               "; NUMERODOCUMENTOPRINCIPAL:",
                               partes_vi_list,
                               perl = TRUE),
         partes_vi_list = gsub(", NACIONALIDADE:",
                               "; NACIONALIDADE:",
                               partes_vi_list,
                               perl = TRUE),
         partes_vi_list = gsub(", ASSISTENCIAJUDICIARIA:",
                               "; ASSISTENCIAJUDICIARIA:",
                               partes_vi_list,
                               perl = TRUE),
         partes_vi_list = gsub(", INTIMACAOPENDENTE:",
                               "; INTIMACAOPENDENTE:",
                               partes_vi_list,
                               perl = TRUE)) %>% 
  separate_rows(partes_vi_list,
                sep = "{") %>% 
  separate(partes_vi_list,
           sep = "; ",
           into = c("polo_vi",
                    "nome_vi",
                    "sexo_vi",
                    "tipoPessoa_vi",
                    "numeroDocumentoPrincipal_vi",
                    "nacionalidade_vi",
                    "assistenciaJudiciaria_vi",
                    "intimacaoPendente_vi")) %>% 
  mutate(across(everything(), 
                gsub, 
                pattern = ".*: ", 
                replacement = ""),
         partes_vi_desc = "VÍTIMA") %>% 
  filter((!is.na(nome_vi) &
            nome_vi != "") &
           (!is.na(nome_vi) &
              nome_vi != "")) %>% 
  unique() %>% 
  select(-assistenciaJudiciaria_vi,
         -intimacaoPendente_vi) 

## Agregando os dados por processo e grau

partes_vi_agg <- aggregate(nome_vi ~ numprocess + grau + tipoPessoa_vi, 
                           data = partes_vi,
                           paste,
                           collapse = "\n")

## Preparando os dados para o join

partes_vi <- partes_vi %>% 
  select(-nome_vi,
         -numeroDocumentoPrincipal_vi) %>% 
  unique()

## Juntando os dados tratados

partes_vi <- left_join(partes_vi,
                       partes_vi_agg) %>% 
  select(numprocess:polo_vi,
         nome_vi,
         sexo_vi:tipoPessoa_vi)

## Preparando os dados para o join

df <- df %>% 
  select(lat,
         lon,
         ano:dt_inicio_situacao_novo,
         dt_inicio_situacao_novo_v2,
         dt_inicio_situacao_julgado,
         dt_inicio_situacao_baixado,
         movimento) %>% 
  mutate(classe = str_to_upper(classe))

## Juntando com o banco principal

df <- left_join(df,
                partes_pa) %>% 
  unique()


df <- left_join(df,
                partes_at) %>% 
  unique()

df <- left_join(df,
                partes_tc) %>% 
  unique()

df <- left_join(df,
                partes_fl) %>% 
  unique()

df <- left_join(df,
                partes_tj) %>% 
  unique()

df <- left_join(df,
                partes_ad) %>% 
  unique()

df <- left_join(df,
                partes_vi) %>% 
  unique()

## Limpeza dos caracteres de texto desconfigurados

df$nome_pa <- encoder(df$nome_pa)

df$nome_at <- encoder(df$nome_at)

df$nome_tc <- encoder(df$nome_tc)

df$nome_fl <- encoder(df$nome_fl)

df$nome_tj <- encoder(df$nome_tj)

df$nome_ad <- encoder(df$nome_ad)

df$nome_vi <- encoder(df$nome_vi)

## 2.2. Classes ------------------------------------------------------------

## Preparando os dados para o join

classes <- classes %>% 
  select(codigo_01,
         descricao_01,
         codigo_06,
         descricao_06,
         arvore_classe) %>% 
  mutate(descricao_06 = str_to_upper(descricao_06)) %>% 
  rename("classe" = "descricao_06",
         "cod_classe" = "codigo_06",
         "hier_classe" = "arvore_classe",
         "codigo_01_classe" = "codigo_01",
         "descricao_01_classe" = "descricao_01") %>% 
  filter(!cod_classe %in% c(990,
                            12154,
                            1044,
                            1669,
                            1725,
                            1460,
                            1030,
                            244,
                            1678,
                            428,
                            1036,
                            12083,
                            12084,
                            1726,
                            191,
                            1032,
                            1348)) %>% 
  unique()

assuntos <- assuntos %>% 
  select(codigo_01,
         descricao_01,
         codigo_06,
         descricao_06,
         arvore_assunto) %>% 
  rename("co_assunto_v2" = "codigo_06",
         "hier_assunto" = "arvore_assunto") %>% 
  unique()

## Separando os assuntos em diferentes linhas para cada
## processo

df2 <- df %>% 
  mutate(co_assunto_v2 = strsplit(as.character(co_assunto), 
                                  ",")) %>% 
  unnest(co_assunto_v2)

## Juntando os dados

df2 <- left_join(df2,
                 classes,
                 na_matches = "never") %>% 
  unique()

df2 <- left_join(df2,
                 assuntos,
                 na_matches = "never") %>% 
  unique()

## Preparando os dados

natureza_classes <- natureza_classes %>% 
  mutate(cod_classe = as.character(cod_classe))

## Juntando com as informações da natureza das classes

df2 <- left_join(df2,
                 natureza_classes) %>% 
  rename("ano_inicio_situacao_novo" = "dt_inicio_situacao_novo_v2",
         "cod_assunto" = "co_assunto_v2",
         "assunto" = "descricao_06",
         "codigo_01_assunto" = "codigo_01",
         "descricao_01_assunto" = "descricao_01",
         "natureza_classe" = "natureza")

## Criando uma terceira versão com 
## somente algumas colunas

filtrado <- df2 %>% 
  filter(ano_inicio_situacao_novo >= "2020") %>% 
  select(lat,
         lon,
         ano_inicio_situacao_novo,
         uf,
         cod_ibge,
         municipio,
         numprocess,
         grau,
         tribunal,
         cod_orgao,
         orgaojulgador,
         natureza_classe,
         hier_classe,
         codigo_01_classe,
         descricao_01_classe,
         cod_classe,
         classe,
         hier_assunto,
         codigo_01_assunto,
         descricao_01_assunto,
         cod_assunto,
         assunto,
         flg_julgamento) %>% 
  mutate(across(everything(),
                str_to_upper))

## Criando um banco para os movimentos

movimentos <- df2 %>% 
  select(numprocess,
         data_ajuizamento,
         ano_inicio_situacao_novo,
         dt_inicio_situacao_novo,
         uf,
         municipio,
         grau,
         tribunal,
         movimento) %>% 
  unique()

## Reorganizando os dados

df2 <- df2 %>% 
  select(numprocess,
         data_ajuizamento,
         ano_inicio_situacao_novo,
         dt_inicio_situacao_novo,
         dt_inicio_situacao_julgado,
         dt_inicio_situacao_baixado,
         tempo_tramitacao,
         esfera,
         uf:tribunal,
         cod_orgao,
         orgaojulgador,
         natureza_classe,
         hier_classe,
         codigo_01_classe,
         descricao_01_classe,
         cod_classe,
         classe,
         hier_assunto,
         codigo_01_assunto,
         descricao_01_assunto,
         cod_assunto,
         assunto,
         partes_pa_desc:tipoPessoa_vi,
         flg_julgamento,
         lat,
         lon) %>% 
  mutate(across(everything(),
                str_to_upper))

# 3. Salvando os dados ----------------------------------------------------

## Banco com todas as variáveis

write.csv(df2,
          "data/output/DataJud/datajud_allvars_filt_v26102022.csv",
          fileEncoding = "UTF-8",
          row.names = F)

saveRDS(df2,
        "data/output/DataJud/datajud_allvars_filt_v26102022.rds")

## Banco reduzido

write.csv(filtrado,
          "data/output/DataJud/datajud_filt_v26102022.csv",
          fileEncoding = "UTF-8",
          row.names = F)

saveRDS(filtrado,
        "data/output/DataJud/datajud_filt_v26102022.rds")

## Movimentações

saveRDS(movimentos,
        "data/output/DataJud/datajud_movimentos_v26102022.rds")
