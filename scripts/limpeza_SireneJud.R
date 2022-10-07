
## TÍTULO: ESTRUTURAÇÃO DADOS SIRENEJUD
## DATA: 03/09/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(tidyverse)
library(sf)
library(data.table)
library(abjutils)

# 1. Data -----------------------------------------------------------------

## Carregando os dados

municipios <- readxl::read_xls("data/input/SireneJud/lista_de_municipios_Amazonia_Legal_2020.xls")

df <- fread("data/input/SireneJud/shapes_datajud.csv",
            encoding = "UTF-8")

## Padronizando os dados

municipios <- municipios %>% 
  mutate(NM_MUN = str_to_upper(rm_accent(NM_MUN)),
         NM_MUN = ifelse(CD_MUN == 1100049,
                         "CACOAL",
                         NM_MUN))

## Filtrando somente os estados de interesse

df <- df %>% 
  filter(uf %in% municipios$SIGLA)

## Estruturando as coordenadas geográficas 
## e ajustando outros detalhes

df <- df %>% 
  extract(geom,
          c('lat', 'lon'),
            '\\((.*) (.*)\\)',
            convert = TRUE) %>%
  mutate(municipio = rm_accent(municipio),
         ano_v2 = str_sub(numprocess,
                          12,
                          15),
         compar = ifelse(lubridate::year(dt_inicio_situacao_novo) == ano_v2,
                         TRUE,
                         FALSE),
         dt_inicio_situacao_novo = ifelse(is.na(dt_inicio_situacao_novo),
                                             as.character(ano_v2),
                                             as.character(dt_inicio_situacao_novo)),
         dt_inicio_situacao_novo_v2 = str_sub(dt_inicio_situacao_novo,
                                              1,
                                              4)) %>%
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
                           noassuntos))

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
                replacement = "")) %>% 
  filter((!is.na(nome_pa) &
          nome_pa != "") &
         (!is.na(nome_pa) &
          nome_pa != "")) %>% 
  unique() %>% 
  select(-assistenciaJudiciaria_pa,
         -intimacaoPendente_pa) 

## Agregando os dados por processo e grau

partes_pa_agg <- aggregate(nome_pa ~ numprocess + grau, 
                           data = partes2,
                           paste,
                           collapse = "\n")

## Preparando os dados para o join

partes_pa <- partes_pa %>% 
  select(-nome_pa,
         -numeroDocumentoPrincipal_pa) %>% 
  unique()

## Juntando os dados tratados

partes_pa <- left_join(partes_pa,
                       partes_pa_agg) %>% 
  select(numprocess:polo_pa,
         nome_pa,
         sexo_pa:tipoPessoa_pa)

## Criando um banco temporário com apenas as partes do POLO PASSIVO 

partes_at <- df %>% 
  select(numprocess,
         grau,
         partes_at_desc,
         partes_at_list) %>% 
  unique()

## Reorganizando os dados do POLO PASSIVO

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
                replacement = "")) %>% 
  filter((!is.na(nome_at) &
            nome_at != "") &
           (!is.na(nome_at) &
              nome_at != "")) %>% 
  unique() %>% 
  select(-assistenciaJudiciaria_at,
         -intimacaoPendente_at) 

## Agregando os dados por processo e grau

partes_at_agg <- aggregate(nome_at ~ numprocess + grau, 
                           data = partes2,
                           paste,
                           collapse = "\n")

## Preparando os dados para o join

partes_at <- partes_at %>% 
  select(-nome_at,
         -numeroDocumentoPrincipal_at) %>% 
  unique()

## Juntando os dados tratados

partes_at <- left_join(partes_at,
                       partes_at_agg) %>% 
  select(numprocess:polo_at,
         nome_at,
         sexo_at:tipoPessoa_at)

## Criando uma segunda versão com 
## somente algumas colunas

df2 <- df %>% 
  select(lat,
         lon,
         ano,
         uf,
         municipio,
         grau,
         tribunal,
         cod_orgao,
         orgaojulgador,
         classe,
         noassuntos,
         numprocess,
         dt_inicio_situacao_novo_v2,
         flg_julgamento) 

# 3. Salvando os dados ----------------------------------------------------

## Banco com todas as variáveis

write.csv(df,
          "data/output/SireneJud/sirenejud_allvars_filt_v05092022.csv",
          fileEncoding = "UTF-8",
          row.names = F)

saveRDS(df,
        "data/output/SireneJud/sirenejud_allvars_filt_v05092022.rds")

## Banco reduzido

write.csv(df2,
          "data/output/SireneJud/sirenejud_filt_v05092022.csv",
          fileEncoding = "UTF-8",
          row.names = F)

saveRDS(df2,
        "data/output/SireneJud/sirenejud_filt_v05092022.rds")

assuntos <- readxl::read_xlsx("assuntos.xlsx")

assuntos <- assuntos %>%
  rename("noassuntos" = "Coluna1") %>% 
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
         noassuntos = stripWhitespace(trimws(noassuntos)))

assuntos <- str_to_upper(assuntos$noassuntos)

df2 <- df %>% 
  mutate(noassuntos = str_to_upper(noassuntos),
         noassuntos = stripWhitespace(trimws(noassuntos))) %>% 
  filter(noassuntos %in% assuntos |
         numprocess %in% c("0004170-69.2015.8.11.0046",
                           "0000911-27.2016.8.11.0080",
                           "0011112-13.2015.8.22.0014"))

teste <- unique(df2$noassuntos)

assuntos <- as.data.frame(assuntos) %>% 
  unique()

teste <- assuntos %>% 
  filter(!assuntos %in% df2$noassuntos)

assuntos_uf <- df2 %>% 
  group_by(dt_inicio_situacao_novo_v2, uf, noassuntos) %>% 
  summarise(frequencia = n()) %>% 
  filter(dt_inicio_situacao_novo_v2 >= "2018")

write.csv(assuntos_uf,
          "sirenejud_assuntos_uf_ano.csv",
          row.names = F,
          fileEncoding = "UTF-8")
