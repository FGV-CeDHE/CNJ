
## PROCESSANDO ARQUIVOS DATAJUD
## DATA: 30/05/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(jsonlite)
library(geobr)
library(tm)
library(abjutils)

## PREPARANDO O AMBIENTE

setwd("CNJ")

# 1. Data -----------------------------------------------------------------

## Criando uma lista com os arquivos disponíveis

files <- list.files(path = "data/input/DataJud",
                    pattern = "json")

## Criando uma lista onde os dados serão salvos

df <- list()

## For loop que carrega e padroniza os dados

for(i in seq_along(files)){
  
  cat("Lendo", i, "\n")
  
  ## Carregando o arquivo
  
  temp <- jsonlite::fromJSON(paste0("data/input/DataJud/",
                                    files[i]),
                                flatten = TRUE)
  
  ## Variável onde o código do Assunto será armazenado
  
  temp$CodigoAssunto <- NA
  
  ## Extraindo o código do Assunto
  
  for(j in 1:nrow(temp)){
    
    cat("Linha", j, "\n")
    
    unlist <- tryCatch({
      suppressMessages({
        
        temp$CodigoAssunto[j] <- paste0(unlist(temp$`_source.dadosBasicos.assunto`[[j]][["codigoNacional"]]),
                                        collapse = " / ")
        
        unlist })}, error = function(e) {print("erro")
          NA})
    
  }
  
  ## Selecionando somente as variáveis que interessam
  
  temp <- temp %>% 
    select(-c(`_index`,
              `_type`,
              `_score`,
              sort,
              `_source.millisInsercao`,
              `_source.dataProtocolo`,
              `_source.idElastic`,
              `_source.protocoloRN`,
              `_source.id`,
              `_source.movimento`,
              `_source.dadosBasicos.assunto`)) 
  
  ## Empilhando os dados
  
  df <- rbind.fill(df,
                  temp)
  
  ## Limpando a memória do R
  
  gc()
  
}

## Carregando os dados dos municípios

municipios <- geobr::read_municipality(year = 2020)

## Selecionando as variáveis de interesse

municipios <- municipios %>%
  as.data.frame() %>% 
  select(name_region,
         code_state,
         abbrev_state,
         code_muni,
         name_muni) %>% 
  rename("CodigoMunicipioIBGE" = "code_muni",
         "CodigoEstadoIBGE" = "code_state",
         "UF" = "abbrev_state",
         "Regiao" = "name_region",
         "NomeMunicipio" = "name_muni")

## Nomes das colunas

colnames <- c("ID",
              "Numero",
              "DataAjuizamento",
              "SiglaTribunal",
              "CodigoOrgao",
              "OrgaoJulgador",
              "CodigoMunicipioIBGE",
              "Instancia",
              "CodigoLocalidade",
              "ClasseProcessual",
              "NivelSigilo",
              "IntervencaoMP",
              "Prioridade",
              "ProcessoVinculado",
              "TotalAssuntos",
              "CodigoAssunto",
              "Grau",
              "Competencia",
              "ValorCausa",
              "TamanhoProcesso")

## Removendo variáveis desnecessários e 
## renomeando as variáveis restantes

df <- df %>% 
  select(`_id`,
         `_source.dadosBasicos.numero`,
         `_source.dadosBasicos.dataAjuizamento`,
         `_source.siglaTribunal`,
         `_source.dadosBasicos.orgaoJulgador.codigoOrgao`,
         `_source.dadosBasicos.orgaoJulgador.nomeOrgao`,
         `_source.dadosBasicos.orgaoJulgador.codigoMunicipioIBGE`,
         `_source.dadosBasicos.orgaoJulgador.instancia`,
         `_source.dadosBasicos.codigoLocalidade`,
         `_source.dadosBasicos.classeProcessual`,
         `_source.dadosBasicos.nivelSigilo`,
         `_source.dadosBasicos.intervencaoMP`,
         `_source.dadosBasicos.prioridade`,
         `_source.dadosBasicos.processoVinculado`,
         `_source.dadosBasicos.totalAssuntos`,
         `CodigoAssunto`,
         `_source.grau`,
         `_source.dadosBasicos.competencia`,
         `_source.dadosBasicos.valorCausa`,
         `_source.dadosBasicos.tamanhoProcesso`)


names(df) <- colnames

## Juntando com os dados do DataJud

df <- left_join(df,
                municipios) %>% 
  mutate(DataAjuizamento = as.Date(str_sub(DataAjuizamento,
                                           start = 1,
                                           end = 8),
                                   format = "%Y%m%d"),
         OrgaoJulgador = str_to_upper(rm_accent(case_when(OrgaoJulgador == "VICE-PRESIDÃNCIA" ~ "VICE-PRESIDÊNCIA",
                                                OrgaoJulgador == "�RG�O ESPECIAL" ~ "ÓRGÃO ESPECIAL",
                                                OrgaoJulgador == "�RG�O ESPECIAL DO TRIBUNAL DE JUSTI�A DO ESTADO DE SANTA CATARINA" ~ "ÓRGÃO ESPECIAL DO TRIBUNAL DE JUSTIÇA DO ESTADO DE SANTA CATARINA",
                                                OrgaoJulgador == "VICE-PRESID?NCIA" ~ "VICE-PRESIDÊNCIA",
                                                OrgaoJulgador == "VARA UNICA VINCULADA DE TEJU�UOCA" ~ "VARA UNICA VINCULADA DE TEJUÇUOCA",
                                                OrgaoJulgador == "VARA UNICA VINCULADA DE ITAI�ABA" ~ "VARA UNICA VINCULADA DE ITAIÇABA",
                                                OrgaoJulgador == "VARA UNICA DE S�O FRANCISCO DO MARANH�O" ~ "VARA UNICA DE SÃO FRANCISCO DO MARANHÃO",
                                                OrgaoJulgador == "VARA UNICA DE S�O DOMINGOS DO AZEIT�O" ~ "VARA UNICA DE SÃO DOMINGOS DO AZEITÃO",
                                                OrgaoJulgador == "VARA UNICA DE MON��O" ~ "VARA UNICA DE MONÇÃO",
                                                OrgaoJulgador == "VARA UNICA DE ITINGA DO MARANH�O" ~ "VARA UNICA DE ITINGA DO MARANHÃO",
                                                OrgaoJulgador == "VARA UNICA DE IGARAP� GRANDE" ~ "VARA UNICA DE IGARAPÉ GRANDE",
                                                OrgaoJulgador == "VARA UNICA DA COMARCA DE VI�OSA DO CEARA" ~ "VARA UNICA DA COMARCA DE VIÇOSA DO CEARA",
                                                OrgaoJulgador == "GAB. DES. FEDERAL JOSÉ L�?ZARO ALFREDO GUIMARÃES" ~ "GAB. DES. FEDERAL JOSÉ LÁZARO ALFREDO GUIMARÃES",
                                                OrgaoJulgador == "SUBSECRETARIA DE RECURSOS EXTRAORDIN�?RIOS, ESPECIAIS E ORDIN�?RIOS" ~ "SUBSECRETARIA DE RECURSOS EXTRAORDINÁRIOS, ESPECIAIS E ORDINÁRIOS",
                                                OrgaoJulgador == "PRESIDÃNCIA" ~ "PRESIDÊNCIA",
                                                T ~ OrgaoJulgador))),
         CodigoAssunto = ifelse(grepl("NA", CodigoAssunto),
                                NA,
                                CodigoAssunto),
         Regiao = str_to_upper(ifelse(Regiao == "CENTRO OESTE",
                                      "CENTRO-OESTE",
                                      Regiao)),
         IntervencaoMP = ifelse(IntervencaoMP == "TRUE",
                                1,
                                ifelse(IntervencaoMP == "FALSE",
                                       0,
                                       NA)),
         Prioridade = NA,
         ProcessoVinculado = ifelse(ProcessoVinculado == "list()",
                                    NA,
                                    ProcessoVinculado),
         Assunto = ifelse(Assunto == "NA - NA",
                          NA,
                          Assunto),
         NomeMunicipio = str_to_upper(NomeMunicipio)) %>% 
  select(ID,
         Numero,
         DataAjuizamento,
         SiglaTribunal,
         CodigoOrgao,
         OrgaoJulgador,
         Instancia,
         CodigoLocalidade,
         Regiao,
         CodigoEstadoIBGE,
         UF,
         CodigoMunicipioIBGE,
         NomeMunicipio,
         ClasseProcessual,
         NivelSigilo,
         IntervencaoMP,
         Prioridade,
         ProcessoVinculado,
         CodigoAssunto,
         Grau,
         Competencia,
         ValorCausa,
         TamanhoProcesso) %>% 
    arrange(UF, NomeMunicipio, DataAjuizamento)

## Adicionando pontuação no número do processo

stri_sub(df$Numero, 8, 2) <- "-"
stri_sub(df$Numero, 11, 2) <- "."
stri_sub(df$Numero, 16, 2) <- "."
stri_sub(df$Numero, 18, 2) <- "."
stri_sub(df$Numero, 21, 2) <- "."

## Eliminando repetições na variável 'Assunto'

df$Assunto <- as.list(strsplit(df$Assunto,
                               " / "))

for(i in 1:nrow(df)){
  
  cat("Lendo", i, "\n")
  
  if(!is.na(df$Assunto[i])){
    
    df$Assunto[i] <- list(unique(unlist(df$Assunto[i])))
    
  }
  
}

## Preparando a coluna com o texto final

df$Assunto <- gsub("^c\\(|\\)$", 
                   "", 
                   df$Assunto)

df$Assunto <- gsub('"', 
                   '', 
                   unlist(df$Assunto))

df$Assunto <- gsub(", +", 
                   " / ", 
                   df$Assunto)

## Remove os espaços em branco em excesso

for (i in colnames(df)){
  
  df[[i]] <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", 
                  df[[i]], 
                  perl = TRUE)
}

## Salvando o banco final

saveRDS(df,
        "data/output/DataJud/dataJud_final_17072022.rds")
