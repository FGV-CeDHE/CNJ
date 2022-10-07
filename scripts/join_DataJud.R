
## JOIN DATAJUD X TRIBUNAIS
## DATA: 15/07/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(tm)

# 1. Data -----------------------------------------------------------------

## Carregando os dados do DataJud

datajud <- readRDS("data/output/DataJud/dataJud_final_17072022.rds")

## Carregando os dados do STF

stf <- readRDS("data/output/STF/acórdãos_STF_03082022_com filtros.rds")

## Carregando os dados do STJ

stj <- readRDS("data/output/STJ/acórdãos_STJ_31082022_com filtros.rds")

## Carregando os dados do TJAM

tjam <- readRDS("data/output/TJAM/acórdãos_TJAM_19072022_com filtros.rds")

## Carregando os dados do TJMT

tjmt <- readRDS("data/output/TJMT/acórdãos_TJMT_19082022_com filtros.rds")

## Carregando os dados do TJPA

tjpa <- readRDS("data/output/TJPA/acórdãos_TJPA_03082022_com filtros.rds")

## Carregando os dados do TRF1

trf1 <- readRDS("data/output/TRF1/acórdãos_TRF1_25072022_com filtros.rds")

# 2. Join -----------------------------------------------------------------

## 2.1. STF ---------------------------------------------------------------

## Criando uma lista com os números dos processos

nrprocessos <- unique(stf$Numero)

## Verificando quantos processos existem em ambos os bancos e
## renomeando as variáveis para o join

processosstf <- datajud %>% 
  filter(Numero %in% nrprocessos) %>% 
  rename("ID2" = "ID",
         "SiglaTribunal2" = "SiglaTribunal")

## Juntando os bancos

processosstf <- left_join(processosstf,
                       stf) %>% 
  select(ID2,
         IDPortal,
         Numero,
         DataAjuizamento,
         DataJulgamento,
         DataPublicacao,
         SiglaTribunal2,
         OrgaoJudiciante,
         CodigoOrgao,
         OrgaoJulgador,
         Instancia,
         Relator,
         Partes,
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
         Assunto,
         Tema,
         Grau,
         Competencia,
         ValorCausa,
         TamanhoProcesso,
         TipoDocumento,
         TipoAcao,
         TipoRecurso,
         Ementa,
         Decisao,
         Tese,
         DadosPublicacao,
         Indexacao,
         Legislacao,
         Observacao,
         Doutrina,
         URL_InteiroTeor,
         ACP:`recursos naturais/biodiversidade/ecologico`) %>% 
  mutate(OrgaoJudiciante = str_to_upper(OrgaoJudiciante),
         Relator = str_to_upper(Relator),
         Partes = gsub('"', "", Partes),
         Partes = gsub('\\{|\\}', "", Partes),
         Partes = gsub('\\:', ": ", Partes),
         Partes = gsub('\\,', "\n", Partes),
         Ementa = gsub("\\Ementa: ", "", Ementa),
         Ementa = gsub("\\EMENTA: ", "", Ementa),
         Ementa = stripWhitespace(gsub("\\EMENTA ", "", Ementa)),
         Decisao = gsub("\\Decisão: ", "", Decisao),
         Decisao = stripWhitespace(gsub("\\Decisão ", "", Decisao)),
         DataAjuizamento = as.Date(DataAjuizamento,
                                   format = "%Y-%m-%d"),
         DataPublicacao = as.Date(DataPublicacao,
                                   format = "%d/%m/%Y"),
         DataJulgamento = as.Date(DataJulgamento,
                                  format = "%d/%m/%Y")) %>% 
  rename("ID" = "ID2",
         "SiglaTribunal" = "SiglaTribunal2") %>% 
  arrange(UF, NomeMunicipio, DataAjuizamento)

## Remove os espaços em branco em excesso

for (i in colnames(processosstf)){
  
  processosstf[[i]] <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", 
                            processosstf[[i]], 
                            perl = TRUE)
}

## 2.2. STJ ---------------------------------------------------------------

## Criando uma lista com os números dos processos

nrprocessos <-  trimws(unique(stj$Numero))

## Verificando quantos processos existem em ambos os bancos e
## renomeando as variáveis para o join

processosstj <- datajud %>% 
  filter(Numero %in% nrprocessos) %>% 
  rename("OrgaoJulgador2" = "OrgaoJulgador",
         "Assunto2" = "Assunto")

## Juntando os bancos

processosstj <- left_join(processosstj,
                           stj) %>% 
  select(ID,
         Numero,
         DataAjuizamento,
         DataJulgamento,
         DataPublicacao,
         Autuacao,
         SiglaTribunal,
         TribunalOrigem,
         CodigoOrgao,
         OrgaoJulgador2,
         OrgaoJulgador,
         Instancia,
         Relator,
         Partes,
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
         Ramo,
         Assunto2,
         Assunto,
         Grau,
         Competencia,
         ValorCausa,
         TamanhoProcesso,
         Tipo,
         Processo,
         Localizacao,
         UltimaFase,
         Ementa,
         Acordao,
         URL_InteiroTeor,
         ACP:`recursos naturais/biodiversidade/ecologico`) %>% 
  mutate(Relator = str_to_upper(Relator)) %>% 
  rename("OrgaoJulgadorSTJ" = "OrgaoJulgador",
         "OrgaoJulgador" = "OrgaoJulgador2",
         "AssuntoSTJ" = "Assunto",
         "Assunto" = "Assunto2") %>% 
  arrange(UF, NomeMunicipio, DataAjuizamento)

## Remove os espaços em branco em excesso

for (i in colnames(processosstj)){
  
  processosstj[[i]] <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", 
                             processosstj[[i]], 
                             perl = TRUE)
}

## 2.3. TJAM --------------------------------------------------------------

## Criando uma lista com os números dos processos

nrprocessos <-  trimws(unique(tjam$Numero))

## Verificando quantos processos existem em ambos os bancos e
## renomeando as variáveis para o join

processostjam <- datajud %>% 
  filter(Numero %in% nrprocessos) %>% 
  rename("OrgaoJulgador2" = "OrgaoJulgador")

## Juntando os bancos

processostjam <- left_join(processostjam,
                           tjam) %>% 
  select(ID,
         Numero,
         DataAjuizamento,
         DataJulgamento,
         DataPublicacao,
         SiglaTribunal,
         Comarca,
         CodigoOrgao,
         OrgaoJulgador2,
         OrgaoJulgador,
         Instancia,
         Relator,
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
         Assunto,
         ClasseAssunto,
         Grau,
         Competencia,
         ValorCausa,
         TamanhoProcesso,
         Ementa,
         URL_InteiroTeor,
         ACP:`recursos naturais/biodiversidade/ecologico`) %>% 
  mutate(Relator = str_to_upper(Relator)) %>% 
  rename("OrgaoJulgadorTJAM" = "OrgaoJulgador",
         "OrgaoJulgador" = "OrgaoJulgador2") %>% 
  arrange(UF, NomeMunicipio, DataAjuizamento)

## Remove os espaços em branco em excesso

for (i in colnames(processostjam)){
  
  processostjam[[i]] <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", 
                             processostjam[[i]], 
                             perl = TRUE)
}

## 2.4. TJMT --------------------------------------------------------------

## Criando uma lista com os números dos processos

nrprocessos <-  trimws(unique(tjmt$Numero))

## Verificando quantos processos existem em ambos os bancos e
## renomeando as variáveis para o join

processostjmt <- datajud %>% 
  filter(Numero %in% nrprocessos) %>% 
  rename("OrgaoJulgador2" = "OrgaoJulgador",
         "Assunto2" = "Assunto")

## Juntando os bancos

processostjmt <- left_join(processostjmt,
                           tjmt) %>% 
  select(ID,
         Numero,
         DataAjuizamento,
         DataJulgamento,
         DataPublicacao,
         SiglaTribunal,
         CodigoOrgao,
         OrgaoJulgador2,
         OrgaoJulgador,
         Instancia,
         Relator,
         CodigoLocalidade,
         Regiao,
         CodigoEstadoIBGE,
         UF,
         CodigoMunicipioIBGE,
         NomeMunicipio,
         ClasseProcessual,
         Classe,
         ClasseFeito,
         NivelSigilo,
         IntervencaoMP,
         Prioridade,
         ProcessoVinculado,
         Acao,
         Assunto2,
         Grau,
         Competencia,
         ValorCausa,
         TamanhoProcesso,
         TipoProcesso,
         TipoJulgamento,
         Ementa,
         URL_InteiroTeor,
         ACP:`recursos naturais/biodiversidade/ecologico`) %>% 
  mutate(Relator = str_to_upper(Relator)) %>% 
  rename("OrgaoJulgadorTJMT" = "OrgaoJulgador",
         "OrgaoJulgador" = "OrgaoJulgador2",
         "Assunto" = "Assunto2") %>% 
  arrange(UF, NomeMunicipio, DataAjuizamento)

## Remove os espaços em branco em excesso

for (i in colnames(processostjmt)){
  
  processostjmt[[i]] <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", 
                             processostjmt[[i]], 
                             perl = TRUE)
}


## 2.5. TJPA --------------------------------------------------------------

## Criando uma lista com os números dos processos

nrprocessos <- unique(tjpa$Numero)

## Verificando quantos processos existem em ambos os bancos e
## renomeando as variáveis para o join

processostjpa <- datajud %>% 
  filter(Numero %in% nrprocessos) %>% 
  rename("ID2" = "ID")

## Juntando os bancos

processostjpa <- left_join(processostjpa,
                           tjpa) %>% 
  select(ID2,
         NumeroDocumento,
         NumeroAcordao,
         Numero,
         DataAjuizamento,
         DataJulgamento,
         DataPublicacao,
         SiglaTribunal,
         OrgaoJudiciante,
         CodigoOrgao,
         OrgaoJulgador,
         Instancia,
         Relator,
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
         Assunto,
         Grau,
         Competencia,
         ValorCausa,
         TamanhoProcesso,
         TipoDocumento,
         TipoAcao,
         TipoRecurso,
         Ementa,
         Secao,
         URL_InteiroTeor,
         ACP:`recursos naturais/biodiversidade/ecologico`) %>% 
  mutate(OrgaoJudiciante = str_to_upper(OrgaoJudiciante),
         Relator = str_to_upper(Relator),
         TipoAcao = str_to_upper(TipoAcao),
         Ementa = gsub("\\Ementa: ", "", Ementa),
         Ementa = gsub("\\EMENTA: ", "", Ementa),
         Ementa = stripWhitespace(gsub("\\EMENTA ", "", Ementa)),
         DataAjuizamento = as.Date(DataAjuizamento,
                                   format = "%Y-%m-%d"),
         DataPublicacao = as.Date(DataPublicacao,
                                   format = "%d/%m/%Y"),
         DataJulgamento = as.Date(DataJulgamento,
                                  format = "%d/%m/%Y"),
         TipoDocumento = ifelse(TipoDocumento == "ACÃ RDÃ O",
                                 "ACÓRDÃO",
                                 TipoDocumento),
         Secao = ifelse(Secao == "CÃ VEL",
                        "CÍVEL",
                        Secao)) %>% 
  rename("ID" = "ID2") %>% 
  arrange(UF, NomeMunicipio, DataAjuizamento)

## Remove os espaços em branco em excesso

for (i in colnames(processostjpa)){
  
  processostjpa[[i]] <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", 
                            processostjpa[[i]], 
                            perl = TRUE)
}

## 2.6. TJRO --------------------------------------------------------------


## 2.7. TRF1 --------------------------------------------------------------

## Criando uma lista com os números dos processos

nrprocessos <-  trimws(unique(trf1$Numero))

## Verificando quantos processos existem em ambos os bancos e
## renomeando as variáveis para o join

processostrf1 <- datajud %>% 
  filter(Numero %in% nrprocessos) %>% 
  rename("OrgaoJulgador2" = "OrgaoJulgador")

## Juntando os bancos

processostrf1 <- left_join(processostrf1,
                           trf1) %>% 
  select(ID,
         Numero,
         DataAjuizamento,
         DataJulgamento,
         DataPublicacao,
         FontePublicacao,
         SiglaTribunal,
         Origem,
         CodigoOrgao,
         OrgaoJulgador2,
         OrgaoJulgador,
         Instancia,
         Relator,
         RelatorConvocado,
         RelatorAcordao,
         Revisor,
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
         Assunto,
         Classe,
         Grau,
         Competencia,
         ValorCausa,
         TamanhoProcesso,
         Ementa,
         Decisao,
         Texto,
         #URL_InteiroTeor,
         ACP:`recursos naturais/biodiversidade/ecologico`) %>% 
  rename("OrgaoJulgadorTRF1" = "OrgaoJulgador",
         "OrgaoJulgador" = "OrgaoJulgador2") %>% 
  arrange(UF, NomeMunicipio, DataAjuizamento)

## Remove os espaços em branco em excesso

for (i in colnames(processostrf1)){
  
  processostrf1[[i]] <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", 
                             processostrf1[[i]], 
                             perl = TRUE)
}


# 3. Export ---------------------------------------------------------------

## 3.1. STF ----------------------------------------------------------------

## Salvando os bancos gerados

write.csv(processosstf,
          "data/output/DataJud/join_DataJud_STF_com filtros_18072022.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

writexl::write_xlsx(processosstf,
                    "data/output/DataJud/join_DataJud_STF_com filtros_18072022.xlsx")

saveRDS(processosstf,
        "data/output/DataJud/join_DataJud_STF_com filtros_18072022.rds")

## 3.2. STJ ----------------------------------------------------------------

## Salvando os bancos gerados

write.csv(processosstj,
          "data/output/DataJud/join_DataJud_STJ_com filtros_31082022.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

writexl::write_xlsx(processosstj,
                    "data/output/DataJud/join_DataJud_STJ_com filtros_31082022.xlsx")

saveRDS(processosstj,
        "data/output/DataJud/join_DataJud_STJ_com filtros_31082022.rds")

## 3.3. TJAM ---------------------------------------------------------------

write.csv(processostjam,
          "data/output/DataJud/join_DataJud_TJAM_com filtros_20072022.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

writexl::write_xlsx(processostjam,
                    "data/output/DataJud/join_DataJud_TJAM_com filtros_20072022.xlsx")

saveRDS(processostjam,
        "data/output/DataJud/join_DataJud_TJAM_com filtros_20072022.rds")

## 3.4. TJMT ---------------------------------------------------------------

write.csv(processostjmt,
          "data/output/DataJud/join_DataJud_TJMT_com filtros_19082022.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

writexl::write_xlsx(processostjmt,
                    "data/output/DataJud/join_DataJud_TJMT_com filtros_19082022.xlsx")

saveRDS(processostjmt,
        "data/output/DataJud/join_DataJud_TJMT_com filtros_19082022.rds")

## 3.5. TJPA ---------------------------------------------------------------

write.csv(processostjpa,
          "data/output/DataJud/join_DataJud_TJPA_com filtros_18072022.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

writexl::write_xlsx(processostjpa,
                    "data/output/DataJud/join_DataJud_TJPA_com filtros_18072022.xlsx")

saveRDS(processostjpa,
        "data/output/DataJud/join_DataJud_TJPA_com filtros_18072022.rds")

## 3.6. TJRO ---------------------------------------------------------------


## 3.7. TRF1 ---------------------------------------------------------------

write.csv(processostrf1,
          "data/output/DataJud/join_DataJud_TRF1_com filtros_25072022.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

writexl::write_xlsx(processostrf1,
                    "data/output/DataJud/join_DataJud_TRF1_com filtros_25072022.xlsx")

saveRDS(processostrf1,
        "data/output/DataJud/join_DataJud_TRF1_com filtros_25072022.rds")


