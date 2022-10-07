
## TÍTULO: COLETA DE CÓDIGOS E DESCRIÇÕES DAS CLASSES,
## ASSUNTOS E MOVIMENTOS.
## AUTORA: REBECA CARVALHO
## DATA: 05/06/2022

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(RCurl)
library(XML)
library(gsubfn)
library(tm)

## FUNÇÃO DE REFERÊNCIA

source("http://pastebin.com/raw.php?i=XtzN1NMs")

strdehtml <- function(s) {
  ret <- gsubfn("&#([0-9]+);", function(x) rawToChar(as.raw(as.numeric(x))), s)
  ret <- gsubfn("&([^;]+);", function(x) htmlchars[x], ret)
  return(ret)
}

# 1. Data -----------------------------------------------------------------

## Carregando o banco do DatJud

df <- readRDS("data/output/DataJud/dataJud_final_15072022.rds")

## 1.1. Classes Processuais ------------------------------------------------

## Verificando quantos movimentos distintos 
## apareceram nos dados

classes <- unique(df$ClasseProcessual)

## Carregando a função que coleta
## as descrições das classes

retorna_nome_codigo_TPU <- function(codigo, dt_frame=F){
  
  body = paste0('<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:sgt="https://www.cnj.jus.br/sgt/sgt_ws.php">
     <soapenv:Header/>
     <soapenv:Body>
        <sgt:pesquisarItemPublicoWS>
           <tipoTabela>C</tipoTabela>
           <tipoPesquisa>C</tipoPesquisa>
           <valorPesquisa>',codigo,'</valorPesquisa>
        </sgt:pesquisarItemPublicoWS>
     </soapenv:Body>
  </soapenv:Envelope>')
  
  reader = basicTextGatherer()
  
  curlPerform(
    url = "https://www.cnj.jus.br/sgt/sgt_ws.php",
    postfields = body,
    writefunction = reader$update
  )
  
  xml <- reader$value()
  
  aux <- xmlToList(XML::xmlParse(xml))
  
  data_aux = data_frame(codigo = aux$Body$pesquisarItemPublicoWSResponse$return$Item$cod_item,
                        nome = aux$Body$pesquisarItemPublicoWSResponse$return$Item$nome,
                        glossario =  aux$Body$pesquisarItemPublicoWSResponse$return$Item$dscGlossario)
  
  if(dt_frame == T) return(data_aux)
  
  return(aux$Body$pesquisarItemPublicoWSResponse$return$Item$nome)
}

## Data frame onde os dados serão
## armazenados posteriormente

cod_classes <- data_frame()

## For loop que coleta as informações

for(i in seq_along(classes)){
  
  cat("Lendo", i, "\n")
  
  
  temp <- retorna_nome_codigo_TPU(classes[i],
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

## Salvando os dados

saveRDS(cod_classes,
        "data/output/DataJud/cod_classes.rds")

writexl::write_xlsx(cod_classes,
                    "data/output/DataJud/cod_classes.xlsx")

## 1.2. Assuntos -----------------------------------------------------------

## Verificando quantos movimentos distintos 
## apareceram nos dados

assuntos <- unique(c(unlist(str_split(df$CodigoAssunto,
                                     " / "))))

## Carregando a função que coleta
## as descrições das classes

retorna_nome_codigo_TPU <- function(codigo, dt_frame=F){
  
  body = paste0('<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:sgt="https://www.cnj.jus.br/sgt/sgt_ws.php">
     <soapenv:Header/>
     <soapenv:Body>
        <sgt:pesquisarItemPublicoWS>
           <tipoTabela>A</tipoTabela>
           <tipoPesquisa>C</tipoPesquisa>
           <valorPesquisa>',codigo,'</valorPesquisa>
        </sgt:pesquisarItemPublicoWS>
     </soapenv:Body>
  </soapenv:Envelope>')
  
  reader = basicTextGatherer()
  
  curlPerform(
    url = "https://www.cnj.jus.br/sgt/sgt_ws.php",
    # httpheader = headerfields,
    postfields = body,
    writefunction = reader$update
  )
  
  xml <- reader$value()
  
  aux <- xmlToList(XML::xmlParse(xml))
  
  data_aux = data_frame(codigo = aux$Body$pesquisarItemPublicoWSResponse$return$Item$cod_item,
                        nome = aux$Body$pesquisarItemPublicoWSResponse$return$Item$nome,
                        glossario = aux$Body$pesquisarItemPublicoWSResponse$return$Item$dscGlossario)
  
  if(dt_frame==T) return(data_aux)
  
  return(aux$Body$pesquisarItemPublicoWSResponse$return$Item$nome)

  }

## Data frame onde os dados serão
## armazenados posteriormente

cod_assuntos <- data_frame()

## For loop que coleta as informações

for(i in seq_along(assuntos)){
  
  cat("Lendo", i, "\n")
  
  
  temp <- retorna_nome_codigo_TPU(assuntos[i],
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

## Carregando a função que coleta
## as descrições dos movimentos

retorna_nome_codigo_TPU <- function(codigo, dt_frame=F){
  
  body = paste0('<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:sgt="https://www.cnj.jus.br/sgt/sgt_ws.php">
     <soapenv:Header/>
     <soapenv:Body>
        <sgt:pesquisarItemPublicoWS>
           <tipoTabela>M</tipoTabela>
           <tipoPesquisa>C</tipoPesquisa>
           <valorPesquisa>',codigo,'</valorPesquisa>
        </sgt:pesquisarItemPublicoWS>
     </soapenv:Body>
  </soapenv:Envelope>')

  reader = basicTextGatherer()
  
  curlPerform(
    url = "https://www.cnj.jus.br/sgt/sgt_ws.php",
  # httpheader = headerfields,
   postfields = body,
    writefunction = reader$update
  )
  
  xml <- reader$value()
  
  aux <- xmlToList(XML::xmlParse(xml))
  
  data_aux = data_frame(codigo = aux$Body$pesquisarItemPublicoWSResponse$return$Item$cod_item,
                        nome = aux$Body$pesquisarItemPublicoWSResponse$return$Item$nome,
                        glossario = aux$Body$pesquisarItemPublicoWSResponse$return$Item$dscGlossario)
  
  if(dt_frame==T) return(data_aux)
  return(aux$Body$pesquisarItemPublicoWSResponse$return$Item$nome)
}

## Data frame onde os dados serão
## armazenados posteriormente

cod_movimentos <- data_frame()

## For loop que coleta as informações

for(i in seq_along(movimentos)){
  
  cat("Lendo", i, "\n")


  temp <- retorna_nome_codigo_TPU(movimentos[i],
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

## Salvando os dados

saveRDS(cod_movimentos,
        "data/output/DataJud/cod_movimentos.rds")

writexl::write_xlsx(cod_movimentos,
                    "data/output/DataJud/cod_movimentos.xlsx")

# 2. Join -----------------------------------------------------------------

## 2.1. Classes Processuais ------------------------------------------------

## Preparando os dados para o join

cod_classes <- cod_classes %>% 
  rename("ClasseProcessual" = "Código",
         "DescricaoClasseProcessual" = "Descrição")

df$ClasseProcessual <- as.character(df$ClasseProcessual)

## Juntando com os dados do DataJud

df <- left_join(df,
                cod_classes)

## 2.2. Assuntos -----------------------------------------------------------

## Verificando em quantas linhas há mais de um
## assunto

df <- df %>% 
  mutate(CodigoAssunto = ifelse(CodigoAssunto == "",
                                NA,
                                CodigoAssunto)) %>% 
  mutate(NumAssuntos = count.fields(textConnection(CodigoAssunto),
                                            sep = "/"))

## Salvando em outro banco as linhas com mais de um assunto

df2 <- df %>% 
  filter(NumAssuntos > 1)

## Filtrando somente as linhas com apenas um assunto

df <- df %>% 
  filter(NumAssuntos == 1)

## Preparando os dados para o join

cod_assuntos <- cod_assuntos %>% 
  rename("CodigoAssunto" = "Código",
         "DescricaoAssunto" = "Descrição")

## Juntando os dados

df <- left_join(df,
                cod_assuntos) %>% 
  mutate(Assunto = paste0(CodigoAssunto,
                          " - ",
                          DescricaoAssunto))

## Vetor onde será armazenado o texto
## da nova variável assunto

texto <- list()

df2$Assunto <- NA

## For loop que faz o match entre código e descrição do assunto

for(i in 1:nrow(df2)){
  
  cat("Lendo", i, "\n")
  
  assuntos <- unlist(str_split(df2$CodigoAssunto[i],
                               " / "))
  
  texto <- list()
  
  unlist <- tryCatch({
    suppressMessages({
      
      for(j in seq_along(assuntos)){
        
        cat("Assunto", j, "\n")
    
        temp <- cod_assuntos %>% 
          filter(CodigoAssunto == assuntos[j])
    
        texto[[j]] <- paste0(assuntos[j],
                        " - ",
                        temp$DescricaoAssunto)
 }
      
      df2$Assunto[i] <- paste0(unlist(texto),
                              collapse = " / ")
    
    unlist })}, error = function(e) {
      NA})
  
  
}

## Organizando os dados

df <- df %>% 
  mutate(ClasseProcessual2 = paste0(ClasseProcessual,
                                    " - ",
                                    DescricaoClasseProcessual)) %>% 
  select(-ClasseProcessual) %>% 
  rename("ClasseProcessual" = "ClasseProcessual2") %>% 
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
         Assunto,
         Grau,
         Competencia,
         ValorCausa,
         TamanhoProcesso) %>%
  mutate(ClasseProcessual = str_to_upper(ClasseProcessual),
         Assunto = str_to_upper(Assunto)) %>% 
  arrange(UF, NomeMunicipio, DataAjuizamento)

## Salvando o banco final

saveRDS(df,
        "data/output/DataJud/dataJud_final_15072022.rds")
