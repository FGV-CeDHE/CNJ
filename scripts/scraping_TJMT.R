
## TÍTULO: WEB SCRAPING DA JURISPRUDÊNCIA DO TJMT
## DATA: 17/05/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(abjutils)
library(RSelenium)
library(httr)

## OBJETIVOS

#'         - Realizar raspagem de dados da jurisprudência 
#'           disponibilizada no site https://jurisprudencia.tjmt.jus.br/catalogo.

#'         - Coleta do máximo possível de metadados e documentos.

#'         - PALAVRAS-CHAVE: 1. Ementa; 2. Lei e 3. Documento.

#'         - INFORMAÇÕES IMPORTANTES: 
        
#'           1. Identificador Único;
#'           2. Partes;
#'           3. Ementa ou Inteiro Teor;
#'           4. URL;
#'           5. Órgão Julgador;
#'           6. Data de Publicação.

#'         - PERÍODO: 
       
#'           1. Processos publicados entre 01/01/2017 até 15/06/2022.

## FUNÇÕES

source("functions/windowSwitch.R", 
       encoding = "UTF-8")

source("functions/wordFilter.R", 
       encoding = "UTF-8")

# 1. Ambiente -------------------------------------------------------------

## URL de referência

url_base <- "https://jurisprudencia.tjmt.jus.br/catalogo"

## Preparando o navegador

driver <- rsDriver(chromever = "103.0.5060.53", ## Versão varia a depender do computador.
                  browser = "chrome", 
                  port = as.integer(4492), 
                  check = TRUE)

## Ativando o navegador

remDr <- driver[["client"]]

# 2. Raspagem dos dados ---------------------------------------------------

## Acessando a url salva

remDr$navigate(url_base)

## Procurando a seção de Busca Avançada

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="btnConsultaAcordao"]/app-busca-jurisprudencia-catalogo/form/div/div[2]/div/button')$clickElement()

## Procurando a seção para 
## consulta de Acórdãos e Decisões Judiciais

webElem00 <- remDr$findElement(using = "xpath", 
                               '//*[@id="txtBusca"]')

## Informando quais as palavras-chave

webElem00$sendKeysToElement(list("."))

## Procurando a seção "Data da Publicação"

webElem00 <- remDr$findElement(using = "xpath", 
                               '//*[@id="btnConsultaAcordao"]/app-busca-jurisprudencia-catalogo/form/div/p-dialog/div/div[2]/div[2]/div/div/div[1]/div[2]/div/p-calendar[1]/span/input')

## Informando qual a data de referência

webElem00$sendKeysToElement(list("01/01/2017"))

webElem00 <- remDr$findElement(using = "xpath",
                               '//*[@id="btnConsultaAcordao"]/app-busca-jurisprudencia-catalogo/form/div/p-dialog/div/div[2]/div[2]/div/div/div[1]/div[2]/div/p-calendar[1]/span/button')$clickElement()

webElem00 <- remDr$findElement(using = "xpath", 
                               '//*[@id="btnConsultaAcordao"]/app-busca-jurisprudencia-catalogo/form/div/p-dialog/div/div[2]/div[2]/div/div/div[1]/div[2]/div/p-calendar[2]/span/input')

## Informando qual a data de referência

webElem00$sendKeysToElement(list("15/06/2022"))

webElem00 <- remDr$findElement(using = "xpath",
                               '//*[@id="btnConsultaAcordao"]/app-busca-jurisprudencia-catalogo/form/div/p-dialog/div/div[2]/div[2]/div/div/div[1]/div[2]/div/p-calendar[2]/span/button')$clickElement()

## Voltando a página principal

webElement00 <- remDr$findElement(using = "xpath",
                                  '/html/body/app-root/main/app-catalogo/div[1]/div/div[1]/div/div/app-busca-jurisprudencia-catalogo/form/div/p-dialog/div/div[1]/span/p-header/button')$clickElement()

## Procurando o botão 'Pesquisar'

webElem00 <- remDr$findElement(using = "xpath", 
                               '//*[@id="btnPesquisar"]')$clickElement()

## 346.109 resultados de Acórdão. 
## 493.877 resultados de Decisões Monocráticas.

## 12 informações disponíveis: 

#'      1. Julgado em; 
#'      2. Órgão Julgador;
#'      3. Classe Feito; 
#'      4. Ação; 
#'      5. Assunto; 
#'      6. Publicado em; 
#'      7. Classe;
#'      8. Relator; 
#'      9. Tipo de Processo; 
#'      10. Tipo de Julgamento;
#'      11. Ementa; e
#'      12. Inteiro Teor.

## 2.1. Acórdãos -----------------------------------------------------------

## Criando uma data frame onde os dados serão armazenados

df_final <- data.frame()

salva <- 50

## Salvando o id da página principal

main_handle <- remDr$getWindowHandles()[[1]]

## For loop que faz o download dos detalhes
## referentes ao Acórdão

for(i in 1:69222){ ## 69.222 páginas de informação.
                      ## 346.109 registros.
  
  cat("Lendo página", i, "\n")
  
  ## Rolando a página para cima
  
  webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "home"))
  
  ## Criando um data frame para armazenar os dados
  ## encontrados em cada página
  
  df_pagina <- data.frame()
  
  ## URL da página atual
  
  URL_Pagina <- remDr$getCurrentUrl()[[1]]
  
  ## Separando em blocos
  
  foo <- data.frame(do.call('rbind', strsplit(as.character(URL_Pagina),
                                              '&',
                                              fixed = TRUE)))
  
  ## Coletando o número da página
  
  Pagina <- parse_number(foo$X4)
  
  ## Tempo de intervalo entre um 
  ## processamento e outro.
  
  Sys.sleep(10)
  
  ## Verificando se a página carregou
  
  disponivel <- NA

  disponivel <- tryCatch({
    suppressMessages({
  
  disponivel <- remDr$findElement('xpath', 
                                  "/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[1]/article/header/div[1]/span[2]")$isElementDisplayed()[[1]]
  
  disponivel })}, error = function(e) {print("erro")
    FALSE})
  
  if(disponivel == "FALSE"){
    
    cat("Atualizando a página")
    
    remDr$refresh()
    
  }
  
  for(j in 1:5){ ## 05 resultados por página.
    
    cat("Resultado", j, "\n")

    ## Variável "ID"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt1 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt1) && tentativas < 10){
      
      webElemtxt1 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'ID'", "\n")
          
          ## Informa o xpath 
          
          webElem1 <- remDr$findElement('xpath', 
                                        paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                               j,
                                               "]/article/header/div[1]/span[2]"))
          
          ## Coleta o texto
          
          webElemtxt1 <- webElem1$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt1 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      ID <- unlist(webElemtxt1)
      
      ## Muda o formato da variável para character
      
      ID <- as.character(ID)
      
    }
    
    ## Variável "JULGADO EM"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt2 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
     
    tentativas <- 0
    
    while(is.na(webElemtxt2) && tentativas < 10){
      
      webElemtxt2 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Julgado em'", "\n")
          
          ## Informa o xpath 
          
          webElem2 <- remDr$findElement('xpath', 
                                        paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                        j,
                                        "]/article/div/div[1]/div/div/div[1]/div"))
          
          ## Coleta o texto
          
          webElemtxt2 <- webElem2$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt2 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt2 <- as.character(unlist(webElemtxt2))
      
      ## Deixando somente a informação que interessa
      
     # JulgadoEm <- sub('.*: ', '', JulgadoEm)
      
    }
    
    ## Variável "PUBLICADO EM"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt3 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt3) && tentativas < 10){
      
      webElemtxt3 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Publicado em'", "\n")
          
          ## Informa o xpath 
          
          webElem3 <- remDr$findElement('xpath', 
                                        paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                               j,
                                               "]/article/div/div[1]/div/div/div[2]/div"))
          
          ## Coleta o texto
          
          webElemtxt3 <- webElem3$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt3 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt3 <- as.character(unlist(webElemtxt3))
      
      ## Deixando somente a informação que interessa
      
      #PublicadoEm <- sub('.*: ', '', PublicadoEm)
      
    }
    
    ## Variável "ÓRGÃO JULGADOR"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt4 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt4) && tentativas < 10){
      
      webElemtxt4 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Órgão Julgador'", "\n")
          
          ## Informa o xpath 
          
          webElem4 <- remDr$findElement('xpath', 
                                        paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                               j,
                                               "]/article/div/div[1]/div/div/div[3]/div"))
          
          ## Coleta o texto
          
          webElemtxt4 <- webElem4$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt4 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt4 <- as.character(unlist(webElemtxt4))
      
      ## Deixando somente a informação que interessa
      
    #  OrgaoJulgador <- sub('.*: ', '', OrgaoJulgador)
      
    }
    
    ## Variável "CLASSE"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt5 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt5) && tentativas < 10){
      
      webElemtxt5 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Classe'", "\n")
          
          ## Informa o xpath 
          
          webElem5 <- remDr$findElement('xpath', 
                                        paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                               j,
                                               "]/article/div/div[1]/div/div/div[4]/div"))
          
          ## Coleta o texto
          
          webElemtxt5 <- webElem5$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt5 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt5 <- as.character(unlist(webElemtxt5))
      
      ## Deixando somente a informação que interessa
      
      #Classe <- sub('.*: ', '', Classe)
      
    }
  
    ## Variável "CLASSE FEITO"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt6 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt6) && tentativas < 10){
      
      webElemtxt6 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Classe Feito'", "\n")
          
          ## Informa o xpath 
          
          webElem6 <- remDr$findElement('xpath', 
                                        paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                               j,
                                               "]/article/div/div[1]/div/div/div[5]/div"))
          
          ## Coleta o texto
          
          webElemtxt6 <- webElem6$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt6 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt6 <- as.character(unlist(webElemtxt6))
      
      ## Deixando somente a informação que interessa
      
      #ClasseFeito <- sub('.*: ', '', ClasseFeito)
      
    }
    
    ## Variável "RELATOR"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt7 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt7) && tentativas < 10){
      
      webElemtxt7 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Relator'", "\n")
          
          ## Informa o xpath 
          
          webElem7 <- remDr$findElement('xpath', 
                                        paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                               j,
                                               "]/article/div/div[1]/div/div/div[6]/div"))
          
          ## Coleta o texto
          
          webElemtxt7 <- webElem7$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt7 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt7 <- as.character(unlist(webElemtxt7))
      
      ## Deixando somente a informação que interessa
      
      #Relator <- sub('.*: ', '', Relator)
      
    }
    
    ## Variável "Ação"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt8 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt8) && tentativas < 10){
      
      webElemtxt8 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Ação'", "\n")
          
          ## Informa o xpath 
          
          webElem8 <- remDr$findElement('xpath', 
                                        paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                               j,
                                               "]/article/div/div[1]/div/div/div[7]/div"))
          
          ## Coleta o texto
          
          webElemtxt8 <- webElem8$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt8 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt8 <- as.character(unlist(webElemtxt8))
      
      ## Deixando somente a informação que interessa
      
     # Acao <- sub('.*: ', '', Acao)
      
    }
    
    ## Variável "TIPO DO PROCESSO"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt9 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt9) && tentativas < 10){
      
      webElemtxt9 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Tipo do Processo'", "\n")
          
          ## Informa o xpath 
          
          webElem9 <- remDr$findElement('xpath', 
                                        paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                               j,
                                               "]/article/div/div[1]/div/div/div[8]/div"))
          
          ## Coleta o texto
          
          webElemtxt9 <- webElem9$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt9 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt9 <- as.character(unlist(webElemtxt9))
      
      ## Deixando somente a informação que interessa
      
     # TipoProcesso <- sub('.*: ', '', TipoProcesso)
      
    }
    
    ## Variável "ASSUNTO"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt10 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt10) && tentativas < 10){
      
      webElemtxt10 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Assunto'", "\n")
          
          ## Informa o xpath 
          
          webElem10 <- remDr$findElement('xpath', 
                                        paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                               j,
                                               "]/article/div/div[1]/div/div/div[9]/div"))
          
          ## Coleta o texto
          
          webElemtxt10 <- webElem10$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt10 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt10 <- as.character(unlist(webElemtxt10))
      
      ## Deixando somente a informação que interessa
      
     # Assunto <- sub('.*: ', '', Assunto)
      
    }
    
    ## Variável "Tipo de Julgamento"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt11 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt11) && tentativas < 10){
      
      webElemtxt11 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Tipo de Julgamento'", "\n")
          
          ## Informa o xpath 
          
          webElem11 <- remDr$findElement('xpath', 
                                         paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                j,
                                                "]/article/div/div[1]/div/div/div[10]/div"))
          
          ## Coleta o texto
          
          webElemtxt11 <- webElem11$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt11 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt11 <- as.character(unlist(webElemtxt11))
      
      ## Deixando somente a informação que interessa
      
     # TipoJulgamento <- sub('.*: ', '', TipoJulgamento)
      
    }
    
    ## Variável "EMENTA"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt12 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt12) && tentativas < 10){
      
      webElemtxt12 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Ementa'", "\n")
          
          ## Coleta o texto
          
          webElemtxt12 <- remDr$findElement('css selector', 
                                            paste0("body > app-root > main > app-consulta > div > div:nth-child(4) > div.resultado.ng-star-inserted > div > div > section > app-card-resultado:nth-child(",
                                                   j,
                                                   ") > article > div > section > div > div > div.tab_panel2.tab-consulta-judicial"))$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt12 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      Ementa <- as.character(unlist(webElemtxt12))
      
    }
    
    ## Variável 'URL_Ementa'

    ## Cria uma variável vazia onde as informações serão armazenadas
# 
#     webElemtxt13 <- NA
# 
#     ## Variável que contabiliza as tentativas de
#     ## coletar a informação
# 
#     tentativas <- 0
# 
#     while(is.na(webElemtxt13) && tentativas < 5){
# 
#       webElemtxt13 <- tryCatch({
#         suppressMessages({
# 
#           ## Atualizando a contagem de tentativas
# 
#           tentativas <- tentativas + 1
# 
#           new_tab <- NULL
# 
#           disponivel <- NA
# 
#           cat("Lendo 'URL_Ementa'", "\n")
# 
#           disponivel <- remDr$findElement('xpath',
#                                           paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
#                                                  j,
#                                                  "]/article/header/div[2]/ul/li[1]/ul/li[1]"))$isElementDisplayed()[[1]]
# 
#           n <- 0
# 
#           while(disponivel == "FALSE" && n < 5){
# 
#             n <- n + 1
# 
#             ## Rolando a página para baixo
# 
#             webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "down_arrow"))
# 
#             ## Informa o xpath
# 
#             webElem13 <- remDr$findElement('xpath',
#                                            paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
#                                                   j,
#                                                   "]/article/header/div[2]/div[2]"))$clickElement()
# 
#             disponivel <- remDr$findElement('xpath',
#                                             paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
#                                                    j,
#                                                    "]/article/header/div[2]/ul/li[1]/ul/li[1]"))$isElementDisplayed()[[1]]
# 
#           }
# 
#             ## Escolhendo o 'PDF'
# 
#             webElem13 <- remDr$findElement('xpath',
#                                            paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
#                                                   j,
#                                                   "]/article/header/div[2]/ul/li[1]/ul/li[1]"))$clickElement()
# 
#             ## Mudando para a nova página
# 
#             new_tab <- remDr$getWindowHandles()[[2]]
# 
#             if(length(new_tab) == 1){
# 
#             myswitch(remDr, new_tab)
# 
#             webElemtxt13 <- remDr$getCurrentUrl()[[1]]
# 
#             }
#           ## Mensagem e atribuição no caso de erro
# 
#           webElemtxt13 })}, error = function(e) {print("erro")
#             NA})
# 
#       ## Salvando na variável URL_Ementa
# 
#       URL_Ementa <- webElemtxt13
# 
#     }
# 
#     if(length(new_tab) == 1){
# 
#       ## Fechando a janela
# 
#       remDr$closeWindow()
# 
#       ## Mudando para janela principal
# 
#       myswitch(remDr, main_handle)
# 
#     }

    ## INTEIRO TEOR
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt14 <- NA

    ## Variável que contabiliza as tentativas de
    ## coletar a informação

    tentativas <- 0

     while(is.na(webElemtxt14) && tentativas < 10){

      webElemtxt14 <- tryCatch({
        suppressMessages({

          ## Atualizando a contagem de tentativas

          tentativas <- tentativas + 1
          
          disponivel <- NA

          cat("Lendo 'Inteiro Teor'", "\n")
          
          disponivel <- remDr$findElement('css selector',
                                          paste0("body > app-root > main > app-consulta > div > div:nth-child(4) > div.resultado.ng-star-inserted > div > div > section > app-card-resultado:nth-child(",
                                                 j,
                                                 ") > article > div > section > div > div > div.tab_panel3.tab-consulta-judicial.ng-star-inserted"))$isElementDisplayed()[[1]]

 
          while(disponivel == "FALSE"){
            
            ## Rolando a página para baixo
            
            webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "down_arrow"))

            ## Atualizando a aba para o 'INTEIRO TEOR'
  
            webElem14 <- remDr$findElement('xpath',
                                           paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                  j,
                                                  "]/article/header/div[2]/div[1]/ul/li[2]"))$clickElement()
            
            disponivel <- remDr$findElement('css selector',
                                            paste0("body > app-root > main > app-consulta > div > div:nth-child(4) > div.resultado.ng-star-inserted > div > div > section > app-card-resultado:nth-child(",
                                                   j,
                                                   ") > article > div > section > div > div > div.tab_panel3.tab-consulta-judicial.ng-star-inserted"))$isElementDisplayed()[[1]]
          
          }
          
          ## Coleta o texto
          
          webElemtxt14 <- remDr$findElement('css selector',
                                            paste0("body > app-root > main > app-consulta > div > div:nth-child(4) > div.resultado.ng-star-inserted > div > div > section > app-card-resultado:nth-child(",
                                                   j,
                                                   ") > article > div > section > div > div > div.tab_panel3.tab-consulta-judicial.ng-star-inserted"))$getElementText()

          ## Mensagem e atribuição no caso de erro

          webElemtxt14 })}, error = function(e) {print("Concluído")
            NA})
      
      ## Extrai a informação coletada
      
      # InteiroTeor <- as.character(unlist(webElemtxt14))

     }
    
    ## Variável 'URL_InteiroTeor'
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt15 <- NA
    
    ## Variável que contabiliza as tentativas de
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt15) && tentativas < 5){
      
      webElemtxt15 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          new_tab <- NULL
          
          disponivel <- NA
          
          cat("Lendo 'URL_InteiroTeor'", "\n")
          
          disponivel <- remDr$findElement('xpath',
                                          paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                 j,
                                                 "]/article/header/div[2]/ul/li[1]/ul/li[1]"))$isElementDisplayed()[[1]]
          
          n <- 0
          
          while(disponivel == "FALSE" && n < 5){
            
            n <- n + 1
            
            ## Rolando a página para baixo
            
            ## Rolando a página para baixo
            
            webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "down_arrow"))
            
            ## Informa o xpath
            
            webElem15 <- remDr$findElement('xpath',
                                           paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                  j,
                                                  "]/article/header/div[2]/div[2]"))$clickElement()
            
            disponivel <- remDr$findElement('xpath',
                                            paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                   j,
                                                   "]/article/header/div[2]/ul/li[1]/ul/li[1]"))$isElementDisplayed()[[1]]
            
          }
            
           ## Escolhendo o 'PDF'
            
           webElem15 <- remDr$findElement('xpath',
                                           paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                  j,
                                                  "]/article/header/div[2]/ul/li[1]/ul/li[1]"))$clickElement()
            
            ## Mudando para a nova página
            
            new_tab <- unlist(remDr$getWindowHandles())[unlist(remDr$getWindowHandles()) != main_handle]
            
            if(length(new_tab) == 1){
            
            myswitch(remDr, new_tab)
            
            webElemtxt15 <- remDr$getCurrentUrl()[[1]]
            
            }
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt15 })}, error = function(e) {print("erro")
            NA})
      
      ## Salvando na variável URL_InteiroTeor
      
      URL_InteiroTeor <- webElemtxt15
      
    }
      
    if(length(new_tab) == 1){
    
      ## Fechando a janela
      
      remDr$closeWindow()
      
      ## Mudando para janela principal
      
      myswitch(remDr, main_handle)
      
      }
    
    ## Atribui ao data frame criado a informação extraÍda 
    ## referente ao Inquérito Civil
    
    df_linha <- data.frame(Pagina, ID, webElemtxt2, webElemtxt3, webElemtxt4, 
                           webElemtxt5, webElemtxt6, webElemtxt7, webElemtxt8, 
                           webElemtxt9, webElemtxt10, webElemtxt11,
                           Ementa, URL_InteiroTeor,
                           stringsAsFactors = F)
    
    ## Empilha todos os Inquéritos Civis encontrados na página 
    ## em um úncio dataframe
    
    df_pagina <- bind_rows(df_pagina, df_linha)
    
  }
  
  ## Empilha todos os Inquéritos Civis em um banco final
  
  df_final <- bind_rows(df_final, df_pagina)
  
  ## Rolando a página para baixo
  
  webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "end"))
  webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "end"))
  
  ## Salvando o xpath da função "próxima página"
  
  webElem00 <- remDr$findElement(using = "css selector", 
                                "body > app-root > main > app-consulta > div > div:nth-child(5) > div > app-paginacao > ul > li.botao-paginacao.proximo > span")$clickElement()
  
  ## Rolando a página para cima
  
  webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "home"))
  
if(i >= salva){ 
  
  saveRDS(df_final,
          "CeDHE/data/output/TJMT/acórdãos_tjmt_10082022_temp.rds")
  
  salva <- salva + 50
  
}
  
}

## Alterando o diretório

setwd("C:/Users/beca_/OneDrive - usp.br/Documentos/CeDHE")

## Salvando o banco

saveRDS(df_final,
        "data/output/TJMT/acórdãos_tjmt_10082022_temp.rds")

# 2.2. Decisões Monocráticas ----------------------------------------------

## Criando uma data frame onde os dados serão armazenados

df_final <- data.frame()

## Procurando a aba das 'Decisões Monocráticas'

webElem00 <- remDr$findElement(using = "xpath",
                               '//*[@id="Decisao"]')

## Clicando no botão

webElem00$clickElement()

## For loop que faz o download dos detalhes
## referentes ao Acórdão

for(i in 1:1){ ## 2.115 páginas de informação. 
               ## 10.575 registros.
  
  cat("Lendo página", i, "\n")
    
    ## Criando um data frame para armazenar os dados
    ## encontrados em cada página
    
    df_pagina <- data.frame()
    
    ## Tempo de intervalo entre um 
    ## processamento e outro.
    
    Sys.sleep(10)
    
    for(j in 1:5){ ## 05 resultados por página.
      
      cat("Resultado", j, "\n")
    
      ## Variável "ID"
      
      ## Cria uma variável vazia onde as informações serão armazenadas
      
      webElemtxt1 <- NA
      
      ## Variável que contabiliza as tentativas de 
      ## coletar a informação
      
      tentativas <- 0
      
      while(is.na(webElemtxt1) && tentativas < 10){
        
        webElemtxt1 <- tryCatch({
          suppressMessages({
            
            ## Atualizando a contagem de tentativas
            
            tentativas <- tentativas + 1
            
            cat("Lendo 'ID'", "\n")
            
            ## Informa o xpath 
            
            webElem1 <- remDr$findElement('xpath', 
                                          paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                 j,
                                                 "]/article/header/div[1]/span[2]"))
            
            ## Coleta o texto
            
            webElemtxt1 <- webElem1$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt1 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        ID <- unlist(webElemtxt1)
        
        ## Muda o formato da variável para character
        
        ID <- as.character(ID)
        
        ## Intervalo entre um processamento e outro
        
        Sys.sleep(0)
        
      }
      
      ## Variável "JULGADO EM"
      
      ## Cria uma variável vazia onde as informações serão armazenadas
      
      webElemtxt2 <- NA
      
      ## Variável que contabiliza as tentativas de 
      ## coletar a informação
      
      tentativas <- 0
      
      while(is.na(webElemtxt2) && tentativas < 10){
        
        webElemtxt2 <- tryCatch({
          suppressMessages({
            
            ## Atualizando a contagem de tentativas
            
            tentativas <- tentativas + 1
            
            cat("Lendo 'Julgado em'", "\n")
            
            ## Informa o xpath 
            
            webElem2 <- remDr$findElement('xpath', 
                                          paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                 j,
                                                 "]/article/div/div[1]/div/div/div[1]/div"))
            
            ## Coleta o texto
            
            webElemtxt2 <- webElem2$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt2 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        JulgadoEm <- unlist(webElemtxt2)
        
        ## Muda o formato da variável para character
        
        JulgadoEm <- as.character(JulgadoEm)
        
        ## Deixando somente a informação que interessa
        
        # JulgadoEm <- sub('.*: ', '', JulgadoEm)
        
        ## Intervalo entre um processamento e outro
        
        Sys.sleep(0)
        
      }
      
      ## Variável "PUBLICADO EM"
      
      ## Cria uma variável vazia onde as informações serão armazenadas
      
      webElemtxt3 <- NA
      
      ## Variável que contabiliza as tentativas de 
      ## coletar a informação
      
      tentativas <- 0
      
      while(is.na(webElemtxt3) && tentativas < 10){
        
        webElemtxt3 <- tryCatch({
          suppressMessages({
            
            ## Atualizando a contagem de tentativas
            
            tentativas <- tentativas + 1
            
            cat("Lendo 'Publicado em'", "\n")
            
            ## Informa o xpath 
            
            webElem3 <- remDr$findElement('xpath', 
                                          paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                 j,
                                                 "]/article/div/div[1]/div/div/div[2]/div"))
            
            ## Coleta o texto
            
            webElemtxt3 <- webElem3$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt3 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        PublicadoEm <- unlist(webElemtxt3)
        
        ## Muda o formato da variável para character
        
        PublicadoEm <- as.character(PublicadoEm)
        
        ## Deixando somente a informação que interessa
        
        # PublicadoEm <- sub('.*: ', '', PublicadoEm)
        
        ## Intervalo entre um processamento e outro
        
        Sys.sleep(0)
        
      }
      
      ## Variável "ÓRGÃO JULGADOR"
      
      ## Cria uma variável vazia onde as informações serão armazenadas
      
      webElemtxt4 <- NA
      
      ## Variável que contabiliza as tentativas de 
      ## coletar a informação
      
      tentativas <- 0
      
      while(is.na(webElemtxt4) && tentativas < 10){
        
        webElemtxt4 <- tryCatch({
          suppressMessages({
            
            ## Atualizando a contagem de tentativas
            
            tentativas <- tentativas + 1
            
            cat("Lendo 'Órgão Julgador'", "\n")
            
            ## Informa o xpath 
            
            webElem4 <- remDr$findElement('xpath', 
                                          paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                 j,
                                                 "]/article/div/div[1]/div/div/div[3]/div"))
            
            ## Coleta o texto
            
            webElemtxt4 <- webElem4$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt4 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        OrgaoJulgador <- unlist(webElemtxt4)
        
        ## Muda o formato da variável para character
        
        OrgaoJulgador <- as.character(OrgaoJulgador)
        
        ## Deixando somente a informação que interessa
        
        # OrgaoJulgador <- sub('.*: ', '', OrgaoJulgador)
        
        ## Intervalo entre um processamento e outro
        
        Sys.sleep(0)
        
      }
      
      ## Variável "CLASSE"
      
      ## Cria uma variável vazia onde as informações serão armazenadas
      
      webElemtxt5 <- NA
      
      ## Variável que contabiliza as tentativas de 
      ## coletar a informação
      
      tentativas <- 0
      
      while(is.na(webElemtxt5) && tentativas < 10){
        
        webElemtxt5 <- tryCatch({
          suppressMessages({
            
            ## Atualizando a contagem de tentativas
            
            tentativas <- tentativas + 1
            
            cat("Lendo 'Classe'", "\n")
            
            ## Informa o xpath 
            
            webElem5 <- remDr$findElement('xpath', 
                                          paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                 j,
                                                 "]/article/div/div[1]/div/div/div[4]/div"))
            
            ## Coleta o texto
            
            webElemtxt5 <- webElem5$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt5 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        Classe <- unlist(webElemtxt5)
        
        ## Muda o formato da variável para character
        
        Classe <- as.character(Classe)
        
        ## Deixando somente a informação que interessa
        
        # Classe <- sub('.*: ', '', Classe)
        
        ## Intervalo entre um processamento e outro
        
        Sys.sleep(0)
        
      }
      
      ## Variável "CLASSE FEITO"
      
      ## Cria uma variável vazia onde as informações serão armazenadas
      
      webElemtxt6 <- NA
      
      ## Variável que contabiliza as tentativas de 
      ## coletar a informação
      
      tentativas <- 0
      
      while(is.na(webElemtxt6) && tentativas < 10){
        
        webElemtxt6 <- tryCatch({
          suppressMessages({
            
            ## Atualizando a contagem de tentativas
            
            tentativas <- tentativas + 1
            
            cat("Lendo 'Classe Feito'", "\n")
            
            ## Informa o xpath 
            
            webElem6 <- remDr$findElement('xpath', 
                                          paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                 j,
                                                 "]/article/div/div[1]/div/div/div[5]/div"))
            
            ## Coleta o texto
            
            webElemtxt6 <- webElem6$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt6 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        ClasseFeito <- unlist(webElemtxt6)
        
        ## Muda o formato da variável para character
        
        ClasseFeito <- as.character(ClasseFeito)
        
        ## Deixando somente a informação que interessa
        
        # ClasseFeito <- sub('.*: ', '', ClasseFeito)
        
        ## Intervalo entre um processamento e outro
        
        Sys.sleep(0)
        
      }
      
      ## Variável "RELATOR"
      
      ## Cria uma variável vazia onde as informações serão armazenadas
      
      webElemtxt7 <- NA
      
      ## Variável que contabiliza as tentativas de 
      ## coletar a informação
      
      tentativas <- 0
      
      while(is.na(webElemtxt7) && tentativas < 10){
        
        webElemtxt7 <- tryCatch({
          suppressMessages({
            
            ## Atualizando a contagem de tentativas
            
            tentativas <- tentativas + 1
            
            cat("Lendo 'Relator'", "\n")
            
            ## Informa o xpath 
            
            webElem7 <- remDr$findElement('xpath', 
                                          paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                 j,
                                                 "]/article/div/div[1]/div/div/div[6]/div"))
            
            ## Coleta o texto
            
            webElemtxt7 <- webElem7$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt7 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        Relator <- unlist(webElemtxt7)
        
        ## Muda o formato da variável para character
        
        Relator <- as.character(Relator)
        
        ## Deixando somente a informação que interessa
        
        # Relator <- sub('.*: ', '', Relator)
        
        ## Intervalo entre um processamento e outro
        
        Sys.sleep(0)
        
      }
      
      ## Variável "Ação"
      
      ## Cria uma variável vazia onde as informações serão armazenadas
      
      webElemtxt8 <- NA
      
      ## Variável que contabiliza as tentativas de 
      ## coletar a informação
      
      tentativas <- 0
      
      while(is.na(webElemtxt8) && tentativas < 10){
        
        webElemtxt8 <- tryCatch({
          suppressMessages({
            
            ## Atualizando a contagem de tentativas
            
            tentativas <- tentativas + 1
            
            cat("Lendo 'Ação'", "\n")
            
            ## Informa o xpath 
            
            webElem8 <- remDr$findElement('xpath', 
                                          paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                 j,
                                                 "]/article/div/div[1]/div/div/div[7]/div"))
            
            ## Coleta o texto
            
            webElemtxt8 <- webElem8$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt8 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        Acao <- unlist(webElemtxt8)
        
        ## Muda o formato da variável para character
        
        Acao <- as.character(Acao)
        
        ## Deixando somente a informação que interessa
        
        # Acao <- sub('.*: ', '', Acao)
        
        ## Intervalo entre um processamento e outro
        
        Sys.sleep(0)
        
      }
      
      ## Variável "TIPO DO PROCESSO"
      
      ## Cria uma variável vazia onde as informações serão armazenadas
      
      webElemtxt9 <- NA
      
      ## Variável que contabiliza as tentativas de 
      ## coletar a informação
      
      tentativas <- 0
      
      while(is.na(webElemtxt9) && tentativas < 10){
        
        webElemtxt9 <- tryCatch({
          suppressMessages({
            
            ## Atualizando a contagem de tentativas
            
            tentativas <- tentativas + 1
            
            cat("Lendo 'Tipo do Processo'", "\n")
            
            ## Informa o xpath 
            
            webElem9 <- remDr$findElement('xpath', 
                                          paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                 j,
                                                 "]/article/div/div[1]/div/div/div[8]/div"))
            
            ## Coleta o texto
            
            webElemtxt9 <- webElem9$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt9 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        TipoProcesso <- unlist(webElemtxt9)
        
        ## Muda o formato da variável para character
        
        TipoProcesso <- as.character(TipoProcesso)
        
        ## Deixando somente a informação que interessa
        
        # TipoProcesso <- sub('.*: ', '', TipoProcesso)
        
        ## Intervalo entre um processamento e outro
        
        Sys.sleep(0)
        
      }
      
      ## Variável "ASSUNTO"
      
      ## Cria uma variável vazia onde as informações serão armazenadas
      
      webElemtxt10 <- NA
      
      ## Variável que contabiliza as tentativas de 
      ## coletar a informação
      
      tentativas <- 0
      
      while(is.na(webElemtxt10) && tentativas < 10){
        
        webElemtxt10 <- tryCatch({
          suppressMessages({
            
            ## Atualizando a contagem de tentativas
            
            tentativas <- tentativas + 1
            
            cat("Lendo 'Assunto'", "\n")
            
            ## Informa o xpath 
            
            webElem10 <- remDr$findElement('xpath', 
                                           paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                  j,
                                                  "]/article/div/div[1]/div/div/div[9]/div"))
            
            ## Coleta o texto
            
            webElemtxt10 <- webElem10$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt10 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        Assunto <- unlist(webElemtxt10)
        
        ## Muda o formato da variável para character
        
        Assunto <- as.character(Assunto)
        
        ## Deixando somente a informação que interessa
        
        # Assunto <- sub('.*: ', '', Assunto)
        
        ## Intervalo entre um processamento e outro
        
        Sys.sleep(0)
        
      }
      
      ## Variável "Tipo de Julgamento"
      
      ## Cria uma variável vazia onde as informações serão armazenadas
      
      webElemtxt11 <- NA
      
      ## Variável que contabiliza as tentativas de 
      ## coletar a informação
      
      tentativas <- 0
      
      while(is.na(webElemtxt11) && tentativas < 10){
        
        webElemtxt11 <- tryCatch({
          suppressMessages({
            
            ## Atualizando a contagem de tentativas
            
            tentativas <- tentativas + 1
            
            cat("Lendo 'Tipo de Julgamento'", "\n")
            
            ## Informa o xpath 
            
            webElem11 <- remDr$findElement('xpath', 
                                           paste0("/html/body/app-root/main/app-consulta/div/div[4]/div[2]/div/div/section/app-card-resultado[",
                                                  j,
                                                  "]/article/div/div[1]/div/div/div[10]/div"))
            
            ## Coleta o texto
            
            webElemtxt11 <- webElem11$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt11 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        TipoJulgamento <- unlist(webElemtxt11)
        
        ## Muda o formato da variável para character
        
        TipoJulgamento <- as.character(TipoJulgamento)
        
        ## Deixando somente a informação que interessa
        
        # TipoJulgamento <- sub('.*: ', '', TipoJulgamento)
        
        ## Intervalo entre um processamento e outro
        
        Sys.sleep(0)
        
      }
      
      ## Variável "EMENTA"
      
      ## Cria uma variável vazia onde as informações serão armazenadas
      
      webElemtxt12 <- NA
      
      ## Variável que contabiliza as tentativas de 
      ## coletar a informação
      
      tentativas <- 0
      
      while(is.na(webElemtxt12) && tentativas < 10){
        
        webElemtxt12 <- tryCatch({
          suppressMessages({
            
            ## Atualizando a contagem de tentativas
            
            tentativas <- tentativas + 1
            
            cat("Lendo 'Ementa'", "\n")
            
            ## Informa o xpath 
            
            webElem12 <- remDr$findElement('css selector', 
                                           paste0("body > app-root > main > app-consulta > div > div:nth-child(4) > div.resultado.ng-star-inserted > div > div > section > app-card-resultado:nth-child(",
                                                  j,
                                                  ") > article > div > section > div > div > div.tab_panel2.tab-consulta-judicial"))
            
            ## Coleta o texto
            
            webElemtxt12 <- webElem12$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt12 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        Ementa <- unlist(webElemtxt12)
        
        ## Muda o formato da variável para character
        
        Ementa <- as.character(Ementa)
        
        ## Intervalo entre um processamento e outro
        
        Sys.sleep(0)
        
      }
      
      ## Atribui ao data frame criado a informação extraÍda 
      ## referente ao Inquérito Civil
      
      df_linha <- data.frame(Pagina, ID, JulgadoEm, PublicadoEm, OrgaoJulgador, 
                             Classe, ClasseFeito, Relator, Acao, 
                             TipoProcesso, Assunto, TipoJulgamento,
                             Ementa, 
                             stringsAsFactors = F)
      
      ## Empilha todos os Inquéritos Civis encontrados na página 
      ## em um úncio dataframe
      
      df_pagina <- bind_rows(df_pagina, df_linha)
      
    }
    
    ## Empilha todos os Inquéritos Civis em um banco final
    
    df_final <- bind_rows(df_final, df_pagina)
    
    ## Salvando o xpath da função "próxima página"
    
    webElem00 <- remDr$findElement(using = "css selector", 
                                   "body > app-root > main > app-consulta > div > div:nth-child(3) > div > app-paginacao > ul > li.botao-paginacao.proximo > span")
    
    ## Clicando no botão
    
    webElem00$clickElement()
    
    ## Rolando a página para cima
    
    webElem00 <- remDr$findElement("css", "body")
    
    webElem00$sendKeysToElement(list(key = "home"))
    
}

## Alterando o diretório

setwd("C:/Users/beca_/OneDrive - usp.br/Documentos/CeDHE")

## Salvando o banco

saveRDS(df_final,
        "data/output/decisões mon_tjmt_30052022_temp.rds")

# 3. Limpeza --------------------------------------------------------------

## 3.1. Acórdãos -----------------------------------------------------------

df_final <- readRDS("data/output/TJMT/acórdãos_tjmt_10082022_temp.rds")

## Quebrando as strings em várias colunas

df_final <- df_final %>% 
  separate(webElemtxt2,
           sep = "\\: ",
           into = c("webElemtxt2_1",
                    "webElemtxt2")) %>% 
  separate(webElemtxt3,
           sep = "\\: ",
           into = c("webElemtxt3_1",
                    "webElemtxt3")) %>% 
  separate(webElemtxt4,
           sep = "\\: ",
           into = c("webElemtxt4_1",
                    "webElemtxt4")) %>% 
  separate(webElemtxt5,
           sep = "\\: ",
           into = c("webElemtxt5_1",
                    "webElemtxt5")) %>% 
  separate(webElemtxt6,
           sep = "\\: ",
           into = c("webElemtxt6_1",
                    "webElemtxt6")) %>% 
  separate(webElemtxt7,
           sep = "\\: ",
           into = c("webElemtxt7_1",
                    "webElemtxt7")) %>%
  separate(webElemtxt8,
           sep = "\\: ",
           into = c("webElemtxt8_1",
                    "webElemtxt8")) %>%
  separate(webElemtxt9,
           sep = "\\: ",
           into = c("webElemtxt9_1",
                    "webElemtxt9")) %>%
  separate(webElemtxt10,
           sep = "\\: ",
           into = c("webElemtxt10_1",
                    "webElemtxt10")) %>%
  separate(webElemtxt11,
           sep = "\\: ",
           into = c("webElemtxt11_1",
                    "webElemtxt11"))

## Organizando e estruturando o banco

df_final <- df_final %>%
  filter(!is.na(ID)) %>% 
  mutate(Numero = ID,
         DataJulgamento = webElemtxt2,
         DataPublicacao = webElemtxt3,
         OrgaoJulgador = webElemtxt4,
         Classe = webElemtxt5,
         ClasseFeito = ifelse(webElemtxt6_1 == "ClasseFeito",
                              webElemtxt6,
                              NA),
         Relator = ifelse(webElemtxt6_1 == "Relator",
                          webElemtxt6,
                          ifelse(webElemtxt7_1 == "Relator",
                                 webElemtxt7,
                                 NA)),
         Acao = ifelse(webElemtxt7_1 == "Ação",
                       webElemtxt7,
                       ifelse(webElemtxt8_1 == "Ação",
                              webElemtxt8,
                              NA)),
         TipoProcesso = ifelse(webElemtxt8_1 == "Tipo do Processo",
                               webElemtxt8,
                               ifelse(webElemtxt9_1 == "Tipo do Processo",
                                      webElemtxt9,
                                      NA)),
         Assunto = ifelse(webElemtxt9_1 == "Assunto",
                          webElemtxt9,
                          ifelse(webElemtxt10_1 == "Assunto",
                                 webElemtxt10,
                                 NA)),
         TipoJulgamento = ifelse(webElemtxt10_1 == "Tipo de julgamento",
                                 webElemtxt10,
                                 ifelse(webElemtxt11_1 == "Tipo de julgamento",
                                        webElemtxt10,
                                        NA))) %>% 
  mutate(DataJulgamento = as.Date(DataJulgamento,
                                  format = "%d/%m/%Y"),
         DataPublicacao = as.Date(DataPublicacao,
                                  format = "%d/%m/%Y"),
         TipoProcesso = str_to_upper(TipoProcesso),
         Relator = str_to_upper(Relator),
         Acao = str_to_upper(Acao),
         Classe = str_to_upper(Classe),
         ClasseFeito = str_to_upper(ClasseFeito),
         Assunto = str_to_upper(Assunto),
         OrgaoJulgador = str_to_upper(OrgaoJulgador),
         TipoJulgamento = str_to_upper(TipoJulgamento)) %>% 
  select(Numero,
         TipoProcesso,
         TipoJulgamento,
         Acao,
         Classe,
         ClasseFeito,
         Assunto,
         Relator,
         OrgaoJulgador,
         DataPublicacao,
         DataJulgamento,
         Ementa,
         URL_InteiroTeor) %>% 
  arrange(DataJulgamento,
          OrgaoJulgador)

## Remove os espaços em branco em excesso

for (i in colnames(df_final)){
  
  df_final[[i]] <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", 
                        df_final[[i]], 
                        perl = TRUE)
}

# 4. Filtragem ------------------------------------------------------------

## Filtrando o período de análise, removendo linhas duplicadas e
## criando coluna temporária para a busca de palavras

df_final_filtros <- df_final %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  filter(DataJulgamento >= "2018-01-01" &
         DataJulgamento <= "2022-04-30") %>% 
  unique() %>% 
  mutate(EmentaTemp = str_to_lower(Ementa),
         EmentaTemp = str_squish(EmentaTemp),
         EmentaTemp = iconv(EmentaTemp,
                            from = "UTF-8",
                            to = "ASCII//TRANSLIT"))

## Localizando as palavras-chave

df_final_filtros <- busca_palavras_chave(dataframe_final = df_final_filtros,
                                         palavras_chave = names,
                                         regex = regex,
                                         nome_coluna = 'EmentaTemp')

## Removendo a coluna de texto temporária e
## ordenando os dados

df_final_filtros <- df_final_filtros %>% 
  select(-EmentaTemp) %>% 
  arrange(DataJulgamento,
          OrgaoJulgador)

# 5. Salva ----------------------------------------------------------------

## Salva o banco original, sem filtros

saveRDS(df_final,
        "data/output/TJMT/acórdãos_TJMT_19082022.rds")

write.csv(df_final,
          "data/output/TJMT/acórdãos_TJMT_19082022.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

## Salva o banco com os filtros de período e tema

write.csv(df_final_filtros,
          "data/output/TJMT/acórdãos_TJMT_19082022_com filtros.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

saveRDS(df_final_filtros,
        "data/output/TJMT/acórdãos_TJMT_19082022_com filtros.rds")
