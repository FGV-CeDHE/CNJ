
## TÍTULO: WEB SCRAPING DA JURISPRUDÊNCIA DO STJ
## DATA: 10/06/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(abjutils)
library(RSelenium)
library(httr)
library(tm)

## OBJETIVOS

#'         - Realizar raspagem de dados da jurisprudência 
#'           disponibilizada no site https://scon.stj.jus.br/SCON/.

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

#'           1. Processos julgados entre 01/01/2017 e 15/06/2022.

## DIRETÓRIO

setwd("CNJ")


## FUNÇÕES

source("functions/windowSwitch.R", 
       encoding = "UTF-8")

source("functions/wordFilter.R", 
       encoding = "UTF-8")

# 1. Ambiente -------------------------------------------------------------

## URL de referência

url_base <- "https://scon.stj.jus.br/SCON/"

## Funções extras do Chrome

eCaps <- list(chromeOptions = list(
  args = c('--no-sandbox', 
           '--disable-gpu', 
           '--start-maximized',
           '--disable-popup-blocking',
           '--disable-extensions',
           '--disable-blink-features=AutomationControlled'),
  prefs = list('plugins.always_open_pdf_externally' = TRUE,
               'profile.default_content_settings.popups' = 0L,
               'download.prompt_for_download' = FALSE,
               'directory_upgrade' = TRUE,
               'start_maximized' = TRUE,
               'use.automation_extension' = FALSE),
  detach = FALSE,
  excludeSwitches = list("enable-automation")))

## Preparando o navegador

driver <- rsDriver(chromever = "103.0.5060.53", ## Versão varia a depender do computador.
                   browser = "chrome",
                   port = as.integer(4495),
                   check = TRUE,
                   extraCapabilities = eCaps)

## Ativando o navegador

remDr <- driver[["client"]]

# 2. Raspagem dos dados ---------------------------------------------------

## Acessando a URL salva

remDr$navigate(url_base)

## Procurando a seção de Busca Avançada

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="idMostrarPesquisaAvancada"]')$clickElement()

## Procurando a seção para 
## consulta de Acórdãos e Decisões Judiciais

webElem00 <- remDr$findElement(using = "xpath", 
                               '//*[@id="pesquisaLivre"]')

## Informando quais as palavras-chave

webElem00$sendKeysToElement(list("a"))

## Procurando a seção "Data de Julgamento"

webElem00 <- remDr$findElement(using = "xpath", 
                               '//*[@id="dtde1"]')

## Informando qual a data de referência

webElem00$sendKeysToElement(list("01/01/2017"))

## Procurando a seção "Data de Julgamento"

webElem00 <- remDr$findElement(using = "xpath", 
                               '//*[@id="dtde2"]')

## Informando qual a data de referência

webElem00$sendKeysToElement(list("15/06/2022"))

webElem00 <- remDr$findElement(using = "xpath",
                               '//*[@id="ui-datepicker-div"]/div[2]/button[2]')$clickElement()

## Procurando o botão 'Pesquisar'

webElem00 <- remDr$findElement(using = "xpath", 
                               '//*[@id="frmConsulta"]/div[6]/div/div[9]/div/button[2]')

## Clicando no botão

webElem00$clickElement()

## Alterando o número de documentos por página

webElem00 <- remDr$findElement(using = "xpath", 
                               '//*[@id="qtdDocsPagina"]')

## Clicando no elemento

webElem00$clickElement()

## Selecionando 50 documentos por página 

webElem00$sendKeysToElement(list(key = "down_arrow",
                                 key = "down_arrow"))

## 193.360 resultados de Acórdão. 

## Informações disponíveis:

#'      1. ID;
#'      2. Processo; 
#'      3. Relator;
#'      4. Órgão Julgador; 
#'      5. Data do Julgamento; 
#'      6. Data da Publicação; 
#'      7. Agravente; 
#'      8. Advogado;
#'      9. Localização; 
#'      10. Tipo; 
#'      11. Ramo do Direito;
#'      12. Assunto;
#'      13. Tribunal de Origem;
#'      14. Números de Origem;
#'      15. Autuação;
#'      16. Ementa; e
#'      17. Inteiro Teor.

## 2.1. Acórdãos -----------------------------------------------------------

## Criando uma data frame onde os dados serão armazenados

df_final <- data.frame()

## Salvando o id da página principal

main_handle <- remDr$getWindowHandles()[[1]]

## Valor inicial para a mudança de página

pagina <- 51

## Índice para salvar os dados

salva <- 50

## For loop que faz o download dos detalhes
## referentes ao Acórdão

for(i in 1:3866){ ## 3.866 páginas de informação.
                    ## 193.375 registros.
  
  cat("Lendo página", i, "\n")
  
  ## Criando um data frame para armazenar os dados
  ## encontrados em cada página
  
  df_pagina <- data.frame()
  
  ## Coletando o número da página
  
  Pagina <- i
  
  for(j in 1:50){ ## 50 resultados por página.
    
    cat("Resultado", j, "\n")
    
    ## Variável "PROCESSO"
    
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
          
          cat("Lendo 'Processo'", "\n")
          
          ## Informa o xpath 
          
          webElem1 <- remDr$findElement("xpath",
                                        paste0('//*[@id="corpopaginajurisprudencia"]/div[4]/div[2]/div[2]/div[',
                                               j,
                                               ']/div[3]/div[1]'))
          
          ## Coleta o texto
          
          webElemtxt1 <- webElem1$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt1 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt1 <- as.character(unlist(webElemtxt1))
      
    }
    
    ## Variável "RELATOR"
    
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
          
          cat("Lendo 'Relator'", "\n")
          
          ## Informa o xpath 
          
          webElem2 <- remDr$findElement('xpath', 
                                        paste0('//*[@id="corpopaginajurisprudencia"]/div[4]/div[2]/div[2]/div[',
                                        j,
                                        ']/div[4]/div[1]'))
          
          ## Coleta o texto
          
          webElemtxt2 <- webElem2$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt2 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt2 <- as.character(unlist(webElemtxt2))
    
    }
    
    ## Variável "ÓRGÃO JULGADOR"
    
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
          
          cat("Lendo 'Órgão Julgador'", "\n")
          
          ## Informa o xpath 
          
          webElem3 <- remDr$findElement('xpath', 
                                        paste0('//*[@id="corpopaginajurisprudencia"]/div[4]/div[2]/div[2]/div[',
                                               j,
                                               ']/div[4]/div[2]'))
          
          ## Coleta o texto
          
          webElemtxt3 <- webElem3$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt3 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt3 <- as.character(unlist(webElemtxt3))
      
    }
    
    ## Variável "DATA DO JULGAMENTO"
    
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
          
          cat("Lendo 'Data do Julgamento'", "\n")
          
          ## Informa o xpath 
          
          webElem4 <- remDr$findElement('xpath', 
                                        paste0('//*[@id="corpopaginajurisprudencia"]/div[4]/div[2]/div[2]/div[',
                                               j,
                                               ']/div[4]/div[3]'))
          
          ## Coleta o texto
          
          webElemtxt4 <- webElem4$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt4 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt4 <- as.character(unlist(webElemtxt4))
      
    }
    
    ## Variável "DATA DA PUBLICAÇÃO"
    
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
          
          cat("Lendo 'Data da Publicação'", "\n")
          
          ## Informa o xpath 
          
          webElem5 <- remDr$findElement('xpath', 
                                        paste0('//*[@id="corpopaginajurisprudencia"]/div[4]/div[2]/div[2]/div[',
                                               j,
                                               ']/div[4]/div[4]'))
          
          ## Coleta o texto
          
          webElemtxt5 <- webElem5$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt5 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt5 <- as.character(unlist(webElemtxt5))
      
    }
    
    ## Variável "EMENTA"
    
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
          
          cat("Lendo 'Ementa'", "\n")
          
          ## Informa o xpath 
          
          webElem6 <- remDr$findElement('xpath', 
                                        paste0('//*[@id="corpopaginajurisprudencia"]/div[4]/div[2]/div[2]/div[',
                                               j,
                                               ']/div[5]/div'))
          
          ## Coleta o texto
          
          webElemtxt6 <- webElem6$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt6 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt6 <- as.character(unlist(webElemtxt6))
      
      ## Deixando somente a informação que interessa
      
    }
    
    ## Variável "ACÓRDÃO"
    
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
          
          cat("Lendo 'Acórdão'", "\n")
          
          ## Informa o xpath 
          
          webElem7 <- remDr$findElement('xpath', 
                                        paste0('//*[@id="corpopaginajurisprudencia"]/div[4]/div[2]/div[2]/div[',
                                               j,
                                               ']/div[6]/div'))
          
          ## Coleta o texto
          
          webElemtxt7 <- webElem7$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt7 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt7 <- as.character(unlist(webElemtxt7))
      
    }
    
    ## Valor inicial da variável new_tab
    
    new_tab <- NULL
    
    webElem00 <- tryCatch({
      suppressMessages({
    
    ## Abrindo nova janela para mais informações
    
    webElem00 <- remDr$findElement("xpath",
                                   paste0('//*[@id="corpopaginajurisprudencia"]/div[4]/div[2]/div[2]/div[',
                                          j,
                                          ']/div[1]/div[4]/a[2]'))
    
    webElem00$clickElement()
   
    if(length(remDr$getWindowHandles()) == 1){
      
      webElem00$clickElement()
      
    }
    
    ## Mudando para a nova página
    
    new_tab <- unlist(remDr$getWindowHandles())[unlist(remDr$getWindowHandles()) != main_handle]
    
    if(length(new_tab) == 1){
      
      windowSwitch(remDr, new_tab)
      
    }
    
    webElem00 })}, error = function(e) {print("erro")
      NA})
    
    ## Intervalo de espera
    
    Sys.sleep(10)
    
    ## Variável "TIPO DO PROCESSO"
    
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
          
          cat("Lendo 'Tipo do Processo'", "\n")
          
          ## Informa o xpath 
          
          webElem8 <- remDr$findElement('xpath', 
                                        '//*[@id="idProcessoDetalhesBloco1"]/div[1]')
          
          ## Coleta o texto
          
          webElemtxt8 <- webElem8$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt8 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt8 <- as.character(unlist(webElemtxt8))
      
    }
    
    ## Variável "PARTES"
    
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
          
          cat("Lendo 'Partes'", "\n")
          
          ## Informa o xpath 
          
          webElem9 <- remDr$findElement('xpath', 
                                        '//*[@id="idDetalhesPartesAdvogadosProcuradores"]')
          
          ## Coleta o texto
          
          webElemtxt9 <- webElem9$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt9 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt9 <- as.character(unlist(webElemtxt9))
      
    }
    
    ## Variável "LOCALIZAÇÃO"
    
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
          
          cat("Lendo 'Localização'", "\n")
          
          ## Informa o xpath 
          
          webElem10 <- remDr$findElement('xpath', 
                                         '//*[@id="idProcessoDetalhesBloco1"]/div[3]')
          
          ## Coleta o texto
          
          webElemtxt10 <- webElem10$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt10 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt10 <- as.character(unlist(webElemtxt10))
      
    }
    
    ## Variável "TIPO"
    
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
          
          cat("Lendo 'Tipo'", "\n")
          
          ## Informa o xpath 
          
          webElem11 <- remDr$findElement('xpath', 
                                         '//*[@id="idProcessoDetalhesBloco1"]/div[4]')
          
          ## Coleta o texto
          
          webElemtxt11 <- webElem11$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt11 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt11 <- as.character(unlist(webElemtxt11))
      
    }
    
    ## Variável "AUTUAÇÃO"
    
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
          
          cat("Lendo 'Autuação'", "\n")
          
          ## Coleta o texto
          
          webElem12 <- remDr$findElement('xpath', 
                                         '//*[@id="idProcessoDetalhesBloco1"]/div[5]')
          
          ## Coleta o texto
          
          webElemtxt12 <- webElem12$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt12 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt12 <- as.character(unlist(webElemtxt12))
      
    }
    
    ## Variável "NÚMERO ÚNICO"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt13 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt13) && tentativas < 10){
      
      webElemtxt13 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Número Único'", "\n")
          
          ## Coleta o texto
          
          webElem13 <- remDr$findElement('xpath', 
                                         '//*[@id="idProcessoDetalhesBloco1"]/div[6]')
          
          ## Coleta o texto
          
          webElemtxt13 <- webElem13$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt13 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt13 <- as.character(unlist(webElemtxt13))
      
    }
    
    ## Variável "RAMO DO DIREITO"
    
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
          
          cat("Lendo 'Ramo do Direito'", "\n")
          
          ## Coleta o texto
          
          webElem14 <- remDr$findElement('xpath', 
                                         '//*[@id="idProcessoDetalhesBloco2"]/div[2]')
          
          ## Coleta o texto
          
          webElemtxt14 <- webElem14$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt14 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt14 <- as.character(unlist(webElemtxt14))
      
    }
    
    ## Variável "ASSUNTO"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt15 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt15) && tentativas < 10){
      
      webElemtxt15 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Assunto'", "\n")
          
          ## Coleta o texto
          
          webElem15 <- remDr$findElement('xpath', 
                                         '//*[@id="idProcessoDetalhesBloco2"]/div[3]')
          
          ## Coleta o texto
          
          webElemtxt15 <- webElem15$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt15 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt15 <- as.character(unlist(webElemtxt15))
      
    }
    
    ## Variável "TRIBUNAL DE ORIGEM"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt16 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt16) && tentativas < 10){
      
      webElemtxt16 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Tribunal de Origem'", "\n")
          
          ## Coleta o texto
          
          webElem16 <- remDr$findElement('xpath', 
                                         '//*[@id="idProcessoDetalhesBloco3"]/div[1]')
          
          ## Coleta o texto
          
          webElemtxt16 <- webElem16$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt16 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt16 <- as.character(unlist(webElemtxt16))
      
    }
    
    ## Variável "NÚMEROS DE ORIGEM"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt17 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt17) && tentativas < 10){
      
      webElemtxt17 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Números de Origem'", "\n")
          
          ## Coleta o texto
          
          webElem17 <- remDr$findElement('xpath', 
                                         '//*[@id="idProcessoDetalhesBloco3"]/div[2]')
          
          ## Coleta o texto
          
          webElemtxt17 <- webElem17$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt17 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt17 <- as.character(unlist(webElemtxt17))
      
    }
    
    ## Variável "ÚLTIMA FASE"
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt18 <- NA
    
    ## Variável que contabiliza as tentativas de 
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt18) && tentativas < 10){
      
      webElemtxt18 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'Última Fase'", "\n")
          
          ## Coleta o texto
          
          webElem18 <- remDr$findElement('xpath', 
                                         '//*[@id="idProcessoDetalhesBloco4"]/div[1]')
          
          ## Coleta o texto
          
          webElemtxt18 <- webElem18$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt18 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt18 <- as.character(unlist(webElemtxt18))
      
    }
    
    tentativas <- 0
    
    while(tentativas < 10){
      
      decisoes <- tryCatch({
        suppressMessages({
          
          tentativas <- tentativas + 1
          
          ## Trocando para a aba 'Decisões'
          
          webElem00 <- remDr$findElement("xpath",
                                         '//*[@id="idSpanAbaDecisoes"]')$clickElement()
    
    decisoes })}, error = function(e) {print("erro")
      NA})
      
    }
    
    ## Variável 'URL_Ementa'
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt19 <- NA
    
    ## Variável que contabiliza as tentativas de
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt19) && tentativas < 10){
      
      webElemtxt19 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'URL_Ementa", "\n")
          
          ## Procurando o link da ementa
          
          webElem19 <- remDr$findElement('css selector',
                                         '#idDivDecisoes > div.classDivConteudoPesquisaProcessual > div.classDivLinhaDecisoesDocumentos > div > div.clsDecisoesIntTeorRevistaBlocoInterno > div:nth-child(1) > a')$clickElement()
          
          ## Mudando para a nova página
          
          pdf <- remDr$getWindowHandles()[[3]]
          
          if(length(pdf) == 1){
            
            windowSwitch(remDr, pdf)
            
            webElemtxt19 <- remDr$getCurrentUrl()[[1]]
              
            ## Fechando a janela
              
            remDr$closeWindow()
              
            ## Mudando para janela principal
              
            windowSwitch(remDr, new_tab)
              
          }
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt19 })}, error = function(e) {print("erro")
            NA})
      
      ## Salvando na variável URL_Ementa
      
      URL_Ementa <- webElemtxt19
      
    }
    
    ## Variável 'URL_InteiroTeor'
    
    ## Cria uma variável vazia onde as informações serão armazenadas
    
    webElemtxt20 <- NA
    
    ## Variável que contabiliza as tentativas de
    ## coletar a informação
    
    tentativas <- 0
    
    while(is.na(webElemtxt20) && tentativas < 10){
      
      webElemtxt20 <- tryCatch({
        suppressMessages({
          
          ## Atualizando a contagem de tentativas
          
          tentativas <- tentativas + 1
          
          cat("Lendo 'URL_InteiroTeor", "\n")
          
          ## Procurando o link da ementa
          
          webElem20 <- remDr$findElement('xpath',
                                         '//*[@id="idDivDecisoes"]/div[2]/div[1]/div/div[1]')$clickElement()
          
          ## Mudando para a nova página
          
          pdf <- remDr$getWindowHandles()[[3]]
          
          if(length(pdf) == 1){
            
            windowSwitch(remDr, pdf)
            
            webElemtxt20 <- remDr$getCurrentUrl()[[1]]
            
            ## Fechando a janela
            
            remDr$closeWindow()
            
            ## Mudando para janela principal
            
            windowSwitch(remDr, new_tab)
            
          }
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt20 })}, error = function(e) {print("erro")
            NA})
      
      ## Salvando na variável URL_Ementa
      
      URL_InteiroTeor <- webElemtxt20
      
    }
    
    ## Fechando a janela aberta
    
    if(length(new_tab) == 1){
      
      ## Fechando a janela
      
      remDr$closeWindow()
      
      ## Mudando para janela principal
      
      windowSwitch(remDr, main_handle)
      
    }
    
    ## Atribui ao data frame criado a informação extraÍda 
    ## referente ao Inquérito Civil
    
    df_linha <- data.frame(Pagina, webElemtxt1, webElemtxt2, webElemtxt3, webElemtxt4, 
                           webElemtxt5, webElemtxt6, webElemtxt7, webElemtxt8, 
                           webElemtxt9, webElemtxt10, webElemtxt11, webElemtxt12,
                           webElemtxt13, webElemtxt14, webElemtxt15, webElemtxt16,
                           webElemtxt17, webElemtxt18, URL_Ementa, URL_InteiroTeor,
                           stringsAsFactors = F)
    
    ## Empilha todos os Inquéritos Civis encontrados na página 
    ## em um úncio dataframe
    
    df_pagina <- bind_rows(df_pagina, df_linha)
    
  }
  
  ## Empilha todos os processos em um banco final
  
  df_final <- bind_rows(df_final, df_pagina)
  
  ## Script de navegação base
  
  script <- paste0("javascript:navegaForm('",
                   pagina,
                   "');")
  
  ## Salvando o xpath do botão 'Próxima página'
  
  webElem0 <- remDr$findElement(using = "xpath", 
                                '//*[@id="navegacao"]/div/a[1]')
  
  ## Executando a paginação
  
  remDr$executeScript(script, 
                      args = list(webElem0))
  
  ## Atualizando o valor da pagina
  
  pagina <- pagina + 50
  
  if(i >= salva){
    
   saveRDS(df_final,
            "CeDHE/data/output/STJ/acórdãos_stj_18072022_temp.rds")
    
   salva <- salva + 10
    
  }
  
}

## Alterando o diretório

setwd("C:/Users/beca_/OneDrive - usp.br/Documentos/CeDHE")

## Salvando o banco

saveRDS(df_final,
        "CeDHE/data/output/STJ/acórdãos_stj_18072022_temp.rds")

# 3. Limpeza --------------------------------------------------------------

## 3.1. Acórdãos -----------------------------------------------------------

## Carregando os dados brutos

df_final <- readRDS("data/output/STJ/acórdãos_STJ_29082022_temp.rds")

## Quebrando as strings em várias colunas

df_final <- df_final %>% 
  separate(webElemtxt8,
           sep = ":",
           into = c("webElemtxt8_1",
                    "webElemtxt8")) %>% 
  separate(webElemtxt10,
           sep = ":",
           into = c("webElemtxt10_1",
                    "webElemtxt10")) %>% 
  separate(webElemtxt11,
           sep = ":",
           into = c("webElemtxt11_1",
                    "webElemtxt11")) %>% 
  separate(webElemtxt12,
           sep = ":",
           into = c("webElemtxt12_1",
                    "webElemtxt12")) %>% 
  separate(webElemtxt13,
           sep = ":",
           into = c("webElemtxt13_1",
                    "webElemtxt13")) %>% 
  separate(webElemtxt14,
           sep = ":",
           into = c("webElemtxt14_1",
                    "webElemtxt14")) %>%
  separate(webElemtxt15,
           sep = ":",
           into = c("webElemtxt15_1",
                    "webElemtxt15")) %>%
  separate(webElemtxt16,
           sep = ":",
           into = c("webElemtxt16_1",
                    "webElemtxt16")) %>%
  separate(webElemtxt17,
           sep = ":",
           into = c("webElemtxt17_1",
                    "webElemtxt17")) %>% 
  separate(webElemtxt18,
           sep = ":\n",
           into = c("webElemtxt18_1",
                    "webElemtxt18"))

## Organizando e estruturando o banco

df_final <- df_final %>%
  mutate(Relator = gsub("RELATORA",
                        "",
                        webElemtxt2),
         OrgaoJulgador = gsub("ÓRGÃO JULGADOR",
                              "",
                              webElemtxt3),
         DataJulgamento = gsub("DATA DO JULGAMENTO",
                               "",
                               webElemtxt4),
         DataPublicacao = gsub("DATA DA PUBLICAÇÃO/FONTE\nDJe ",
                               "",
                               webElemtxt5),
         Ementa = gsub("EMENTA",
                       "",
                       webElemtxt6),
         Acordao = gsub("ACÓRDÃO",
                        "",
                        webElemtxt7),
         Processo = ifelse(webElemtxt8_1 == "PROCESSO",
                           webElemtxt8,
                           NA),
         Partes = webElemtxt9,
         Localizacao = ifelse(webElemtxt10_1 == "LOCALIZAÇÃO",
                              webElemtxt10,
                              NA),
         Tipo = ifelse(webElemtxt11_1 == "TIPO",
                       webElemtxt11,
                       NA),
         Autuacao = ifelse(webElemtxt12_1 == "AUTUAÇÃO",
                           webElemtxt12,
                           NA),
         Numero = ifelse(webElemtxt13_1 == "NÚMERO ÚNICO",
                         webElemtxt13,
                         NA),
         Ramo = ifelse(webElemtxt14_1 == "RAMO DO DIREITO",
                       webElemtxt14,
                       NA),
         Assunto = ifelse(webElemtxt14_1 == "ASSUNTO(S)",
                          webElemtxt14,
                          ifelse(webElemtxt15_1 == "ASSUNTO(S)",
                                 webElemtxt15,
                                 NA)),
         TribunalOrigem = ifelse(webElemtxt16_1 == "TRIBUNAL DE ORIGEM",
                                 webElemtxt16,
                                 NA),
         UltimaFase = ifelse(webElemtxt18_1 == "ÚLTIMA FASE" |
                             webElemtxt18_1 == "ÚLTIMA FASE",
                             webElemtxt18,
                             NA)) %>% 
  mutate(Relator = gsub("RELATOR",
                        "",
                        Relator),
         Assunto = stripWhitespace(Assunto),
         Tipo = gsub("\\.",
                     "",
                     Tipo),
         DataJulgamento = as.Date(trimws(DataJulgamento),
                                  format = "%d/%m/%Y"),
         DataPublicacao = as.Date(trimws(DataPublicacao),
                                  format = "%d/%m/%Y"),
         Autuacao = as.Date(trimws(Autuacao),
                                   format = "%d/%m/%Y"),
         Tipo = str_to_upper(Tipo),
         Relator = str_to_upper(Relator),
         Assunto = str_to_upper(Assunto),
         OrgaoJulgador = str_to_upper(OrgaoJulgador),
         Partes = str_to_upper(Partes),
         Localizacao = str_to_upper(Localizacao)) %>% 
  select(Numero,
         Tipo,
         Processo,
         Ramo,
         Assunto,
         Relator,
         Partes,
         TribunalOrigem,
         OrgaoJulgador,
         Localizacao,
         UltimaFase,
         Autuacao,
         DataPublicacao,
         DataJulgamento,
         Ementa,
         Acordao,
         URL_InteiroTeor) %>%
  filter(!is.na(Numero)) %>% 
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
        "data/output/STJ/acórdãos_STJ_31082022.rds")

write.csv(df_final,
          "data/output/STJ/acórdãos_STJ_31082022.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

## Salva o banco com os filtros de período e tema

write.csv(df_final_filtros,
          "data/output/STJ/acórdãos_STJ_31082022_com filtros.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

saveRDS(df_final_filtros,
        "data/output/STJ/acórdãos_STJ_31082022_com filtros.rds")
