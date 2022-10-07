
## TÍTULO: WEB SCRAPING DA JURISPRUDÊNCIA DO TJAM
## DATA: 22/06/2022
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
#'           disponibilizada no site https://consultasaj.tjam.jus.br/cjsg/resultadoCompleta.do.

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

source("scripts/windowSwitch.R", 
       encoding = "UTF-8")

source("scripts/filter.R", 
       encoding = "UTF-8")

# 1. Ambiente -------------------------------------------------------------

## URL de referência

url_base <- "https://consultasaj.tjam.jus.br/cjsg/resultadoCompleta.do"

## Preparando o navegador

driver <- rsDriver(chromever = "103.0.5060.24", ## Versão varia a depender do computador.
                   browser = "chrome", 
                   port = as.integer(4494), 
                   check = TRUE)

## Ativando o navegador

remDr <- driver[["client"]]

# 2. Raspagem dos dados ---------------------------------------------------

## Criando uma data frame onde os dados serão armazenados

df_final <- data.frame()

## Acessando a url salva

remDr$navigate(url_base)

## Procurando a seção de consulta de 
## Acórdãos e Decisões Monocráticas

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="iddados.buscaInteiroTeor"]')$sendKeysToElement(list("ementa OU lei OU documento OU a"))

## Selecionando acórdãos de todas as origens

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="origemRecursal"]')$clickElement()

## Lista dos anos que serão processados

anos <- c(2017,
          2018,
          2019,
          2020,
          2021,
          2022)

## Criando uma lista com o número de páginas em 
## cada ano

paginas <- c(2507,
             2743,
             3316,
             3445,
             6086,
             2943)

## Índice de referência dos anos

pagina <- 1

## Índice de referência para exportação

salva <- 100

## For loop que realiza a mineração

for(ano in anos){
  
  cat("Lendo", ano, "\n")
  
  ## Procurando a seção "Data de Julgamento"
  
  webElem00 <- remDr$findElement(using = "xpath", 
                                 '//*[@id="iddados.dtJulgamentoInicio"]')$sendKeysToElement(list(paste0("01/01/",
                                                                                                        ano)))
  
  webElem00 <- remDr$findElement(using = "xpath",
                                 '//*[@id="iddados.dtJulgamentoFim"]')$sendKeysToElement(list(paste0("31/12/",
                                                                                                     ano)))
  
  ## Procurando o botão 'Pesquisar'
  
  webElem00 <- remDr$findElement(using = "xpath", 
                                 '//*[@id="pbSubmit"]')$clickElement()

## 25.066 acórdãos em 2017/2.507 páginas.
## 27.430 acórdãos em 2018/2.743 páginas.
## 33.156 acórdãos em 2019/3.316 páginas.
## 34.441 acórdãos em 2020/3.445 páginas.
## 60.859 acórdãos em 2021/6.086 páginas.
## 29.303 acórdãos em 2022/2.943 páginas.

## 09 informações disponíveis: 

#'      1. ID;
#'      2. Classe/Assunto;
#'      3. Relator;
#'      4. Comarca;
#'      5. Órgão Julgador;
#'      6. Data do Julgamento;
#'      7. Data de Publicação;
#'      8. Ementa;
#'      9. URL do Inteiro Teor.

## 2.1. Acórdãos -----------------------------------------------------------

## For loop que faz o download dos detalhes
## referentes ao Acórdão

for(i in 1:paginas[pagina]){ 
  
  cat("Lendo página", i, "\n")
  
  ## Criando um data frame para armazenar os dados
  ## encontrados em cada página
  
  df_pagina <- data.frame()
  
  ## Coletando o número da página
  
  Pagina <- i
  
  ## Tempo de intervalo entre um 
  ## processamento e outro.
  
  Sys.sleep(10)
  
  for(j in 1:10){ ## 10 resultados por página.
    
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
                                        paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                               j,
                                               ']/td[2]/table/tbody/tr[1]/td/a[1]'))
          
          ## Coleta o texto
          
          webElemtxt1 <- webElem1$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt1 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      ID <- as.character(unlist(webElemtxt1))
      
    }
    
    ## Variável "CLASSE/ASSUNTO"
    
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
          
          cat("Lendo 'Classe/Assunto'", "\n")
          
          ## Informa o xpath 
          
          webElem2 <- remDr$findElement('xpath', 
                                        paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                               j,
                                               "]/td[2]/table/tbody/tr[2]/td"))
          
          ## Coleta o texto
          
          webElemtxt2 <- webElem2$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt2 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt2 <- as.character(unlist(webElemtxt2))
      
    }
    
    ## Variável "RELATOR"
    
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
          
          cat("Lendo 'Relator'", "\n")
          
          ## Informa o xpath 
          
          webElem3 <- remDr$findElement('xpath', 
                                        paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                               j,
                                               ']/td[2]/table/tbody/tr[3]/td'))
          
          ## Coleta o texto
          
          webElemtxt3 <- webElem3$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt3 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt3 <- as.character(unlist(webElemtxt3))
      
    }
    
    ## Variável "COMARCA"
    
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
          
          cat("Lendo 'Comarca'", "\n")
          
          ## Informa o xpath 
          
          webElem4 <- remDr$findElement('xpath', 
                                        paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                               j,
                                               ']/td[2]/table/tbody/tr[4]/td'))
          
          ## Coleta o texto
          
          webElemtxt4 <- webElem4$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt4 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt4 <- as.character(unlist(webElemtxt4))
      
    }
    
    ## Variável "ÓRGÃO JULGADOR"
    
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
          
          cat("Lendo 'Órgão Julgador'", "\n")
          
          ## Informa o xpath 
          
          webElem5 <- remDr$findElement('xpath', 
                                        paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                               j,
                                               ']/td[2]/table/tbody/tr[5]/td'))
          
          ## Coleta o texto
          
          webElemtxt5 <- webElem5$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt5 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt5 <- as.character(unlist(webElemtxt5))
      
    }
    
    ## Variável "DATA DO JULGAMENTO"
    
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
          
          cat("Lendo 'Data do Julgamento'", "\n")
          
          ## Informa o xpath 
          
          webElem6 <- remDr$findElement('xpath', 
                                        paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                               j,
                                               "]/td[2]/table/tbody/tr[6]/td"))
          
          ## Coleta o texto
          
          webElemtxt6 <- webElem6$getElementText()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt6 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      webElemtxt6 <- as.character(unlist(webElemtxt6))
      
    }
    
    ## Variável "DATA DE PUBLICAÇÃO"
    
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
          
          cat("Lendo 'Data de Publicação'", "\n")
          
          ## Informa o xpath 
          
          webElem7 <- remDr$findElement('xpath', 
                                        paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                               j,
                                               ']/td[2]/table/tbody/tr[7]/td'))
          
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
    
    ## Variável "EMENTA"
    
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
          
          cat("Lendo 'Ementa'", "\n")
          
          ## Informa o xpath 
          
          webElem8 <- remDr$findElement('xpath', 
                                        paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                               j,
                                               ']/td[2]/table/tbody/tr[1]/td/a[3]/img'))$clickElement()
          
          webElem8 <- remDr$findElement('xpath', 
                                        paste0('//*[@id="popupModalDiv"]'))
          
          ## Coleta o texto
          
          webElemtxt8 <- webElem8$getElementText()
          
          ## Fechando o popup
          
          webElem8 <- remDr$findElement('xpath', 
                                        paste0('//*[@id="popupModalBotaoFechar"]'))$clickElement()
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt8 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      Ementa <- as.character(unlist(webElemtxt8))
      
    }
    
    ## Variável "URL DO INTEIRO TEOR"
    
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
          
          cdacordao <- NA
          cdforo <- NA
          
          cat("Lendo 'URL do Inteiro Teor'", "\n")
          
          ## Informa o xpath 
          
          cdacordao <- remDr$findElement('xpath', 
                                        paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                               j,
                                               "]/td[2]/table/tbody/tr[1]/td/a[2]"))$getElementAttribute("cdacordao")[[1]]
          
          cdforo <- remDr$findElement('xpath', 
                                      paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                             j,
                                             "]/td[2]/table/tbody/tr[1]/td/a[2]"))$getElementAttribute("cdforo")[[1]]
          
          if(!is.na(cdacordao) &
             !is.na(cdforo)){
            
            webElemtxt9 <- "COMPLETO"
            
          }
          
          ## Mensagem e atribuição no caso de erro
          
          webElemtxt9 })}, error = function(e) {print("erro")
            NA})
      
      ## Extrai a informação coletada
      
      URL_InteiroTeor <- paste0("https://consultasaj.tjam.jus.br/cjsg/getArquivo.do?conversationId=&cdAcordao=",
                                cdacordao,
                                "&cdForo=",
                                cdforo)
      }
    
    ## Atribui ao data frame criado a informação extraÍda 
    ## referente ao Inquérito Civil
    
    df_linha <- data.frame(Pagina, ID, webElemtxt2, webElemtxt3, webElemtxt4, 
                           webElemtxt5, webElemtxt6, webElemtxt7, 
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
  
  if(i > 1){
  
  ## Salvando o xpath da função "próxima página"
  
  webElem00 <- remDr$findElement(using = "css selector", 
                                 '#paginacaoInferior-A > table > tbody > tr:nth-child(1) > td:nth-child(2) > div > a:nth-child(7)')$clickElement()
  
  } else{
    
    ## Salvando o xpath da função "próxima página"
    
    webElem00 <- remDr$findElement(using = "css selector", 
                                   '#paginacaoInferior-A > table > tbody > tr:nth-child(1) > td:nth-child(2) > div > a:nth-child(6)')$clickElement()
  }
  
  if(i >= salva){ 
    
    saveRDS(df_final,
            "CeDHE/data/output/TJAM/acórdãos_tjam_11072022_temp.rds")
    
    salva <- salva + 100
    
  }
  
  }
  
  pagina <- pagina + 1
  
  }

## Salvando o banco

saveRDS(df_final,
        "CeDHE/data/output/TJAM/acórdãos_tjam_12072022_temp.rds")

## 2.2. Decisões Monocráticas ----------------------------------------------

## Criando uma data frame onde os dados serão armazenados

df_final <- data.frame()

## Acessando a url salva

remDr$navigate(url_base)

## Procurando a seção de consulta de 
## Acórdãos e Decisões Monocráticas

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="iddados.buscaInteiroTeor"]')$sendKeysToElement(list("ementa OU lei OU documento OU a"))

## Selecionando acórdãos de todas as origens

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="origemRecursal"]')$clickElement()

## Lista dos anos que serão processados

anos <- c(2017,
          2018,
          2019,
          2020,
          2021,
          2022)

## Criando uma lista com o número de páginas em 
## cada ano

paginas <- c(2507,
             2743,
             3316,
             3445,
             6086,
             2943)

## Índice de referência dos anos

pagina <- 1

## Índice de referência para exportação

salva <- 100

for(ano in anos){
  
  cat("Lendo", ano, "\n")
  
  ## Procurando a seção "Data de Julgamento"
  
  webElem00 <- remDr$findElement(using = "xpath", 
                                 '//*[@id="iddados.dtJulgamentoInicio"]')$sendKeysToElement(list(paste0("01/01/",
                                                                                                        ano)))
  
  webElem00 <- remDr$findElement(using = "xpath",
                                 '//*[@id="iddados.dtJulgamentoFim"]')$sendKeysToElement(list(paste0("31/12/",
                                                                                                     ano)))
  
  ## Procurando o botão 'Pesquisar'
  
  webElem00 <- remDr$findElement(using = "xpath", 
                                 '//*[@id="pbSubmit"]')$clickElement()
  
  ## 25.066 acórdãos em 2017/2.507 páginas.
  ## 27.430 acórdãos em 2018/2.743 páginas.
  ## 33.156 acórdãos em 2019/3.316 páginas.
  ## 34.441 acórdãos em 2020/3.445 páginas.
  ## 60.859 acórdãos em 2021/6.086 páginas.
  ## 29.303 acórdãos em 2022/2.943 páginas.
  
  ## 09 informações disponíveis: 
  
  #'      1. ID;
  #'      2. Classe/Assunto;
  #'      3. Relator;
  #'      4. Comarca;
  #'      5. Órgão Julgador;
  #'      6. Data do Julgamento;
  #'      7. Data de Publicação;
  #'      8. Ementa;
  #'      9. URL do Inteiro Teor.
  
  ## For loop que faz o download dos detalhes
  ## referentes ao Acórdão
  
  for(i in 1:paginas[pagina]){ 
    
    cat("Lendo página", i, "\n")
    
    ## Criando um data frame para armazenar os dados
    ## encontrados em cada página
    
    df_pagina <- data.frame()
    
    ## Coletando o número da página
    
    Pagina <- i
    
    ## Tempo de intervalo entre um 
    ## processamento e outro.
    
    Sys.sleep(10)
    
    for(j in 1:10){ ## 10 resultados por página.
      
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
                                          paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                                 j,
                                                 ']/td[2]/table/tbody/tr[1]/td/a[1]'))
            
            ## Coleta o texto
            
            webElemtxt1 <- webElem1$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt1 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        ID <- as.character(unlist(webElemtxt1))
        
      }
      
      ## Variável "CLASSE/ASSUNTO"
      
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
            
            cat("Lendo 'Classe/Assunto'", "\n")
            
            ## Informa o xpath 
            
            webElem2 <- remDr$findElement('xpath', 
                                          paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                                 j,
                                                 "]/td[2]/table/tbody/tr[2]/td"))
            
            ## Coleta o texto
            
            webElemtxt2 <- webElem2$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt2 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        webElemtxt2 <- as.character(unlist(webElemtxt2))
        
      }
      
      ## Variável "RELATOR"
      
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
            
            cat("Lendo 'Relator'", "\n")
            
            ## Informa o xpath 
            
            webElem3 <- remDr$findElement('xpath', 
                                          paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                                 j,
                                                 ']/td[2]/table/tbody/tr[3]/td'))
            
            ## Coleta o texto
            
            webElemtxt3 <- webElem3$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt3 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        webElemtxt3 <- as.character(unlist(webElemtxt3))
        
      }
      
      ## Variável "COMARCA"
      
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
            
            cat("Lendo 'Comarca'", "\n")
            
            ## Informa o xpath 
            
            webElem4 <- remDr$findElement('xpath', 
                                          paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                                 j,
                                                 ']/td[2]/table/tbody/tr[4]/td'))
            
            ## Coleta o texto
            
            webElemtxt4 <- webElem4$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt4 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        webElemtxt4 <- as.character(unlist(webElemtxt4))
        
      }
      
      ## Variável "ÓRGÃO JULGADOR"
      
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
            
            cat("Lendo 'Órgão Julgador'", "\n")
            
            ## Informa o xpath 
            
            webElem5 <- remDr$findElement('xpath', 
                                          paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                                 j,
                                                 ']/td[2]/table/tbody/tr[5]/td'))
            
            ## Coleta o texto
            
            webElemtxt5 <- webElem5$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt5 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        webElemtxt5 <- as.character(unlist(webElemtxt5))
        
      }
      
      ## Variável "DATA DO JULGAMENTO"
      
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
            
            cat("Lendo 'Data do Julgamento'", "\n")
            
            ## Informa o xpath 
            
            webElem6 <- remDr$findElement('xpath', 
                                          paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                                 j,
                                                 "]/td[2]/table/tbody/tr[6]/td"))
            
            ## Coleta o texto
            
            webElemtxt6 <- webElem6$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt6 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        webElemtxt6 <- as.character(unlist(webElemtxt6))
        
      }
      
      ## Variável "DATA DE PUBLICAÇÃO"
      
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
            
            cat("Lendo 'Data de Publicação'", "\n")
            
            ## Informa o xpath 
            
            webElem7 <- remDr$findElement('xpath', 
                                          paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                                 j,
                                                 ']/td[2]/table/tbody/tr[7]/td'))
            
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
      
      ## Variável "EMENTA"
      
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
            
            cat("Lendo 'Ementa'", "\n")
            
            ## Informa o xpath 
            
            webElem8 <- remDr$findElement('xpath', 
                                          paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                                 j,
                                                 ']/td[2]/table/tbody/tr[1]/td/a[3]/img'))$clickElement()
            
            webElem8 <- remDr$findElement('xpath', 
                                          paste0('//*[@id="popupModalDiv"]'))
            
            ## Coleta o texto
            
            webElemtxt8 <- webElem8$getElementText()
            
            ## Fechando o popup
            
            webElem8 <- remDr$findElement('xpath', 
                                          paste0('//*[@id="popupModalBotaoFechar"]'))$clickElement()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt8 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        Ementa <- as.character(unlist(webElemtxt8))
        
      }
      
      ## Variável "URL DO INTEIRO TEOR"
      
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
            
            cdacordao <- NA
            cdforo <- NA
            
            cat("Lendo 'URL do Inteiro Teor'", "\n")
            
            ## Informa o xpath 
            
            cdacordao <- remDr$findElement('xpath', 
                                           paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                                  j,
                                                  "]/td[2]/table/tbody/tr[1]/td/a[2]"))$getElementAttribute("cdacordao")[[1]]
            
            cdforo <- remDr$findElement('xpath', 
                                        paste0('//*[@id="divDadosResultado-A"]/table/tbody/tr[',
                                               j,
                                               "]/td[2]/table/tbody/tr[1]/td/a[2]"))$getElementAttribute("cdforo")[[1]]
            
            if(!is.na(cdacordao) &
               !is.na(cdforo)){
              
              webElemtxt9 <- "COMPLETO"
              
            }
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt9 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        URL_InteiroTeor <- paste0("https://consultasaj.tjam.jus.br/cjsg/getArquivo.do?conversationId=&cdAcordao=",
                                  cdacordao,
                                  "&cdForo=",
                                  cdforo)
      }
      
      ## Atribui ao data frame criado a informação extraÍda 
      ## referente ao Inquérito Civil
      
      df_linha <- data.frame(Pagina, ID, webElemtxt2, webElemtxt3, webElemtxt4, 
                             webElemtxt5, webElemtxt6, webElemtxt7, 
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
    
    if(i > 1){
      
      ## Salvando o xpath da função "próxima página"
      
      webElem00 <- remDr$findElement(using = "css selector", 
                                     '#paginacaoInferior-A > table > tbody > tr:nth-child(1) > td:nth-child(2) > div > a:nth-child(7)')$clickElement()
      
    } else{
      
      ## Salvando o xpath da função "próxima página"
      
      webElem00 <- remDr$findElement(using = "css selector", 
                                     '#paginacaoInferior-A > table > tbody > tr:nth-child(1) > td:nth-child(2) > div > a:nth-child(6)')$clickElement()
    }
    
    if(i >= salva){ 
      
      saveRDS(df_final,
              "CeDHE/data/output/TJAM/acórdãos_tjam_11072022_temp.rds")
      
      salva <- salva + 100
      
    }
    
  }
  
  pagina <- pagina + 1
  
}

## Salvando o banco

saveRDS(df_final,
        "CeDHE/data/output/TJAM/decisões_mon_tjam_11072022_temp.rds")

# 3. Limpeza --------------------------------------------------------------

## 3.1. Acórdãos -----------------------------------------------------------

## Carregando o dado salvo

df_final <- readRDS("data/output/TJAM/acórdãos_tjam_11072022_temp.rds")

## Quebrando as strings em várias colunas

df_final <- df_final %>% 
  separate(X1,
           sep = "\\: ",
           into = c("X1_1",
                    "X1")) %>% 
  separate(X2,
           sep = "\\: ",
           into = c("X2_1",
                    "X2")) %>% 
  separate(X3,
           sep = "\\: ",
           into = c("X3_1",
                    "X3")) %>% 
  separate(X4,
           sep = "\\: ",
           into = c("X4_1",
                    "X4")) %>% 
  separate(X5,
           sep = "\\: ",
           into = c("X5_1",
                    "X5")) %>% 
  separate(X6,
           sep = "\\: ",
           into = c("X6_1",
                    "X6")) %>% 
  separate(X7,
           sep = "\\: ",
           into = c("X7_1",
                    "X7")) %>% 
  separate(X8,
           sep = "\\: ",
           into = c("X8_1",
                    "X8")) %>% 
  separate(X9,
           sep = "\\: ",
           into = c("X9_1",
                    "X9")) %>% 
  separate(X10,
           sep = "\\: ",
           into = c("X10_1",
                    "X10")) %>% 
  separate(X11,
           sep = "\\: ",
           into = c("X11_1",
                    "X11")) %>% 
  separate(X12,
           sep = "\\: ",
           into = c("X12_1",
                    "X12")) %>% 
  separate(X13,
           sep = "\\: ",
           into = c("X13_1",
                    "X13")) %>% 
  separate(X14,
           sep = "\\: ",
           into = c("X14_1",
                    "X14")) %>% 
  separate(X15,
           sep = "\\: ",
           into = c("X15_1",
                    "X15"))

## Organizando e estruturando o banco

df_final <- df_final %>% 
  mutate(ClasseAssunto = str_to_upper(webElemtxt2),
         Relator = str_to_upper(webElemtxt3),
         Comarca = str_to_upper(webElemtxt4),
         OrgaoJulgador = str_to_upper(webElemtxt5),
         DataJulgamento = as.Date(webElemtxt6,
                                  format = "%d/%m/%Y"),
         DataPublicacao = as.Date(webElemtxt7,
                                  format = "%d/%m/%Y"),
         Ementa = gsub("\\Ementa sem formatação X\n    E M E N T A ", 
                        "",
                        Ementa,
                        ignore.case = TRUE),
         Ementa = gsub("\\Ementa sem formatação X\n    EMENTA: ", 
                        "",
                        Ementa,
                        ignore.case = TRUE),
         Ementa = gsub(c("\\Ementa sem formatação X\n    EMENTA : "),
                        "",
                        Ementa,
                        ignore.case = TRUE),
         Ementa = gsub(c("\\Ementa sem formatação X\n "),
                        "",
                        Ementa,
                        ignore.case = TRUE),
         Ementa = stripWhitespace(gsub("\\EMENTA. ",
                                        "",
                                        Ementa,
                                        ignore.case = TRUE)),
         Ementa = stripWhitespace(gsub("\\EMENTA - ",
                                        "",
                                        Ementa,
                                        ignore.case = TRUE)),
         Ementa = stripWhitespace(gsub("\\EMENTA ",
                                        "",
                                        Ementa,
                                        ignore.case = TRUE)),
         Ementa = stripWhitespace(gsub("\\EMENTA-",
                                        "",
                                        Ementa,
                                        ignore.case = TRUE)),
         Ementa = stripWhitespace(gsub("\\EMENTA:: ",
                                        "",
                                        Ementa,
                                        ignore.case = TRUE)),
         Ementa = stripWhitespace(gsub(" EMENTA(1), (2) e (3): ",
                                        "",
                                        Ementa,
                                        fixed = TRUE)),
         Ementa = stripWhitespace(gsub("\\EMENTA(1):",
                                        "",
                                        Ementa,
                                        ignore.case = TRUE)),
         Ementa = stripWhitespace(gsub(" EMENTA(1): ",
                                        "",
                                        Ementa,
                                        fixed = TRUE)),
         Ementa = stripWhitespace(gsub("EMENTA(1): ",
                                        "",
                                        Ementa,
                                        fixed = TRUE)),
         Ementa = stripWhitespace(gsub("\\EMENTA(2):",
                                        "",
                                        Ementa,
                                        ignore.case = TRUE)),
         Ementa = stripWhitespace(gsub(" EMENTA(2): ",
                                        "",
                                        Ementa,
                                        fixed = TRUE)),
         Ementa = stripWhitespace(gsub("\\EMENTA:.",
                                        "",
                                        Ementa,
                                        ignore.case = TRUE)),
         Ementa = stripWhitespace(gsub("\\ ementa: ",
                                        "",
                                        Ementa,
                                        ignore.case = TRUE)),
         Ementa = stripWhitespace(gsub("\\EMENTAS:",
                                        "",
                                        Ementa,
                                        ignore.case = TRUE)),
         Ementa = stripWhitespace(gsub("\\EMENTA.:",
                                        "",
                                        Ementa,
                                        ignore.case = TRUE)),
         Ementa = stripWhitespace(gsub("\\E M E N T A:",
                                       "",
                                       Ementa,
                                       ignore.case = TRUE))) %>% 
  rename("Numero" = "ID") %>% 
  select(Numero,
         ClasseAssunto,
         Relator,
         Comarca,
         OrgaoJulgador,
         DataPublicacao,
         DataJulgamento,
         Ementa,
         URL_InteiroTeor) %>% 
  arrange(DataJulgamento,
          Comarca,
          OrgaoJulgador)

## Remove os espaços em branco em excesso

for (i in colnames(df_final)){
  
  df_final[[i]] <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", 
                        df_final[[i]], 
                        perl = TRUE)
}

## 3.2. Decisões Monocráticas ----------------------------------------------



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
          Comarca,
          OrgaoJulgador)

# 5. Salva ----------------------------------------------------------------

## Salva o banco original, sem filtros

saveRDS(df_final,
        "data/output/TJAM/acórdãos_TJAM_19072022.rds")

write.csv(df_final,
          "data/output/TJAM/acórdãos_TJAM_19072022.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

## Salva o banco com os filtros de período e tema

write.csv(df_final_filtros,
          "data/output/TJAM/acórdãos_TJAM_20072022_com filtros.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

writexl::write_xlsx(df_final_filtros,
                    "data/output/TJAM/acórdãos_TJAM_20072022_com filtros.csv")

saveRDS(df_final_filtros,
        "data/output/TJAM/acórdãos_TJAM_20072022_com filtros.rds")
