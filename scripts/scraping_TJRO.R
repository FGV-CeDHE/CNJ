
## TÍTULO: WEB SCRAPING DA JURISPRUDÊNCIA DO TJRO
## DATA: 22/06/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(abjutils)
library(RSelenium)
library(httr)

## OBJETIVOS

#'         - Realizar raspagem de dados da jurisprudência 
#'           disponibilizada no site https://webapp.tjro.jus.br/juris/consulta/consultaJuris.jsf.

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

url_base <- "https://webapp.tjro.jus.br/juris/consulta/consultaJuris.jsf"

## Pasta onde os dados serão salvos

file_path <- "C:\\Users\\beca_\\OneDrive - usp.br\\Documentos\\CeDHE\\data\\input\\TJRO"

## Funções extras

eCaps <- list(
  chromeOptions =
    list(prefs = list(
      "plugins.always_open_pdf_externally" = FALSE,
      "profile.default_content_settings.popups" = 0L,
      "download.prompt_for_download" = FALSE,
      "directory_upgrade" = TRUE,
      'download.default_directory' = file_path)))

## Configurando driver

driver <- rsDriver(
                   chromever = "103.0.5060.24", ## Versão varia a depender do computador.
                   browser = "chrome", 
                   port = as.integer(4493), 
                   check = TRUE,
                   extraCapabilities = eCaps)

## Ativando o navegador

remDr <- driver[["client"]]

# 2. Raspagem dos dados ---------------------------------------------------

## 2.1. Acórdãos -----------------------------------------------------------

## Alterando o diretório

setwd("data/input/TJRO")

## Criando uma data frame onde os dados serão armazenados

#df_final <- data.frame()

## Salvando o id da página principal

main_handle <- remDr$getWindowHandles()[[1]]

## Acessando a url salva

remDr$navigate(url_base)

## Procurando a seção de consulta de 
## Acórdãos e Decisões Monocráticas

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="frmJuris:formConsultaJuris:iPesquisa"]')$sendKeysToElement(list("ementa ou lei ou documento ou a"))

## Selecionando apenas os acórdãos

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="frmJuris:formConsultaJuris:chbSelectSumula"]/div[2]')$clickElement()

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="frmJuris:formConsultaJuris:chbSelectMonocratica"]/div[2]')$clickElement()

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="frmJuris:formConsultaJuris:chbSelectPresidencia"]/div[2]')$clickElement()


webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="frmJuris:formConsultaJuris:chbSelectTurmaRecursalDm"]/div[2]')$clickElement()


## Site é pouco permissivo na busca e, portanto, os dados
## terão que ser buscados por meses, anos, órgãoS julgadores e relatores.
## Possui um limite de 600 resultados por pesquisa.

## Lista dos anos que serão processados

anos <- c(
  #2017
       #   2018
          2019
          # 2020,
          # 2021,
          # 2022
          )

## Lista dos meses

meses <- c(8:12)

## Lista dos órgãos julgadores

orgaos <- (1:13)

## Lista de relatores

relatores <- c(1:96)

## Raspagem dos dados

for(ano in anos){
  for(mes in meses){
    for(orgao in orgaos){
      
      for(relator in relatores){
        
        cat("Lendo", ano, mes, orgao, relator, "\n")
        
        Sys.sleep(15)
        
        webElem99 <- "ok"
          
          webElem99 <- tryCatch({
            suppressMessages({
              
              disponivel <- remDr$findElement(using = "xpath",
                                              '//*[@id="frmJuris:formConsultaJuris:iPesquisa"]')$isElementDisplayed()[[1]]
              
              
              webElem99 })}, error = function(e) {print("erro")
                "erro"})
          
          if(webElem99 == "erro"){
            
            ## Acessando a url salva
            
            remDr$navigate(url_base)
            
            ## Limpando a consulta
            
            webElem00 <- remDr$findElement(using = "xpath", 
                                           '//*[@id="frmJuris:formConsultaJuris:btLimpar"]')$clickElement()
            
            ## Atualizando a página
            
            remDr$refresh()
            
            ## Procurando a seção de consulta de 
            ## Acórdãos e Decisões Monocráticas
            
            webElement00 <- remDr$findElement(using = "xpath",
                                              '//*[@id="frmJuris:formConsultaJuris:iPesquisa"]')$sendKeysToElement(list("ementa ou lei ou documento ou a"))
            
            ## Selecionando apenas os acórdãos
            
            webElement00 <- remDr$findElement(using = "xpath",
                                              '//*[@id="frmJuris:formConsultaJuris:chbSelectSumula"]/div[2]')$clickElement()
            
            webElement00 <- remDr$findElement(using = "xpath",
                                              '//*[@id="frmJuris:formConsultaJuris:chbSelectMonocratica"]/div[2]')$clickElement()
            
            webElement00 <- remDr$findElement(using = "xpath",
                                              '//*[@id="frmJuris:formConsultaJuris:chbSelectPresidencia"]/div[2]')$clickElement()
            
            
            webElement00 <- remDr$findElement(using = "xpath",
                                              '//*[@id="frmJuris:formConsultaJuris:chbSelectTurmaRecursalDm"]/div[2]')$clickElement()
            
          }
  
  ## Procurando a seção "Ano de Julgamento"
  
  webElem00 <- remDr$findElement(using = "css selector", 
                                 '#frmJuris\\:formConsultaJuris\\:iAnoJulgamento')$clickElement()
  
  remDr$findElement(using = "css selector", 
                    '#frmJuris\\:formConsultaJuris\\:iAnoJulgamento')$sendKeysToElement(list(paste0("",
                                                                                                    ano)))
  
  ## Procurando a seção "Mês de Julgamento"
  
  webElem00 <- remDr$findElement(using = "css selector", 
                                 '#frmJuris\\:formConsultaJuris\\:sMesJulgamento')$clickElement()
  
  remDr$findElement(using = "css selector", 
                    paste0('#frmJuris\\:formConsultaJuris\\:sMesJulgamento_',
                           mes))$clickElement()
  
  ## Procurando o Órgão Julgador
  
  webElem00 <- remDr$findElement(using = "css selector", 
                                 '#frmJuris\\:formConsultaJuris\\:sOrgaoJulgador')$clickElement()
  
  remDr$findElement(using = "css selector", 
                    paste0('#frmJuris\\:formConsultaJuris\\:sOrgaoJulgador_',
                           orgao))$clickElement()
  
  ## Procurando o Relator
  
  webElem00 <- remDr$findElement(using = "css selector", 
                                 '#frmJuris\\:formConsultaJuris\\:sRelatorProc')$clickElement()
  
  remDr$findElement(using = "css selector", 
                    paste0('#frmJuris\\:formConsultaJuris\\:sRelatorProc_',
                           relator))$clickElement()
  
  ## Procurando o botão 'Pesquisar'
  
  webElem00 <- remDr$findElement(using = "xpath", 
                                 '//*[@id="frmJuris:formConsultaJuris:btPesquisar"]')$clickElement()
  
  ## 25.066 acórdãos em 2017/2.507 páginas.
  ## 27.430 acórdãos em 2018/2.743 páginas.
  ## 33.156 acórdãos em 2019/3.316 páginas.
  ## 34.441 acórdãos em 2020/3.445 páginas.
  ## 60.859 acórdãos em 2021/6.086 páginas.
  ## 29.423 acórdãos em 2022/2.943 páginas.
  
  ## 05 informações disponíveis: 
  
  #'      1. ID;
  #'      2. Data do Julgamento;
  #'      3. Outras Informações (Relator, Órgão Julgador, etc)
  #'      4. Ementa;
  #'      5. Inteiro Teor.

  Sys.sleep(15)

  disponivel <- NA

  disponivel <- tryCatch({
    suppressMessages({
    
      disponivel <- remDr$findElement('xpath', 
                      '//*[@id="frmJuris:formDetalhesJuris:painelResultadosPesquisa_data"]/tr/td/span/input')$isElementDisplayed()[[1]]
      
      disponivel })}, error = function(e) {print("erro")
        FALSE})
  
  if(disponivel == "TRUE"){
    
    ## Procurando o primeiro tipo
    
    webElem1 <- remDr$findElement('xpath', 
                                '//*[@id="frmJuris:formDetalhesJuris:painelResultadosPesquisa_data"]/tr/td/span/input')$clickElement()
  
    ## Verificando quantos acórdãos tem nesta busca
    
    numacordaos <- remDr$findElement('xpath', 
                                     '//*[@id="frmJuris:formDetalhesJuris:painelResultadosPesquisa_data"]/tr/td/span/input')$getElementAttribute("value")[[1]]
  
    ## Número de páginas
    
    paginas <- abs(extract_numeric(numacordaos))/10
    
    if(paginas %% 1 != 0){
    
      paginas <- as.integer(paginas) + 1
      
    } else {
      
      paginas <- as.integer(paginas)
      
    }
  
  ## For loop que faz o download dos detalhes
  ## referentes ao Acórdão
  
  for(i in 1:paginas){ 
    
    cat("Lendo página", i, "\n")
    
    ## Criando um data frame para armazenar os dados
    ## encontrados em cada página
    
    df_pagina <- data.frame()
    
    ## Coletando o número da página
    
    Pagina <- i
    
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
                                          paste0('//*[@id="frmJuris:formDetalhesJuris:painelProcessosAcordaos_data"]/tr[',
                                                 j,
                                                 ']/td/span[1]/input'))
            ## Coleta o texto
            
            
            webElemtxt1 <- webElem1$getElementAttribute("value")
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt1 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        ID <- as.character(unlist(webElemtxt1))
        
      }
      
      ## Variável "DATA DO JULGAMENTO"
      
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
            
            cat("Lendo 'Data do Julgamento e Ementa'", "\n")
            
            ## Informa o xpath 
            
            webElem2 <- remDr$findElement('xpath', 
                                          paste0('//*[@id="frmJuris:formDetalhesJuris:painelProcessosAcordaos_data"]/tr[',
                                                 j,
                                                 "]"))
            
            ## Coleta o texto
            
            webElemtxt2 <- webElem2$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt2 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        webElemtxt2 <- as.character(unlist(webElemtxt2))
        
        ## Separando as informações
        
        webElemtxt2 <- str_split(webElemtxt2,
                                    pattern = "\n")
        
        ## Salvando nas variáveis adequadas
        
        DataJulgamento <- unlist(webElemtxt2)[1]
        
        Ementa <- unlist(webElemtxt2)[2]
        
      }
      
      ## Valor inicial da variável new_tab
      
      new_tab <- NULL
      
      webElem00 <- tryCatch({
        suppressMessages({
      
      ## Abrindo nova janela para mais informações
      
      webElem00 <- remDr$findElement("xpath",
                                     paste0('//*[@id="frmJuris:formDetalhesJuris:painelProcessosAcordaos_data"]/tr[',
                                            j,
                                            ']/td/span[1]'))
      
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
      
      ## Variável "OUTRAS INFORMAÇÕES"
      
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
            
            cat("Lendo 'Outras Informações'", "\n")
            
            ## Informa o xpath 
            
            webElem3 <- remDr$findElement('xpath', 
                                          paste0('//*[@id="frmJuris:pgPesquisaEmentaDetalhesDocumento"]/tbody/tr[3]/td'))
            
            ## Coleta o texto
            
            webElemtxt3 <- webElem3$getElementText()
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt3 })}, error = function(e) {print("erro")
              NA})
        
        ## Extrai a informação coletada
        
        webElemtxt3 <- as.character(unlist(webElemtxt3))
        
      }
      
      ## Variável "INTEIRO TEOR"
      
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
            
            cat("Lendo 'Inteiro Teor'", "\n")
            
            ## Informa o xpath 
            
            webElem4 <- remDr$findElement('xpath', 
                                          '//*[@id="frmJuris:tvTabs"]/ul/li[2]/a')$clickElement()
            
            ## Movendo a página para cima
            
            webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "home"))
            
            cat("Download do Acórdão", "\n")
            
            ## Informa o xpath 
            
            webElem00 <- remDr$findElement('xpath', 
                                          '//*[@id="frmJuris:j_idt46"]')$clickElement()
            
            webElemtxt4 <- "COMPLETO"
            
            ## Mensagem e atribuição no caso de erro
            
            webElemtxt4 })}, error = function(e) {print("erro")
              NA})
        
      }
      
      if(length(new_tab) >= 1 &
         remDr$getCurrentWindowHandle() != main_handle){
        
        ## Fechando a janela
        
        remDr$closeWindow()
        
        ## Mudando para janela principal
        
        windowSwitch(remDr, main_handle)
        
      }
      
      ## Atribui ao data frame criado a informação extraÍda 
      ## referente ao Inquérito Civil
      
      df_linha <- data.frame(ID, 
                             DataJulgamento, 
                             webElemtxt3,
                             Ementa,
                             stringsAsFactors = F)
      
      ## Empilha todos os Inquéritos Civis encontrados na página 
      ## em um úncio dataframe
      
      df_pagina <- bind_rows(df_pagina, df_linha)
      
    }
    
    ## Empilha todos os Inquéritos Civis em um banco final
    
    df_final <- bind_rows(df_final, df_pagina)
    
    ## Rolando a página para baixo
    
    webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "end"))
    
    if(paginas > 1){
    
    ## Salvando o xpath da função "próxima página"
    
    webElem00 <- remDr$findElement(using = "xpath", 
                                   '//*[@id="frmJuris:formDetalhesJuris:painelProcessosAcordaos_paginator_bottom"]/a[3]')$clickElement()
    
    }
    
    ## Rolando a página para cima
    
    webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "home"))
    
  }
    
    disponivel <- NA
    
    tentativas <- 0
    
    while(is.na(disponivel) && tentativas < 10){
      
      tentativas <- tentativas + 1
      
      webElemtxt1 <- tryCatch({
        suppressMessages({
          
          disponivel <- webElem00 <- remDr$findElement('xpath', 
                                                       '//*[@id="j_idt28:btVoltar"]')$isElementDisplayed()[[1]]
          
          ## Rolando a página para cima
          
          webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "home"))
          
          ## Voltando para a página inicial
          
          webElem00 <- remDr$findElement('xpath', 
                                         '//*[@id="j_idt28:btVoltar"]')$clickElement()
          
          disponivel })}, error = function(e) {print("erro")
            NA})
      
    }
    
  } else if(disponivel == "FALSE"){
    
    disponivel <- NA
    
    tentativas <- 0
    
    while(is.na(disponivel) && tentativas < 10){
      
      tentativas <- tentativas + 1
      
      webElemtxt1 <- tryCatch({
        suppressMessages({
          
          disponivel <- webElem00 <- remDr$findElement('xpath', 
                                                       '//*[@id="j_idt28:btVoltar"]')$isElementDisplayed()[[1]]
    
          ## Rolando a página para cima
          
          webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "home"))
          
          ## Voltando para a página inicial
          
          webElem00 <- remDr$findElement('xpath', 
                                        '//*[@id="j_idt28:btVoltar"]')$clickElement()
          
    disponivel })}, error = function(e) {print("erro")
      NA})
      
    }
    
  }
  
  }
      saveRDS(df_final,
              "acórdãos_TJRO_12072022_temp.rds")
       }
    }
}

setwd("C:/Users/beca_/OneDrive - usp.br/Documentos/CeDHE")

## Salvando o banco

saveRDS(df_final,
        "data/output/TJRO/acórdãos_TJRO_12072022_temp.rds")

# 2.2. Decisões Monocráticas ----------------------------------------------

## Alterando o diretório

setwd("data/input/TJRO")

## Criando uma data frame onde os dados serão armazenados

#df_final <- data.frame()

## Salvando o id da página principal

main_handle <- remDr$getWindowHandles()[[1]]

## Acessando a url salva

remDr$navigate(url_base)

## Procurando a seção de consulta de 
## Acórdãos e Decisões Monocráticas

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="frmJuris:formConsultaJuris:iPesquisa"]')$sendKeysToElement(list("ementa ou lei ou documento ou a"))

## Selecionando apenas os acórdãos

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="frmJuris:formConsultaJuris:chbSelectSumula"]/div[2]')$clickElement()

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="frmJuris:formConsultaJuris:chbSelectMonocratica"]/div[2]')$clickElement()

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="frmJuris:formConsultaJuris:chbSelectPresidencia"]/div[2]')$clickElement()


webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="frmJuris:formConsultaJuris:chbSelectTurmaRecursalDm"]/div[2]')$clickElement()

## Site é pouco permissivo na busca e, portanto, os dados
## terão que ser buscados por meses, anos, órgãoS julgadores e relatores.
## Possui um limite de 600 resultados por pesquisa.

## Lista dos anos que serão processados

anos <- c(2017,
          2018,
          2019,
          2020,
          2021,
          2022)

## Lista dos meses

meses <- c(1:12)

## Lista dos órgãos julgadores

orgaos <- c(1:13)

## Lista de relatores

relatores <- c(1:96)

## Raspagem dos dados

for(ano in anos){
  for(mes in meses){
    for(orgao in orgaos){
      for(relator in relatores){
        
        cat("Lendo", ano, mes, orgao, relator, "\n")
        
        Sys.sleep(10)
        
        ## Procurando a seção "Ano de Julgamento"
        
        webElem00 <- remDr$findElement(using = "css selector", 
                                       '#frmJuris\\:formConsultaJuris\\:iAnoJulgamento')$clickElement()
        
        remDr$findElement(using = "css selector", 
                          '#frmJuris\\:formConsultaJuris\\:iAnoJulgamento')$sendKeysToElement(list(paste0("",
                                                                                                          ano)))
        
        ## Procurando a seção "Mês de Julgamento"
        
        webElem00 <- remDr$findElement(using = "css selector", 
                                       '#frmJuris\\:formConsultaJuris\\:sMesJulgamento')$clickElement()
        
        remDr$findElement(using = "css selector", 
                          paste0('#frmJuris\\:formConsultaJuris\\:sMesJulgamento_',
                                 mes))$clickElement()
        
        ## Procurando o Órgão Julgador
        
        webElem00 <- remDr$findElement(using = "css selector", 
                                       '#frmJuris\\:formConsultaJuris\\:sOrgaoJulgador')$clickElement()
        
        remDr$findElement(using = "css selector", 
                          paste0('#frmJuris\\:formConsultaJuris\\:sOrgaoJulgador_',
                                 orgao))$clickElement()
        
        ## Procurando o Relator
        
        webElem00 <- remDr$findElement(using = "css selector", 
                                       '#frmJuris\\:formConsultaJuris\\:sRelatorProc')$clickElement()
        
        remDr$findElement(using = "css selector", 
                          paste0('#frmJuris\\:formConsultaJuris\\:sRelatorProc_',
                                 relator))$clickElement()
        
        ## Procurando o botão 'Pesquisar'
        
        webElem00 <- remDr$findElement(using = "xpath", 
                                       '//*[@id="frmJuris:formConsultaJuris:btPesquisar"]')$clickElement()
        
        ## 25.066 acórdãos em 2017/2.507 páginas.
        ## 27.430 acórdãos em 2018/2.743 páginas.
        ## 33.156 acórdãos em 2019/3.316 páginas.
        ## 34.441 acórdãos em 2020/3.445 páginas.
        ## 60.859 acórdãos em 2021/6.086 páginas.
        ## 29.423 acórdãos em 2022/2.943 páginas.
        
        ## 05 informações disponíveis: 
        
        #'      1. ID;
        #'      2. Data do Julgamento;
        #'      3. Outras Informações (Relator, Órgão Julgador, etc)
        #'      4. Ementa;
        #'      5. Inteiro Teor.
        
        Sys.sleep(10)
        
        disponivel <- NA
        
        disponivel <- tryCatch({
          suppressMessages({
            
            disponivel <- remDr$findElement('xpath', 
                                            '//*[@id="frmJuris:formDetalhesJuris:painelResultadosPesquisa_data"]/tr/td/span/input')$isElementDisplayed()[[1]]
            
            disponivel })}, error = function(e) {print("erro")
              FALSE})
        
        if(disponivel == "TRUE"){
          
          ## Procurando o primeiro tipo
          
          webElem1 <- remDr$findElement('xpath', 
                                        '//*[@id="frmJuris:formDetalhesJuris:painelResultadosPesquisa_data"]/tr/td/span/input')$clickElement()
          
          ## Verificando quantos acórdãos tem nesta busca
          
          numacordaos <- remDr$findElement('xpath', 
                                           '//*[@id="frmJuris:formDetalhesJuris:painelResultadosPesquisa_data"]/tr/td/span/input')$getElementAttribute("value")[[1]]
          
          ## Número de páginas
          
          paginas <- abs(extract_numeric(numacordaos))/10
          
          if(is.integer(paginas) == FALSE){
            
            paginas <- as.integer(paginas) + 1
            
          } else {
            
            paginas <- as.integer(paginas)
            
          }
          
          ## For loop que faz o download dos detalhes
          ## referentes ao Acórdão
          
          for(i in 1:paginas){ 
            
            cat("Lendo página", i, "\n")
            
            ## Criando um data frame para armazenar os dados
            ## encontrados em cada página
            
            df_pagina <- data.frame()
            
            ## Coletando o número da página
            
            Pagina <- i
            
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
                                                  paste0('//*[@id="frmJuris:formDetalhesJuris:painelProcessosAcordaos_data"]/tr[',
                                                         j,
                                                         ']/td/span[1]/input'))
                    ## Coleta o texto
                    
                    
                    webElemtxt1 <- webElem1$getElementAttribute("value")
                    
                    ## Mensagem e atribuição no caso de erro
                    
                    webElemtxt1 })}, error = function(e) {print("erro")
                      NA})
                
                ## Extrai a informação coletada
                
                ID <- as.character(unlist(webElemtxt1))
                
              }
              
              ## Variável "DATA DO JULGAMENTO"
              
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
                    
                    cat("Lendo 'Data do Julgamento e Ementa'", "\n")
                    
                    ## Informa o xpath 
                    
                    webElem2 <- remDr$findElement('xpath', 
                                                  paste0('//*[@id="frmJuris:formDetalhesJuris:painelProcessosAcordaos_data"]/tr[',
                                                         j,
                                                         "]"))
                    
                    ## Coleta o texto
                    
                    webElemtxt2 <- webElem2$getElementText()
                    
                    ## Mensagem e atribuição no caso de erro
                    
                    webElemtxt2 })}, error = function(e) {print("erro")
                      NA})
                
                ## Extrai a informação coletada
                
                webElemtxt2 <- as.character(unlist(webElemtxt2))
                
                ## Separando as informações
                
                webElemtxt2 <- str_split(webElemtxt2,
                                         pattern = "\n")
                
                ## Salvando nas variáveis adequadas
                
                DataJulgamento <- unlist(webElemtxt2)[1]
                
                Ementa <- unlist(webElemtxt2)[2]
                
              }
              
              ## Valor inicial da variável new_tab
              
              new_tab <- NULL
              
              webElem00 <- tryCatch({
                suppressMessages({
                  
                  ## Abrindo nova janela para mais informações
                  
                  webElem00 <- remDr$findElement("xpath",
                                                 paste0('//*[@id="frmJuris:formDetalhesJuris:painelProcessosAcordaos_data"]/tr[',
                                                        j,
                                                        ']/td/span[1]'))
                  
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
              
              ## Variável "OUTRAS INFORMAÇÕES"
              
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
                    
                    cat("Lendo 'Outras Informações'", "\n")
                    
                    ## Informa o xpath 
                    
                    webElem3 <- remDr$findElement('xpath', 
                                                  paste0('//*[@id="frmJuris:pgPesquisaEmentaDetalhesDocumento"]/tbody/tr[3]/td'))
                    
                    ## Coleta o texto
                    
                    webElemtxt3 <- webElem3$getElementText()
                    
                    ## Mensagem e atribuição no caso de erro
                    
                    webElemtxt3 })}, error = function(e) {print("erro")
                      NA})
                
                ## Extrai a informação coletada
                
                webElemtxt3 <- as.character(unlist(webElemtxt3))
                
              }
              
              ## Variável "INTEIRO TEOR"
              
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
                    
                    cat("Lendo 'Inteiro Teor'", "\n")
                    
                    ## Informa o xpath 
                    
                    webElem4 <- remDr$findElement('xpath', 
                                                  '//*[@id="frmJuris:tvTabs"]/ul/li[2]/a')$clickElement()
                    
                    webElem4 <- remDr$findElement('xpath', 
                                                  '//*[@id="divDtDocumento"]')
                    
                    ## Coleta o texto
                    
                    webElemtxt4 <- webElem4$getElementText()
                    
                    ## Movendo a página para cima
                    
                    webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "home"))
                    
                    cat("Download do Acórdão", "\n")
                    
                    ## Informa o xpath 
                    
                    webElem4 <- remDr$findElement('xpath', 
                                                  '//*[@id="frmJuris:j_idt46"]')$clickElement()
                    
                    ## Mensagem e atribuição no caso de erro
                    
                    webElemtxt4 })}, error = function(e) {print("erro")
                      NA})
                
                ## Extrai a informação coletada
                
                InteiroTeor <- as.character(unlist(webElemtxt4))
                
              }
              
              if(length(new_tab) == 1){
                
                ## Fechando a janela
                
                remDr$closeWindow()
                
                ## Mudando para janela principal
                
                windowSwitch(remDr, main_handle)
                
              }
              
              ## Atribui ao data frame criado a informação extraÍda 
              ## referente ao Inquérito Civil
              
              df_linha <- data.frame(ID, 
                                     DataJulgamento, 
                                     webElemtxt3,
                                     Ementa,
                                     InteiroTeor,
                                     stringsAsFactors = F)
              
              ## Empilha todos os Inquéritos Civis encontrados na página 
              ## em um úncio dataframe
              
              df_pagina <- bind_rows(df_pagina, df_linha)
              
            }
            
            ## Empilha todos os Inquéritos Civis em um banco final
            
            df_final <- bind_rows(df_final, df_pagina)
            
            ## Rolando a página para baixo
            
            webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "end"))
            
            if(paginas > 1){
              
              ## Salvando o xpath da função "próxima página"
              
              webElem00 <- remDr$findElement(using = "xpath", 
                                             '//*[@id="frmJuris:formDetalhesJuris:painelProcessosAcordaos_paginator_bottom"]/a[3]')$clickElement()
              
            }
            ## Rolando a página para cima
            
            webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "home"))
            
          }
          
          ## Voltando para a página inicial
          
          webElem1 <- remDr$findElement('xpath', 
                                        '//*[@id="j_idt28:btVoltar"]')$clickElement()
          
        } else if(disponivel == "FALSE"){
          
          ## Voltando para a página inicial
          
          webElem1 <- remDr$findElement('xpath', 
                                        '//*[@id="j_idt28:btVoltar"]')$clickElement()
          
        }
        
      }
    }
  }
}

## Salvando o banco

saveRDS(df_final,
        "data/output/TJRO/decisões_mon_tjro_05082022_temp.rds")

# 3. Download documentos --------------------------------------------------


download.file("https://jurisprudencia-api.tjmt.jus.br/VisualizaRelatorio/RelatorioEmentaJurisprudencia?id=131770199&colegiado=Segunda&tipoProcesso=Acordao",
              destfile = "temp.pdf",
              mode="wb")