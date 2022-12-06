
## TÍTULO: WEB SCRAPING DA JURISPRUDÊNCIA DO TJRO
## DATA: 22/06/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(abjutils)
library(RSelenium)
library(httr)
library(qdapRegex)
library(tm)
library(gsubfn)

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

## DIRETÓRIO

setwd("CNJ")

## FUNÇÕES

source("functions/windowSwitch.R", 
       encoding = "UTF-8")

source("functions/wordFilter.R", 
       encoding = "UTF-8")

# 1. Ambiente -------------------------------------------------------------

## URL de referência

url_base <- "https://webapp.tjro.jus.br/juris/consulta/consultaJuris.jsf"

## Funções extras

eCaps <- list(
  chromeOptions =
    list(prefs = list(
      "plugins.always_open_pdf_externally" = FALSE,
      "profile.default_content_settings.popups" = 0L,
      "download.prompt_for_download" = FALSE,
      "directory_upgrade" = TRUE)))

## Configurando driver

driver <- rsDriver(
                   chromever = "106.0.5249.61", ## Versão varia a depender do computador.
                   browser = "chrome", 
                   port = as.integer(4495), 
                   check = TRUE,
                   extraCapabilities = eCaps)

## Ativando o navegador

remDr <- driver[["client"]]

# 2. Raspagem dos dados ---------------------------------------------------

## Criando uma data frame onde os dados serão armazenados

df_final <- data.frame()

## Salvando o id da página principal

main_handle <- remDr$getWindowHandles()[[1]]

## Acessando a url salva

remDr$navigate(url_base)

## Procurando a seção de consulta de 
## Acórdãos e Decisões Monocráticas

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="frmJuris:formConsultaJuris:iPesquisa"]')$sendKeysToElement(list("ementa ou lei ou documento ou ."))

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
                                              '//*[@id="frmJuris:formConsultaJuris:iPesquisa"]')$sendKeysToElement(list("ementa ou lei ou documento ou ."))
            
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
    
    if(is.na(paginas)){
      
      paginas <- 60
    
   } else if(paginas %% 1 != 0){
    
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
      
      Sys.sleep(5)
      
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
      # 
      # webElemtxt4 <- NA
      # 
      # ## Variável que contabiliza as tentativas de 
      # ## coletar a informação
      # 
      # tentativas <- 0
      # 
      # while(is.na(webElemtxt4) && tentativas < 10){
      #   
      #   webElemtxt4 <- tryCatch({
      #     suppressMessages({
      #       
      #       ## Atualizando a contagem de tentativas
      #       
      #       tentativas <- tentativas + 1
      #       
      #       cat("Lendo 'Inteiro Teor'", "\n")
      #       
      #       ## Informa o xpath 
      #       
      #       webElem4 <- remDr$findElement('xpath', 
      #                                     '//*[@id="frmJuris:tvTabs"]/ul/li[2]/a')$clickElement()
      #       
      #       ## Movendo a página para cima
      #       
      #       webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "home"))
      #       
      #       cat("Download do Acórdão", "\n")
      #       
      #       ## Informa o xpath 
      #       
      #       webElem00 <- remDr$findElement('xpath', 
      #                                     '//*[@id="frmJuris:j_idt46"]')$clickElement()
      #       
      #       webElemtxt4 <- "COMPLETO"
      #       
      #       ## Mensagem e atribuição no caso de erro
      #       
      #       webElemtxt4 })}, error = function(e) {print("erro")
      #         NA})
      #   
      # }
      
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
              "data/output/TJRO/acórdãos_TJRO_12072022_temp.rds")
       }
    }
}

## Salvando o banco

saveRDS(df_final,
        "data/output/TJRO/acórdãos_TJRO_12072022_temp.rds")

# 3. Limpeza --------------------------------------------------------------

## Carregando o dado salvo

df_final <- readRDS("data/output/TJRO/acórdãos_TJRO_24102022_temp.rds")

## Quebrando as strings em várias colunas

df_final <- df_final %>% 
  mutate(webElemtxt3 = trimws(gsub("\\n+", "|", 
                                   webElemtxt3,
                                   perl = TRUE)),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Embargante", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Embargantes", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Agravante", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Agravante", 
                                  "Agravado",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Agravantes", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Advogados", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Advogado", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Procuradora", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Paciente", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "IntdA", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Intdo", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Intdos", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Interessado", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Apelante/Apelado", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Apelante", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Apelante", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Apelantes", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Recorrente", 
                                  "Relator",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Adamir", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "José", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Itaú", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "RELATÓRIO", 
                                  "voto",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "EMENTA", 
                                  "Especial",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "do", 
                                  "Especial",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "EMENTA", 
                                  "Cível",
                                  extract = F,
                                  fixed = T,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "João", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Jose", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Luiziana", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Banco", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Louise", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Maria", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Recdo:", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Departamento:", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Karina", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Zenobrio", 
                                  "Mimessi",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Santo", 
                                  "Grangeia",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Ministério", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Biocal", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Taísa", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Interes", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Pedro", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Aline", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Embte", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Rosalino", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Estado", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Marco", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Adriana", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Luciana", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Apte", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Mônica", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Alcir", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Elias", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Mapfre", 
                                  ")",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Desembargador", 
                                  ")",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Advogado", 
                                  ")",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "MFB", 
                                  "Lauer",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "AUTOS", 
                                  "FILHO",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "OI", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Rafaella", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Darco", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Ana", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Apnte", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Terra", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Daniel", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Neirelene", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Francisco", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Altamiro", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Leonor", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Polimport", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Rosiney", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Mauro", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Tranquillo", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Francisca", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Francesco", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = rm_between(webElemtxt3, 
                                  "Município", 
                                  "\\:",
                                  extract = F,
                                  trim = F,
                                  clean = F),
         webElemtxt3 = stripWhitespace(trimws(str_squish(webElemtxt3)))) 

## Segundo processo de limpeza

df_final <- df_final %>% 
  mutate() %>% 
  separate(webElemtxt3,
           sep = "\\|",
           into = paste0("type_", 1:14)) %>% 
  select(ID,
         DataJulgamento,
         type_1:type_8,
         type_13,
         type_14,
         Ementa)

## Organizando e estruturando o banco

df_final <- df_final %>% 
  mutate(Numero = gsub(".*º",
                       "",
                       ID),
         Ementa = gsub("EMENTA: ",
                       "",
                       Ementa,
                       fixed = TRUE),
         DataJulgamento = gsub(".*:",
                               "",
                               DataJulgamento),
         DataDistribuicao = ifelse(grepl("Data de distribuição",
                                         type_1),
                                   type_1,
                                   ifelse(grepl("Data de distribuição",
                                         type_1),
                                   type_2,
                                   NA)),
         type_14 = ifelse(grepl("Processo publicado no Diário Oficial em",
                                 type_8),
                          type_8,
                          type_14),
         type_14 = ifelse(grepl("Processo publicado no Diário Oficial em",
                                type_13),
                          type_13,
                          type_14),
         type_13 = ifelse(grepl("Processo publicado no Diário Oficial em",
                                type_13),
                          NA,
                          type_13),
         DataPublicacao = as.character(strapplyc(type_14, 
                                    "\\d+/\\d+/\\d+", 
                                    simplify = TRUE)),
         Origem = ifelse(grepl("Origem",
                               type_2),
                         type_2,
                         ifelse(grepl("Origem",
                                      type_3),
                                type_3,
                                ifelse(grepl("Origem",
                                             type_4),
                                       type_4,
                                       ifelse(grepl("Origem",
                                                    type_5),
                                              type_5,
                                              ifelse(grepl("Origem",
                                                           type_6),
                                                     type_6,
                                                     ifelse(grepl("Origem",
                                                                  type_7),
                                                            type_7,
                                                            NA)))))),
         Relator = type_13) %>% 
  separate(Numero,
           sep = " - ",
           into = c("Numero",
                    "Tipo")) %>% 
  mutate(DataDistribuicao = gsub("Data de distribuição: ",
                                  "",
                                  DataDistribuicao),
         DataDistribuicao = gsub("Data de distribuição : ",
                                  "",
                                  DataDistribuicao),
         DataDistribuicao = gsub("Data de distribuição o: ",
                                  "",
                                  DataDistribuicao),
         DataDistribuicao = gsub("Data de distribuição ",
                                  "",
                                  DataDistribuicao),
         DataDistribuicao = gsub("D",
                                  "",
                                  DataDistribuicao),
         DataDistribuicao = gsub("\\:",
                                  "",
                                  DataDistribuicao),
         DataPublicacao = as.Date(DataPublicacao,
                                  format = "%d/%m/%Y"),
         DataJulgamento = as.Date(DataJulgamento,
                                  format = "%d/%m/%Y"),
         DataDistribuicao = as.Date(DataDistribuicao,
                                    format = "%d/%m/%Y"),
         Origem = gsub("Origem: : ",
                       "",
                       Origem),
         Origem = gsub("Origem : ",
                       "",
                       Origem),
         Origem = gsub("Origem: ",
                       "",
                       Origem),
         Origem = sub("^[0-9].{20}?",
                       "",
                       Origem),
         Origem = sub("^[0-9].{4}?",
                       "",
                       Origem),
         Origem = sub("\\- ", "", Origem),
         Origem = sub("^-",
                       "",
                       Origem),
         Origem = sub("^–",
                       "",
                       Origem),
         Origem = sub("^ – ",
                       "",
                       Origem),
         Origem = ifelse(Numero == "0005775-51.2016.822.0000",
                         "1ª Vara Criminal da Comarca de Rolim de Moura/RO",
                         Origem),
         Origem = gsub(" :.*",
                       "",
                       Origem),
         Origem = ifelse(Origem %in% c("0501", "0023",
                                       "0020", "0005",
                                       "0004", "0001",
                                       "0000", ".0000",
                                       "35020148220002",
                                       "50620188220015",
                                       "64620158220001",
                                       "75720178220014",
                                       "84420148220601",
                                       "93520168220005",
                                       "175720178220014",
                                       "535020148220002",
                                       "593520168220005",
                                       "684420148220601",
                                       "750620188220015",
                                       "964620158220001"),
                         NA,
                         Origem),
         Origem = gsub(".0001 ",
                       "",
                       Origem),
         Origem = gsub(".0005 ",
                       "",
                       Origem),
         Origem = gsub(".0501 ",
                       "",
                       Origem),
         Origem = gsub(".0011 ",
                       "",
                       Origem),
         Relator = gsub(".*: ",
                        "",
                        Relator),
         Relator = gsub(":.*",
                        "",
                        Relator),
         Relator = gsub("É COMO VOTO.",
                        NA,
                        Relator,
                        ignore.case = TRUE),
         Origem = stripWhitespace(trimws(str_squish(Origem))),
         Relator = stripWhitespace(trimws(str_squish(Relator))),
         Tipo = stripWhitespace(trimws(str_squish(Tipo))),
         Ementa = stripWhitespace(trimws(str_squish(Ementa))),
         Origem = str_to_upper(Origem),
         Relator = str_to_upper(Relator),
         Tipo = str_to_upper(Tipo)) %>% 
  select(Numero,
         Tipo,
         DataDistribuicao,
         DataJulgamento,
         DataPublicacao,
         Origem,
         Relator,
         Ementa) %>% 
  arrange(DataJulgamento)

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
  arrange(DataJulgamento)

# 5. Salva ----------------------------------------------------------------

## Salva o banco original, sem filtros

saveRDS(df_final,
        "data/output/TJRO/acórdãos_TJRO_26102022.rds")

write.csv(df_final,
          "data/output/TJRO/acórdãos_TJRO_26102022.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

## Salva o banco com os filtros de período e tema

write.csv(df_final_filtros,
          "data/output/TJRO/acórdãos_TJRO_26102022_com filtros.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

saveRDS(df_final_filtros,
        "data/output/TJRO/acórdãos_TJRO_26102022_com filtros.rds")
