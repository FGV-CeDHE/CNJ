
## TÍTULO: WEB SCRAPING DA JURISPRUDÊNCIA DO TJRO
## DATA: 28/10/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(abjutils)
library(RSelenium)
library(httr)
library(tm)

## OBJETIVOS

#'         - Realizar download dos acórdãos do TJRO selecionados
#'           para análise qualitativa.

## AMBIENTE

setwd("CNJ")

## FUNÇÕES

source("functions/windowSwitch.R", 
       encoding = "UTF-8")

# 1. Ambiente -------------------------------------------------------------

## URL de referência

url_base <- "https://webapp.tjro.jus.br/juris/consulta/consultaJuris.jsf"

## Pasta onde os dados serão salvos

file_path <- "C:\\Users\\beca_\\OneDrive - usp.br\\Documentos\\CeDHE\\CNJ\\data\\input\\TJRO"

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
  chromever = "106.0.5249.61", ## Versão varia a depender do computador.
  browser = "chrome", 
  port = as.integer(4495), 
  check = TRUE,
  extraCapabilities = eCaps)

## Ativando o navegador

remDr <- driver[["client"]]

# 2. Data -----------------------------------------------------------------

## Carregando o arquivo de referência

processos <- readxl::read_xlsx("data/input/TJRO/processos selecionados_TJRO.xlsx")

## Preparando os dados

processos <- processos %>% 
  mutate(
         files = gsub("\\.",
                       "",
                       Numero),
         Numero = gsub("822",
                       "",
                       Numero),
         files = gsub("\\-",
                       "",
                       files),
         files = stripWhitespace(trimws(str_squish(files))))

## Transformando em uma lista

processos <- unique(processos$Numero)

# 3. Documentos -----------------------------------------------------------

## Salvando o id da página principal

main_handle <- remDr$getWindowHandles()[[1]]

## Acessando a url salva

remDr$navigate(url_base)

## Selecionando apenas os acórdãos

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="frmJuris:formConsultaJuris:chbSelectSumula"]/div[2]')$clickElement()

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="frmJuris:formConsultaJuris:chbSelectMonocratica"]/div[2]')$clickElement()

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="frmJuris:formConsultaJuris:chbSelectPresidencia"]/div[2]')$clickElement()


webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="frmJuris:formConsultaJuris:chbSelectTurmaRecursalDm"]/div[2]')$clickElement()


## For loop que faz o download de cada documento

for(i in seq_along(processos)){
  
  cat("Lendo", i, "\n")
  
  Sys.sleep(2)
  
  ## Limpando os resultados
  
  webElement00 <- remDr$findElement(using = "xpath",
                                    '//*[@id="frmJuris:formConsultaJuris:iNumProcesso"]')$clearElement()
  
  Sys.sleep(3)
  
  tentativas <- 0
  
  while(tentativas < 10){
  
  ## Procurando a seção de consulta dos processos
  
  webElement00 <- remDr$findElement(using = "xpath",
                                   '//*[@id="frmJuris:formConsultaJuris:iNumProcesso"]')$clickElement()
  
  remDr$findElement(using = "xpath",
                    '//*[@id="frmJuris:formConsultaJuris:iNumProcesso"]')$sendKeysToElement(list(processos[i]))
  
  tentativas <- tentativas + 1
  
  }
  
  Sys.sleep(1)
  
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
    
    ## Valor inicial da variável new_tab
    
    new_tab <- NULL
    
    webElem00 <- tryCatch({
      suppressMessages({
        
        ## Abrindo nova janela para mais informações
        
        webElem00 <- remDr$findElement("xpath",
                                       paste0('//*[@id="frmJuris:formDetalhesJuris:painelProcessosAcordaos_data"]/tr[1]/td/span[1]'))
        
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

# 4. Limpeza --------------------------------------------------------------

## Carregando o arquivo de referência

processos <- readxl::read_xlsx("data/input/TJRO/processos selecionados_TJRO.xlsx")

## Preparando os dados

processos <- processos %>% 
  mutate(
    files = gsub("\\.",
                 "",
                 Numero),
    files = gsub("\\-",
                 "",
                 files),
    files = stripWhitespace(trimws(str_squish(files))))

## Cria uma lista com os nomes dos arquivos baixados

files <- list.files(path = "data/input/TJRO",
                    pattern = ".pdf")

## Transforma em data frame

files <- as.data.frame(files)

## Seleciona somente o padrão numérico 
## dos nomes dos arquivos e padroniza os resultados

files <- files %>% 
  mutate(files = str_sub(files,
                         19,
                         38),
         files = stripWhitespace(trimws(str_squish(files)))) %>% 
  unique()

## Junta com o arquivo dos processos de referência

processos <- left_join(processos,
                       files)

## Muda o diretório

setwd("data/input/TJRO")

## Renomeia os arquivos do TJRO

for(i in seq_along(files)){
  
  cat("Lendo", i, "\n")
  
  file <- str_sub(files[i],
                  19,
                  38)
  
  temp <- processos %>% 
    filter(grepl(file,
                 files))
  
  file.rename(files[i],
              paste0(temp$Numero,
                     ".pdf"))
  
}
