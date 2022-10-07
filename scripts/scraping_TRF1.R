
## TÍTULO: WEB SCRAPING DA JURISPRUDÊNCIA DO TRF1
## DATA: 28/06/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(abjutils)
library(RSelenium)
library(httr)

## OBJETIVOS

#'         - Realizar raspagem de dados da jurisprudência 
#'           disponibilizada no site https://www2.cjf.jus.br/jurisprudencia/trf1/index.xhtml.

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

#'           1. Processos julgados entre 01/01/2017 até 15/06/2022.

## FUNÇÕES

source("scripts/windowSwitch.R", 
       encoding = "UTF-8")

source("scripts/filter.R", 
       encoding = "UTF-8")

# 1. Ambiente -------------------------------------------------------------

## URL de referência

url_base <- "https://www2.cjf.jus.br/jurisprudencia/trf1/index.xhtml"

## Preparando o navegador

driver <- rsDriver(chromever = "103.0.5060.24", ## Versão varia a depender do computador.
                   browser = "chrome", 
                   port = as.integer(4494), 
                   check = TRUE)

## Ativando o navegador

remDr <- driver[["client"]]

# 2. Raspagem dos dados ---------------------------------------------------

## Acessando a url salva

remDr$navigate(url_base)

## Procurando a seção de Busca Avançada

webElement00 <- remDr$findElement(using = "xpath",
                                  '//*[@id="formulario:ckbAvancada"]/div[2]')$clickElement()

## Procurando a seção para consulta de Acórdãos 

webElem00 <- remDr$findElement(using = "xpath", 
                               '//*[@id="formulario:textoLivre"]')

## Informando quais as palavras-chave

webElem00$sendKeysToElement(list("ementa ou lei ou documento ou a"))

## Procurando a seção "Data de Julgamento"

webElem00 <- remDr$findElement(using = "xpath", 
                               '//*[@id="formulario:j_idt37_input"]')$sendKeysToElement(list("01/01/2017"))

webElem00 <- remDr$findElement(using = "xpath", 
                               '//*[@id="formulario:j_idt39_input"]')$sendKeysToElement(list("15/06/2022"))

## É necessário selecionar o recaptcha manualmente.

## Procurando o botão 'Pesquisar'

webElem00 <- remDr$findElement(using = "xpath", 
                               '//*[@id="formulario:actPesquisar"]')$clickElement()

## Alterando o número de documentos por página

webElem00 <- remDr$findElement(using = "xpath", 
                               "//*[contains(@id,'formulario:tabelaDocumentos:')]")

## Clicando no elemento

webElem00$clickElement()

## Selecionando 50 documentos por página 

webElem00$sendKeysToElement(list(key = "down_arrow"))

## 198.195 resultados de Acórdão. 

## Informações disponíveis:

#'      1. ID;
#'      2. Tipo;
#'      3. Número;
#'      4. Classe;
#'      5. Relator;
#'      6. Origem;
#'      7. Órgão Julgador;
#'      8. Data;
#'      9. Data de Publicação;
#'      10. Fonte da Publicação;
#'      11. Ementa;
#'      12. Decisão;
#'      13. Texto;
#'      14. Inteiro Teor.

## 2.1. Acórdãos -----------------------------------------------------------

## Criando uma data frame onde os dados serão armazenados

#df_final <- data.frame()

## Salvando o id da página principal

main_handle <- remDr$getWindowHandles()[[1]]

## For loop que faz o download dos detalhes
## referentes ao Acórdão

for(i in 1:3964){ ## 3.964 páginas de informação.
                  ## 198.195 registros.
  
  cat("Lendo página", i, "\n")
  
  ## Criando um data frame para armazenar os dados
  ## encontrados em cada página
  
  df_pagina <- data.frame()
  
  ## Coletando o número da página
  
  Pagina <- i
  
  ## Intervalo entre um processamento e outro
  
  Sys.sleep(15)
  
  ## Cria uma variável vazia onde as informações serão armazenadas
  
  webElem1 <- NA
  
  ## Variável que contabiliza as tentativas de 
  ## coletar a informação
  
  tentativas <- 0
  
  ## Coletando as informações de toda a página
  
  while(is.na(webElem1) |
        tentativas < 10){
    
    webElem1 <- tryCatch({
      suppressMessages({
        
        ## Atualizando a contagem de tentativas
        
        tentativas <- tentativas + 1
        
        ## Informa o xpath 
        
        webElem1 <- remDr$findElements("xpath",
                                       "//*[contains(@id,'item_resultado-')]")
        
        ## Mensagem e atribuição no caso de erro
        
        webElem1 })}, error = function(e) {print("erro")
          NA})
  }
  
  
  for(j in 1:50){ ## 50 resultados por página.
    
    cat("Resultado", j, "\n")
    
    webElem00 <- tryCatch({
      suppressMessages({
    
    ## Coletando o texto
    
    webElemtxt1 <- as.character(unlist(webElem1[[j]]$getElementText()))
    
    ## Extraindo as informações coletadas
    
    df_linha <- data.frame(do.call('rbind', strsplit(webElemtxt1,
                                                     '\n',
                                                     fixed = TRUE)))
    
    ## Cria uma variável com a Página do processo
    
    df_linha$Pagina <- Pagina
    
    ## Empilha todos os Inquéritos Civis encontrados na página 
    ## em um úncio dataframe
    
    df_pagina <- bind_rows(df_pagina, df_linha)
    
    webElem00 })}, error = function(e) {print("erro")
      NA})
    
  }
  
  ## Empilha todos os processos em um banco final
  
  df_final <- bind_rows(df_final, df_pagina)
  
  ## Rolando a página para baixo
  
  webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "end"))
  webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "end"))
  
  ## Salvando o xpath da função "próxima página"
  
  webElem00 <- remDr$findElement(using = "css selector", 
                                 "#formulario\\:tabelaDocumentos_paginator_bottom > a.ui-paginator-next.ui-state-default.ui-corner-all")
  
  Sys.sleep(1)
  
  webElem00$clickElement()
  
  ## Rolando a página para cima
  
  webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "home"))
  
}

## Salvando o banco

saveRDS(df_final,
        "data/output/TRF1/acórdãos_TRF1_23072022_temp.rds")

## 2.2. Decisões Monocráticas ----------------------------------------------

## Criando uma data frame onde os dados serão armazenados

#df_final <- data.frame()

## Salvando o id da página principal

main_handle <- remDr$getWindowHandles()[[1]]

## For loop que faz o download dos detalhes
## referentes ao Acórdão

for(i in 946:3964){ ## 3.964 páginas de informação.
  ## 198.195 registros.
  
  cat("Lendo página", i, "\n")
  
  ## Criando um data frame para armazenar os dados
  ## encontrados em cada página
  
  df_pagina <- data.frame()
  
  ## Coletando o número da página
  
  Pagina <- i
  
  ## Intervalo entre um processamento e outro
  
  Sys.sleep(15)
  
  ## Cria uma variável vazia onde as informações serão armazenadas
  
  webElem1 <- NA
  
  ## Variável que contabiliza as tentativas de 
  ## coletar a informação
  
  tentativas <- 0
  
  ## Coletando as informações de toda a página
  
  while(is.na(webElem1) && tentativas < 10){
    
    webElem1 <- tryCatch({
      suppressMessages({
        
        ## Atualizando a contagem de tentativas
        
        tentativas <- tentativas + 1
        
        ## Informa o xpath 
        
        webElem1 <- remDr$findElements("xpath",
                                       "//*[contains(@id,'item_resultado-')]")
        
        ## Mensagem e atribuição no caso de erro
        
        webElem1 })}, error = function(e) {print("erro")
          NA})
  }
  
  for(j in 1:50){ ## 50 resultados por página.
    
    cat("Resultado", j, "\n")
    
    webElem00 <- tryCatch({
      suppressMessages({
        
        ## Coletando o texto
        
        webElemtxt1 <- as.character(unlist(webElem1[[j]]$getElementText()))
        
        ## Extraindo as informações coletadas
        
        df_linha <- data.frame(do.call('rbind', strsplit(webElemtxt1,
                                                         '\n',
                                                         fixed = TRUE)))
        
        ## Cria uma variável com a Página do processo
        
        df_linha$Pagina <- Pagina
        
        ## Empilha todos os Inquéritos Civis encontrados na página 
        ## em um úncio dataframe
        
        df_pagina <- bind_rows(df_pagina, df_linha)
        
        webElem00 })}, error = function(e) {print("erro")
          NA})
    
  }
  
  ## Empilha todos os processos em um banco final
  
  df_final <- bind_rows(df_final, df_pagina)
  
  ## Rolando a página para baixo
  
  webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "end"))
  
  ## Salvando o xpath da função "próxima página"
  
  webElem00 <- remDr$findElement(using = "css selector", 
                                 "#formulario\\:tabelaDocumentos_paginator_bottom > a.ui-paginator-next.ui-state-default.ui-corner-all")$clickElement()
  
  ## Rolando a página para cima
  
  webElem00 <- remDr$findElement("css", "body")$sendKeysToElement(list(key = "home"))
  
}

## Salvando o banco

saveRDS(df_final,
        "data/output/TRF1/acórdãos_TRF1_07072022_temp.rds")

# 3. Limpeza --------------------------------------------------------------

## 3.1. Acórdãos -----------------------------------------------------------

df_final <- readRDS("data/output/TRF1/acórdãos_TRF1_23072022_temp.rds")

## Organizando e estruturando o banco

df_final <- df_final %>% 
  mutate(Tipo = X2,
         Numero = X4,
         Classe = X7,
         Relator = ifelse(X8 == "Relator(a)",
                          X9,
                          NA),
         RelatorConvocado = ifelse(X10 == "Relator convocado",
                                   X11,
                                   ifelse(X12 == "Relator convocado",
                                          X13,
                                          NA)),
         RelatorAcordao = ifelse(X10 == "Relator para Acórdão",
                                 X11,
                                 NA),
         Revisor = ifelse(X10 == "Revisor",
                          X11,
                          ifelse(X12 == "Revisor",
                                 X13,
                                 NA)),
         Origem = ifelse(X8 == "Origem",
                         X9,
                         ifelse(X10 == "Origem",
                                X11,
                                ifelse(X12 == "Origem",
                                       X13,
                                       ifelse(X14 == "Origem",
                                              X15,
                                              NA)))),
         OrgaoJulgador = ifelse(X12 == "Órgão julgador",
                                X13,
                                ifelse(X14 == "Órgão julgador",
                                       X15,
                                       ifelse(X16 == "Órgão julgador",
                                              X17,
                                              NA))),
         DataJulgamento = ifelse(X12 == "Data",
                       X13,
                       ifelse(X14 == "Data",
                              X15,
                              ifelse(X16 == "Data",
                                     X17,
                                     ifelse(X18 == "Data",
                                            X19,
                                            NA)))),
         DataPublicacao = ifelse(X14 == "Data da publicação",
                                 X15,
                                 ifelse(X16 == "Data da publicação",
                                        X17,
                                        ifelse(X18 == "Data da publicação",
                                               X19,
                                               ifelse(X20 == "Data da publicação",
                                                      X21,
                                                      NA)))),
         FontePublicacao = ifelse(X16 == "Fonte da publicação",
                                  X17,
                                  ifelse(X18 == "Fonte da publicação",
                                         X19,
                                         ifelse(X20 == "Fonte da publicação",
                                                X21,
                                                NA))),
         Ementa = ifelse(X19 == "Ementa",
                         X20,
                         ifelse(X21 == "Ementa",
                                X22,
                                ifelse(X22 == "Ementa",
                                       X23,
                                       ifelse(X23 == "Ementa",
                                              X24,
                                              ifelse(X24 == "Ementa",
                                                     X25,
                                                     ifelse(X25 == "Ementa",
                                                            X26,
                                                            NA)))))),
         Decisao = ifelse(X23 == "Decisão",
                          X24,
                        ifelse(X24 == "Decisão",
                               X25,
                               ifelse(X25 == "Decisão",
                                      X26,
                                      ifelse(X27 == "Decisão",
                                             X28,
                                             NA)))),
         Texto = ifelse(X25 == "Texto",
                         X26,
                         ifelse(X26 == "Texto",
                                X27,
                                ifelse(X27 == "Texto",
                                       X28,
                                       ifelse(X28 == "Texto",
                                              X29,
                                              ifelse(X29 == "Texto",
                                                     X30,
                                                     NA)))))) %>% 
  mutate(DataJulgamento = as.Date(DataJulgamento,
                                  format = "%d/%m/%Y"),
         DataPublicacao = as.Date(DataPublicacao,
                                  format = "%d/%m/%Y"),
         Tipo = str_to_upper(Tipo),
         Relator = str_to_upper(Relator),
         RelatorConvocado = str_to_upper(RelatorConvocado),
         RelatorAcordao = str_to_upper(RelatorAcordao),
         Revisor = str_to_upper(Revisor),
         Origem = str_to_upper(Origem),
         OrgaoJulgador = str_to_upper(OrgaoJulgador),
         FontePublicacao = str_to_upper(FontePublicacao)) %>% 
  select(Numero,
         Tipo,
         Classe,
         Relator,
         RelatorConvocado,
         RelatorAcordao,
         Revisor,
         Origem,
         OrgaoJulgador,
         FontePublicacao,
         DataPublicacao,
         DataJulgamento,
         Ementa,
         Decisao,
         Texto) %>% 
  arrange(DataJulgamento,
          Origem,
          OrgaoJulgador)

## Remove os espaços em branco em excesso

for (i in colnames(df_final)){
  
  df_final[[i]] <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", 
                        df_final[[i]], 
                        perl = TRUE)
}

## 3.2. Decisões Monocráticas ----------------------------------------------


# 4. Inteiro Teor ---------------------------------------------------------

## Criando uma lista com os números únicos dos processos

processos <- na.omit(unique(df_final$Numero))

## URL de referência

url_base <- "https://pje2g.trf1.jus.br/consultapublica/ConsultaPublica/listView.seam"

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
  excludeSwitches = list("enable-automation")
))

## Preparando o navegador

driver <- rsDriver(chromever = "103.0.5060.53", ## Versão varia a depender do computador.
                   browser = "chrome",
                   port = as.integer(4495),
                   check = TRUE,
                   extraCapabilities = eCaps
)

## Ativando o navegador

remDr <- driver[["client"]]

## Acessando o endereço

remDr$navigate(url_base)

## Salvando o id da página principal

main_handle <- remDr$getWindowHandles()[[1]]

## Data frame onde os dados serão armazenados

temp <- list()
inteiroteor <- list()

## Índice que recarrega a página a cada certo número de consultas

recarrega <- 100

## Coletando a URL do Inteiro Teor dos processos

for(i in seq_along(processos)){
  
  cat("Lendo", i, "\n")
  
  ## Índice de tentativas
  
  ## Zerando o banco temporário novamente
  
  temp <- list()
  
  webElem99 <- tryCatch({
    suppressMessages({
      
      tentativas_princ <- 0
      
      while(is.null(nrow(temp)) && tentativas_princ < 10){
        
        tentativas_princ <- tentativas_princ + 1
  
  ## Procurando a seção para consulta de Acórdãos 
  
  webElem00 <- remDr$findElement(using = "xpath", 
                                 '//*[@id="fPP:numProcesso-inputNumeroProcessoDecoration:numProcesso-inputNumeroProcesso"]')
  
  webElem00$clearElement()
  
  ## Informando quais as palavras-chave
  
  webElem00$sendKeysToElement(list(processos[i]))
  
  ## Clicando no botão 'Pesquisar'
  
  webElem00 <- remDr$findElement(using = "xpath", 
                                 '//*[@id="fPP:searchProcessos"]')$clickElement()

    
  Sys.sleep(5)
  
  ## Valor inicial da variável new_tab
  
  new_tab <- NULL
  
  webElem00 <- tryCatch({
    suppressMessages({
      
      tentativas <- 0
      
      while(length(remDr$getWindowHandles()) == 1 && tentativas < 10){
        
        tentativas <- tentativas + 1
        
        ## Abrindo nova página com detalhes do acórdão
        
        webElem00 <- remDr$findElement(using = "xpath", 
                                       '/html/body/div[6]/div/div/div/div[2]/form/div[2]/div/table/tbody/tr/td[1]/a')
        
        webElem00$clickElement()
        
      }
      
      ## Mudando para a nova página
      
      new_tab <- unlist(remDr$getWindowHandles())[unlist(remDr$getWindowHandles()) != main_handle]
      
      if(length(new_tab) == 1){
        
        windowSwitch(remDr, new_tab)
        
      }
      
      ## Procurando o Acórdão
      
      Sys.sleep(2)
      
      webElem00 <- remDr$findElement(using = "xpath", 
                                     '//*[contains(@id, "processoDocumentoGridTab:2") and contains(@id, ":idView")]')
      
      webElem00 <- str_split(webElem00$getElementAttribute("onclick"),
                             ",")[[1]][2]
      
      webElem00 <- str_extract(str_split(webElem00,
                                         ";")[[1]][1],
                               "(?<=\\').*(?=\\')")
      
      webElem00 })}, error = function(e) {print("erro")
        NA})
  
  ## Fechando a janela aberta
  
  if(length(new_tab) == 1){
    
    ## Fechando a janela
    
    remDr$closeWindow()
    
    ## Mudando para janela principal
    
    windowSwitch(remDr, main_handle)
    
  }
  
  ## Salvando os dados
  
  temp$Numero <- processos[i]
  temp$URL_InteiroTeor <- webElem00
  
  ## Transformando em data frame
  
  temp <- as.data.frame(temp)
  
      }
  
  webElem99 })}, error = function(e) {print("erro")
    NA})
  
  ## Empilhando os dados
  
  inteiroteor2 <- rbind(inteiroteor2,
                       temp)
  
  ## Condição que recarrega a página
  
  if(i >= recarrega){
    
    remDr$refresh()
    # 
    # saveRDS(inteiroteor,
    #         "data/output/TRF1/URLS_inteiro teor_TRF1.rds")
    
    recarrega <- recarrega + 100
    
  }
  
}


  # 5. Filtragem ------------------------------------------------------------

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
          Origem,
          OrgaoJulgador)

# 6. Salva ----------------------------------------------------------------

## Salva o banco original, sem filtros

saveRDS(df_final,
        "data/output/TRF1/acórdãos_TRF1_25072022.rds")

write.csv(df_final,
          "data/output/TRF1/acórdãos_TRF1_25072022.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

## Salva o banco com os filtros de período e tema

write.csv(df_final_filtros,
          "data/output/TRF1/acórdãos_TRF1_25072022_com filtros.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

saveRDS(df_final_filtros,
        "data/output/TRF1/acórdãos_TRF1_25072022_com filtros.rds")

saveRDS(inteiroteor,
        "data/output/TRF1/inteiro teor_TRF1.rds")
