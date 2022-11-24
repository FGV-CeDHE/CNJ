
## TÍTULO: WEB SCRAPING DA JURISPRUDÊNCIA DO TRF1 - VERSÃO AUXILIAR
## DATA: 09/09/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(abjutils)
library(RSelenium)
library(httr)

## OBJETIVOS

#'         - Realizar raspagem de dados da jurisprudência 
#'           disponibilizada no site https://portal.trf1.jus.br/portaltrf1/pagina-inicial.htm.

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

## AMBIENTE DE TRABALHO

setwd("CNJ")

## FUNÇÕES

source("functions/windowSwitch.R", 
       encoding = "UTF-8")

# 1. Ambiente -------------------------------------------------------------

## Carregando os processos de referência

processos_2018_2019 <- readxl::read_xlsx("data/input/TRF1/mineração_detalhada_TRF1_2018-2019.xlsx")

processos_2020 <- readxl::read_xlsx("data/input/TRF1/mineração_detalhada_TRF1_2020.xlsx")

processos_2021 <- readxl::read_xlsx("data/input/TRF1/mineração_detalhada_TRF1_2021.xlsx")

## Empilhando os dados

processos <- rbind.fill(processos_2018_2019,
                        processos_2020,
                        processos_2021)

## Filtrando somente os anos necessários

processos <- processos %>% 
  filter(lubridate::year(DataJulgamento) %in% c(2020, 2021))

## Deixando somente os números dos processos

processos <- unique(processos$Numero)

## URL de referência

url_base <- "https://portal.trf1.jus.br/portaltrf1/pagina-inicial.htm"

## Preparando o navegador

driver <- rsDriver(chromever = "105.0.5195.19", ## Versão varia a depender do computador.
                   browser = "chrome", 
                   port = as.integer(4494), 
                   check = TRUE)

## Ativando o navegador

remDr <- driver[["client"]]

# 2. Raspagem dos dados ---------------------------------------------------

## Acessando a url salva

remDr$navigate(url_base)

## Criando uma data frame onde os dados serão armazenados

# df_final <- data.frame(Numero = NA,
#                        NumeroOriginal = NA)

## Salvando o id da página principal

main_handle <- remDr$getWindowHandles()[[1]]

## For loop que faz o download dos detalhes
## referentes ao Acórdão

for(i in seq_along(processos)){ 
                               
  cat("Lendo processo", i, "\n")
  
  ## Criando um data frame para armazenar os dados
  ## encontrados em cada página
  
  df_linha <- data.frame()
  
  ## Criando uma variável para o Número do Processo
  
  Numero <- processos[i]
  
  ## Procurando a seção de Consulta Processual
  
  webElement00 <- remDr$findElement(using = "xpath",
                                    '//*[@id="proc"]')$clickElement()
  
  ## Informando o número do processo
  
  webElement00 <- remDr$findElement(using = "xpath",
                                    '//*[@id="proc"]')$sendKeysToElement(list(processos[i]))
  
  ## Procurando o botão 'Pesquisar'
  
  webElem00 <- remDr$findElement(using = "xpath", 
                                 '//*[@id="btEnviar"]')$clickElement()
  
  
  ## Intervalo entre um processamento e outro
  
  Sys.sleep(2)
  
  ## Mudando para a nova página
  
  new_tab <- unlist(remDr$getWindowHandles())[unlist(remDr$getWindowHandles()) != main_handle]
  
  if(length(new_tab) == 1){
    
    windowSwitch(remDr, new_tab)
    
  }
  
  ## Variável "NumeroOriginal"
  
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
        
        cat("Lendo 'Número Processo Originário'", "\n")
        
        ## Informa o xpath 
        
        webElem1 <- remDr$findElement('xpath', 
                                      '//*[@id="aba-processo"]/table/tbody/tr[8]/td/a')
        
        ## Coleta o texto
        
        webElemtxt1 <- webElem1$getElementText()
        
        ## Mensagem e atribuição no caso de erro
        
        webElemtxt1 })}, error = function(e) {print("erro")
          NA})
    
    ## Extrai a informação coletada
    
    NumeroOriginal <- unlist(webElemtxt1)
    
    ## Muda o formato da variável para character
    
    NumeroOriginal <- as.character(NumeroOriginal)
    
  }
  
  webElem99 <- tryCatch({
    suppressMessages({
  
  ## Clicando no link
  
  remDr$findElement('xpath',
                    '//*[@id="aba-processo"]/table/tbody/tr[8]/td/a')$clickElement()
  
  
  ## Intervalo entre um processamento e outro
  
  Sys.sleep(2)
  
  ## Mudando para a nova página
  
  new_tab <- unlist(remDr$getWindowHandles())[unlist(remDr$getWindowHandles()) != main_handle]
  new_tab <- new_tab[new_tab != remDr$getCurrentWindowHandle()]
  
  if(length(new_tab) == 1){
    
    windowSwitch(remDr, new_tab)
    
  }
  
  webElem99 })}, error = function(e) {print("erro")
    "erro"})
  
  ## Variável "VARA"
  
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
        
        cat("Lendo 'Vara'", "\n")
        
        ## Informa o xpath 
        
        webElem2 <- remDr$findElement('xpath', 
                                      '//*[@id="aba-processo"]/table/tbody')
        
        ## Coleta o texto
        
        webElemtxt2 <- as.character(unlist(webElem2$getElementText()))
        
        ## Extraindo as informações coletadas
        
        df_linha <- data.frame(do.call('rbind', strsplit(webElemtxt2,
                                                         '\n',
                                                         fixed = TRUE)))
        
        df_linha$Numero <- Numero
        df_linha$NumeroOriginal <- NumeroOriginal
        
        ## Mensagem e atribuição no caso de erro
        
        webElemtxt2 })}, error = function(e) {print("erro")
          NA})
    
  }
  
  if(length(new_tab) == 1){
    
    ## Fechando a janela
    
    remDr$closeWindow()
    
    ## Mudando para a nova página
    
    new_tab <- unlist(remDr$getWindowHandles())[unlist(remDr$getWindowHandles()) != main_handle]
    
    if(length(new_tab) == 1){
      
      windowSwitch(remDr, new_tab)
      
      ## Fechando a janela
      
      remDr$closeWindow()
      
    }
    
    windowSwitch(remDr, main_handle)
    
  }
  
  ## Salva as informações coletadas
  
  ## Empilha todos os processos em um banco final
  
  df_final <- rbind.fill(df_final, 
                         df_linha)
  
  ## Limpando a consulta
  
  webElement00 <- remDr$findElement(using = "xpath",
                                    '//*[@id="proc"]')$clearElement()
  
}

## Salvando o banco

saveRDS(df_final,
        "data/output/TRF1/acórdãos_aux_TRF1_11102022_temp.rds")

# 3. Limpeza --------------------------------------------------------------

## Carregando os dados brutos

df_final <- readRDS("data/output/TRF1/acórdãos_aux_TRF1_10092022_temp.rds")
df_final2 <- readRDS("data/output/TRF1/acórdãos_aux_TRF1_11102022_temp.rds")

## Empilhando os dados

df_final <- rbind.fill(df_final,
                       df_final2)

## Características das varas

varas <- readxl::read_xlsx("data/input/TRF1/varas_TRF1.xlsx")

## Carregando os processos de referência

processos_2018_2019 <- readxl::read_xlsx("data/input/TRF1/mineração_detalhada_TRF1_2018-2019.xlsx")

processos_2020 <- readxl::read_xlsx("data/input/TRF1/mineração_detalhada_TRF1_2020.xlsx")

processos_2021 <- readxl::read_xlsx("data/input/TRF1/mineração_detalhada_TRF1_2021.xlsx")

## Empilhando os dados

processos <- rbind.fill(processos_2018_2019,
                        processos_2020,
                        processos_2021)

## Municípios da Amazônia Legal

## Carregando os dados

municipios <- readxl::read_xls("data/input/SireneJud/municípios_amazônia legal_2020.xls")

municipios <- municipios %>% 
  mutate(NM_MUN = str_to_upper(rm_accent(NM_MUN)),
         NM_MUN = ifelse(CD_MUN == 1100049,
                         "CACOAL",
                         NM_MUN))

## Quebrando as strings em várias colunas

df_final <- df_final %>% 
  dplyr::rename("NumeroOrigem" = "NumeroOriginal") %>% 
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

## Reorganizando os dados

df_final <- df_final %>% 
  mutate(NumeroOrigem = ifelse(Numero == "0043060-91.2009.4.01.9199",
                               "00.10.66200-410662004/MT",
                               ifelse(Numero == "0009598-85.2015.4.01.0000",
                                      "00.22.37200-722372007/GVS",
                                      ifelse(Numero == "0067459-92.2016.4.01.0000",
                                             "00.04.31201-64312016/PSS",
                                             ifelse(Numero == "0010402-29.2010.4.01.0000",
                                                    "00.15.73200-915732009/MA",
                                                    ifelse(Numero == "0027132-71.2017.4.01.0000",
                                                           "00.06.77200-96772009/JFAM",
                                                           ifelse(Numero == "0079864-24.2010.4.01.9199",
                                                                  "00.02.51200-12512001/MA",
                                                                  ifelse(Numero == "0026498-51.2012.4.01.0000",
                                                                         "00.00.95201-1952011/STM",
                                                                         ifelse(Numero == "0025991-17.2017.4.01.0000",
                                                                                "00.02.75201-62752016/JFRR",
                                                                                ifelse(Numero == "0038170-80.2017.4.01.0000",
                                                                                       "00.01.43200-91432009/JFAM",
                                                                                       ifelse(Numero == "0032777-67.2013.4.01.9199",
                                                                                              "00.01.21200-71212007/AM",
                                                                                              ifelse(Numero == "0024544-57.2018.4.01.0000",
                                                                                                     "00.00.00000-00/JFDF",
                                                                                                     ifelse(Numero == "0055652-46.2014.4.01.0000",
                                                                                                            "00.02.20200-62202006/MT",
                                                                                                            NumeroOrigem))))))))))))) %>% 
  separate(NumeroOrigem,
           sep = "/",
           into = c("NumeroOrigem",
                    "TribunalOrigem")) %>% 
  mutate(Vara = ifelse(X3_1 == "Vara",
                       X3,
                       ifelse(X4_1 == "Vara",
                              X4,
                              ifelse(X5_1 == "Vara",
                                     X5,
                                     NA))),
         Vara = ifelse(Numero == "0018258-82.2017.4.01.3400",
                       "20ª VARA BRASÍLIA",
                       Vara)) %>% 
  mutate(Ano = str_sub(Numero,
                       12,
                       15)) %>% 
  filter(!is.na(NumeroOrigem)) %>% 
  select(Numero,
         NumeroOrigem,
         Ano,
         TribunalOrigem,
         Vara)

## Juntando com os dados das varas

df_final <- left_join(df_final,
                      varas)

## Juntando com os dados dos processos

processos <- left_join(processos,
                       df_final)

## Reorganizando os dados

processos <- processos %>% 
  select(Numero,
         NumeroOrigem,
         Tipo:Origem,
         TribunalOrigem,
         Vara,
         UF,
         Municipios,
         OrgaoJulgador:`recursos naturais/biodiversidade/ecologico`) %>% 
  filter(UF %in% municipios$SIGLA) %>% 
  mutate(Municipios = str_to_upper(Municipios),
         TribunalOrigem = str_to_upper(TribunalOrigem),
         Vara = str_to_upper(Vara))

# 4. Salva ----------------------------------------------------------------

write.csv(processos,
          "data/output/TRF1/processos_amazônia legal_2018-2021_TRF1.csv",
          fileEncoding = "UTF-8")
