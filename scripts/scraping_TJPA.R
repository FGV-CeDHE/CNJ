
## DIRETÓRIO

setwd("CNJ")

## FUNÇÕES

source("functions/wordFilter.R", 
       encoding = "UTF-8")

#função de mineração do TST
minera_objeto_unitario <- function(elementMain=NULL, download=download,fat_temp=fat_temp){
  #inicializa objeto
  objeto_auxiliar <- mining_object()
  objeto_auxiliar$tribunal <- "TJPA"
  
  #numero CNJ
  elementMain$findChildElements(using = 'xpath',
                               value = 'div[2]/div[1]/p') -> element
  
  if(length(element)>0){
    element <- element[[1]]
    objeto_auxiliar$numero_processo <- element$getElementText()[[1]]
  }
  
  
  #ID documento
  elementMain$findChildElements(using = 'xpath',
                                         value = 'div[2]/div[2]/p') -> element
  if(length(element)>0){
    element <- element[[1]]
    objeto_auxiliar$numero_documento <- element$getElementText()[[1]]
  }
  #ID acordao
  elementMain$findChildElements(using = 'xpath',
                                         value = 'div[2]/div[3]/p') -> element
  if(length(element)>0){
    element <- element[[1]]
    objeto_auxiliar$numero_acordao <- element$getElementText()[[1]]
  }
  #tipo de processo
  elementMain$findChildElements(using = 'xpath',
                                         value = 'div[3]/div[1]/p') -> element
  if(length(element)>0){
    element <- element[[1]]
    objeto_auxiliar$tipo_ação <- element$getElementText()[[1]]
  }
  #orgao julgador
  elementMain$findChildElements(using = 'xpath',
                                         value = 'div[3]/div[2]/p') -> element
  if(length(element)>0){
    element <- element[[1]]  
    objeto_auxiliar$orgao_judiciante <- element$getElementText()[[1]]
  }
  #tipo de documento
  elementMain$findChildElements(using = 'xpath',
                                         value = 'div[3]/div[3]/p') -> element
  if(length(element)>0){
    element <- element[[1]]
    objeto_auxiliar$tipo_documento <- element$getElementText()[[1]]
  }
  #relator
  elementMain$findChildElements(using = 'xpath',
                                         value = ' div[4]/div[1]/p') -> element
  if(length(element)>0){
    element <- element[[1]]
    objeto_auxiliar$relator <- stringr::str_remove(string = element$getElementText()[[1]],"Relator: ")
  }
  #secao
  elementMain$findChildElements(using = 'xpath',
                                         value = 'div[4]/div[2]') -> element
  if(length(element)>0){
    element <- element[[1]]
    objeto_auxiliar$secao <- stringr::str_remove(string = element$getElementText()[[1]],"Seção: ")
  }
  #data julgamento
  elementMain$findChildElements(using = 'xpath',
                                         value = 'div[6]/div[1]') -> element
  if(length(element)>0){
    element <- element[[1]]
    objeto_auxiliar$data_julgamento <- stringr::str_remove(string = element$getElementText()[[1]],"Data de Julgamento: ")
  }
  #data julgamento
  elementMain$findChildElements(using = 'xpath',
                                         value = 'div[6]/div[2]') -> element
  if(length(element)>0){
    element <- element[[1]]
    objeto_auxiliar$data_publicacao <- stringr::str_remove(string = element$getElementText()[[1]],"Data de Publicação: ")
  }
  #link
  elementMain$findChildElements(using = 'xpath',
                                         value = 'div[7]/div[1]/a') -> element
  if(length(element)>0){
    element <- element[[1]]
    objeto_auxiliar$link_html_internet <- element$getElementAttribute(attrName = "href")[[1]]
  }
  #leitura de ementa
  #page <- rvest::read_html(objeto_auxiliar$link_html_internet)
  #
  #ini <- which(tolower(rvest::html_text(rvest::html_nodes(x = page,xpath = "/html/body/p")))=='ementa')[1]+1
  #fim <- which(tolower(rvest::html_text(rvest::html_nodes(x = page,xpath = "/html/body/p")))=='relatório')[1]-1
  
  #salvando pedaço de ementa
  elementMain$findChildElement(using = 'xpath',
                               value = "div/div/span") -> element
  
  objeto_auxiliar$ementa <- element$getElementText()[[1]]
  
  #abrindo e salvando ementa
  if(length(elementMain$findChildElements(using = 'xpath',
                                         value = "div/div/a[contains(@class, 'span_ementa ver-ementa')]"))>0){
    
    elementMain$findChildElement(using = 'xpath',
                                 value = "div/div/a[contains(@class, 'span_ementa ver-ementa')]") -> element
    element$clickElement()
    
    elementMain$findChildElement(using = 'xpath',
                                 value = "div/div/span/span[contains(@class, 'text_ementa')]") -> element
    
    objeto_auxiliar$ementa <- paste0(objeto_auxiliar$ementa, element$getElementText()[[1]])
  }


  
  
  
  #objeto_auxiliar$ementa <- paste0(rvest::html_text(rvest::html_nodes(x = page,xpath = paste0("/html/body/p[position()<=",fim,"]"))[ini:fim],trim = T),collapse = "")
  objeto_auxiliar$ementa
  objeto_auxiliar$ID <- paste0("TJPA",
                               objeto_auxiliar$numero_processo,
                               objeto_auxiliar$relator,
                               paste0(stringr::str_remove_all(string = objeto_auxiliar[,c("data_publicacao","data_julgamento")],pattern = "/"),collapse = ""))
  
  
  
  if(download==T){
    tryCatch(expr = download.file(url = objeto_auxiliar$link_html_internet,destfile = paste0("TJPA/", objeto_auxiliar$ID,"_documento.html"),),finally = print("Passou"))
    
    objeto_auxiliar$link_html_local <- paste0("TJPA/", objeto_auxiliar$ID,"_documento.html")
  }
  
  return(objeto_auxiliar)
}

minera_pagina <- function(client, download=F,fat_temp=1){
  
  list_of_elements <- client$findElements(using = 'xpath',
                                          value = "/html/body/div/div/div/div/div/div/ul/div[contains(@class,'panel_results')]")
  
  for(i in 1:length(list_of_elements)){
    if(i==1){
      result_pag <- minera_objeto_unitario(elementMain = list_of_elements[[i]],
                                           download = download,
                                           fat_temp = fat_temp)
    }else{
      result_pag <- bind_rows(result_pag, 
                              minera_objeto_unitario(elementMain = list_of_elements[[i]],
                                                     download = download,
                                                     fat_temp = fat_temp))
    }
  }
  
  return(result_pag)
  

}

waiting <- function(client){
  
  
  
  element <- client$findElements(using = 'xpath',
                                 value = '/html/body/app-root/app-loader/div')

  
  
  count<-0
  while(length(element)>0 & count < 500){
    element <- client$findElements(using = 'xpath',
                                 value = '/html/body/app-root/app-loader/div')

    Sys.sleep(fat_temp*0.5)
    count<-count+1
  }
  Sys.sleep(fat_temp*0.5)
}

realiza_busca <- function(client,fat_temp=1,palavras_chaves,dataIni, dataFim){
  Sys.sleep(fat_temp)
  

  #wait
  
  #palavras chaves na busca
  element <- client$findElement(using = "xpath",
                                value = "/html/body/div/div/form/div/div/div/input[contains(@class,'form-control search_q')]")
  
  element$sendKeysToElement(list(palavras_chaves))
  
  
  #data

  #filtrando as datas
  element <- client$findElement(using = 'xpath',
                                value = '//*[@id="julg_dataIni"]')
  
  element$sendKeysToElement(list(dataIni))
  
  #if(stringr::str_length(day(now()))<2){
  #  dia <- paste0("0", day(now()))
  #}else{
  #  dia <- day(now())
  #}
  #if(stringr::str_length(month(now()))<2){
  #  mes <- paste0("0", month(now()))
  #}else{
  #  mes <- month(now())
  #}
  
  element <- client$findElement(using = 'xpath',
                                value = '//*[@id="julg_dataFim"]')
  
  #element$sendKeysToElement(list(paste0(dia,"/",mes,"/",year(now()))))
  element$sendKeysToElement(list(dataFim))
  #clicando na busca
  element <- client$findElement(using = "xpath",
                                value = '/html/body/div/div/form/div/div/div/div/button[contains(@class,"btn btn-primary btn_buscar margin_left_30")]')
  element$clickElement()
  
  Sys.sleep(fat_temp)
  
}


#rodando a mineração
setwd("mineracao/")

source("MB01_master_functions.R")
source("B01_general_auxiliar_functions.R")

system("docker run -d -p 4446:4444 -p 5902:5900 selenium/standalone-firefox-debug")

palavras_chaves <- c('ementa OR lei OR documento OR a OR o OR e')

browser <- 'firefox'
fat_temp <- 0.8
download<-TRUE
port<-4446




require(RSelenium)
require(rvest)
require(dplyr)
require(xml2)
require(lubridate)
source("B01_general_auxiliar_functions.R")





client <- remoteDriver(port = port, browser = browser, remoteServerAddr = "localhost")



initLink <- "http://gsa-index.tjpa.jus.br/consultas/search?q=&client=consultas&proxystylesheet=consultas&site=jurisprudencia&sort=date%3AD%3AS%3Ad1&aba=JP"



client$open()

dataIni <- as.Date(paste("01","01",2017,sep = "-"),format = "%d-%m-%Y")
dataFim <- as.Date(now())
dataIniAux <- dataIni
dataFimAux <- dataFim  
final<-NULL

while(dataFim-dataIniAux > 0){
  client$navigate(initLink)

    

    #datas auxiliares para busca
  dataIniAuxf <- paste(stringr::str_extract(dataIniAux,"[[:digit:]]{2}$"),
                    stringr::str_extract(dataIniAux,"(?<=-)[[:digit:]]{2}(?=-)"),
                    stringr::str_extract(dataIniAux,"^[[:digit:]]{4}(?=-)"),sep = "/")
  
  dataFimAuxf <- paste(stringr::str_extract(dataFimAux,"[[:digit:]]{2}$"),
                    stringr::str_extract(dataFimAux,"(?<=-)[[:digit:]]{2}(?=-)"),
                    stringr::str_extract(dataFimAux,"^[[:digit:]]{4}(?=-)"),sep = "/")

  #checando o numero de resultados - limite de 1000 
  realiza_busca(client = client,fat_temp = 1.5,palavras_chaves = palavras_chaves,dataIni = dataIniAuxf, dataFim=dataFimAuxf)

  element <- client$findElement(using = 'xpath',
                     value='/html/body/div[1]/div[2]/div[2]/div/div[1]/div[1]/div/span[3]')
  
  #checando o numero
  if(as.numeric(element$getElementText()[[1]])>=1000){
    #caso maior que mil, refaz a busca com periodo menor
    dataFimAux <- dataIniAux+(dataFimAux-dataIniAux)/2
  }else{
    #caso menor que mil, faz a paginação
    elementControl <- client$findElements(using = 'xpath',
                                          value='/html/body/div/div/div/div/div/div/div/span/a[contains(text(),"Próximo")]')
    
    while(length(elementControl)>0){
      objeto_aux <- minera_pagina(client = client,download = T,fat_temp = 1)
      if(is.null(final==T)){
        final <- objeto_aux
      }else{
        final <- bind_rows(final, objeto_aux)
      }
      
      elementControl <- client$findElements(using = 'xpath',
                                            value='/html/body/div/div/div/div/div/div/div/span/a[contains(text(),"Próximo")]')
      if(length(elementControl)>0)  elementControl[[1]]$clickElement()
    }

    
    write.csv2(final,"resultado_consolidado_TJPA.csv")
    dataIniAux <- dataFimAux+1
    dataFimAux <- dataFim
  }
  
  gc()
    

}
  
#apos a mineracao, tratamento do reusltado
aux <- read.csv2("resultado_consolidado_TJPA.csv")
aux <- aux[,c(-1,-2)]

aux <- aux[duplicated(aux$numero_processo)==FALSE,]  

aux$link_html_local <- stringr::str_replace_all(aux$link_html_local,"TJPA/TJPA/","TJPA/")

write.csv2(aux,"resultado_TJPA_final_2017_a_2022.csv")

files <- list.files('TJPA')

for(i in 1:length(aux$ID)){
  #aux$link_html_local[i] %in% files
  e <- try(aux$inteiro_teor[i] <- html_text(read_html(aux$link_html_local[i])))
  if(inherits(e, "try-error")) {
    aux$inteiro_teor[i] <- html_text(read_html(aux$link_html_internet[i]))
  }
             

  print(i/length(aux$ID))
}
write.csv2(aux,"int_teor_resultado_TJPA_final_2017_a_2022.csv")


# 3. Limpeza --------------------------------------------------------------

## Carregando os dados do STF

df_final <- readRDS("data/input/TJPA/tjpa_com filtros.rds")

## Organizando os dados

df_final <- df_final %>% 
  rename("NumeroDocumento" = "numero_documento",
         "NumeroAcordao" = "numero_acordao",
         "Numero" = "numero_processo",
         "DataJulgamento" = "data_julgamento",
         "DataPublicacao" = "data_publicacao",
         "OrgaoJudiciante" = "orgao_judiciante",
         "Relator" = "relator",
         "TipoDocumento" = "tipo_documento",
         "TipoAcao" = "tipo_ação",
         "TipoRecurso" = "tipo_recurso",
         "URL_InteiroTeor" = "link_html_internet",
         "Ementa" = "ementa",
         "Secao" = "secao") %>% 
  select(NumeroDocumento,
         NumeroAcordao,
         Numero,
         DataJulgamento,
         DataPublicacao,
         Secao,
         OrgaoJudiciante,
         Relator,
         TipoAcao,
         TipoRecurso,
         Ementa,
         URL_InteiroTeor) %>% 
  mutate(OrgaoJudiciante = str_to_upper(OrgaoJudiciante),
         Relator = str_to_upper(Relator),
         TipoAcao = str_to_upper(TipoAcao),
         Ementa = gsub("\\Ementa: ", "", Ementa),
         Ementa = gsub("\\EMENTA: ", "", Ementa),
         Ementa = stripWhitespace(gsub("\\EMENTA ", "", Ementa)),
         DataPublicacao = as.Date(DataPublicacao,
                                  format = "%d/%m/%Y"),
         DataJulgamento = as.Date(DataJulgamento,
                                  format = "%d/%m/%Y"),
         Secao = ifelse(Secao == "CÃ VEL",
                        "CÍVEL",
                        Secao)) %>% 
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
        "data/output/TJPA/acórdãos_TJPA_11112022.rds")

write.csv(df_final,
          "data/output/TJPA/acórdãos_TJPA_11112022.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

## Salva o banco com os filtros de período e tema

write.csv(df_final_filtros,
          "data/output/TJPA/acórdãos_TJPA_11112022_com filtros.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

saveRDS(df_final_filtros,
        "data/output/TJPA/acórdãos_TJPA_11112022_com filtros.rds")


# 6. Download dos documentos ----------------------------------------------

urls <- unique(tjpa$link_html_internet)

for(i in seq_along(urls)){
  
  cat("Lendo", i, "\n")
  
  Numero <- str_split(urls[i],
                      "/")[[1]][7]
  
  Numero <- str_split(Numero,
                      "\\+")[[1]][1]
  
  download.file(urls[i],
                paste0("data/output/TJPA/Inteiro Teor/inteiroteor_TJPA_",
                       Numero,
                       ".html"), 
                mode = "wb",
                quiet = TRUE)
  
  
}

htmls <- list.files(path = "data/output/TJPA/Inteiro Teor",
                    pattern = ".html")

temp <- list()

inteiroteor <- data.frame("Id" = rep(NA,
                                         84338),
                          "InteiroTeor" = rep(NA,
                                              84338))

for(i in 35309:84338){
  
  cat("Lendo", i, "\n")
  
  webElem00 <- tryCatch({
    suppressMessages({
  
  inteiroteor$Id[i] <- parse_number(htmls[i])
  
  temp <- xml2::read_html(paste0("data/output/TJPA/Inteiro Teor/",
                                 htmls[i]),
                          encoding = "UTF-8")
  
  temp <- rvest::html_text(temp)
  
  inteiroteor$InteiroTeor[i] <- temp

  webElem00 })}, error = function(e) {
    NA})
  
}

saveRDS(inteiroteor,
        "data/output/TJPA/Inteiro Teor/inteiro teor_tjpa.rds")
