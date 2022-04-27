#função de mineração do TST




MF_TST_mining <- function(palavras_chaves, fat_temp = 1, port =  4567L, browser = "chrome", download = F){
  require(RSelenium)
  require(rvest)
  require(dplyr)
  require(xml2)
  require(lubridate)
  source("B01_general_auxiliar_functions.R")
  

  
  final <- mining_object(c("ementa"))
  #final <-  final[0,]
  
  
  client <- remoteDriver(port = port, browser = browser, remoteServerAddr = "localhost")
  
  
  initLink <- "https://jurisprudencia.stf.jus.br/pages/search"

  
  
  client$open()
  client$navigate(initLink)
  counter <- 0
  while(counter < 6){
    
    element <- client$findElements(using = "xpath",
                                   value = "//*[@id='mat-input-0']")
    if(length(element)==0){
      Sys.sleep(0.5)
      counter <- counter+1
    }else{
      counter <- 6
    }
  }
  
  #esperando
  #waiting(client)
  
  element <- client$findElement(using = "xpath",
                                value = "//*[@id='mat-input-0']")
  
  element$sendKeysToElement(list(palavras_chaves))
  
  element <- client$findElement(using = "xpath",
                                value = "/html/body/app-root/app-home/main/search/div/search-input/div/div/div/div/mat-form-field/div/div[1]/div[4]/div/mat-icon[2]")
  element$clickElement()

  #filtrando as datas
  element <- client$findElement(using = 'xpath',
                                value = '/html/body/app-root/app-home/main/search/div/div/div/div[1]/div[2]/div[3]/div/div[2]/mat-form-field[1]/div/div[1]/div[3]/input')
  
  element$sendKeysToElement(list('01012017'))
  
  if(stringr::str_length(day(now()))<2){
    dia <- paste0("0", day(now()))
  }else{
    dia <- day(now())
  }
  if(stringr::str_length(month(now()))<2){
    mes <- paste0("0", month(now()))
  }else{
    mes <- month(now())
  }
  
  element <- client$findElement(using = 'xpath',
                                value = '/html/body/app-root/app-home/main/search/div/div/div/div[1]/div[2]/div[3]/div/div[2]/mat-form-field[2]/div/div[1]/div[3]/input')
  
  element$sendKeysToElement(list(paste0(dia,mes,year(now()))))
  
  element <- client$findElement(using = "xpath",
                                value = '/html/body/app-root/app-home/main/search/div/div/div/div[2]/div/div[2]/div[1]/a/h4')
  element$clickElement()

  #wait
  
  objeto_auxiliar <- mining_object()
  objeto_auxiliar$tribunal <- "STF"
  
  #cabeçalho
  element <- client$findElement(using = "xpath",
                                value =  '/html/body/app-root/app-home/main/app-search-detail/div/div/div[2]/mat-tab-group/div/mat-tab-body[1]/div/div/div[1]')
  
  cab <- element$findChildElements(using = 'xpath',value = "div/h4")
 
  for(k in length(cab)){
    if(k==1) objeto_auxiliar$id_portal <- cab[[k]]$getElementText()[[1]]
    if(k==2) objeto_auxiliar$tipo_ação <- cab[[k]]$getElementText()[[1]]
    if(stringr::str_detect(string = cab[[k]]$getElementText()[[1]],pattern = "Relator\\(a\\)")) objeto_auxiliar$relator <-  stringr::str_extract(string = cab[[k]]$getElementText()[[1]],pattern = "(?<=Relator.{0,10}: ).+")
    if(stringr::str_detect(string = cab[[k]]$getElementText()[[1]],pattern = "Órgão julgador")) objeto_auxiliar$relator <-  stringr::str_extract(string = cab[[k]]$getElementText()[[1]],pattern = "(?<=Órgão julgador.{0,10}: ).+")
  }
  
  cab <- element$findChildElements(using = 'xpath',value = "div/div")
  
  if(stringr::str_detect(cab[[1]]$getElementText()[[1]],"Julgamento")) objeto_auxiliar$data_julgamento <- stringr::str_extract(string = cab[[1]]$getElementText()[[1]],pattern = "(?<=Julgamento.{0,10}: ).+")
  if(stringr::str_detect(cab[[1]]$getElementText()[[1]],"Publicação")) objeto_auxiliar$data_julgamento <- stringr::str_extract(string = cab[[1]]$getElementText()[[1]],pattern = "(?<=Publicação.{0,10}: ).+")
  
  
'/html/body/app-root/app-home/main/app-search-detail/div/div/div[1]/mat-tab-group/div/mat-tab-body[1]/div/div/div[3]'
  
    
}
  
  