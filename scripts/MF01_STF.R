#função de mineração do TST
setwd("mineracao/")
minera_objeto_unitario <- function(client, download=F,fat_temp=1){
  
  #inicializa objeto
  objeto_auxiliar <- mining_object()
  objeto_auxiliar$tribunal <- "STF"
  
  #cabeçalho
  element <- client$findElements(using = "xpath",
                                value =  '/html/body/app-root/app-home/main/app-search-detail/div/div/div[2]/mat-tab-group/div/mat-tab-body[1]/div/div/div[1]')
  if(length(element)==0)element <- client$findElements(using = "xpath",
                                                        value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div[2]/div/div[1]')                              
  
  element <- element[[1]]
  cab <- element$findChildElements(using = 'xpath',value = "div/h4")
  
  for(k in 1:length(cab)){
    if(k==1) objeto_auxiliar$id_portal <- cab[[k]]$getElementText()[[1]]
    if(k==2) objeto_auxiliar$tipo_ação <- cab[[k]]$getElementText()[[1]]
    if(stringr::str_detect(string = cab[[k]]$getElementText()[[1]],pattern = "Relator\\(a\\)")) objeto_auxiliar$relator <-  stringr::str_extract(string = cab[[k]]$getElementText()[[1]],pattern = "(?<=Relator.{0,10}: ).+")
    if(stringr::str_detect(string = cab[[k]]$getElementText()[[1]],pattern = "Órgão julgador")) objeto_auxiliar$orgao_judiciante <-  stringr::str_extract(string = cab[[k]]$getElementText()[[1]],pattern = "(?<=Órgão julgador.{0,10}: ).+")
  }
  
  cab <- element$findChildElements(using = 'xpath',value = "div/div")
  
  if(stringr::str_detect(cab[[1]]$getElementText()[[1]],"Julgamento")) objeto_auxiliar$data_julgamento <- stringr::str_extract(string = cab[[1]]$getElementText()[[1]],pattern = "(?<=Julgamento.{0,10}: ).+")
  if(stringr::str_detect(cab[[1]]$getElementText()[[1]],"Publicação")) objeto_auxiliar$data_publicacao <- stringr::str_extract(string = cab[[1]]$getElementText()[[1]],pattern = "(?<=Publicação.{0,10}: ).+")
  
  #corpo do texto
  
  element <- client$findElements(using = "xpath",
                                 value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div/mat-tab-group/div/mat-tab-body[1]/div/div/div')
  if(length(element)==0) element  <- client$findElements(using = "xpath",
                                                        value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div[2]/div/div')   

  for(i in 1:length(element)){

    title <- element[[i]]$findChildElements(using = "xpath",
                                            value = "h4")
    if(length(title)==1) {

      
      if(title[[1]]$getElementText()[[1]]=="Publicação"){
        dados_elemento <- element[[i]]$findChildElements(using = "xpath",
                                                         value = "div")
        objeto_auxiliar$dados_publicacao <- dados_elemento[[1]]$getElementText()[[1]]
      }else if(title[[1]]$getElementText()[[1]]=="Partes"){
        dados_elemento <- element[[i]]$findChildElements(using = "xpath",
                                                         value = "div")
        
        partes <- dados_elemento[[1]]$getElementText()[[1]]
        
        partes <- stringr::str_split(stringr::str_split(partes,pattern = "\n")[[1]],pattern = ":")
        partes_json <- c()
        for (s in 1:length(partes)) {
          aux <- paste0('"', stringr::str_trim(partes[[s]]),'"',collapse = ":")
          partes_json <- c(partes_json,aux)
        }
        objeto_auxiliar$partes <- paste0("{",paste0(partes_json,collapse = ","),"}")
      }else if(title[[1]]$getElementText()[[1]]=="Ementa"){
        dados_elemento <- element[[i]]$findChildElements(using = "xpath",
                                                         value = "div")
        
        objeto_auxiliar$ementa <- dados_elemento[[1]]$getElementText()[[1]]
      }else if(title[[1]]$getElementText()[[1]]=="Decisão"){
        dados_elemento <- element[[i]]$findChildElements(using = "xpath",
                                                         value = "div")
        
        objeto_auxiliar$decisao <- dados_elemento[[1]]$getElementText()[[1]]
      }else if(title[[1]]$getElementText()[[1]]=="Tema"){
        dados_elemento <- element[[i]]$findChildElements(using = "xpath",
                                                         value = "div")
        
        objeto_auxiliar$tema <- dados_elemento[[1]]$getElementText()[[1]]
      }else if(title[[1]]$getElementText()[[1]]=="Tese"){
        dados_elemento <- element[[i]]$findChildElements(using = "xpath",
                                                         value = "div")
        
        objeto_auxiliar$tese <- dados_elemento[[1]]$getElementText()[[1]]
      }else if(title[[1]]$getElementText()[[1]]=="Indexação"){
        dados_elemento <- element[[i]]$findChildElements(using = "xpath",
                                                         value = "div")
        
        objeto_auxiliar$indexacao <- dados_elemento[[1]]$getElementText()[[1]]
      }else if(title[[1]]$getElementText()[[1]]=="Legislação"){
        dados_elemento <- element[[i]]$findChildElements(using = "xpath",
                                                         value = "div")
        
        objeto_auxiliar$legislacao <- dados_elemento[[1]]$getElementText()[[1]]
      }else if(title[[1]]$getElementText()[[1]]=="Observação"){
        dados_elemento <- element[[i]]$findChildElements(using = "xpath",
                                                         value = "div")
        
        objeto_auxiliar$observacao <- dados_elemento[[1]]$getElementText()[[1]]
      }else if(title[[1]]$getElementText()[[1]]=="Doutrina"){
        dados_elemento <- element[[i]]$findChildElements(using = "xpath",
                                                         value = "div")
        
        objeto_auxiliar$doutrina <- dados_elemento[[1]]$getElementText()[[1]]
      }
    }
    
  }
  
  #codigo cnj
  
  main_handle <- client$getWindowHandles()[[1]]
  element <- client$findElements(using = "xpath",
                                value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div/mat-tab-group/div/mat-tab-body/div/div/div/div/div/mat-icon[contains(@mattooltip,"Acompanhamento processual")]')
  
  if(length(element)==0) element <- client$findElements(using = "xpath",
                                                       value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div[2]/div/div[1]/div[2]/div/mat-icon[contains(@mattooltip,"Acompanhamento processual")]')
  
  element <- element[[1]]                                     
  count_tries <- 0
  while(length(client$getWindowHandles())<2 & count_tries<3){
    element <- client$findElements(using = "xpath",
                                   value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div/mat-tab-group/div/mat-tab-body/div/div/div/div/div/mat-icon[contains(@mattooltip,"Acompanhamento processual")]')
    
    if(length(element)==0) element <- client$findElements(using = "xpath",
                                                          value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div[2]/div/div[1]/div[2]/div/mat-icon[contains(@mattooltip,"Acompanhamento processual")]')
    
    element <- element[[1]]  
    element$clickElement() 
    Sys.sleep(fat_temp*0.5)
    
    count <- 0
    while (length(client$getWindowHandles())<2 & count < 5) {
      Sys.sleep(fat_temp*1)
      print("1")
      count <- count+1
      if(count==5){
        return("erro_paginacao")
        #client$navigate(client$getCurrentUrl()[[1]])
        #waiting(client)
      }
    }
    count_tries <- count_tries+1
  }
  
  new_tab <- unlist(client$getWindowHandles())[unlist(client$getWindowHandles()) != main_handle]
  client$switchToWindow(windowId = new_tab)
  
  count_up <- 0
  while(length(client$findElements(using = "xpath",
                                   value = '/html/body/div/section/div/div/div/div/div/div/div/div/div[contains(@class,"processo-rotulo")]'))<1){
    Sys.sleep(fat_temp*0.6)
    print("teste")
    count_up <- count_up+1
    if(count_up%%10==0) {
      client$closeWindow()
      client$switchToWindow(windowId = main_handle)
      count_tries <- 0
      while(length(client$getWindowHandles())<2 & count_tries<3){
        element <- client$findElements(using = "xpath",
                                       value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div/mat-tab-group/div/mat-tab-body/div/div/div/div/div/mat-icon[contains(@mattooltip,"Acompanhamento processual")]')
        
        if(length(element)==0) element <- client$findElements(using = "xpath",
                                                              value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div[2]/div/div[1]/div[2]/div/mat-icon[contains(@mattooltip,"Acompanhamento processual")]')
        
        element <- element[[1]]  
        element$clickElement() 
        Sys.sleep(fat_temp*0.6)
        
        count <- 0
        while (length(client$getWindowHandles())<2 & count < 5) {
          Sys.sleep(fat_temp*1)
          print("4")
          count <- count+1
          if(count==5){
            client$refresh()         
            waiting(client,fat_temp)
          }
        }
        count_tries <- count_tries+1
      }
      
      new_tab <- unlist(client$getWindowHandles())[unlist(client$getWindowHandles()) != main_handle]
      client$switchToWindow(windowId = new_tab)
      count<-0
      Sys.sleep(fat_temp*(0.6))
    }
    if(count_up %% 31==0){
      client$closeWindow()
      client$switchToWindow(windowId = main_handle)
      return(objeto_auxiliar)
      
      }
  }
  
  
  element <- client$findElements(using = "xpath",
                                value = '/html/body/div/section/div/div/div/div/div/div/div/div/div[contains(@class,"processo-rotulo")]')
  
  count <- 0
  while(length(element)<1 & count < 50){
    element <- client$findElements(using = "xpath",
                                   value = '/html/body/div/section/div/div/div/div/div/div/div/div/div[contains(@class,"processo-rotulo")]')
    count <- count+1 
    Sys.sleep(fat_temp*0.5)
    }

  element <- element[[1]]
  if(length(element)>0){
    print(element$getElementText()[[1]])
    print(stringr::str_extract(string = element$getElementText()[[1]],pattern = "(?<=NÚMERO ÚNICO: ).+"))
    objeto_auxiliar$numero_processo <- stringr::str_extract(string = element$getElementText()[[1]],pattern = "(?<=NÚMERO ÚNICO: ).+")
  }
  
  client$closeWindow()
  client$switchToWindow(windowId = main_handle)
  
  #pdf inteiro teor
  objeto_auxiliar$ID <- paste0("STF",
                               objeto_auxiliar$numero_processo,
                               objeto_auxiliar$relator,
                               paste0(stringr::str_remove_all(string = objeto_auxiliar[,c("data_publicacao","data_julgamento")],pattern = "/"),collapse = ""))
  

  element <- client$findElements(using = "xpath",
                                value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div/mat-tab-group/div/mat-tab-body/div/div/div/div/div/mat-icon[contains(@mattooltip,"Inteiro teor")]')
  
  if(length(element) == 0) element <- client$findElements(using = "xpath",
                                                          value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div/div/div/div/div/mat-icon[contains(@mattooltip,"Íntegra da decisão")]')
  
  
  element <- element[[1]]
  
  
  
  
  count_tries <- 0
  while(length(client$getWindowHandles())<2 & count_tries<3){
    element <- client$findElements(using = "xpath",
                                   value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div/mat-tab-group/div/mat-tab-body/div/div/div/div/div/mat-icon[contains(@mattooltip,"Inteiro teor")]')
    
    if(length(element) == 0) element <- client$findElements(using = "xpath",
                                                            value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div/div/div/div/div/mat-icon[contains(@mattooltip,"Íntegra da decisão")]')
    
    
    element <- element[[1]]
    
    
    
    element$clickElement() 
    Sys.sleep(fat_temp*0.6)
    
    count <- 0
    while (length(client$getWindowHandles())<2 & count < 1) {
      Sys.sleep(fat_temp*1)
      print("Erro em: carregando pagina do documento")
      count <- count+1
      if(count==5){
        client$refresh()         
        waiting(client,fat_temp)
      }
    }
    count_tries <- count_tries+1
  }
  
  new_tab <- unlist(client$getWindowHandles())[unlist(client$getWindowHandles()) != main_handle]
  
  if(length(new_tab)>0){
    client$switchToWindow(windowId = new_tab)
    
    while(client$getCurrentUrl()[[1]] == 'about:blank'){
      Sys.sleep(fat_temp*0.5)
    }
    objeto_auxiliar$link_pdf_internet <- client$getCurrentUrl()[[1]]
    if(download==T){
      tryCatch(expr = download.file(url = objeto_auxiliar$link_pdf_internet,destfile = paste0("STF/", objeto_auxiliar$ID,"_documento.pdf")),finally = print("Passou"))
      
      objeto_auxiliar$link_pdf_local <- paste0("STF/", "STF_", objeto_auxiliar$numero_processo, "_",objeto_auxiliar$relator,"_documento.pdf")
    }
    client$closeWindow()
  }
  client$switchToWindow(windowId = main_handle)
  
  return(objeto_auxiliar)
}

waiting <- function(client, fat_temp=1){
  
  
  
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

navega_ate_paginacao <- function(client, pagina=NULL,fat_temp=1,datainicio,datafim,palavras_chaves){
  Sys.sleep(fat_temp*2)
  
  
  counter <- 0
  while(counter < 6){
    
    element <- client$findElements(using = "xpath",
                                   value = "//*[@id='mat-input-0']")
    if(length(element)==0){
      Sys.sleep(fat_temp*0.5)
      counter <- counter+1
    }else{
      counter <- 6
    }
  }
  
  #esperando
  waiting(client,fat_temp)
  
  element <- client$findElement(using = "xpath",
                                value = "//*[@id='mat-input-0']")
  
  element$sendKeysToElement(list(palavras_chaves))
  
  element <- client$findElement(using = "xpath",
                                value = "/html/body/app-root/app-home/main/search/div/search-input/div/div/div/div/mat-form-field/div/div[1]/div[4]/div/mat-icon[2]")
  element$clickElement()
  
  waiting(client,fat_temp)
  #filtrando as datas
  element <- client$findElement(using = 'xpath',
                                value = '/html/body/app-root/app-home/main/search/div/div/div/div[1]/div[2]/div[3]/div/div[2]/mat-form-field[1]/div/div[1]/div[3]/input')
  
  element$sendKeysToElement(list(datainicio))
  
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
  element$sendKeysToElement(sendKeys = list(selKeys$enter))
  
  waiting(client,fat_temp)
  element <- client$findElement(using = 'xpath',
                                value = '/html/body/app-root/app-home/main/search/div/div/div/div[1]/div[2]/div[3]/div/div[2]/mat-form-field[2]/div/div[1]/div[3]/input')

  #element$sendKeysToElement(list(paste0(dia,mes,year(now()))))
  element$sendKeysToElement(list(datafim))
  element$sendKeysToElement(sendKeys = list(selKeys$enter))
  waiting(client,fat_temp)
  
  element <- client$findElement(using = "xpath",
                                value = '/html/body/app-root/app-home/main/search/div/div/div/div[2]/div/div[2]/div[1]/a/h4')
  element$clickElement()
  
  waiting(client,fat_temp)
  
  if(is.null(pagina)==F){
    element <- client$findElement(using = "xpath",
                                  value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div[3]/paginator/nav/div/div/mat-form-field/div/div[1]/div[3]/input')
    element$clearElement()
    
    element$sendKeysToElement(list(as.character(pagina)))
    element$clickElement()
    element$sendKeysToElement(sendKeys = list(selKeys$enter))
    
    waiting(client,fat_temp)
    
  }
  
}



require(RSelenium)
require(rvest)
require(dplyr)
require(xml2)
require(lubridate)

#rodando a mineração



source("B01_general_auxiliar_functions.R")

system("docker run -d -p 4445:4444 -p 5901:5900 selenium/standalone-firefox-debug")

palavras_chaves <- c('ementa OU lei OU documento OU a')


port<-4445
browser <- 'firefox'
download<-FALSE
fat_temp<-1.5




#final <-  final[0,]


client <- remoteDriver(port = port, browser = browser, remoteServerAddr = "localhost")



initLink <- "https://jurisprudencia.stf.jus.br/pages/search"



client$open()


meses <- 1:12
anos <- 2017:2022

final <- NULL
listfiles <- list.files(path = "STF", pattern = "[[:digit:]]+_mineraSTF",full.names = T)
for(i in listfiles){
  if(is.null(final)){
    final <- read.csv2(i)[,-1]
  }else{
    final<-bind_rows(final, read.csv2(i)[,-1])
  }
}


for(ano in anos){
  for(mes in 1:12){
   
    if((ano == 2019 & mes %in% c(11))){
      client$navigate(initLink)
      dataIni <- as.Date(paste("01",mes,ano,sep = "-"),format = "%d-%m-%Y")
      dataFim <- dataIni %m+% months(1)

      dataIni <- paste0(stringr::str_extract(dataIni,"[[:digit:]]{2}$"),
                        stringr::str_extract(dataIni,"(?<=-)[[:digit:]]{2}(?=-)"),
                        stringr::str_extract(dataIni,"^[[:digit:]]{4}(?=-)"))
      
      dataFim <- paste0(stringr::str_extract(dataFim,"[[:digit:]]{2}$"),
                        stringr::str_extract(dataFim,"(?<=-)[[:digit:]]{2}(?=-)"),
                        stringr::str_extract(dataFim,"^[[:digit:]]{4}(?=-)"))

      navega_ate_paginacao(client = client,fat_temp = 1.5,datainicio = dataIni,datafim = dataFim, palavras_chaves = palavras_chaves)
      
      #preparando paginação
      element <- client$findElement(using = 'xpath',
                                    value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div/paginator/nav/div/span')
      
      num_docs <- as.numeric(paste0(stringr::str_extract_all(string = element$getElementText()[[1]],pattern = "[[:digit:]]+")[[1]],collapse = ""))
      
      objeto_final <- minera_objeto_unitario(client = client,download = F)
      objeto_final <- objeto_final[0,]
      
      for(i in 1:num_docs){
        
        element <- client$findElements(using = "xpath",
                                       value =  '/html/body/app-root/app-home/main/app-search-detail/div/div/div[2]/mat-tab-group/div/mat-tab-body[1]/div/div/div[1]')
        if(length(element)==0)element <- client$findElements(using = "xpath",
                                                             value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div[2]/div/div[1]')                              
        
        
        while(length(element)==0){
          client$refresh()         
          waiting(client,fat_temp)
          Sys.sleep(fat_temp*1)
          element <- client$findElements(using = "xpath",
                                         value =  '/html/body/app-root/app-home/main/app-search-detail/div/div/div[2]/mat-tab-group/div/mat-tab-body[1]/div/div/div[1]')
          if(length(element)==0)element <- client$findElements(using = "xpath",
                                                               value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div[2]/div/div[1]')                              
          
        }
        
        objeto_auxiliar <- minera_objeto_unitario(client = client,download = download,fat_temp = 1.5)
        
        if(length(objeto_auxiliar)==1){
          control<-TRUE
          while(length(objeto_auxiliar)==1){
            client$navigate(initLink)
            navega_ate_paginacao(client = client,pagina = i,fat_temp = 1.5,datainicio = dataIni,datafim = dataFim,palavras_chaves = palavras_chaves)
            objeto_auxiliar <- minera_objeto_unitario(client = client,download = download,fat_temp = 1.5)
            
            if(length(objeto_auxiliar)==1){
              control <- TRUE
            }else{
              control<-FALSE
            }
            
          }
        }
        objeto_auxiliar$id_mineracao <- i
        objeto_final <- bind_rows(objeto_final, objeto_auxiliar)
        
        element <- client$findElements(using = 'xpath',
                                      value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div/paginator/nav/ul/li/a/span/i[contains(@class, "fa fa-angle-right")]')
        
        if(length(element)==0){
          client$navigate(initLink)
          navega_ate_paginacao(client = client,fat_temp = 1.5,datainicio = dataIni,datafim = dataFim, palavras_chaves = palavras_chaves)
          element <- client$findElements(using = 'xpath',
                                         value = '/html/body/app-root/app-home/main/app-search-detail/div/div/div/paginator/nav/ul/li/a/span/i[contains(@class, "fa fa-angle-right")]')
          
        }
        element <- element[[1]]
        element$clickElement()
        waiting(client,fat_temp)
        print(paste0("Documento ", i, " de ", num_docs, ": " ,round(x = i/num_docs*100, digits =2),"%"))
        write.csv2(objeto_final, paste0(dataIni,"_mineraSTF.csv"))
        

        gc()
      }
      if(is.null(final)==T){
        final = objeto_final
      }
      else{
        final <- bind_rows(final, objeto_final)
      }
      write.csv2(final, paste0("STF/consolidado","_mineraSTF.csv"))

                                                                                                                            
    }
    
    }
}
                                                                                                  









  