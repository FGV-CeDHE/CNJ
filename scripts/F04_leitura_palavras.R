
leitura_palavras <- function(palavras_chaves){
  require(pdftools)
  
  
  k <- min(which(is.na(vetor[,17])))
  
  for(i in k:length(vetor$ID)){
    print(paste0(round(i/length(vetor$ID)*100,digits = 3), "%"))
    link <- vetor$link_pdf_internet[i]
    
    download.file(link,destfile = "PRT/aux")
    
    if(file.size("PRT/aux")==0){
      texto <- ""
    }else if(stringr::str_detect(system2(command = "file",args = paste0(" -b --mime-type ", "/home/rodolfo/Documents/Freelas/CeDHEMineracao/PRT/aux"),stdout = TRUE),"opendocument")){
      file.rename(from = "PRT/aux", to = "PRT/aux.odt")
      texto <- readtext::readtext(file =  "PRT/aux.odt")
      texto <- texto$text
    }else if(stringr::str_detect(system2(command = "file",args = paste0(" -b --mime-type ", "/home/rodolfo/Documents/Freelas/CeDHEMineracao/PRT/aux"),stdout = TRUE),"rtf")){
      file.rename(from = "PRT/aux", to = "PRT/aux.rtf")
      texto <- readtext::readtext(file =  "PRT/aux.rtf")
      texto <- texto$text
    }else if(stringr::str_detect(system2(command = "file",args = paste0(" -b --mime-type ", "/home/rodolfo/Documents/Freelas/CeDHEMineracao/PRT/aux"),stdout = TRUE),"msword")){
      texto <- textreadr::read_doc(file = "PRT/aux")
      
    }else{
    
      
      texto <- paste0(pdftools::pdf_text("PRT/aux"), collapse = " ")
      count <- stringr::str_length(texto)/pdftools::pdf_info("PRT/aux")$pages
      print(count)
      if(count<1200) texto <- paste0(pdftools::pdf_ocr_text("PRT/aux"), collapse = " ")
    }
    
    texto <- tolower(texto)
    
    for(k in palavras_chaves){
      
      aux <- stringr::str_detect(iconv(texto, from="UTF-8",to="ASCII//TRANSLIT"), iconv(k, from="UTF-8",to="ASCII//TRANSLIT"))
      
      if(aux==T) {
        vetor[i,k] <<- 1
      }else{
        vetor[i,k] <<- 0
      }
    }
  }
}
