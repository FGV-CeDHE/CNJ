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








TST <- mining_object(c("ementa", palavras_chaves[1]))

Sys.sleep(10)

for(i in 8:length(palavras_chaves)){
  TSTaux <- MF_TST_mining(palavras_chaves = palavras_chaves[i],
                          fat_temp = 0.3, 
                          port = 4445L,
                          browser = "chrome",
                          download = F)
  write.csv2(TSTaux, "TST/TST_parcialaux.csv")
  TST <- bind_mining_dataframes(vetor_final = TST,vetor_auxiliar = TSTaux,palavra_chave = palavras_chaves[i])
  write.csv2(TST, "TST/TST_parcial.csv")
}

TST <- TST[duplicated(TST)==F,]
TST <- TST[is.na(TST$ID)==F,]

aux <- unique(TST$ID)

for(i in 1:length(aux)){
  
  if(length(TST[TST$ID==aux[i],]$ID)>1){
    ids <- which(TST$ID==aux[i])
    for(k in ids){
      
      TST[k,]$ID <- paste0(TST[k,]$ID,LETTERS[which(ids == k)])
    }
    print(paste0(round(100*i/length(aux),digits = 3),"%"))
  }
}

  write.csv2(TST, "TST/TST.csv")
  