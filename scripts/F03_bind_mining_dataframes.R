#função para juntar palavras chaves diferentes
bind_mining_dataframes <- function(vetor_final, vetor_auxiliar, palavra_chave){
  
  #criando nova palavra chave
  vetor_final[,c(palavra_chave)] <- 0
  
  #prenchendo palavra chave dos processos repetidos
  vetor_final[vetor_final$ID %in% vetor_auxiliar$ID, c(palavra_chave)] <- 1
  
  #separando processos novos
  aux <- vetor_auxiliar[vetor_auxiliar$ID %in% vetor_final$ID == F,] 
  
  #incluindo palavras chaves antigas nos processos novos
  if(length(aux$ID)>0) aux[,names(vetor_final)[names(vetor_final) %in% names(aux) == F]] <- 0
  
  #retornando novo banco
  return(bind_rows(vetor_final, aux))
}


