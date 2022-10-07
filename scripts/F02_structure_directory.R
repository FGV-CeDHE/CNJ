#função que cria a estrutura de dados

strucutre_directory <- function(sigla_tribunal, merge_data = FALSE){
  
  if(merge_data==TRUE){return(print("Funcionalidade não implementada"))}
  else{
    dir.create(path = paste0(sigla_tribunal))
    dir.create(path = paste0(sigla_tribunal,"/Documentos"))
  }
}
