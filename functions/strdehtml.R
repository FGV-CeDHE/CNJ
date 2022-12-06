
## PACOTES UTILIZADOS

library(gsubfn)

## OBJETIVOS

#'         - Função que remove caracteres especiais e outros elementos HTML 
#'           do texto.

## REFERÊNCIA HTML

source("http://pastebin.com/raw.php?i=XtzN1NMs")

## FUNÇÃO

strdehtml <- function(string) {
  
  ret <- gsubfn("&#([0-9]+);", function(x) rawToChar(as.raw(as.numeric(x))), string)
  
  ret <- gsubfn("&([^;]+);", function(x) htmlchars[x], ret)
  
  return(ret)

  }