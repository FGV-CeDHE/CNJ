
## PACOTES UTILIZADOS

library(RCurl)
library(XML)

## OBJETIVOS

#'         - Função que extrai a hierarquia das movimentações
#'           listadas nas Tabelas Processuais Unificadas a partir 
#'           do código.

## FUNÇÃO

retorna_familia_TPU_M <- function(codigo, dt_frame = F){
  
  body = paste0('<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:sgt="https://www.cnj.jus.br/sgt/sgt_ws.php">
     <soapenv:Header/>
     <soapenv:Body>
        <sgt:getStringPaisItemPublicoWS>
           <seqItem>', codigo, '</seqItem>
            <tipoItem>M</tipoItem>
        </sgt:getStringPaisItemPublicoWS>
     </soapenv:Body>
  </soapenv:Envelope>')
  
  reader = basicTextGatherer()
  
  curlPerform(
    url = "https://www.cnj.jus.br/sgt/sgt_ws.php",
    postfields = body,
    writefunction = reader$update
  )
  
  xml <- reader$value()
  
  aux <- xmlToList(XML::xmlParse(xml))
  
  data_aux = data_frame(familia = aux$Body$getStringPaisItemPublicoWSResponse$return,
                        codigo = codigo)
  
  if(dt_frame == T) return(data_aux)
  
  return(aux$Body$getStringPaisItemPublicoWSResponse$return)
  
}