
## PACOTES UTILIZADOS

library(RCurl)
library(XML)

## OBJETIVOS

#'         - Função que extrai o nome e a descrição dos movimentos
#'           listados nas Tabelas Processuais Unificadas a partir 
#'           do código.

## FUNÇÃO

retorna_nome_codigo_TPU_M <- function(codigo, dt_frame=F){
  
  body = paste0('<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:sgt="https://www.cnj.jus.br/sgt/sgt_ws.php">
     <soapenv:Header/>
     <soapenv:Body>
        <sgt:pesquisarItemPublicoWS>
           <tipoTabela>M</tipoTabela>
           <tipoPesquisa>C</tipoPesquisa>
           <valorPesquisa>',codigo,'</valorPesquisa>
        </sgt:pesquisarItemPublicoWS>
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
  
  data_aux = data_frame(codigo = aux$Body$pesquisarItemPublicoWSResponse$return$Item$cod_item,
                        nome = aux$Body$pesquisarItemPublicoWSResponse$return$Item$nome,
                        glossario = aux$Body$pesquisarItemPublicoWSResponse$return$Item$dscGlossario)
  
  if(dt_frame==T) return(data_aux)
  return(aux$Body$pesquisarItemPublicoWSResponse$return$Item$nome)
  
}