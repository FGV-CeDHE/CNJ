#fução que inicializa um objeto de mineração


mining_object <- function(new_varnames = NULL, size = 1){
  #nomes das variáveis
  varnames <- c("ID",
                "numero_processo",
                "tribunal",
                "relator",
                "orgao_judiciante",
                "data_julgamento",
                "data_publicacao",
                "tipo_documento",
                "tipo_ação",
                "tipo_recurso",
                "link_pdf_internet",
                "link_pdf_local",
                "link_html_internet",
                "link_html_local")
  
  #adicionando variáveis
  varnames <- c(varnames, new_varnames)
  
  #inicializando vetor
  aux <- data.frame(matrix(ncol = length(varnames), nrow = size))
  #nomeando colunas do vetor
  names(aux) <- varnames
  
  return(aux)
}
