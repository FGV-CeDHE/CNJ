library(stringr)
library(dplyr)

##FUNÇÃO DE FILTRAGEM----
busca_palavras_chave <- function(dataframe_final, palavras_chave, nome_coluna, contagem=F, ementa=T){
  require(stringr)
  
  for(k in 1:length(palavras_chave)){
    dataframe_final[,palavras_chave[k]] <- str_count(dataframe_final[,nome_coluna], 
                                                     tolower(iconv(palavras_chave[k], from="UTF-8",to="ASCII//TRANSLIT")))
    
    print(palavras_chave[k])

  }
  return(dataframe_final)
}

##LISTA DE PALAVRAS CHAVES----
palavras_chave <- c("Ação civil pública",
                    "mineração",
                    "garimpo",
                    "minério",
                    "mineral",
                    "mineradora",
                    "agricultura",
                    "cultivo",
                    "plantação",
                    "pecuária",
                    "gado",
                    "pasto",
                    "madeira",
                    "madeireiro",
                    "rodovia",
                    "ferrovia",
                    "hidrovia",
                    "hidrelétricas",
                    "energia",
                    "UHE",
                    "usina",
                    "empreendimento",
                    "risco",
                    "dano",
                    "ilegal",
                    "contaminação",
                    "ameaça",
                    "vulnerabilidade",
                    "exploração ilegal",
                    "demarcação",
                    "desafetação",
                    "alteração dos limites",
                    "revisão dos limites",
                    "desmatamento",
                    "queimada",
                    "invasão",
                    "conflito fundiário",
                    "conflito agrário",
                    "disputa por terra",
                    "grilagem; impacto",
                    "degradação;",
                    "ambiental",
                    "indígena",
                    "tradicionais",
                    "quilombola",
                    "meio ambiente",
                    "unidade de conservação",
                    "Amazônia",
                    "floresta",
                    "socioambiental")

#LEITURA DE BANCO E RENOME DA COLUNA DE EMENTA----
banco <- read.csv2("mineracao/resultado_TJPA_final_2017_a_2022.csv")



#FILTRO DE TERMOS----
#Criar colunas de acordo com os termos pré estabelecidos para as próprias pesquisadoras manusearem a base

banco_com_filtros <- banco %>% 
  #deixar pronto para análise, tudo em minúscula e sem espaços extras e sem acentos
  mutate(coluna_texto_tratada = str_to_lower(ementa),
         coluna_texto_tratada = str_squish(coluna_texto_tratada),
         coluna_texto_tratada = iconv(coluna_texto_tratada, from="UTF-8",to="ASCII//TRANSLIT"))


banco_com_filtros <- busca_palavras_chave(dataframe_final = banco_com_filtros,
                                          palavras_chave = palavras_chave,
                                          nome_coluna = 'coluna_texto_tratada')

write.csv2(banco_com_filtros,"mineracao/resultado_TJPA_final_2017_a_2022_com_filtros.csv")
