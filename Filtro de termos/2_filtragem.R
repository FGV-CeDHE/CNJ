library(stringr)
library(dplyr)

##FUNÇÃO DE FILTRAGEM----
busca_palavras_chave <- function(dataframe_final, palavras_chave,regex, nome_coluna, contagem=F, ementa=T, count=F){
  require(stringr)
  
  for(k in 1:length(palavras_chave)){
    if(count==T){
      dataframe_final[,palavras_chave[k]] <- str_count(dataframe_final[,nome_coluna], 
                                                       tolower(iconv(regex[k], from="UTF-8",to="ASCII//TRANSLIT")))
    }else{
      dataframe_final[,palavras_chave[k]] <- ifelse(str_detect(dataframe_final[,nome_coluna], 
                                                       tolower(iconv(regex[k], from="UTF-8",to="ASCII//TRANSLIT")))>0,1,0)
    }
    
    print(palavras_chave[k])

  }
  return(dataframe_final)
}

##LISTA DE PALAVRAS CHAVES----

regex <- c(" a..o.{1,3}civil.{1,3}p.blica | acp",
           " suspens.o.{1,6}seguran.a ",
           " suspens.o.{1,6} senten.a ",
           " suspens.o.{1,6}tutela.{1,3}antecipada ",
           " suspens.o.{1,6}jurisdi..o ",
           " suspens.o.{1,3}liminar | SL ",
           " suspens.o.{1,6}liminar.{1,7}senten.a | SLS ",
           " suspens.o.{1,6}liminar.{1,6}antecipa..o.{1,6}tutela| SLAT ",
           " incidente.{1,6}suspens.o ",
           " minera..o |extra..o.{1,5}min.rio ",
           " garimp(o|eiro.) ",
           " min.rio | minera(l|dor.) ",
           " agricult.",
           " (produtor|trabalhador).{1,3}rural|produtor.{1,3}agropecu.rio ",
           " cultivo ",
           " planta..o ",
           " pecu.r.|agropecu.",
           " gado ",
           " pasto ",
           " madeir(a|eir.)",
           " rodov.",
           " ferrov.",
           " hidrov.",
           " hidrel.trica ",
           " energia ",
           " uhe ",
           " usina ",
           " empreendimento ",
           " risco ",
           " dano ",
           " contamina.",
           " amea.a ",
           " Impacto.",
           " degrada.",
           " explora.{1,3}Ilega.",
           " desmata.",
           " queimada ",
           " conflito.{1,5}(fundi.rio|agr.rio) ",
           " disputa.{1,6}terra ",
           " invas.o ",
           " grilagem ",
           " demarca.",
           " desafeta.",
           " (altera.|revis.){1,6}limites ",
           " vulnerab.",
           " ind.gena.",
           " tradicion(ais|alidade) ",
           " quilombo.",
           " deslocamento.{1,4}(compuls.rio|involunt.rio|for..ado) ",
           " direitos.{1,3}humano ",
           " ambient.|socioambient.",
           " meio.{1,3}ambiente ",
           " unidade.{1,6}conserva..o ",
           " amaz.nia ",
           " floresta ",
           " biodiversidade|recurso.{1,2}natura(is|l)|ecol.gico ")

names <- c("ACP",
           "suspensão de segurança",
           "suspensão de sentença",
           "suspensão de tutela antecipada",
           "suspensão da jurisdição",
           "suspensão de liminar",
           "suspensão de liminar e de sentença",
           "suspensão de liminar ou antecipação de tutela",
           "incidente de suspensão",
           "mineração/extração",
           "garimpo/garimpeiro",
           "minério/mineral/minerador",
           "agricultura/agricultor",
           "produtor/trabalhador rural/agropecuário",
           "cultivo",
           "plantação",
           "pecuária/agropecuária",
           "gado",
           "pasto",
           "madeira/madeireiro",
           "rodovia",
           "ferrovia",
           "hidrovia",
           "hidrelétrica",
           "energia",
           "UHE",
           "usina",
           "empreendimento",
           "risco",
           "dano",
           "contaminação",
           "ameaça",
           "impacto",
           "degradação",
           "exploração ilegal",
           "desmatamento/desmatou/desmatado",
           "queimada",
           "conflito fundiário/agrário",
           "disputa por terra",
           "invasão",
           "grilagem",
           "demarcação",
           "desafetação",
           "alteração/revisão dos limites",
           "vulnerabilidade",
           "indígena/indígenas",
           "tradicionais",
           "quilombola",
           "deslocamento",
           "direitos humanos",
           "ambiental/socioambiental",
           "meio ambiente",
           "unidade de conservação",
           "amazônia",
           "floresta",
           "recursos naturais/biodiversidade/ecologico")

#LEITURA DE BANCO E RENOME DA COLUNA DE EMENTA----
banco <- read.csv2("STF/consolidado_mineraSTF.csv")

#duplicações
#STF
banco$ID <- paste0(banco$ID,banco$id_portal)
banco[duplicated(banco$ID)==F,]->banco


#FILTRO DE TERMOS----
#Criar colunas de acordo com os termos pré estabelecidos para as próprias pesquisadoras manusearem a base

banco_com_filtros <- banco %>% 
  #deixar pronto para análise, tudo em minúscula e sem espaços extras e sem acentos
  mutate(coluna_texto_tratada = str_to_lower(ementa),
         coluna_texto_tratada = str_squish(coluna_texto_tratada),
         coluna_texto_tratada = iconv(coluna_texto_tratada, from="UTF-8",to="ASCII//TRANSLIT"))


banco_com_filtros <- busca_palavras_chave(dataframe_final = banco_com_filtros,
                                          palavras_chave = names,
                                          regex = regex,
                                          nome_coluna = 'coluna_texto_tratada')

openxlsx::write.xlsx(banco_com_filtros,"STF/resultado_STF_final_2017_a_2022_com_filtros.xlsx")
write.csv2(banco_com_filtros,"STF/resultado_STF_final_2017_a_2022_com_filtros.csv")
