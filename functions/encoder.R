
## PACOTES UTILIZADOS

library(tidyverse)
library(tm)

encoder <- function(string){
  
  string <- gsub("\\U00E9",
                 "É",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00FA",
                 "Ú",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00F4",
                 "Ô",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00C9",
                 "É",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00DA",
                 "Ú",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00C1",
                 "Á",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00C7",
                 "Ç",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00C3",
                 "Ã",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00CD",
                 "Í",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00E7",
                 "Ç",
                 string,
                 fixed = T)
  
  string <- gsub("MINIST\\UFFFDRIO P\\UFFFDBLICO DO ESTADO DO MARANH\\UFFFDO (CNPJ=05.483.9120001-85)",
                 "MINISTÉRIO PÚBLICO DO ESTADO DO MARANHÃO (CNPJ=05.483.9120001-85)",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00ED",
                 "Í",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00E1",
                 "Á",
                 string,
                 fixed = T)
  
  string <- gsub("MINIST\\UFFFDRIO P\\UFFFDBLICO DO ESTADO DO MARANH\\UFFFDO",
                 "MINISTÉRIO PÚBLICO DO ESTADO DO MARANHÃO",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00BA",
                 "º",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00D4",
                 "Ô",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00AA",
                 "ª",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00E3",
                 "Ã",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00F3",
                 "Ó",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00C0",
                 "Á",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00C",
                 "Ê",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00D3",
                 "Ó",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00B0",
                 "º",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00C2",
                 "Â",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00EA",
                 "Ê",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00E2",
                 "Â",
                 string,
                 fixed = T)
  
  string <- gsub("MINIST\\UFFFDRIO P\\UFFFDBLICO",
                 "MINISTÉRIO PÚBLICO",
                 string,
                 fixed = T)
  
  string <- gsub("SA\\UFFFDDE",
                 "SAÚDE",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00E2",
                 "Â",
                 string,
                 fixed = T)
  
  string <- gsub("MARANH\\UFFFDO",
                 "MARANHÃO",
                 string,
                 fixed = T)
  
  string <- gsub("IN\\UFFFDS",
                 "INÊS",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00D5",
                 "Õ",
                 string,
                 fixed = T)
  
  string <- gsub("P\\UFFFDBLICA",
                 "PÚBLICA",
                 string,
                 fixed = T)
  
  string <- gsub("P\\UFFFDBLICO",
                 "PÚBLICO",
                 string,
                 fixed = T)
  
  string <- gsub("\\U0096",
                 "-",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00F5",
                 "Õ",
                 string,
                 fixed = T)
  
  string <- gsub("S\\UFFFDO",
                 "SÃO",
                 string,
                 fixed = T)
  
  string <- gsub("POL\\UFFFDCIA",
                 "POLÍCIA",
                 string,
                 fixed = T)
  
  string <- gsub("TRIBUT\\UFFFDRIA",
                 "TRIBUTÁRIA",
                 string,
                 fixed = T)
  
  string <- gsub("MUNIC\\UFFFDPIO DE SÃO LU\\UFFFDS",
                 "MUNICÍPIO DE SÃO LUÍS",
                 string,
                 fixed = T)
  
  string <- gsub("COM\\UFFFDRCIO",
                 "COMÉRCIO",
                 string,
                 fixed = T)
  
  string <- gsub("IND\\UFFFDSTRIA",
                 "INDÚSTRIA",
                 string,
                 fixed = T)
  
  string <- gsub("AGROPECU\\UFFFDRIA",
                 "AGROPECUÁRIA",
                 string,
                 fixed = T)
  
  string <- gsub("ALIAN\\UFFFDA",
                 "ALIANÇA",
                 string,
                 fixed = T)
  
  string <- gsub("SERVI\\UFFFDOS",
                 "SERVIÇOS",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00E0",
                 "Á",
                 string,
                 fixed = T)
  
  string <- gsub("CER\\UFFFDMICA",
                 "CERÂMICA",
                 string,
                 fixed = T)
  
  string <- gsub("CONS\\UFFFDRCIO",
                 "CONSÓRCIO",
                 string,
                 fixed = T)
  
  string <- gsub("MUNIC\\UFFFDPIO",
                 "MUNICÍPIO",
                 string,
                 fixed = T)
  
  string <- gsub("ASSOCIA\\UFFFD\\UFFFDO",
                 "ASSOCIAÇÃO",
                 string,
                 fixed = T)
  
  string <- gsub("CARV\\UFFFDO",
                 "CARVÃO",
                 string,
                 fixed = T)
  
  string <- gsub("UNI\\UFFFDO",
                 "UNIÃO",
                 string,
                 fixed = T)
  
  string <- gsub("PARTICIPA\\UFFFD\\UFFFDES",
                 "PARTICIPAÇÕES",
                 string,
                 fixed = T)
  
  string <- gsub("ADMINISTRA\\UFFFD\\UFFFDO",
                 "ADMINISTRAÇÃO",
                 string,
                 fixed = T)
  
  string <- gsub("INCORPORA\\UFFFD\\UFFFDES",
                 "INCORPORAÇÕES",
                 string,
                 fixed = T)
  
  string <- gsub("CONSTRU\\UFFFD\\UFFFDES",
                 "CONSTRUÇÕES",
                 string,
                 fixed = T)
  
  string <- gsub("ENERG\\UFFFDTICA",
                 "ENERGÉTICA",
                 string,
                 fixed = T)
  
  string <- gsub("MENDON\\UFFFDA",
                 "MENDONÇA",
                 string,
                 fixed = T)
  
  string <- gsub("RESTAURA\\UFFFD\\UFFFDO",
                 "RESTAURAÇÃO",
                 string,
                 fixed = T)
  
  string <- gsub("CL\\UFFFDUDIO",
                 "CLÁUDIO",
                 string,
                 fixed = T)
  
  string <- gsub("APURA\\UFFFD\\UFFFDO",
                 "APURAÇÃO",
                 string,
                 fixed = T)
  
  string <- gsub("JOSEL\\UFFFDNDIAMA",
                 "JOSELÂNDIA-MA",
                 string,
                 fixed = T)
  
  string <- gsub("MOIS\\UFFFDS",
                 "MOISÉS",
                 string,
                 fixed = T)
  
  string <- gsub("\\UFFFDGUA",
                 "ÁGUA",
                 string,
                 fixed = T)
  
  string <- gsub("JO\\UFFFDO",
                 "JOÃO",
                 string,
                 fixed = T)
  
  string <- gsub("IM\\UFFFDVEIS",
                 "IMÓVEIS",
                 string,
                 fixed = T)
  
  string <- gsub("VIT\\UFFFDRIA",
                 "VITÓRIA",
                 string,
                 fixed = T)
  
  string <- gsub("CONSTRU\\UFFFD\\UFFFDO",
                 "CONSTRUÇÃO",
                 string,
                 fixed = T)
  
  string <- gsub("PA\\UFFFDO",
                 "PAÇO",
                 string,
                 fixed = T)
  
  string <- gsub("GRAJA\\UFFFD",
                 "GRAJAÚ",
                 string,
                 fixed = T)
  
  string <- gsub("IMOBILI\\UFFFDRIA",
                 "IMOBILIÁRIA",
                 string,
                 fixed = T)
  
  string <- gsub("IMOBILI\\UFFFDRIOS",
                 "IMOBILIÁRIOS",
                 string,
                 fixed = T)
  
  string <- gsub("A\\UFFFDAILANDIA",
                 "AÇAILANDIA",
                 string,
                 fixed = T)
  
  string <- gsub("CONCEI\\UFFFD\\UFFFDO",
                 "CONCEIÇÃO",
                 string,
                 fixed = T)
  
  string <- gsub("GUARUJ\\UFFFD",
                 "GUARUJÁ",
                 string,
                 fixed = T)
  
  string <- gsub("PINDAR\\UFFFD",
                 "PINDARÉ",
                 string,
                 fixed = T)
  
  string <- gsub("JATOB\\UFFFD",
                 "JATOBÁ",
                 string,
                 fixed = T)
  
  string <- gsub("JATOB\\UFFFDMA",
                 "JATOBÁ-MA",
                 string,
                 fixed = T)
  
  string <- gsub("PAR\\UFFFD",
                 "PARÁ",
                 string,
                 fixed = T)
  
  string <- gsub("JOS\\UFFFD",
                 "JOSÉ",
                 string,
                 fixed = T)
  
  string <- gsub("INCORPORA\\UFFFD\\UFFFDO",
                 "INCORPORAÇÃO",
                 string,
                 fixed = T)
  
  string <- gsub("CONTRU\\UFFFD\\UFFFDO",
                 "CONSTRUÇÃO",
                 string,
                 fixed = T)
  
  string <- gsub("\\U2013",
                 "-",
                 string,
                 fixed = T)
  
  string <- gsub("CONSTRU\\UFFFDAO",
                 "CONSTRUÇÃO",
                 string,
                 fixed = T)
  
  string <- gsub("CONSTRU\\UFFFDOES",
                 "CONSTRUÇÕES",
                 string,
                 fixed = T)
  
  string <- gsub("ARA\\UFFFDJO",
                 "ARAÚJO",
                 string,
                 fixed = T)
  
  string <- gsub("ITA\\UFFFD",
                 "ITAÚ",
                 string,
                 fixed = T)
  
  string <- gsub("MISS\\UFFFDES",
                 "MISSÕES",
                 string,
                 fixed = T)
  
  string <- gsub("HABITA\\UFFFDAO",
                 "HABITAÇÃO",
                 string,
                 fixed = T)
  
  string <- gsub("LU\\UFFFDS",
                 "LUÍS",
                 string,
                 fixed = T)
  
  string <- gsub("CABE\\UFFFDOTE",
                 "CABEÇOTE",
                 string,
                 fixed = T)
  
  string <- gsub("MAT\\UFFFDES",
                 "MATÕES",
                 string,
                 fixed = T)
  
  string <- gsub("A\\UFFFDAI",
                 "AÇAI",
                 string,
                 fixed = T)
  
  string <- gsub("CAC\\UFFFDRIO",
                 "CALCÁRIO",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00BF",
                 "",
                 string,
                 fixed = T)
  
  string <- gsub("\\U00BF",
                 "",
                 string,
                 fixed = T)
  
  string <- gsub("JUSTI\\UFFFDA",
                 "JUSTIÇA",
                 string,
                 fixed = T)
  
  string <- gsub("JU\\UFFFDZO",
                 "JUÍZO",
                 string,
                 fixed = T)
  
  string <- gsub("VIA\\UFFFD\\UFFFDO",
                 "VIAÇÃO",
                 string,
                 fixed = T)
  
  string <- gsub("AYMOR\\UFFFD",
                 "AYMORÉ",
                 string,
                 fixed = T)
  
  string <- gsub("SUPERINTED\\UFFFDNCIA",
                 "SUPERINTENDÊNCIA",
                 string,
                 fixed = T)
  
  string <- gsub("P\\UFFFDBICO",
                 "PÚBLICO",
                 string,
                 fixed = T)
  
  string <- gsub("MAIOB\\UFFFDO",
                 "MAIOBÃO",
                 string,
                 fixed = T)
  
  string <- gsub("INVESTIGA\\UFFFD\\UFFFDES",
                 "INVESTIGAÇÕES",
                 string,
                 fixed = T)
  
  string <- gsub("FOR\\UFFFDA",
                 "FORÇA",
                 string,
                 fixed = T)
  
  string <- gsub("GON\\UFFFDALVES",
                 "GONÇALVES",
                 string,
                 fixed = T)
  
  string <- gsub("PARNA\\UFFFDBA-MA",
                 "PARNAÍBA-MA",
                 string,
                 fixed = T)
  
  string <- gsub("MARNH\\UFFFDO",
                 "MARANHÃO",
                 string,
                 fixed = T)
  
  string <- gsub("ECON\\UFFFDMICA",
                 "ECONÔMICA",
                 string,
                 fixed = T)
  
  string <- gsub("MINIST\\UFFFDRIO",
                 "MINISTÉRIO",
                 string,
                 fixed = T)
  
  string <- gsub("RENOV\\UFFFDVEIS",
                 "RENOVÁVEIS",
                 string,
                 fixed = T)
  
  string <- gsub("CR\\UFFFDDITO",
                 "CRÉDITO",
                 string,
                 fixed = T)
  
  string <- str_to_upper(rm_accent(string))
  
  string <- stripWhitespace(trimws(str_squish(string)))
           
  
}