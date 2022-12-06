
## TÍTULO: LEITURA E ANÁLISE AUTOMATIZADA DE AÇÕES AMBIENTAIS - ABORDAGEM QUALITATIVA
## DATA: 28/07/2022
## AUTOR: REBECA CARVALHO

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(pdftools)
library(tm)
library(ptstem)
library(stringi)
library(abjutils)
library(extrafont)
library(data.table)

## OBJETIVOS

#'         - Realizar extração do interior teor dos acórdãos nos PDFs coletados.
#'         - Usar o texto extraído para a classificação automática das ações ambientais
#'           em três variáveis distintas: "10 RECORRENTE", "11 SETOR ECONÔMICO" 
#'           e "12 ELEMENTOS PROBATÓRIOS".

## PREPARANDO O AMBIENTE

extrafont::loadfonts()

setwd("CNJ")

# 1. Data -----------------------------------------------------------------

## Versão 01 da análise qualitativa

df <- read_csv("data/input/Acórdãos/análise_quali_v1.csv")

## Carregando base de referência da análise qualitativa

referencia <- read_csv("data/input/Acórdãos/template_análise qualitativa.csv")

## Carregando as informações básicas de cada processo

informacoes <- readxl::read_xlsx("data/input/Acórdãos/controle de seleção_análise qualitativa.xlsx",
                                 col_types = c("text",
                                               "text",
                                               "text",
                                               "date"))

## Carregando os dados do STF

stf <- readRDS("data/output/STF/acórdãos_STF_10112022_com filtros.rds")

## Carregando os dados do STJ

stj <- readRDS("data/output/STJ/acórdãos_STJ_31082022_com filtros.rds")

## Carregando os dados do TJAM

tjam <- readRDS("data/output/TJAM/acórdãos_TJAM_20072022_com filtros.rds")

## Carregando os dados do TJMT

tjmt <- readRDS("data/output/TJMT/acórdãos_TJMT_19082022_com filtros.rds")

## Carregando os dados do TJPA

tjpa <- readRDS("data/output/TJPA/acórdãos_TJPA_11112022_com filtros.rds")

## Carregando os dados do TJRO

tjro <- readRDS("data/output/TJRO/acórdãos_TJRO_26102022_com filtros.rds")

## Carregando os dados do TRF1

trf1 <- readRDS("data/output/TRF1/acórdãos_TRF1_25072022_com filtros.rds")

## 1.1. Limpeza ------------------------------------------------------------

## Arrumando o banco 01 da análise quali

df <- df %>% 
  mutate(Categoria = ifelse(`ID FGV` == "01_TJPA" &
                            `Subcategoria(s)` == "10 Prova testemunhal",
                            "10 ELEMENTOS PROBATÓRIOS",
                            Categoria)) %>% 
  rename("Subcategoria" = "Subcategoria(s)") %>% 
  mutate(Categoria = case_when(Categoria == "3 RECORRENTE" ~ "10 RECORRENTE",
                               Categoria == "4 SETOR ECONÔMICO" ~ "11 SETOR ECONÔMICO",
                               Categoria == "10 ELEMENTOS PROBATÓRIOS" ~ "12 ELEMENTOS PROBATÓRIOS",
                               T ~ "NA"),
         Subcategoria = case_when(Subcategoria == "4 Mineração" ~ "11 Mineração",
                                  Subcategoria == "4 Infraestrutura" ~ "11 Infraestrutura",
                                  Subcategoria == "4 Exploração florestal" ~ "11 Exploração florestal",
                                  Subcategoria == "4 Agronegócio" ~ "11 Agronegócio",
                                  Subcategoria == "4 Comercialização de animais silvestres" ~ "11 Comercialização de animais silvestres",
                                  Subcategoria == "4 999" ~ "11 999",
                                  Subcategoria %in% c("10 Fiscalização, vistoria ou estudos técnicos do órgão ambiental",
                                                      "10 fiscalização ou vistoria do órgão ambiental") ~ "12 Fiscalização, vistoria ou estudos técnicos do órgão ambiental",
                                  Subcategoria == "10 999" ~ "12 999",
                                  Subcategoria == "10 Audiência pública" ~ "12 Sustentação oral em audiência pública",
                                  Subcategoria == "10 Dados do IBGE" ~ "12 Dados do IBGE",
                                  Subcategoria == "10 Inquérito civil" ~ "12 Inquérito civil",
                                  Subcategoria == "10 Prova pericial" ~ "12 Prova pericial",
                                  Subcategoria == "10 Relatório Técnico do Ministério Público" ~ "12 Relatório técnico do Ministério Público",
                                  Subcategoria == "10 prova documental" ~ "12 Demais provas documentais",
                                  Subcategoria == "10 Prova testemunhal" ~ "12 Provas orais, depoimentos ou declarações",
                                  T ~ as.character(Subcategoria)),
         Subsubcategoria = NA,
         `Demais informações` = Subcategoria,
         Subcategoria = ifelse(Categoria == "10 RECORRENTE",
                               NA,
                               Subcategoria),
         `Demais informações` = gsub("3",
                                     "10",
                                     `Demais informações`,
                                     fixed = TRUE),
         `Demais informações` = ifelse(`Demais informações` == "10 Usina Siderúrgica do Pará (USIPAR)",
                                       "10 Usina Siderúrgica do Pará LTDA (USIPAR)",
                                       `Demais informações`)) %>% 
    select(Trecho:Subsubcategoria,
           `Demais informações`,
           Pesquisadora)

## Padronizando as numerações

for (i in 1:nrow(informacoes)) {
  
  cat("Lendo", i, "\n")
  
  if(informacoes$Tribunal[i] == "TJRO"){
    
    stri_sub(informacoes$`Número do processo`[i], 18, 2) <- "."
    
    }
}

## Preparando os dados para o join

stf <- stf %>%
  mutate(SiglaTribunal = "STF")

stj <- stj %>%
  mutate(SiglaTribunal = "STJ")

tjam <- tjam %>%
  mutate(SiglaTribunal = "TJAM")

tjmt <- tjmt %>%
  mutate(SiglaTribunal = "TJMT")

tjpa <- tjpa %>%
  mutate(SiglaTribunal = "TJPA")

tjro <- tjro %>%
  mutate(SiglaTribunal = "TJRO")

trf1 <- trf1 %>%
  mutate(SiglaTribunal = "TRF1")

## Empilhando os bancos

minerados <- rbind.fill(stf,
                        stj,
                        tjam,
                        tjmt,
                        tjpa,
                        tjro,
                        trf1) %>%
  select(Numero,
         SiglaTribunal,
         DataPublicacao,
         DataJulgamento,
         Ementa,
         `ambiental/socioambiental`,
         `meio ambiente`,
         `amazônia`,
         floresta)

## Filtrando somente os casos de interesse

minerados <- minerados %>%
  filter(`ambiental/socioambiental` == 1 |
           `meio ambiente` == 1 |
           amazônia == 1 |
           floresta == 1)

# 2. Acórdãos -------------------------------------------------------------

## Cria uma lista com os nomes dos .pdfs que serão lidos

acordaos <- list.files(path = "data/input/Acórdãos",
                       pattern = ".pdf",
                       ignore.case = TRUE)

## Cria um dataframe vazio em que as decisões
## desfavoráveis serão armazenadas

inteiroteor <- data.frame("InteiroTeor" = rep(NA,
                                              113))

## For loop que extrai os dados dos PDFs

for(i in seq_along(acordaos)){
  
  cat("Lendo",i,"\n")
  
  inteiroteor$`Número do processo`[i] <- str_split(acordaos[i],
                                                   ".pdf")[[1]][1]
  
  inteiroteor$InteiroTeor[i]  <- paste(pdftools::pdf_text(paste0("data/input/Acórdãos/",
                                                                 acordaos[i])),
                                       sep = '',
                                       collapse = '')
  
}

## Fazendo um join parcial

inteiroteor <- left_join(inteiroteor,
                         informacoes) %>%
  select(`Número do processo`,
         `ID FGV`,
         InteiroTeor)

## Adiciona pontuação aos valores da variável 'NÚMERO_MP'

for (i in 1:nrow(inteiroteor)) {
  
  cat("Lendo", i, "\n")
  
  if(is.na(inteiroteor$`ID FGV`[i])){
    
    stri_sub(inteiroteor$`Número do processo`[i], 18, 2) <- "."
    
    }
}

## Preparando os dados para o novo join
 
inteiroteor <- inteiroteor %>% 
  select(-`ID FGV`)

## Juntando com as informações dos processos
 
inteiroteor <- left_join(inteiroteor,
                         informacoes) %>% 
  select(`Número do processo`,
         `ID FGV`,
         Tribunal,
         `Data de Julgamento`,
         InteiroTeor) %>% 
  filter(!is.na(`ID FGV`)) %>% 
  arrange(Tribunal, 
          `ID FGV`)

# 3. Busca ----------------------------------------------------------------

## 3.1. Recorrente ---------------------------------------------------------

### 3.1.1. Categoria --------------------------------------------------------

## Definindo os termos de busca

recorrente_all <- c("APELANTE:",
                "apelante :",
                "apelante[(]s[)]:",
                "APELANTE(S):",
                "apelante(s):",
                "RECTE.(S):",
                "AGTE.(S):",
                "AGRAVANTE          :",
                "AGRAVANTE               :",
                "RECORRENTE                :",
                "recorrente :",
                "Agravante:",
                "agravante :",
                "juizo recorrente",
                "Recorrente:",
                "recorre da",
                "interpo.",
                "interposta por",
                "interposto na",
                "interposto em",
                "apela da",
                "apela de",
                "apelam da",
                "ABEL RAMOS apela (Id 10030667) da",
                "interpela da",
                "opostos pelo",
                "(RECORRENTE)",
                "(APELANTE)",
                "(AGRAVANTE)",
                "(RECORRENTE)")

recorrente_mt <- c("(APELANTE)",
                   "(AGRAVANTE)",
                   "(RECORRENTE)")

## Padronizando os termos

recorrente_all <- str_to_lower(rm_accent(recorrente_all))

recorrente_mt <- str_to_lower(rm_accent(recorrente_mt))

## Adequando ao formato regex

regex <- regex(paste0("([^\\n])*",
                      recorrente_all,
                      "([^\\n]).*",
                       collapse = "|"))

regex_all <- regex(paste0("((?:[^\\n]+\\n){1})",
                      recorrente_all,
                      "(([^\\n]+\\n){2})",
                      collapse = "|"))

regex_mt <- regex(paste0("[^,]*",
                  recorrente_mt,
                  "[^,]*.",
                  collapse = "|"))

## Buscando o trecho com informações
## sobre o recorrente

recorrente <- inteiroteor %>% 
  mutate(InteiroTeor = gsub("\\ +", " ", 
                            InteiroTeor),
         InteiroTeor = gsub("\\n+", "\n", 
                            InteiroTeor),
         InteiroTeor = gsub("advogad. do[(]a[)] apelante:",
                            "",
                            InteiroTeor,
                            ignore.case = TRUE),
         InteiroTeor = gsub("advogados do[(]a[)]\napelante:",
                            "",
                            InteiroTeor,
                            ignore.case = TRUE),
         InteiroTeor = gsub("advogado. do[(]a[)] agravante:",
                            "",
                            InteiroTeor,
                            ignore.case = TRUE),
         InteiroTeor = gsub("Advogados do(a) APELANTE:",
                            "",
                            InteiroTeor,
                            fixed = TRUE)) %>% 
  mutate(InteiroTeor = gsub("in verbis:.*[Destaquei]",
                            "",
                            InteiroTeor),
         InteiroTeor = gsub("in verbis:.*(Destaquei)",
                            "",
                            InteiroTeor),
         InteiroTeor = gsub("Nesse sentido:.*[Destaquei]",
                            "",
                            InteiroTeor),
         InteiroTeor = gsub("Justiça:.*[Destaquei]",
                            "",
                            InteiroTeor),
         InteiroTeor = gsub("Sodalício::.*[Destaquei]",
                            "",
                            InteiroTeor),
         InteiroTeor = gsub("precedente:.*(Destaquei)",
                            "",
                            InteiroTeor)) 


## Buscando os termos
 
recorrente <- recorrente %>%  
  mutate(Trecho = ifelse(Tribunal != "TJMT",
                         as.character(str_extract(str_to_lower(rm_accent(trimws(InteiroTeor))),
                                                       regex_all)),
                         NA),
         Trecho = ifelse(Tribunal == "TJMT",
                         as.character(str_extract(str_to_lower(rm_accent(InteiroTeor)),
                                                               regex_mt)),
                         Trecho)) %>% 
  mutate(Trecho = ifelse(nchar(Trecho) > 350 &
                         Tribunal == "TJMT",
                         as.character(str_extract(str_to_lower(rm_accent(trimws(InteiroTeor))),
                                                  regex)),
                         Trecho),
         Trecho = ifelse(is.na(Trecho) &
                         Tribunal == "TRF1",
                         as.character(str_extract(str_to_lower(rm_accent(InteiroTeor)),
                                                  regex_mt)),
                         Trecho),
         Trecho = ifelse(Tribunal == "TJRO",
                         as.character(str_extract(str_to_lower(rm_accent(trimws(InteiroTeor))),
                                                  regex)),
                         Trecho),
         Trecho = sub(".*parte[(]s[)]:\n\\[", 
                      "", 
                      Trecho)) %>% 
  mutate(Categoria = "10 RECORRENTE") %>% 
  select(InteiroTeor,
         `Número do processo`:`Data de Julgamento`,
         Trecho,
         Categoria)

remove <- c("agravado",
            "apelado",
            "recorrido",
            "advogado",
            "advogados",
            "procurador")

remove <- regex(paste0(remove,
                       ".*",
                       collapse = "|"))

recorrente <- recorrente %>% 
  mutate(Trecho = sub(remove,
                      "",
                      recorrente$Trecho))

### 3.1.2. Subcategoria -----------------------------------------------------

## Definindo os termos de pesquisa

sa <- regex(paste0(str_to_lower(rm_accent(c("s/a",
                               "s/a[.]",
                               "sa$",
                               "s[.]a",
                               "sa[.]",
                               "s a$"))),
                   collapse = "|"))

ltda <- regex(paste0(str_to_lower(rm_accent(c("ltda",
                                 "ltda.",
                                 "limitada",
                                 "limitada.",
                                 "epp"))),
                     collapse = "|"))

me <- regex(paste0(str_to_lower(rm_accent(c("ME$",
                                            "M[.]E[.]$"))),
                   collapse = "|"))

mpf <- regex(paste0(str_to_lower(rm_accent(c("Minist.rio P.blico Federal",
                                "MPF"))),
                    collapse = "|"))

mpe <- regex(paste0(str_to_lower(rm_accent(c("Minist.rio P.blico do Estado",
                                             "MPE"))),
                    collapse = "|"))

executivofed <- regex(paste0(str_to_lower(rm_accent(c("Uni.o",
                                         "Presidente da Rep.blica",
                                         "IBAMA",
                                         "Instituto Brasileiro do Meio Ambiente e dos Recursos Naturais Renov.veis",
                                         "FUNAI",
                                         "Funda..o Nacional do .ndio",
                                         "CONAMA",
                                         "Conselho Nacional do Meio Ambiente",
                                         "ICMBIO",
                                         "Instituto Chico Mendes de Conserva..o da Biodiversidade"))),
                             collapse = "|"))

executivoest <- regex(paste0(str_to_lower(rm_accent(c("agravante : estado",
                                                      "agravante: o estado",
                                                      "^estado",
                                                      "polo ativo: estado",
                                                      "10 estado"))),
                             collapse = "|"))

executivomunic <- regex(paste0(str_to_lower(rm_accent(c("^ Munic.pio",
                                                        "municipio"))),
                               collapse = "|"))

definir <- regex(paste0(str_to_lower(rm_accent(c("Confedera..o",
                                    "Associa..o",
                                    "Articula..o",
                                    "Coordena..o",
                                    "Instituto",
                                    "Ag.ncia"))),
                        collapse = "|"))

## Classificando os resultados encontrados

recorrente <- recorrente %>% 
  mutate(Subsubcategoria = ifelse(str_detect(Trecho,
                                             sa),
                                  "10 SA",
                                  ifelse(str_detect(Trecho,
                                                    ltda),
                                         "10 LTDA",
                                         ifelse(str_detect(Trecho,
                                                           me),
                                                "10 ME",
                                                ifelse(str_detect(Trecho,
                                                                  mpf),
                                                       "10 M Federal",
                                                       ifelse(str_detect(Trecho,
                                                                         mpe),
                                                              "10 M Estadual",
                                                              ifelse(str_detect(Trecho,
                                                                                executivofed),
                                                                     "10 Federal",
                                                                     ifelse(str_detect(Trecho,
                                                                                       executivoest),
                                                                            "10 Estadual",
                                                                            ifelse(str_detect(Trecho,
                                                                                              executivomunic),
                                                                                   "10 Municipal",
                                                                                   NA))))))))) %>% 
  mutate(Subcategoria = ifelse(Subsubcategoria %in% c("10 SA",
                                                      "10 LTDA",
                                                      "10 ME"),
                               "10 Pessoa Jurídica",
                               ifelse(Subsubcategoria %in% c("10 M Federal",
                                                             "10 M Estadual") |
                                      str_detect(Trecho,
                                                 "ministerio publico"),
                                      "10 Ministério Público",
                                      ifelse(Subsubcategoria %in% c("10 Federal",
                                                                    "10 Estadual",
                                                                    "10 Municipal"),
                                             "10 Entidade do Poder Executivo",
                                             ifelse(str_detect(Trecho,
                                                               definir),
                                                    "10 A DEFINIR",
                                                    ifelse(is.na(Trecho),
                                                           "10 999",
                                                           "10 Pessoa Física")))))) %>% 
  mutate(Subsubcategoria = ifelse(Subsubcategoria == "10 M Federal",
                                  "10 Federal",
                                  ifelse(Subsubcategoria == "10 M Estadual",
                                         "10 Estadual",
                                         Subsubcategoria))) %>% 
  select(InteiroTeor,
         `Número do processo`:Categoria,
         Subcategoria,
         Subsubcategoria)

## Aplicando a mesma análise para a versão 01 do quali

df <- df %>% 
  mutate(Trecho2 = str_to_lower(rm_accent(`Demais informações`))) %>% 
  mutate(Trecho = ifelse(Trecho == "xxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
                         NA,
                         Trecho),
         Subsubcategoria = ifelse(str_detect(Trecho2,
                                             sa) &
                                  Categoria == "10 RECORRENTE",
                                  "10 SA",
                                  ifelse(str_detect(Trecho2,
                                                    ltda) &
                                           Categoria == "10 RECORRENTE",
                                         "10 LTDA",
                                         ifelse(str_detect(Trecho2,
                                                           me) &
                                                  Categoria == "10 RECORRENTE",
                                                "10 ME",
                                                ifelse(str_detect(Trecho2,
                                                                  mpf) &
                                                         Categoria == "10 RECORRENTE",
                                                       "10 M Federal",
                                                       ifelse(str_detect(Trecho2,
                                                                         mpe) &
                                                                Categoria == "10 RECORRENTE",
                                                              "10 M Estadual",
                                                              ifelse(str_detect(Trecho2,
                                                                                executivofed) &
                                                                       Categoria == "10 RECORRENTE",
                                                                     "10 Federal",
                                                                     ifelse(str_detect(Trecho2,
                                                                                       executivoest) &
                                                                              Categoria == "10 RECORRENTE",
                                                                            "10 Estadual",
                                                                            ifelse(str_detect(Trecho2,
                                                                                              executivomunic) &
                                                                                     Categoria == "10 RECORRENTE",
                                                                                   "10 Municipal",
                                                                                   NA))))))))) %>% 
  mutate(Subcategoria = ifelse(Subsubcategoria %in% c("10 SA",
                                                      "10 LTDA",
                                                      "10 ME"),
                               "10 Pessoa Jurídica",
                               ifelse(Subsubcategoria %in% c("10 M Federal",
                                                             "10 M Estadual") |
                                        str_detect(Trecho2,
                                                   "ministerio publico"),
                                      "10 Ministério Público",
                                      ifelse(Subsubcategoria %in% c("10 Federal",
                                                                    "10 Estadual",
                                                                    "10 Municipal"),
                                             "10 Entidade do Poder Executivo",
                                             ifelse(str_detect(Trecho2,
                                                               definir),
                                                    "10 A DEFINIR",
                                                    ifelse(is.na(Trecho2) &
                                                           Categoria == "10 RECORRENTE",
                                                           "10 999",
                                                           Subcategoria))))),
         Subcategoria = ifelse(is.na(Subcategoria) &
                              Categoria == "10 RECORRENTE",
                              "10 Pessoa Física",
                              Subcategoria),
         Subcategoria = ifelse(is.na(Trecho) |
                               `Demais informações` == "10 888",
                               "10 999",
                               Subcategoria),
         Subcategoria = ifelse(`Demais informações` == "10 Carmina Silva de Sá",
                               "10 Pessoa Física",
                               Subcategoria)) %>% 
  mutate(Subsubcategoria = ifelse(Subsubcategoria == "10 M Federal",
                                  "10 Federal",
                                  ifelse(Subsubcategoria == "10 M Estadual",
                                         "10 Estadual",
                                         Subsubcategoria)),
         Subsubcategoria = ifelse(`Demais informações` == "10 Carmina Silva de Sá",
                                  NA,
                                  Subsubcategoria)) %>% 
  select(Trecho:Categoria,
         Subcategoria,
         Subsubcategoria,
         `Demais informações`,
         Pesquisadora)

## 3.2. Setor econômico ----------------------------------------------------

### 3.2.1. Subcategoria -----------------------------------------------------

## Preparando os termos que serão buscados

infraestrutura <- regex(paste0(str_to_lower(rm_accent(c("rodovia",
                    "ferrovia",
                    "empreendimento",
                    "barramento",
                    "represamento",
                    "represa",
                    "barragem",
                    "energia hidr.ulica",
                    "gera..o de energia",
                    "usina",
                    "UHE",
                    "hidrel.trica"))),
                    collapse = "|"))

mineracao <- regex(paste0(str_to_lower(rm_accent(c("mina$",
               "mineroduto",
               "garimpo",
               "minera..o",
               "ferro.gusa",
               "min.rio",
               "garimpagem",
               "marmoraria"))),
               collapse = "|"))

agronegocio <- regex(paste0(str_to_lower(rm_accent(c("irriga..o",
                 "agropecu.ri.",
                 "matadouro",
                 "produtor. rura.",
                 "fazenda.",
                 "propriedad. rura.",
                 "im.ve. rura.",
                 "agropastori.",
                 "pecu.aria",
                 "agr.cola",
                 "agricultura",
                 "soja",
                 "pastagem",
                 "capim",
                 "plantio",
                 "sua propriedade",
                 "bovin.",
                 "produ..o.*aliment.cia"))),
                 collapse = "|"))

explorflor <- regex(paste0(str_to_lower(rm_accent(c("madeireir.",
                "madeira",
                "serralheir.",
                "desmat.",
                "desflorest.",
                "deflorest.",
                "produt. floresta.",
                "lenha",
                "carv.o vegetal",
                "destrui.*floresta",
                "destrui.*mata",
                "danific.*floresta",
                "danific.*mata"))),
                collapse = "|"))

## Preparando os dados

setorecon <- inteiroteor %>% 
  mutate(InteiroTeor = gsub("in verbis:.*[Destaquei]",
                            "",
                            InteiroTeor),
         InteiroTeor = gsub("in verbis:.*(Destaquei)",
                            "",
                            InteiroTeor),
         InteiroTeor = gsub("Nesse sentido:.*[Destaquei]",
                            "",
                            InteiroTeor),
         InteiroTeor = gsub("Justiça:.*[Destaquei]",
                            "",
                            InteiroTeor),
         InteiroTeor = gsub("Sodalício::.*[Destaquei]",
                            "",
                            InteiroTeor),
         InteiroTeor = gsub("precedente:.*(Destaquei)",
                            "",
                            InteiroTeor)) 

## Buscando os termos

setorecon <- setorecon %>% 
  mutate(Categoria = "11 SETOR ECONÔMICO",
         "11 Infraestrutura" = str_detect(InteiroTeor,
                                          infraestrutura),
         "11 Mineração" = str_detect(InteiroTeor,
                                     mineracao),
         "11 Agronegócio" = str_detect(InteiroTeor,
                                       agronegocio),
         "11 Exploração florestal" = str_detect(InteiroTeor,
                                                explorflor),
         "11 999" = ifelse(`11 Infraestrutura` == 0 &
                           `11 Mineração` == 0 &
                           `11 Agronegócio` == 0 &
                           `11 Exploração florestal` == 0,
                           TRUE,
                           FALSE))

## Adequando os resultados encontrados

setorecon <- melt(setDT(setorecon), id.vars = c("Número do processo",
                                      "ID FGV",
                                      "Tribunal",
                                      "Data de Julgamento",
                                      "InteiroTeor",
                                      "Categoria"), 
             variable.name = "Subcategoria")

## Filtrando e reorganizando o formato

setorecon <- setorecon %>% 
  filter(value == "TRUE") %>% 
  select(`Número do processo`:`Data de Julgamento`,
         Categoria,
         Subcategoria) %>% 
  arrange(Tribunal,
          `ID FGV`)

## 3.3. Elementos Probatórios ----------------------------------------------

### 3.3.1. Subcategoria -----------------------------------------------------

## Preparando os termos que serão buscados

fiscalizacao <- regex(paste0(str_to_lower(rm_accent(c("relat.rio de fiscaliza..o",
                                                      "document.",
                                                      "rel.torio",
                                                      "relat.rios.*org.os de prote..o ambiental",
                                                      "relat.rios.*.rg.o de prote..o ambiental"))),
                             collapse = "|"))

provaper <- regex(paste0(str_to_lower(rm_accent(c("per.cia",
                                                  "laudo pericial"))),
                         collapse = "|"))

reltec <- regex(paste0(str_to_lower(rm_accent(c("Grupo de Apoio T.cnico Interdisciplinar do Ministério Público",
                                                "GATI",
                                                "relat.rios t.cnicos de inspe..o.*minist.rio p.blico"))),
                       collapse = "|"))

ibge <- regex(paste0(str_to_lower(rm_accent(c("Instituto Brasileiro de Geografia e Estat.stica",
                                                "IBGE"))),
                       collapse = "|"))

inqciv <- regex(paste0(str_to_lower(rm_accent(c("inqu.rito civil"))),
                       collapse = "|"))

sustoral <- regex(paste0(str_to_lower(rm_accent(c("audi.ncia p.blica"))),
                       collapse = "|"))

demprov <- regex(paste0(str_to_lower(rm_accent(c("memoriais",
                                                 "estudos",
                                                 "relat.rios",
                                                 "pareceres",
                                                 "boletim de ocorr.ncia"))),
                         collapse = "|"))

provor <- regex(paste0(str_to_lower(rm_accent(c("prova. ora.",
                                                "oitiva",
                                                "testemunh.",
                                                "audi.ncia de instru..o",
                                                "depoimento",
                                                "depoente",
                                                "Termo de Declara..o"))),
                        collapse = "|"))

autinfr <- regex(paste0(str_to_lower(rm_accent(c("autos de infra..o","
                                                 auto de infra..o"))),
                                  collapse = "|"))


## Preparando os dados

elemtprob <- inteiroteor %>% 
  mutate(InteiroTeor = gsub("in verbis:.*[Destaquei]",
                            "",
                            InteiroTeor),
         InteiroTeor = gsub("in verbis:.*(Destaquei)",
                            "",
                            InteiroTeor),
         InteiroTeor = gsub("Nesse sentido:.*[Destaquei]",
                            "",
                            InteiroTeor),
         InteiroTeor = gsub("Justiça:.*[Destaquei]",
                            "",
                            InteiroTeor),
         InteiroTeor = gsub("Sodalício::.*[Destaquei]",
                            "",
                            InteiroTeor),
         InteiroTeor = gsub("precedente:.*(Destaquei)",
                            "",
                            InteiroTeor)) 

## Buscando os termos

elemtprob <- elemtprob %>% 
  mutate(Categoria = "12 ELEMENTOS PROBATÓRIOS",
         "12 Fiscalização, vistoria ou estudos técnicos do órgão ambiental" = str_detect(InteiroTeor,
                                                                                         fiscalizacao),
         "12 Prova pericial" = str_detect(InteiroTeor,
                                          provaper),
         "12 Relatório técnico do Ministério Público" = str_detect(InteiroTeor,
                                                                   reltec),
         "12 Dados do IBGE" = str_detect(InteiroTeor,
                                         ibge),
         "12 Inquérito civil" = str_detect(InteiroTeor,
                                           inqciv),
         "12 Sustentação oral em audiência pública" = str_detect(InteiroTeor,
                                                                 sustoral),
         "12 Demais provas documentais" = str_detect(InteiroTeor,
                                                     demprov),
         "12 Provas orais, depoimentos ou declarações" = str_detect(InteiroTeor,
                                                                    provor),
         "12 Autos de infração" = str_detect(InteiroTeor,
                                             autinfr),
         "12 999" = ifelse(`12 Fiscalização, vistoria ou estudos técnicos do órgão ambiental` == 0 &
                           `12 Prova pericial` == 0 &
                           `12 Relatório técnico do Ministério Público` == 0 &
                           `12 Dados do IBGE` == 0 &
                           `12 Inquérito civil` == 0 &
                           `12 Sustentação oral em audiência pública` == 0 &
                           `12 Demais provas documentais` == 0 &
                           `12 Provas orais, depoimentos ou declarações` == 0 &
                           `12 Autos de infração` == 0,
                           TRUE,
                           FALSE))

## Adequando os resultados encontrados

elemtprob <- melt(setDT(elemtprob), id.vars = c("Número do processo",
                                                "ID FGV",
                                                "Tribunal",
                                                "Data de Julgamento",
                                                "InteiroTeor",
                                                "Categoria"), 
                  variable.name = "Subcategoria")

## Filtrando e reorganizando o formato

elemtprob <- elemtprob %>% 
  filter(value == "TRUE") %>% 
  select(`Número do processo`:`Data de Julgamento`,
         Categoria,
         Subcategoria) %>% 
  arrange(Tribunal,
          `ID FGV`)

# 4. Ementas --------------------------------------------------------------

## 4.1. Setor Econômico ----------------------------------------------------

## Buscando os termos

minerados <- minerados %>% 
  mutate("Infraestrutura" = ifelse(str_detect(Ementa,
                                       infraestrutura),
                                   1,
                                   0),
         "Mineração" = ifelse(str_detect(Ementa,
                                  mineracao),
                              1,
                              0),
         "Agronegócio" = ifelse(str_detect(Ementa,
                                    agronegocio),
                                1,
                                0),
         "Exploração florestal" = ifelse(str_detect(Ementa,
                                             explorflor),
                                         1,
                                         0),
         "NA" = ifelse(`Infraestrutura` == 0 &
                       `Mineração` == 0 &
                       `Agronegócio` == 0 &
                       `Exploração florestal` == 0,
                       1,
                       0))

## Verificando os números por setor

min_setorecon <- minerados %>% 
  group_by(SiglaTribunal) %>% 
  summarise(freq = n(),
            Infraestrutura = sum(Infraestrutura),
            `Mineração` = sum(`Mineração`),
            `Agronegócio` = sum(`Agronegócio`),
            `Exploração florestal` = sum(`Exploração florestal`),
            `NA` = sum(`NA`))

## Criando uma versão para a camada do SireneJud

minerados2 <- minerados %>% 
  select(Numero:Ementa,
         Infraestrutura:`Exploração florestal`) %>% 
  mutate(Ementa = gsub("^Ementa : ", "", Ementa),
         Ementa = gsub("^Ementa.*", "", Ementa),
         Ementa = gsub("E M E N T A:", "", Ementa))

## 4.2. Terras Públicas ----------------------------------------------------

terrasp <- regex(paste0(str_to_lower(rm_accent(c("terras p.blicas",
                                                 "terras devolutas",
                                                 "terra p.blica",
                                                 "terra devoluta",
                                                 "esta..o ecol.gica",
                                                 "reserva biol.gica",
                                                 "parque nacional",
                                                 "floresta nacional",
                                                 "reserva extrativista",
                                                 "reserva de fauna",
                                                 "reserva de desenvolvimento sustent.vel",
                                                 "terras ind.genas",
                                                 "terra ind.gena",
                                                 "monumento natural",
                                                 "ref.gio de vida silvestre",
                                                 "area de prote..o ambiental",
                                                 "area de de relevante interesse ecol.gico",
                                                 "quilombo",
                                                 "terras",
                                                 "area.",
                                                 "invas.es",
                                                 "invas.o.*quilomb.",
                                                 "ind.gena.",
                                                 "propriedad. p.blic",
                                                 "dom.ni. p.blic",
                                                 "bem. p.blic",
                                                 "bens. p.blic",
                                                 "terra. p.blic",
                                                 "propriedad. federa.",
                                                 "dom.ni. federa.",
                                                 "bem federal",
                                                 "bens federa.",
                                                 "terra. federa.",
                                                 "propriedad. da uni.o",
                                                 "dom.ni. da uni.o",
                                                 "bem da uni.o",
                                                 "bens da uni.o",
                                                 "terra. da uni.o",
                                                 "propriedad. estadua.",
                                                 "dom.ni. estadua.",
                                                 "bem estadual",
                                                 "bens estadua.",
                                                 "terra. estadua.",
                                                 "propriedad. do estado",
                                                 "dom.ni. do estado",
                                                 "bem do estado",
                                                 "bens do estado",
                                                 "terra. do estado",
                                                 "propriedad. municipa.",
                                                 "dom.ni. municipa.",
                                                 "bem municipa.",
                                                 "bens municipa.",
                                                 "terra. municipa.",
                                                 "propriedad. do munic.pio",
                                                 "dom.ni. do munic.pio",
                                                 "bem do munic.pio",
                                                 "bens do munic.pio",
                                                 "terra. do munic.pio"))),
                        collapse = "|"))

## Buscando os termos

minerados <- minerados %>% 
  mutate("Terras Públicas" = ifelse(str_detect(Ementa,
                                        terrasp),
                                    1,
                                    0))

## Verificando os números por setor

min_terraspub <- minerados %>% 
  group_by(SiglaTribunal) %>% 
  summarise(freq = n(),
            `Terras Públicas` = sum(`Terras Públicas`))

# 5. Rbind ----------------------------------------------------------------

## Carregando os dados de referência

recorrente <- readxl::read_xlsx("data/output/Acórdãos/checagem_manual_análise quali.xlsx")

df2 <- read_csv("data/input/Acórdãos/análise_quali_v2.csv")

df3 <- read_csv("data/input/Acórdãos/análise_quali_v3.csv")

## Preparando os dados

df2 <- df2 %>% 
  rename("Categoria" = "CATEGORIA",
         "Subcategoria" = "SUBCATEGORIA")

df3 <- df3 %>% 
  rename("Categoria" = "CATEGORIA",
         "Subcategoria" = "SUBCATEGORIA")

recorrente <- recorrente %>% 
  mutate(Trecho = str_to_upper(Trecho),
         Categoria = str_to_upper(Categoria),
         Subcategoria = str_to_upper(Subcategoria),
         Subsubcategoria = str_to_upper(Subsubcategoria))

## Empilhando os dados

banco <- rbind.fill(df,
                    df2,
                    df3,
                    recorrente,
                    setorecon,
                    elemtprob) %>% 
  mutate(Categoria = str_to_upper(Categoria),
         Subcategoria = str_to_upper(Subcategoria),
         Subsubcategoria = str_to_upper(Subsubcategoria),
         `Demais informações` = str_to_upper(`Demais informações`),
         Categoria = factor(Categoria,
                            levels = c("1 POLO ATIVO >",
                                       "2 POLO PASSIVO >",
                                       "3 INTIMADO >",
                                       "4 QUESTÃO JURÍDICA COLOCADA >",
                                       "5 RELAÇÃO C. PODER EXECUTIVO >",
                                       "6 INDICAÇÃO DE AUTOEXECUÇÃO >",
                                       "7 COMANDO JUDICIAL (DISPOSITIVO) >",
                                       "8 DESTINATÁRIO DO COMANDO JUDICIAL >",
                                       "9 SANÇÕES/CONSEQUÊNCIAS PREVISTAS PARA DESCUMPRIMENTO >",
                                       "10 APRECIAÇÃO DOS PEDIDOS >",
                                       "10 RECORRENTE",
                                       "11 SETOR ECONÔMICO",
                                       "12 ELEMENTOS PROBATÓRIOS")),
         Subcategoria = ifelse(Subcategoria %in% c("3.888",
                                                   "3888"),
                               "3 888",
                               Subcategoria)) %>% 
  arrange(Tribunal,
          `ID FGV`, 
          Categoria, 
          Subcategoria,
          Subsubcategoria) %>% 
  filter(!is.na(`Número do processo`))

# 6. Export ---------------------------------------------------------------

## Salvando os dados

saveRDS(minerados2,
        "data/output/recorte_processos ambientais_14112022.rds")

write.csv(minerados2,
          "data/output/recorte_processos_ambientais_14112022.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

write.csv(df,
          "data/output/Acórdãos/análise_quali_v1.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

writexl::write_xlsx(banco,
                    "data/output/Acórdãos/compilado_análise_qualitativa_16112022.xlsx")
