
## TÍTULO: ANÁLISE DESCRITIVA - P3
## DATA: 20/07/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(geobr)
library(ggsci)
library(sf)
library(ggthemes)
library(nord)
library(scales)

## CONFIGURA O AMBIENTE

windowsFonts("Arial" = windowsFont("Arial"))

# 1. Data -----------------------------------------------------------------

## Lista de municípios da amazônia legal

amazonia <- readxl::read_xlsx("data/input/municípios_amazônia legal.xlsx")

## Carregando os dados do DataJud

datajud <- readRDS("data/output/DataJud/dataJud_final_17072022.rds")

## Carregando os dados do STF

stf <- readRDS("data/output/DataJud/join_DataJud_STF_com filtros_18072022.rds")

stf_comp <- readRDS("data/input/STF/stf_com_filtros.rds")

## Carregando os dados do TJAM

tjam <- readRDS("data/output/DataJud/join_DataJud_TJAM_com filtros_20072022.rds")

tjam_comp <- readRDS("data/output/TJAM/acórdãos_TJAM_20072022_com filtros.rds")

## Carregando os dados do TJPA

tjpa <- readRDS("data/output/DataJud/join_DataJud_TJPA_com filtros_18072022.rds")

tjpa_comp <- readRDS("data/input/TJPA/tjpa_com_filtros.rds")

## Carregando os dados do TRF1

trf1 <- readRDS("data/output/DataJud/join_DataJud_TRF1_com filtros_25072022.rds")

trf1_comp <- readRDS("data/output/TRF1/acórdãos_TRF1_25072022_com filtros.rds")

## Shape dos municípios

municipios <- geobr::read_municipality(year = 2020)

## Organizando os dados

municipios <- municipios %>% 
  select(abbrev_state,
         code_muni,
         geom) %>% 
  rename("CodigoMunicipioIBGE" = "code_muni") %>% 
  mutate(CodigoMunicipioIBGE = as.character(CodigoMunicipioIBGE))

# 2. Gráficos -------------------------------------------------------------

## Criando uma coluna que indica o tribunal
## em cada um dos bancos

stf <- stf %>% 
  mutate(Tribunal = "STF")

tjam <- tjam %>% 
  mutate(Tribunal = "TJAM")

tjpa <- tjpa %>% 
  mutate(Tribunal = "TJPA")

trf1 <- trf1 %>% 
  mutate(Tribunal = "TRF1")

## Empilhando os dados em um mesmo banco

join <- rbind.fill(stf,
                   tjam,
                   tjpa,
                   trf1)

comp <- rbind.fill(stf_comp,
                   tjam_comp,
                   tjpa_comp,
                   trf1_comp)

## 2.1. Número de ações por tribunal ---------------------------------------

numacoes_tribunal <- join %>% 
  group_by(Tribunal,
           OrgaoJulgador,
           NomeMunicipio) %>% 
  summarise(Frequencia = n())

## 2.2. Número de ações por partes -----------------------------------------


## 2.3. Número de ações por região/município -------------------------------

## Calculando quantas ações ambientais
## tem por município

numacoes_join <- join %>% 
 # filter(!is.na(NomeMunicipio)) %>% 
  group_by(Tribunal,
           UF,
           CodigoMunicipioIBGE, 
           NomeMunicipio,
           OrgaoJulgador,
           Instancia,
           Grau) %>% 
  summarise(Frequencia = n()) %>% 
  arrange(desc(Frequencia)) 

## Criando um banco com os municípios do Pará

pa <- municipios %>% 
  filter(abbrev_state == "PA")

## Juntando com os dados do IBGE

numacoes_tjpa <- left_join(numacoes_tjpa,
                           pa) %>% 
  mutate(Frequencia = ifelse(is.na(Frequencia),
                             0,
                             Frequencia))

## Cria intervalos para o grafico

pretty_breaks <- c(0, 2000,
                   4000, 6000,
                   22000)

## Transforma os intervalos em labels

labels <- c()
brks <- pretty_breaks

## Arredonda os valores encontrados 

for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]

## Cria uma nova variavel com os intervalos criados

numacoes_tjpa$breaks2 <- cut(numacoes_tjpa$Frequencia, 
                             breaks = brks, 
                             include.lowest = TRUE, 
                             labels = labels)

## Cria niveis com os intervalos criados acima

brks_scale <- levels(numacoes_tjpa$breaks2)

## Reverte a ordem destes niveis

labels_scale <- rev(brks_scale)

## Atribui uma escala de cores aos intervalos criados

cols <- c("2000" = "#b2e2e2",
          "4000" = "#66c2a4",
          "6000" = "#2ca25f",
          "22000" = "#006d2c")

## Plotando o gráfico

plot_numacoestjpa <- numacoes_tjpa %>% 
  ggplot(aes(fill = breaks2)) +
  geom_sf(mapping = aes(geometry = geom),
          show.legend = "polygon",
          colour = "white") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.margin = unit(c(1,1,1,1.2),"cm"),
        plot.title = element_text(hjust = 0.5, 
                                  family = "Cambria",
                                  size = 11,
                                  margin = unit(c(0, 0, 5, 0), "mm")),
        plot.caption = element_text(family = "Cambria",
                                    size = 11),
        plot.subtitle = element_text(hjust = 0.75, 
                                     color = "grey40",
                                     family = "Cambria",
                                     size = 11,
                                     face = "bold"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(family = "Cambria",
                                    face = "bold",
                                    size = 11),
        legend.text = element_text(family = "Cambria",
                                   size = 11)) +
  scale_fill_manual(
    values = cols,
    breaks = c("2000", "4000", "6000", "22000"),
    limits = c("2000", "4000", "6000", "22000"),
    name = "Número de ações",
    drop = FALSE,
    labels = c("2.000", "4.000", "6.000", "22.000"),
    guide = guide_legend(
      override.aes = list(color = NA),
      direction = "horizontal",
      keyheight = unit(3, units = "mm"),
      keywidth = unit(100/ length(labels), units = "mm"),
      title.position = 'left',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"))

plot_numacoestjpa

### 2.3.1. STF --------------------------------------------------------------

numacoes_stf <- stf %>% 
  group_by(NomeMunicipio) %>% 
  summarise(Frequencia = n())

### 2.3.2. STJ --------------------------------------------------------------


### 2.3.3. TJAM -------------------------------------------------------------


### 2.3.4. TJMT -------------------------------------------------------------


### 2.3.5. TJPA -------------------------------------------------------------

## Calculando quantas ações ambientais
## tem por município

numacoes_tjpa <- datajud %>% 
  filter(UF == "PA") %>% 
  group_by(UF,
           CodigoMunicipioIBGE, 
           NomeMunicipio) %>% 
  summarise(Frequencia = n()) %>% 
  arrange(desc(Frequencia)) %>% 
  head(10)

## Criando um banco com os municípios do Pará

pa <- municipios %>% 
  filter(abbrev_state == "PA")

## Juntando com os dados do IBGE

numacoes_tjpa <- left_join(numacoes_tjpa,
                           pa) %>% 
  mutate(Frequencia = ifelse(is.na(Frequencia),
                             0,
                             Frequencia))

## Cria intervalos para o grafico

pretty_breaks <- c(0, 2000,
                   4000, 6000,
                   22000)

## Transforma os intervalos em labels

labels <- c()
brks <- pretty_breaks

## Arredonda os valores encontrados 

for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]

## Cria uma nova variavel com os intervalos criados

numacoes_tjpa$breaks2 <- cut(numacoes_tjpa$Frequencia, 
                breaks = brks, 
                include.lowest = TRUE, 
                labels = labels)

## Cria niveis com os intervalos criados acima

brks_scale <- levels(numacoes_tjpa$breaks2)

## Reverte a ordem destes niveis

labels_scale <- rev(brks_scale)

## Atribui uma escala de cores aos intervalos criados

cols <- c("2000" = "#b2e2e2",
          "4000" = "#66c2a4",
          "6000" = "#2ca25f",
          "22000" = "#006d2c")

## Plotando o gráfico

plot_numacoestjpa <- numacoes_tjpa %>% 
  ggplot(aes(fill = breaks2)) +
  geom_sf(mapping = aes(geometry = geom),
          show.legend = "polygon",
          colour = "white") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.margin = unit(c(1,1,1,1.2),"cm"),
        plot.title = element_text(hjust = 0.5, 
                                  family = "Cambria",
                                  size = 11,
                                  margin = unit(c(0, 0, 5, 0), "mm")),
        plot.caption = element_text(family = "Cambria",
                                    size = 11),
        plot.subtitle = element_text(hjust = 0.75, 
                                     color = "grey40",
                                     family = "Cambria",
                                     size = 11,
                                     face = "bold"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(family = "Cambria",
                                    face = "bold",
                                    size = 11),
        legend.text = element_text(family = "Cambria",
                                   size = 11)) +
  scale_fill_manual(
    values = cols,
    breaks = c("2000", "4000", "6000", "22000"),
    limits = c("2000", "4000", "6000", "22000"),
    name = "Número de ações",
    drop = FALSE,
    labels = c("2.000", "4.000", "6.000", "22.000"),
    guide = guide_legend(
      override.aes = list(color = NA),
      direction = "horizontal",
      keyheight = unit(3, units = "mm"),
      keywidth = unit(100/ length(labels), units = "mm"),
      title.position = 'left',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"))
 
plot_numacoestjpa

## 2.4. Número de ações envolvendo terras públicas -------------------------


## 2.5. Número de ações com órgãos do Poder Executivo ----------------------


## 2.6. Média de tempo entre ajuizamento x julgamento ----------------------

## Calculando a média de tempo entre ajuizamento x julgamento

## Calcula a duração da investigação em dias

media_ajuzxjulg <- tjpa %>% 
  mutate(DIFF_DIAS = abs(difftime(DataJulgamento,
                                  DataAjuizamento,
                                  units = "days")))

## Gerando um boxplot da distribuição

## 2.7. Número de ações com responsabilização patrimonial ------------------



## 2.8. Classes Processuais ------------------------------------------------

numacoes_classes <- join %>% 
  group_by(ClasseProcessual) %>% 
  summarise(freq = n(),
            prop = round(freq/nrow(join), 4)) %>% 
  arrange(desc(freq)) %>% 
  head(10) %>% 
  mutate(ClasseProcessual = case_when(ClasseProcessual == "202 - AGRAVO DE INSTRUMENTO" ~ "Agravo de Instrumento",
                                      ClasseProcessual == "1032 - RECURSO ESPECIAL" ~ "Recurso Especial",
                                      ClasseProcessual == "417 - APELAÇÃO CRIMINAL" ~ "Apelação Criminal",
                                      ClasseProcessual == "120 - MANDADO DE SEGURANÇA CÍVEL" ~ "Mandado de Segurança Cível",
                                      ClasseProcessual == "1728 - APELAÇÃO / REMESSA NECESSÁRIA" ~ "Apelação/Remessa Necessária",
                                      ClasseProcessual == "283 - AÇÃO PENAL - PROCEDIMENTO ORDINÁRIO" ~ "Ação Penal - Procedimento Ordinário",
                                      ClasseProcessual == "65 - AÇÃO CIVIL PÚBLICA" ~ "Ação Civil Pública",
                                      ClasseProcessual == "11881 - AGRAVO EM RECURSO ESPECIAL" ~ "Agravo em Recurso Especial",
                                      ClasseProcessual == "7 - PROCEDIMENTO COMUM CÍVEL" ~ "Procedimento Comum Cível",
                                      ClasseProcessual == "198 - APELAÇÃO CÍVEL" ~ "Apelação Cível",
                                      ClasseProcessual == "199 - REMESSA NECESSÁRIA CÍVEL" ~ "Remessa Necessária Cível",
                                      TRUE ~ as.character(ClasseProcessual)))
  # mutate(ClasseProcessual = str_wrap(ClasseProcessual,
  #                                    23))

## Ordenando as categorias

ordem <- numacoes_classes$ClasseProcessual

numacoes_classes <- numacoes_classes %>% 
  mutate(ClasseProcessual = factor(ClasseProcessual,
                                   levels = rev(ordem)))

paleta <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
             "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")

## Gerando o gráfico

plot_numacoesclas <- numacoes_classes %>% 
  ggplot(aes(x = ClasseProcessual,
             y = prop,
             fill = ClasseProcessual)) + 
  geom_bar(stat = "identity") +
  geom_label(aes(label = paste0(prop * 100,
                                "%")), 
             hjust = -0.2, 
             vjust = 0.5,
    size = 6, 
    fontface = "bold", 
    family = "Arial",
    fill = "white", 
    color = "black",
    label.size = 0.3) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,0.50,0.10),
                     limits = c(0,0.53),
                     expand = c(0, 0)) +
  labs(x = "Classe Processual",
       y = "% de acórdãos") +
  coord_flip() +
  theme_minimal() +
    theme(legend.position = "none",
          plot.margin = unit(c(1.4,1.4,1.4,1.4),"cm"),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "grey90"),
          axis.ticks = element_line(colour = "grey90"),
          panel.border = element_blank(),
          axis.title = element_text(face = "bold",
                                    size = 25,
                                    family = "Arial"),
          axis.text = element_text(size = 23,
                                   family = "Arial",
                                   face = "bold",
                                   color = "Grey40"),
          axis.text.x = element_text(vjust = 0.5,
                                     margin = margin(t = 6,
                                                     b = 10)),
          axis.text.y = element_text(margin = margin(r = 6,
                                                     l = 10, 
                                                     b = 5))) + 
  scale_fill_manual(values = paleta)
  

plot_numacoesclas

## Salva o gráfico

ggsave(filename = "data/output/figura x_classes processuais.png",
       bg = "white",
       plot = plot_numacoesclas,
       width = 17,
       height = 10,
       dpi = 1000)


## 2.9. Tempo Médio --------------------------------------------------------


# V2 ----------------------------------------------------------------------

tempomedio <- join %>% 
  filter(Tribunal != "STF") %>% 
  filter(DataAjuizamento >= "1990-01-01") %>% 
  select(ID,
         Tribunal,
         Numero:ValorCausa) %>% 
  select(-DataPublicacao) %>% 
  unique() %>% 
  group_by(ID) %>% 
  slice_min(order_by = DataJulgamento) %>% 
  mutate(diff_dias = abs(difftime(DataJulgamento,
                                  DataAjuizamento,
                                  units = "days")))

paleta <- c(
  #"#8c510a", 
            "#dfc27d", "#80cdc1", "#01665e")

plot_tempmedio <- tempomedio2 %>% 
  mutate(diff_dias = round(as.numeric(diff_dias),0)) %>%
  ggplot(aes(x = Tribunal,
             y = diff_dias,
             fill = Tribunal)) +
  geom_boxplot(aes(fill = Tribunal)) +
  scale_x_discrete(expand = c(0.2, 0)) +
  scale_y_continuous(labels = function(x) format(x,
                                                  big.mark = ".",
                                                  decimal.mark = ",",
                                                  scientific = FALSE),
                     limits = c(0, 10660),
                     expand = c(0, 0)) +
  labs(x = "Tribunal",
       y = paste("Tempo decorrido entre a Data de Ajuizamento \ne a Data de Julgamento \n(em dias)")) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = unit(c(1.4,1.4,1.4,1.4),"cm"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey90"),
        axis.ticks = element_line(colour = "grey90"),
        panel.border = element_blank(),
        axis.title = element_text(face = "bold",
                                  size = 25,
                                  family = "Arial"),
        axis.text = element_text(size = 23,
                                 family = "Arial",
                                 face = "bold",
                                 color = "Grey40"),
        axis.text.x = element_text(vjust = 0.5,
                                   margin = margin(t = 6,
                                                   b = 10)),
        axis.text.y = element_text(margin = margin(r = 6,
                                                   l = 10))) + 
  scale_fill_manual(values = paleta)

plot_tempmedio

ggsave(filename = "data/output/figura x_tempo médio_v2.png",
       bg = "white",
       plot = plot_tempmedio,
       width = 17,
       height = 10,
       dpi = 1000)


# V3 ----------------------------------------------------------------------

tempomedio2 <- tempomedio %>% 
  group_by(Tribunal) %>% 
  summarise(mean = mean(diff_dias),
            sd = sd(diff_dias),
            max = max(diff_dias),
            min = min(diff_dias))

paleta <- c(
  #"#8c510a", 
  "#dfc27d", "#80cdc1", "#01665e")

plot_tempmedio <- tempomedio2 %>% 
  mutate(mean = round(as.numeric(mean),0)) %>% 
  ggplot(aes(x = Tribunal,
             y = mean,
             fill = Tribunal)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = paste0(mean,
                                " dias")), 
             hjust = -0.2, 
             vjust = 0.5,
             size = 6, 
             fontface = "bold", 
             family = "Arial",
             fill = "white", 
             color = "black",
             label.size = 0.3) +
  coord_flip() +
  scale_x_discrete(expand = c(0.2, 0)) +
  scale_y_continuous(labels = function(x) format(x,
                                                 big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE),
                     limits = c(0, 2000),
                     expand = c(0, 0)) +
  labs(x = "Tribunal",
       y = paste("Tempo médio decorrido entre a Data de Ajuizamento \ne a Data de Julgamento \n(em dias)")) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = unit(c(1.4,1.4,1.4,1.4),"cm"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey90"),
        axis.ticks = element_line(colour = "grey90"),
        panel.border = element_blank(),
        axis.title = element_text(face = "bold",
                                  size = 25,
                                  family = "Arial"),
        axis.text = element_text(size = 23,
                                 family = "Arial",
                                 face = "bold",
                                 color = "Grey40"),
        axis.text.x = element_text(vjust = 0.5,
                                   margin = margin(t = 6,
                                                   b = 10)),
        axis.text.y = element_text(margin = margin(r = 6,
                                                   l = 10))) + 
  scale_fill_manual(values = paleta)

plot_tempmedio

ggsave(filename = "data/output/figura x_tempo médio_v3.png",
       bg = "white",
       plot = plot_tempmedio,
       width = 17,
       height = 10,
       dpi = 1000)

# V4 ----------------------------------------------------------------------

plot_tempmedio <- tempomedio %>% 
  mutate(diff_dias = round(as.numeric(diff_dias),0)) %>% 
  ggplot(aes(x = diff_dias,
             fill = Tribunal)) +
  geom_histogram()+
  facet_wrap(~Tribunal)
  geom_label(aes(label = paste0(mean,
                                " dias")), 
             hjust = -0.2, 
             vjust = 0.5,
             size = 6, 
             fontface = "bold", 
             family = "Arial",
             fill = "white", 
             color = "black",
             label.size = 0.3) +
  coord_flip() +
  scale_x_discrete(expand = c(0.2, 0)) +
  scale_y_continuous(labels = function(x) format(x,
                                                 big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE),
                     limits = c(0, 2000),
                     expand = c(0, 0)) +
  labs(x = "Tribunal",
       y = paste("Tempo médio decorrido entre a Data de Ajuizamento \ne a Data de Julgamento \n(em dias)")) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = unit(c(1.4,1.4,1.4,1.4),"cm"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey90"),
        axis.ticks = element_line(colour = "grey90"),
        panel.border = element_blank(),
        axis.title = element_text(face = "bold",
                                  size = 25,
                                  family = "Arial"),
        axis.text = element_text(size = 23,
                                 family = "Arial",
                                 face = "bold",
                                 color = "Grey40"),
        axis.text.x = element_text(vjust = 0.5,
                                   margin = margin(t = 6,
                                                   b = 10)),
        axis.text.y = element_text(margin = margin(r = 6,
                                                   l = 10))) + 
  scale_fill_manual(values = paleta)

plot_tempmedio

ggsave(filename = "data/output/figura x_tempo médio_v3.png",
       bg = "white",
       plot = plot_tempmedio,
       width = 17,
       height = 10,
       dpi = 1000)

stat <- tempomedio %>% 
  mutate(diff_dias = round(as.numeric(diff_dias),0)) %>% 
  filter(DataAjuizamento >= "1990-01-01") %>% 
  filter(Tribunal == "TJAM")

quantile(stat$diff_dias, c(.25, .50,  .75, .),
         na.rm = TRUE)

summary(stat$diff_dias)



Summary<-boxplot(stat$diff_dias)$stats
colnames(Summary)<-c("x","y","z")
rownames(Summary)<-c("Min","First Quartile","Median","Third Quartile","Maximum")
Summary

table(stat$diff_dias > 2686)


## 2.10. Assuntos ----------------------------------------------------------

for(i in 1:nrow(join)){
  
  cat("Lendo", i, "\n")
  
  join$AssuntoPrincipal[i] <- str_split(join$Assunto[i],
                                  " / ")[[1]][1]

}

numacoes_assuntos <- join %>% 
  group_by(AssuntoPrincipal) %>% 
  summarise(freq = n(),
            prop = round(freq/nrow(join), 4)) %>% 
  arrange(desc(freq)) %>% 
  head(10) %>% 
  mutate(AssuntoPrincipal = case_when(AssuntoPrincipal == "10106 - RECURSOS MINERAIS" ~ "Recursos Minerais",
                                      AssuntoPrincipal == "10111 - REVOGAÇÃO/CONCESSÃO DE LICENÇA AMBIENTAL" ~ "Revogação/Concessão de Licença Ambiental",
                                      AssuntoPrincipal == "10112 - REVOGAÇÃO/ANULAÇÃO DE MULTA AMBIENTAL" ~ "Revogação/Anulação de Multa Ambiental",
                                      AssuntoPrincipal == "10433 - INDENIZAÇÃO POR DANO MORAL" ~ "Indenização por Dano Moral",
                                      AssuntoPrincipal == "10438 - DANO AMBIENTAL" ~ "Dano Ambiental",
                                      AssuntoPrincipal == "3618 - CRIMES CONTRA O MEIO AMBIENTE E O PATRIMÔNIO GENÉTICO" ~ "Crimes Contra o Meio Ambiente e o Patrimônio Genético",
                                      AssuntoPrincipal == "3620 - CRIMES CONTRA A FLORA" ~ "Crimes Contra a Flora",
                                      AssuntoPrincipal == "3621 - POLUIÇÃO" ~ "Poluição",
                                      AssuntoPrincipal == "8942 - EXTINÇÃO DO PROCESSO SEM RESOLUÇÃO DE MÉRITO" ~ "Extinção do Processo Sem Resolução de Mérito",
                                      AssuntoPrincipal == "9994 - INDENIZAÇÃO POR DANO AMBIENTAL" ~ "Indenização por Dano Ambiental",
                                      TRUE ~ as.character(AssuntoPrincipal))) %>% 
mutate(AssuntoPrincipal = str_wrap(AssuntoPrincipal,
                                   20))

## Ordenando as categorias

ordem <- numacoes_assuntos$AssuntoPrincipal

numacoes_assuntos <- numacoes_assuntos %>% 
  mutate(AssuntoPrincipal = factor(AssuntoPrincipal,
                                   levels = rev(ordem)))

paleta <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
            "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")

## Gerando o gráfico

plot_numacoesassunt <- numacoes_assuntos %>% 
  ggplot(aes(x = AssuntoPrincipal,
             y = prop,
             fill = AssuntoPrincipal)) + 
  geom_bar(stat = "identity") +
  geom_label(aes(label = paste0(prop * 100,
                                "%")), 
             hjust = -0.2, 
             vjust = 0.5,
             size = 6, 
             fontface = "bold", 
             family = "Arial",
             fill = "white", 
             color = "black",
             label.size = 0.3) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,0.20,0.05),
                     limits = c(0,0.21),
                     expand = c(0, 0)) +
  labs(x = "Assunto",
       y = "% de acórdãos") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = unit(c(1.4,1.4,1.4,1.4),"cm"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey90"),
        axis.ticks = element_line(colour = "grey90"),
        panel.border = element_blank(),
        axis.title = element_text(face = "bold",
                                  size = 25,
                                  family = "Arial"),
        axis.text = element_text(size = 23,
                                 family = "Arial",
                                 face = "bold",
                                 color = "Grey40"),
        axis.text.x = element_text(vjust = 0.5,
                                   margin = margin(t = 6,
                                                   b = 10)),
        axis.text.y = element_text(margin = margin(r = 6,
                                                   l = 10, 
                                                   b = 5))) + 
  scale_fill_manual(values = paleta)


plot_numacoesassunt

## Salva o gráfico

ggsave(filename = "data/output/figura x_assuntos.png",
       bg = "white",
       plot = plot_numacoesassunt,
       width = 19,
       height = 12,
       dpi = 1000)
