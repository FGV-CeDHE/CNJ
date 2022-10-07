
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

amazonia <- readxl::read_xls("data/input/SireneJud/lista_de_municipios_Amazonia_Legal_2020.xls")

## Carregando os dados do DataJud

datajud <- datajud <- readRDS("data/output/SireneJud/sirenejud_allvars_filt_v05092022.rds")

## Shape dos municípios

municipios <- geobr::read_municipality(year = 2020)

## 1.1. Limpeza ------------------------------------------------------------

## Organizando os dados dos municípios

municipios <- municipios %>% 
  filter(abbrev_state %in% amazonia$SIGLA) %>% 
  as.data.frame() %>% 
  select(name_muni) %>% 
  mutate(name_muni = str_to_upper(name_muni))

## Reestruturando os dados do DataJud

datajud <- datajud %>%
  filter(dt_inicio_situacao_novo >= "2020") %>% 
  select(numprocess,
         dt_inicio_situacao_novo,
         dt_inicio_situacao_julgado,
         dt_inicio_situacao_baixado,
         tempo_tramitacao,
         uf,
         municipio,
         tribunal,
         grau,
         cod_orgao,
         orgaojulgador,
         porte_tribunal_nome,
         esfera,
         classe,
         co_assunto,
         noassuntos,
         flg_julgamento,
         complex_assunto,
         partes_pa_desc,
         partes_pa_list,
         partes_at_desc,
         partes_at_list) %>% 
  mutate(co_assunto = str_replace_all(co_assunto,
                                      "\\[|]",
                                      ""),
         co_assunto = gsub(",.*",
                           "",
                           co_assunto),
         noassuntos = str_replace_all(noassuntos,
                                      "\\[|]",
                                      ""),
         noassuntos = gsub(",.*",
                           "",
                           noassuntos),
         across(everything(), 
                str_to_upper))

# 2. Gráficos -------------------------------------------------------------

## 2.1. Número de ações por tribunal ---------------------------------------

regex <- regex(paste0(" DE ",
                      municipios$name_muni,
                      collapse = "|"))

regex2 <- regex(paste0("",
                      municipios$name_muni,
                      collapse = "|"))

numacoes_tribunal <- datajud %>% 
  filter(grau == "G1") %>%
  mutate(orgaojulgador = gsub("VARA UNICA",
                               "VARA ÚNICA",
                               orgaojulgador)) %>% 
  group_by(
          tribunal,
           orgaojulgador
           #municipio
           ) %>% 
  summarise(frequencia = n()) %>% 
  arrange(desc(frequencia))

writexl::write_xlsx(numacoes_tribunal,
                    "datajud_orgãos julgadores_tribunal.xlsx")

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

numacoes_classes <- datajud %>% 
  mutate(classe = str_to_title(classe)) %>% 
  group_by(classe) %>% 
  summarise(freq = n(),
            prop = round(freq/nrow(datajud), 4)) %>% 
  arrange(desc(freq)) %>% 
  head(10) %>% 
  mutate(classe = case_when(classe == "Agravo De Instrumento" ~ "Agravo de Instrumento",
                                      classe == "Mandado De Segurança Cível" ~ "Mandado de Segurança Cível",
                                      TRUE ~ as.character(classe)))
# mutate(ClasseProcessual = str_wrap(ClasseProcessual,
#                                    23))

## Ordenando as categorias

ordem <- numacoes_classes$classe

numacoes_classes <- numacoes_classes %>% 
  mutate(classe = factor(classe,
                                   levels = rev(ordem)))

paleta <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
            "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")

## Gerando o gráfico

plot_numacoesclas <- numacoes_classes %>% 
  ggplot(aes(x = classe,
             y = prop,
             fill = classe)) + 
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
                     breaks = seq(0,0.40,0.10),
                     limits = c(0,0.42),
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

ggsave(filename = "data/output/figura 11_classes processuais.png",
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

numacoes_assuntos <- datajud %>% 
  mutate(noassuntos = str_to_title(noassuntos)) %>% 
  group_by(noassuntos) %>% 
  summarise(freq = n(),
            prop = round(freq/nrow(datajud), 4)) %>% 
  arrange(desc(freq)) %>% 
  head(10) %>% 
  mutate(noassuntos = case_when(noassuntos == "Revogação/Concessão De Licença Ambiental" ~ "Revogação/Concessão de Licença Ambiental",
                                      noassuntos == "Revogação/Anulação De Multa Ambiental" ~ "Revogação/Anulação de Multa Ambiental",
                                      noassuntos == "Crimes Contra O Meio Ambiente E O Patrimônio Genético" ~ "Crimes Contra o Meio Ambiente e o Patrimônio Genético",
                                      noassuntos == "Crimes Contra A Flora" ~ "Crimes Contra a Flora",
                                      noassuntos == "Crimes Contra A Fauna" ~ "Crimes Contra a Fauna",
                                      noassuntos == "Indenização Por Dano Ambiental" ~ "Indenização por Dano Ambiental",
                                      TRUE ~ as.character(noassuntos))) %>% 
  mutate(noassuntos = str_wrap(noassuntos,
                                     20))

## Ordenando as categorias

ordem <- numacoes_assuntos$noassuntos

numacoes_assuntos <- numacoes_assuntos %>% 
  mutate(noassuntos = factor(noassuntos,
                             levels = rev(ordem)))

paleta <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
            "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")

## Gerando o gráfico

plot_numacoesassunt <- numacoes_assuntos %>% 
  ggplot(aes(x = noassuntos,
             y = prop,
             fill = noassuntos)) + 
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
                     breaks = seq(0,0.38,0.05),
                     limits = c(0,0.38),
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

ggsave(filename = "data/output/figura 12_assuntos.png",
       bg = "white",
       plot = plot_numacoesassunt,
       width = 19,
       height = 12,
       dpi = 1000)


# 2.11. Movimentações -----------------------------------------------------

movimentos2 <- movimentos %>% 
  filter(nome_movimento != "NÃO INFORMADO" &
           sigla_grau == "G1") %>% 
  mutate(dt_inicio_situacao_novo = as.Date(dt_inicio_situacao_novo,
                                           format = "%Y-%m-%d"),
         dt_inicio_situacao = as.Date(dt_inicio_situacao,
                                      format = "%Y-%m-%d"),
         dt_fim_situacao = as.Date(dt_fim_situacao,
                                   format = "%Y-%m-%d")) %>%
  arrange(numprocess, dt_inicio_situacao) %>% 
  group_by(numprocess) %>% 
  mutate(temp = dt_inicio_situacao - lag(dt_inicio_situacao)) 

num_movimentos <- movimentos2 %>% 
  mutate(nome_movimento = str_to_title(nome_movimento)) %>% 
  group_by(nome_movimento) %>% 
  summarise(media = mean(temp, 
                         na.rm = T)) %>% 
  arrange(desc(media)) %>% 
  head(10) %>% 
  mutate(nome_movimento = case_when(nome_movimento == "Finalizada Tramitação Direta Entre Mp E Autoridade Policial" ~ 
                                      "Finalizada Tramitação Direta entre MP e Autoridade Policial",
                                    nome_movimento == "Suspensão Do Processo" ~ "Suspensão do Processo",
                                    nome_movimento == "Extinção Da Punibilidade" ~ "Extinção da Punibilidade",
                                    nome_movimento == "Comutação Da Pena" ~ "Comutação da Pena",
                                    nome_movimento == "Renúncia Ao Direito Pelo Autor" ~ "Renúncia ao Direito pelo Autor",
                                    TRUE ~ as.character(nome_movimento))) %>% 
  mutate(nome_movimento = str_wrap(nome_movimento,
                               25))

## Ordenando as categorias

ordem <- num_movimentos$nome_movimento

num_movimentos <- num_movimentos %>% 
  mutate(nome_movimento = factor(nome_movimento,
                             levels = rev(ordem)))

paleta <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
            "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")

## Gerando o gráfico

plot_nummovimentos <- num_movimentos %>% 
  ggplot(aes(x = nome_movimento,
             y = media,
             fill = nome_movimento)) + 
  geom_bar(stat = "identity") +
  geom_label(aes(label = paste0(round(media,0),
                                " dias")), 
             hjust = -0.2, 
             vjust = 0.5,
             size = 6, 
             fontface = "bold", 
             family = "Arial",
             fill = "white", 
             color = "black",
             label.size = 0.3) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,400,50),
                     limits = c(0,450),
                     expand = c(0, 0)) +
  labs(x = "Movimento",
       y = paste("Tempo médio decorrido até o movimento \n(em dias)")) +
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


plot_nummovimentos

## Salva o gráfico

ggsave(filename = "data/output/figura 13_movimentacoes.png",
       bg = "white",
       plot = plot_nummovimentos,
       width = 20,
       height = 12,
       dpi = 1000)

# 2.11.2. Movimentações por Grupo -----------------------------------------

referencia <- readxl::read_xlsx("data/output/SireneJud/média_fases processuais.xlsx")

referencia <- referencia %>% 
  filter(sigla_grau == "G1") %>% 
  mutate(movimento = factor(movimento,
                            levels = c("Início do Processo - Julgamento em 1ª instância",
                                       "Julgamento em 1ª instância - Julgamento em 2ª instância",
                                       "Julgamento em 1ª ou 2ª instância - Execução",
                                       "Execução - Fim do processo"))) %>% 
  group_by(tribunal) %>% 
  summarise(soma = sum(media_fase, na.rm = TRUE),
            media_fase = media_fase,
            movimento = movimento,
            value = soma + 270) %>% 
  mutate(soma = formatC(soma, format="f", 
                        big.mark=".", digits=0),
         soma = ifelse(!is.na(soma),
                       paste0(soma,
                              " dias"),
                       soma)) %>% 
  group_by(tribunal) %>% 
  mutate(soma = ifelse(movimento == "Execução - Fim do processo",
                       soma,
                       NA),
         value = ifelse(movimento == "Execução - Fim do processo",
                          value,
                          NA))

paleta <- c(
  "#8c510a", "#dfc27d", "#80cdc1", "#01665e")

## Gerando o gráfico

plot_nummovimentos <- referencia %>% 
  ggplot(aes(x = reorder(tribunal,
                       desc(tribunal)),
             y = media_fase,
             fill = reorder(movimento,
                            desc(movimento)))) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,3500,500),
                     limits = c(0,3980),
                     labels = function(x) format(x, big.mark = ".", 
                                                 scientific = FALSE),
                     expand = c(0, 0)) +
  labs(x = "Tribunal",
       y = paste("Tempo médio decorrido em cada fase \n(em dias)")) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top",
        legend.justification = "center",
        legend.title = element_text(face = "bold",
                                    size = 25,
                                    family = "Arial"),
        legend.text = element_text(size = 23,
                                   family = "Arial",
                                   face = "bold",
                                   color = "Grey40"),
        legend.box.margin=margin(7,7,7,7),
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
                                                   b = 10))) + 
  scale_fill_manual(values = paleta,
                    breaks = c("Início do Processo - Julgamento em 1ª instância",
                             "Julgamento em 1ª instância - Julgamento em 2ª instância",
                             "Julgamento em 1ª ou 2ª instância - Execução",
                             "Execução - Fim do processo")) +
  guides(fill = guide_legend(nrow = 2,
                             title = paste("Momento \nProcessual"),
                             keyheight = unit(4, units = "mm"),
                             keywidth = unit(20, units = "mm"),
                             title.position = 'left',
                             title.hjust = 0.5,
                             byrow = T)) +
  geom_label(aes(x = tribunal,
                 y = value,
                 label = soma),
             size = 9, 
             na.rm = T,
             fontface = "bold", 
             family = "Arial",
             fill = "white", 
             color = "black",
             label.size = 0.6)

plot_nummovimentos

## Salva o gráfico

ggsave(filename = "data/output/figura 14_fases processuais_g1.png",
       bg = "white",
       plot = plot_nummovimentos,
       width = 22,
       height = 14,
       dpi = 1000)
