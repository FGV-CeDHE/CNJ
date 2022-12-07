
## TÍTULO: ANÁLISE DESCRITIVA - P4
## DATA: 17/10/2022
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
library(abjutils)

## OBJETIVOS

#'         - Gerar gráficos e tabelas para a produção do relatório final
#'           do projeto.

## CONFIGURA O AMBIENTE

setwd("CNJ")

windowsFonts("Arial" = windowsFont("Arial"))

## FUNÇÕES

source("functions/encoder.R", 
       encoding = "UTF-8")

# 1. Data -----------------------------------------------------------------

## Lista de municípios da amazônia legal

amazonia <- readxl::read_xls("data/input/SireneJud/municípios_amazônia legal_2020.xls")

## Carregando os dados do DataJud

datajud <- datajud <- readRDS("data/output/DataJud/datajud_allvars_filt_v26102022.rds")

## Carregando a lista de infratores ambientais

ibama <- read_delim("data/input/Ibama/auto_infracao.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

## Shape dos municípios

municipios <- geobr::read_municipality(year = 2020)

## Movimentações

movimentos <- readRDS("data/output/DataJud/movimentações_v26102022.rds")

## Produtividade em unidades judiciária da AL

produtividade_ambt <- readRDS("data/output/Módulo de Produtividade/relação sentenças x juízes_unidade jud ambientais_05122022.rds")

produtividade <- readRDS("data/output/Módulo de Produtividade/relação sentenças x juízes_unidade jud gerais_05122022.rds")

## 1.1. Limpeza ------------------------------------------------------------

## Organizando os dados dos municípios

municipios <- municipios %>% 
  filter(abbrev_state %in% amazonia$SIGLA) %>% 
  as.data.frame() %>% 
  select(name_muni) %>% 
  mutate(name_muni = str_to_upper(name_muni))

## Reestruturando os dados do DataJud

datajud <- datajud %>%
  filter(ano_inicio_situacao_novo >= "2020") %>% 
  select(-lat,
         -lon) %>% 
  unique() %>% 
  mutate(classe = str_to_sentence(classe),
         descricao_01_classe = str_to_sentence(descricao_01_classe),
         assunto = str_to_sentence(assunto),
         descricao_01_assunto = str_to_sentence(descricao_01_assunto))

## Juntando os dados

datajud2 <- datajud %>% 
  select(numprocess:classe,
         partes_pa_desc:flg_julgamento) %>% 
  unique() %>% 
  mutate(classe = str_to_sentence(classe),
         descricao_01_classe = str_to_sentence(descricao_01_classe))
 

# 2. Gráficos e tabelas ---------------------------------------------------

## 2.1. Assuntos -----------------------------------------------------------

numacoes_assuntos <- datajud %>% 
  group_by(codigo_01_assunto,
           descricao_01_assunto,
           assunto) %>% 
  summarise(freq = n(),
            prop = round(freq/35514, 4)) %>% 
  arrange(desc(freq)) %>% 
  head(10) %>% 
  mutate(assunto = str_wrap(assunto,
                            23))
## Ordenando as categorias

ordem <- numacoes_assuntos$assunto

numacoes_assuntos <- numacoes_assuntos %>% 
  mutate(assunto = factor(assunto,
                          levels = rev(ordem)))

paleta <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
            "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")[c(2,9)]

## Gerando o gráfico

plot_numacoesassunt <- numacoes_assuntos %>% 
  ggplot(aes(x = assunto,
             y = prop,
             fill = descricao_01_assunto)) + 
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
                     breaks = seq(0,0.40,0.05),
                     limits = c(0,0.42),
                     expand = c(0, 0)) +
  labs(x = "Assunto",
       y = "% de acórdãos") +
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
                                                   b = 5))) + 
  scale_fill_manual(values = paleta) +
  guides(fill = guide_legend(nrow = 1,
                             title = paste("Área de abrangência"),
                             keyheight = unit(4, units = "mm"),
                             keywidth = unit(20, units = "mm"),
                             title.position = 'left',
                             title.hjust = 0.5,
                             byrow = T))


plot_numacoesassunt

## Salva o gráfico

ggsave(filename = "figures/figura 13_assuntos_P4.png",
       bg = "white",
       plot = plot_numacoesassunt,
       width = 19,
       height = 12,
       dpi = 1000)

### 2.1.1. Combinações ------------------------------------------------------

## Adequando as informações de assunto e família

datajud <- datajud %>% 
  mutate(assunto2 = paste0(cod_assunto,
                          " - ",
                          assunto),
         familia = paste0(codigo_01_assunto,
                          " - ",
                          descricao_01_assunto))

## Agregando os resultados em combinações

assunto_agg <- aggregate(cbind(assunto2, familia) ~ numprocess + grau,
                         data = datajud,
                         paste,
                         collapse = " | ")

## Retirando informações duplicadas

assunto_agg$familia <- as.list(strsplit(assunto_agg$familia,
                               " | ",
                               fixed = T))

for(i in 1:nrow(assunto_agg)){

  cat("Lendo", i, "\n")

  assunto_agg$familia[i] <- list(unique(unlist(assunto_agg$familia[i])))

}

## Preparando a coluna 

assunto_agg <- assunto_agg %>% 
  mutate(familia2 = gsub("^c\\(|\\)$", 
                         "", 
                         familia),
         familia2 = gsub("NA",
                        "",
                        familia2,
                        fixed = T),
         familia2 = gsub('" - ", ',
                         "",
                         familia2,
                         fixed = T),
         familia2 = gsub(" - , ",
                         "",
                         familia2,
                         fixed = T),
         familia2 = gsub(",  -",
                         "",
                         familia2,
                         fixed = T),
         across(everything(), ~ str_remove_all(., '"')))

## Verificando as principais combinações

numacoes_assuntos <- assunto_agg %>% 
  group_by(assunto2) %>% 
  summarise(freq = n(),
            prop = round(freq/35514, 4)) %>% 
  arrange(desc(freq)) %>% 
  head(10) %>% 
  mutate(assunto2 = str_wrap(assunto2,
                                 23))

## 2.2. Classes ------------------------------------------------------------

numacoes_classes <- datajud2 %>% 
  group_by(codigo_01_classe,
           descricao_01_classe,
           classe) %>% 
  summarise(freq = n(),
            prop = round(freq/35514, 4)) %>% 
  arrange(desc(freq)) %>% 
  head(10) %>% 
  mutate(classe = str_wrap(classe,
                           20)) 

## Ordenando as categorias

ordem <- numacoes_classes$classe

numacoes_classes <- numacoes_classes %>% 
  mutate(classe = factor(classe,
                         levels = rev(ordem)))

paleta <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
            "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")[c(2,9)]

## Gerando o gráfico

plot_numacoesclas <- numacoes_classes %>% 
  ggplot(aes(x = classe,
             y = prop,
             fill = descricao_01_classe)) + 
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
  labs(x = "Classe processual",
       y = "% de acórdãos") +
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
                                                   b = 5))) + 
  scale_fill_manual(values = paleta) +
  guides(fill = guide_legend(nrow = 1,
                             title = paste("Área de abrangência"),
                             keyheight = unit(4, units = "mm"),
                             keywidth = unit(20, units = "mm"),
                             title.position = 'left',
                             title.hjust = 0.5,
                             byrow = T))


plot_numacoesclas

## Salva o gráfico

ggsave(filename = "figures/figura 12_classes processuais_P4.png",
       bg = "white",
       plot = plot_numacoesclas,
       width = 19,
       height = 12,
       dpi = 1000)

## 2.3. Momentos processuais -----------------------------------------------

### 2.3.1. Gráfico de barras ------------------------------------------------

referencia <- readxl::read_xlsx("data/output/DataJud/média_fases processuais_conhecimento.xlsx")

referencia <- referencia %>% 
  filter(sigla_grau == "G1") %>%
  filter(tribunal != "TJAP") %>% 
  mutate(movimento = factor(movimento,
                            levels = c("Início do Processo - Execução"))) %>% 
  group_by(tribunal) %>% 
  summarise(soma = sum(media_fase, na.rm = TRUE),
            media_fase = media_fase,
            movimento = movimento,
            #value = soma + 360
            value = soma + 400
            ) %>% 
  mutate(soma = formatC(soma, format="f", 
                        big.mark=".", digits=0),
         soma = ifelse(!is.na(soma),
                       paste0(soma,
                              " dias"),
                       soma)) %>% 
  group_by(tribunal) 

paleta <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
            "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")

## Gerando o gráfico

plot_nummovimentos <- referencia %>% 
  ggplot(aes(x = reorder(tribunal,
                         desc(tribunal)),
             y = media_fase,
             fill = tribunal)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,6000,1000),
                     limits = c(0,6400),
                     labels = function(x) format(x, big.mark = ".", 
                                                 scientific = FALSE),
                     expand = c(0, 0)) +
  labs(x = "Tribunal",
       y = paste("Tempo médio decorrido entre o início do processo e a execução \n(em dias)")) +
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
                                                   b = 10))) + 
  scale_fill_manual(values = paleta) +
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

ggsave(filename = "figures/figura xx_fases processuais_g1_conhecimento_P4.png",
       bg = "white",
       plot = plot_nummovimentos,
       width = 19,
       height = 12,
       dpi = 1000)

### 2.3.2. Boxplot ----------------------------------------------------------

referencia <- readxl::read_xlsx("data/output/DataJud/momentos processuais_conhecimento.xlsx")

## Adequando os dados

referencia <- referencia %>% 
  filter(sigla_grau == "G1") %>% 
  filter(tribunal != "TJAP")

## Paleta de cores

paleta <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
            "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")

## Gerando o gráfico

plot_momentos <- referencia %>% 
  ggplot(aes(x = tribunal,
             y = tempo,
             fill = tribunal)) +
  geom_boxplot(aes(fill = tribunal)) +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(labels = function(x) format(x,
                                                 big.mark = ".",
                                                 decimal.mark = ",",
                                                 scientific = FALSE),
                     limits = c(0, 10100),
                     expand = c(0, 0)) +
  labs(x = "Tribunal",
       y = paste("Tempo decorrido entre o início do processo e a execução \n(em dias)")) +
  #coord_flip() +
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
                                                   b = 10))) + 
  scale_fill_manual(values = paleta)

plot_momentos

## Salva o gráfico

ggsave(filename = "figures/figura xx_boxplot_fases processuais_g1_conhecimento_P4.png",
       bg = "white",
       plot = plot_momentos,
       width = 19,
       height = 12,
       dpi = 1000)

## 2.4. Fases processuais --------------------------------------------------

referencia <- readxl::read_xlsx("data/output/DataJud/média_fases processuais.xlsx")

referencia <- referencia %>% 
  filter(sigla_grau == "G1") %>%
  filter(media_fase > 4) %>% 
  mutate(movimento = factor(movimento,
                            levels = c("Investigatória - Conhecimento",
                                       "Conhecimento - Execução",
                                       "Execução - Fim"))) %>% 
  group_by(tribunal) %>% 
  summarise(soma = sum(media_fase, na.rm = TRUE),
            media_fase = media_fase,
            movimento = movimento,
            #value = soma + 360
            value = soma + 175
  ) %>% 
  mutate(soma = formatC(soma, format="f", 
                        big.mark=".", digits=0),
         soma = ifelse(!is.na(soma),
                       paste0(soma,
                              " dias"),
                       soma)) %>% 
  group_by(tribunal) %>% 
  mutate(soma = ifelse(movimento == "Execução - Fim",
                       soma,
                       NA),
         value = ifelse(movimento == "Execução - Fim",
                        value,
                        NA),
         soma = ifelse(tribunal == "TJTO",
                       "571 dias",
                       soma),
         value = ifelse(tribunal == "TJTO",
                        746,
                        value))

paleta <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
            "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")[c(2,4,9)]

## Gerando o gráfico

plot_fases <- referencia %>% 
  ggplot(aes(x = reorder(tribunal,
                         desc(tribunal)),
             y = media_fase,
             fill = reorder(movimento,
                            desc(movimento)))) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,2000,500),
                     limits = c(0,2490),
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
        plot.margin = unit(c(1.4,3.5,1.4,1.4),"cm"),
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
                    breaks = c("Investigatória - Conhecimento",
                               "Conhecimento - Execução",
                               "Execução - Fim")) +
  guides(fill = guide_legend(nrow = 1,
                             title = paste("Fase processual"),
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

plot_fases

## Salva o gráfico

ggsave(filename = "figures/figura 41_fases processuais_g1_P4.png",
       bg = "white",
       plot = plot_fases,
       width = 19,
       height = 12,
       dpi = 1000)

## 2.5. Índice de Atendimento a Demanda (IAD) ------------------------------

## Carregando os dados do DataJud

datajud <- datajud <- readRDS("data/output/DataJud/datajud_allvars_filt_v26102022.rds")

## Calculando o número de casos novos por mês

casos_novos <- datajud %>% 
  filter(!classe %in% c("EXECUÇÃO CONTRA A FAZENDA PÚBLICA",
                        "EXECUÇÃO DA PENA",
                        "EXECUÇÃO DE ALIMENTOS INFÂNCIA E JUVENTUDE",
                        "EXECUÇÃO DE MEDIDAS ALTERNATIVAS NO JUÍZO COMUM",
                        "EXECUÇÃO DE MEDIDAS SÓCIO-EDUCATIVAS",
                        "EXECUÇÃO DE MULTA",
                        "EXECUÇÃO DE TERMO DE AJUSTE DE CONDUTA",
                        "EXECUÇÃO DE TÍTULO EXTRAJUDICIAL",
                        "EXECUÇÃO DE TÍTULO EXTRAJUDICIAL CONTRA A FAZENDA PÚBLICA",
                        "EXECUÇÃO DE TÍTULO JUDICIAL",
                        "EXECUÇÃO FISCAL",
                        "EXECUÇÃO PROVISÓRIA")) %>% 
  select(numprocess,
         uf,
         municipio,
         grau,
         tribunal,
         data_ajuizamento,
         dt_inicio_situacao_novo,
         ano_inicio_situacao_novo,
         dt_inicio_situacao_julgado,
         dt_inicio_situacao_baixado) %>% 
  unique() %>% 
  mutate(data_ajuizamento = as.Date(data_ajuizamento,
                                    format = "%d/%m/%Y"),
         dt_inicio_situacao_novo =  as.Date(dt_inicio_situacao_novo,
                                            format = "%Y-%m-%d"),
         dt_inicio_situacao_julgado = as.Date(dt_inicio_situacao_julgado,
                                              format = "%Y-%m-%d"),
         dt_inicio_situacao_baixado = as.Date(dt_inicio_situacao_baixado,
                                              format = "%Y-%m-%d")) %>% 
  filter(dt_inicio_situacao_novo > "2020-08-31" &
         dt_inicio_situacao_novo <= "2021-08-31") %>% 
  mutate(mes = lubridate::month(dt_inicio_situacao_novo),
         ano = lubridate::year(dt_inicio_situacao_novo),
         ano = ifelse(is.na(ano),
                      ano_inicio_situacao_novo,
                      ano)) %>% 
  group_by(
           tribunal,
           #grau,
           ano,
           mes) %>% 
  summarise(freq = n())

## Calculando o número de casos baixados por mês

casos_baixados <- datajud %>% 
  filter(!classe %in% c("EXECUÇÃO CONTRA A FAZENDA PÚBLICA",
                        "EXECUÇÃO DA PENA",
                        "EXECUÇÃO DE ALIMENTOS INFÂNCIA E JUVENTUDE",
                        "EXECUÇÃO DE MEDIDAS ALTERNATIVAS NO JUÍZO COMUM",
                        "EXECUÇÃO DE MEDIDAS SÓCIO-EDUCATIVAS",
                        "EXECUÇÃO DE MULTA",
                        "EXECUÇÃO DE TERMO DE AJUSTE DE CONDUTA",
                        "EXECUÇÃO DE TÍTULO EXTRAJUDICIAL",
                        "EXECUÇÃO DE TÍTULO EXTRAJUDICIAL CONTRA A FAZENDA PÚBLICA",
                        "EXECUÇÃO DE TÍTULO JUDICIAL",
                        "EXECUÇÃO FISCAL",
                        "EXECUÇÃO PROVISÓRIA")) %>% 
  select(numprocess,
         uf,
         municipio,
         grau,
         tribunal,
         data_ajuizamento,
         dt_inicio_situacao_novo,
         ano_inicio_situacao_novo,
         dt_inicio_situacao_julgado,
         dt_inicio_situacao_baixado) %>% 
  unique() %>% 
  mutate(data_ajuizamento = as.Date(data_ajuizamento,
                                    format = "%d/%m/%Y"),
         dt_inicio_situacao_novo =  as.Date(dt_inicio_situacao_novo,
                                            format = "%Y-%m-%d"),
         dt_inicio_situacao_julgado = as.Date(dt_inicio_situacao_julgado,
                                              format = "%Y-%m-%d"),
         dt_inicio_situacao_baixado = as.Date(dt_inicio_situacao_baixado,
                                              format = "%Y-%m-%d")) %>% 
  filter(dt_inicio_situacao_baixado > "2020-08-31" &
         dt_inicio_situacao_baixado <= "2021-08-31") %>% 
  mutate(mes = lubridate::month(dt_inicio_situacao_baixado),
         ano = lubridate::year(dt_inicio_situacao_baixado)) %>% 
  group_by(
            tribunal,
           # grau,
           ano,
           mes) %>% 
  summarise(freq_baixado = n())

## Juntando os dados

iad <- left_join(casos_novos,
                 casos_baixados)

## Calculando o iad por grau

iad_grau <- iad %>% 
  mutate(ano = 2020) %>% 
  group_by(
           ano,
           tribunal,
           #grau
           ) %>% 
  summarise(freq = sum(freq,
                       na.rm = T),
            freq_baixado = sum(freq_baixado,
                               na.rm = T),
            indice = round((freq_baixado/freq),4) * 100) 

## Índice de Atendimento a Demanda (IAD)

mean(iad_grau$indice)

## 2.6. Início do processo - Primeiro Julgamento ---------------------------

## Carregando a base de movimentações

movimentos <- readRDS("data/output/DataJud/movimentações_v18102022.rds")

## Início do processo

inicio <- movimentos %>%  
  unique() %>% 
  filter(id_situacao %in% c(9, 24, 26, 61, 65, 81, 91)) %>% 
  mutate(dt_inicio_situacao = as.Date(dt_inicio_situacao,
                                      format = "%Y-%m-%d"),
         dt_fim_situacao = as.Date(dt_fim_situacao,
                                   format = "%Y-%m-%d")) %>%
  group_by(numprocess,
           sigla_grau) %>% 
  slice_min(order_by = dt_inicio_situacao) %>% 
  select(numprocess,
         tribunal,
         sigla_grau,
         dt_inicio_situacao,
         dt_fim_situacao) %>%
  mutate(situacao = "INÍCIO") %>% 
  unique()

## Verificando se os resultados estão consistentes

robusto <- inicio %>% 
  group_by(numprocess, 
           sigla_grau) %>% 
  summarise(freq = n())

## Primeiro Julgamento

julgamento <- movimentos %>%  
  unique() %>% 
  filter(id_situacao %in% c(18, 27,28,29, 62, 72, 90, 129)) %>% 
  mutate(dt_inicio_situacao = as.Date(dt_inicio_situacao,
                                      format = "%Y-%m-%d"),
         dt_fim_situacao = as.Date(dt_fim_situacao,
                                   format = "%Y-%m-%d")) %>%
  filter(dt_inicio_situacao > "2021-07-31" &
         dt_inicio_situacao <= "2022-07-31") %>% 
  group_by(numprocess,
           sigla_grau) %>% 
  slice_min(order_by = dt_inicio_situacao) %>% 
  select(numprocess,
         tribunal,
         sigla_grau,
         dt_inicio_situacao,
         dt_fim_situacao) %>%
  mutate(situacao = "JULGAMENTO") %>% 
  unique()

## Verificando se os resultados estão consistentes

robusto <- julgamento %>% 
  group_by(numprocess, 
           sigla_grau) %>% 
  summarise(freq = n())

## Empilhando as duas fases

fases <- rbind(inicio,
               julgamento) %>% 
  arrange(sigla_grau, 
          dt_inicio_situacao)

## Calculando o tempo entre a distribuição
## e o ínicio da execução

fases <- fases %>% 
  mutate(situacao = factor(situacao,
                           levels = c("INÍCIO",
                                      "JULGAMENTO"))) %>% 
  arrange(numprocess,
          sigla_grau,
          situacao) %>% 
  group_by(numprocess,
           sigla_grau) %>% 
  mutate(tempo = dt_inicio_situacao - lag(dt_inicio_situacao)) 

## Removendo as movimentações com data equivocada

fases2 <- fases %>% 
  filter(tempo >= 0 |
         is.na(tempo))

## Calculando o tempo médio para cada um dos tribunais

media_fases <- fases2 %>%
  group_by(sigla_grau,
           tribunal) %>% 
  summarise(media_fase = mean(tempo,
                              na.rm = T)) %>% 
  filter(!is.na(media_fase))

## Média para cada grau

media_fases_grau <- media_fases %>% 
  group_by(sigla_grau) %>% 
  summarise(media_fase = mean(media_fase,
                              na.rm = T))

## 2.7. Partes -------------------------------------------------------------

### 2.7.1. Ministério Público | Polo Ativo ----------------------------------

poloat <- datajud %>% 
  filter(ano_inicio_situacao_novo == "2022" 
         &
         tribunal == "TRF1"
         ) %>% 
  filter(classe == "Ação civil pública") %>% 
  select(numprocess,
         grau,
         nome_at) %>% 
  unique() %>% 
  mutate(nome_at = encoder(nome_at),
         nome_at = str_to_upper(rm_accent(nome_at)),
         nome_at = stripWhitespace(trimws(str_squish(nome_at)))) %>%
  filter(grepl("MINISTERIO PUBLICO",
               nome_at,
               fixed = TRUE)) %>% 
  group_by(nome_at) %>% 
  summarise(freq = n(),
            prop = round(freq/12, 4)) %>% 
  arrange(desc(freq))

sum(poloat$prop)

### 2.7.2. Amicus Curiae ----------------------------------------------------

poload <- datajud %>% 
  filter(ano_inicio_situacao_novo == "2021"
         # &
         # tribunal == "TRF1"
         ) %>% 
  filter(classe == "Ação civil pública") %>% 
  select(numprocess,
         grau,
         tribunal,
         partes_ad_desc:tipoPessoa_ad) %>% 
  unique() %>% 
  filter(!is.na(partes_ad_desc)) %>% 
  mutate(nome_ad = encoder(nome_ad),
         nome_ad = str_to_upper(rm_accent(nome_ad)),
         nome_ad = stripWhitespace(trimws(str_squish(nome_ad))))

## Calculando a proporção

round(nrow(poload)/1438,4) * 100

## 2.8. Gargalos Processuais -----------------------------------------------

## Preparando os dados

gargalos <- movimentos %>% 
  filter(!nome_situacao %in% c("PENDENTE",
                               "TRAMITANDO")) %>%
  filter(nome_movimento != "NÃO INFORMADO" &
         sigla_grau == "G1") %>% 
  mutate(nome_movimento = str_to_sentence(nome_movimento)) %>% 
  mutate(dt_inicio_situacao_novo = as.Date(dt_inicio_situacao_novo,
                                           format = "%Y-%m-%d"),
         dt_inicio_situacao = as.Date(dt_inicio_situacao,
                                      format = "%Y-%m-%d"),
         dt_fim_situacao = as.Date(dt_fim_situacao,
                                   format = "%Y-%m-%d")) %>%
  arrange(numprocess, 
          dt_inicio_situacao) 

## Verificando quantos processos distintos existem

num <- unique(gargalos$numprocess)

## Cria um banco onde os dados serão armazenados

gargalos2 <- list()

## For loop que agrega os movimentos em pares

for(i in seq_along(num)){
  
  cat("Lendo", i, "\n")
  
  temp <- gargalos %>% 
    dplyr::filter(numprocess == num[i]) %>% 
    dplyr::mutate(movimento = NA)
  
  if(nrow(temp) > 1){
    
    for(j in 2:nrow(temp)){
      
      j2 <- j-1
      
      temp$movimento[j] <- paste0(temp$nome_movimento[j2],
                                  " - ",
                                  temp$nome_movimento[j])
      
    }
  }
  
  gargalos2 <- rbind.fill(gargalos2,
                          temp)
}

## Calculando o tempo entre movimentos

gargalos3 <- gargalos2 %>% 
  dplyr::filter(!is.na(movimento)) %>% 
  dplyr::group_by(numprocess) %>% 
  dplyr::mutate(temp = dt_inicio_situacao - lag(dt_inicio_situacao)) %>% 
  dplyr::group_by(movimento) %>% 
  dplyr::summarise(media = mean(temp, 
                         na.rm = T)) %>% 
  dplyr::arrange(desc(media)) %>% 
  dplyr::mutate(movimento = str_wrap(movimento,
                              25)) %>% 
  head(10)

## Ordenando as categorias

ordem <- gargalos3$movimento

gargalos3 <- gargalos3 %>% 
  mutate(movimento = factor(movimento,
                            levels = rev(ordem)))

## Paleta de cores

paleta <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
            "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")

## Gerando o gráfico

plot_gargalos <- gargalos3 %>% 
  ggplot(aes(x = movimento,
             y = media,
             fill = movimento)) + 
  geom_bar(stat = "identity") +
  geom_label(aes(label = paste0(format(round(as.numeric(media), 0), 
                                       big.mark=".") ,
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
  scale_y_continuous(breaks = seq(0,2000,500),
                     limits = c(0,2400),
                     expand = c(0, 0)) +
  labs(x = "Movimento",
       y = paste("Tempo médio decorrido entre os movimentos \n(em dias)")) +
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


plot_gargalos

## Salva o gráfico

ggsave(filename = "figures/figura xx_panorama movimentacoes.png",
       bg = "white",
       plot = plot_gargalos,
       width = 20,
       height = 12,
       dpi = 1000)

## 2.9. Infratores ambientais ----------------------------------------------

## Deixando somente os nomes com cpf/cnj completos

ibama <- ibama %>% 
  mutate(DAT_HORA_AUTO_INFRACAO = as.Date(DAT_HORA_AUTO_INFRACAO,
                                          format = "%d/%m/%Y")) %>% 
  filter(!grepl("\\*",
                CPF_CNPJ_INFRATOR)) %>% 
  mutate(NOME_INFRATOR = str_to_upper(NOME_INFRATOR))

### 2.9.1. Ajuizados x Infrações Ambientais ---------------------------------

## Criando um banco com os nomes dos ajuizados em processos ambientais

ajuizados_pa <- datajud2 %>% 
  select(numprocess,
         grau,
         numeroDocumentoPrincipal_pa) %>% 
  unique() %>% 
  group_by(numeroDocumentoPrincipal_pa) %>% 
  summarise(numprocessos = n()) %>% 
  filter(!is.na(numeroDocumentoPrincipal_pa) &
         numeroDocumentoPrincipal_pa != "" &
         !numeroDocumentoPrincipal_pa %in% c("00000000000",
                                            "00000000000000",
                                            "NULL")) %>% 
  mutate(numeroDocumentoPrincipal_pa = str_pad(numeroDocumentoPrincipal_pa,
                                               width = 14,
                                               side = "left",
                                               pad = "0"),
         numeroDocumentoPrincipal_pa = ifelse(numeroDocumentoPrincipal_pa == "CNPJ N 35674021000177",
                                              "35674021000177",
                                              numeroDocumentoPrincipal_pa)) %>% 
  rename("CPF_CNPJ_INFRATOR" = "numeroDocumentoPrincipal_pa")

ajuizados_at <- datajud2 %>% 
  select(numprocess,
         grau,
         numeroDocumentoPrincipal_at) %>% 
  unique() %>% 
  group_by(numeroDocumentoPrincipal_at) %>% 
  summarise(numprocessos = n()) %>% 
  filter(!is.na(numeroDocumentoPrincipal_at) &
           numeroDocumentoPrincipal_at != "" &
           !numeroDocumentoPrincipal_at %in% c("00000000000",
                                               "00000000000000",
                                               "NULL")) %>% 
  mutate(numeroDocumentoPrincipal_at = str_pad(numeroDocumentoPrincipal_at,
                                               width = 14,
                                               side = "left",
                                               pad = "0")) %>% 
  rename("CPF_CNPJ_INFRATOR" = "numeroDocumentoPrincipal_at")

## Empilhando os dois bancos

ajuizados <- rbind(ajuizados_pa,
                   ajuizados_at) %>% 
  arrange(numprocessos)

## Filtrando somente as pessoas compatíveis entre as bases

infracoes <- ibama %>% 
  mutate(DAT_HORA_AUTO_INFRACAO = as.Date(DAT_HORA_AUTO_INFRACAO,
                                          format = "%d/%m/%Y")) %>% 
  filter(CPF_CNPJ_INFRATOR != "00000000000000") %>% 
  filter(CPF_CNPJ_INFRATOR %in% unique(ajuizados$CPF_CNPJ_INFRATOR)) 

## Juntando os dois bancos

infracoes <- left_join(infracoes,
                       ajuizados) %>% 
  group_by(CPF_CNPJ_INFRATOR,
           numprocessos) %>% 
  summarise(numinfracoes = n()) %>% 
  mutate(grupo = ifelse(numinfracoes <= 5,
                        "Até 05 infrações ambientais",
                        ifelse(numinfracoes > 5 & 
                               numinfracoes <= 30,
                               "Entre 05 e 30 infrações ambientais",
                               ifelse(numinfracoes > 30,
                                      "Mais de 30 infrações ambientais",
                                      NA)))) %>% 
  group_by(grupo) %>% 
  summarise(numinfratores = n(),
            value = ifelse(grupo == "Até 05 infrações ambientais",
                           1548,
                           ifelse(grupo == "Entre 05 e 30 infrações ambientais",
                                  608,
                                  79))) %>% 
  unique() %>% 
  mutate(label = formatC(numinfratores, format="f", 
                                big.mark=".", 
                         digits = 0)) %>% 
  mutate(grupo = str_wrap(grupo,
                            20))

## Paleta de cores

paleta <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
            "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")[c(2,4,9)]

## Gera o gráfico

num_infracoes <- infracoes %>% 
  ggplot(aes(x = reorder(grupo,
                         desc(grupo)),
             y = numinfratores,
             fill = reorder(grupo,
                            desc(grupo)))) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,1500,500),
                     limits = c(0,1700),
                     labels = function(x) format(x, big.mark = ".", 
                                                 scientific = FALSE),
                     expand = c(0, 0)) +
  labs(x = "Número de infrações ambientais",
       y = "Número de ajuizados em ações ambientais") +
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
                                                   b = 10))) + 
  scale_fill_manual(values = paleta) +
  geom_label(aes(x = grupo,
                 y = value,
                 label = label),
             size = 9, 
             na.rm = T,
             fontface = "bold", 
             family = "Arial",
             fill = "white", 
             color = "black",
             label.size = 0.6)

num_infracoes

ggsave(filename = "figures/figura 21_infracoes ambientais.png",
       bg = "white",
       plot = num_infracoes,
       width = 19,
       height = 12,
       dpi = 1000)

### 2.9.2. Tipos de infração ------------------------------------------------

tiposinfr <- ibama %>% 
  filter(CPF_CNPJ_INFRATOR != "00000000000000") %>% 
  filter(CPF_CNPJ_INFRATOR %in% unique(ajuizados$CPF_CNPJ_INFRATOR)) %>% 
  group_by(TIPO_INFRACAO) %>% 
  summarise(freq = n(),
            prop = round(freq/8669,4) * 100) %>% 
  arrange(desc(freq))

## 2.10. Procedência -------------------------------------------------------

## Verificando em quantos processos o Ministério Público
## aparece como polo ativo

ministpub <- datajud2 %>% 
  select(numprocess:classe,
         partes_at_desc:numeroDocumentoPrincipal_at) %>% 
  unique() %>% 
  mutate(nome_at = encoder(nome_at),
         nome_at = str_to_upper(rm_accent(nome_at)),
         nome_at = stripWhitespace(trimws(str_squish(nome_at)))) %>%
  filter(grepl("MINISTERIO PUBLICO",
               nome_at,
               fixed = TRUE)) %>% 
  select(numprocess,
         tribunal,
         grau) %>% 
  unique() %>% 
  mutate(MP = 1)

## Verificando quantos resultados foram encontrados 
## por tribunal e grau

tribunal <- ministpub %>% 
  group_by(tribunal,
           grau) %>% 
  summarise(total = n())
  
## Juntando os dados e filtrando somente
## os movimentos de interesse

movimentos <- left_join(movimentos,
                        ministpub) %>% 
  filter(MP == 1) %>% 
  filter(nome_situacao %in% c("JULGADO COM RESOLUÇÃO DO MÉRITO",
                              "JULGADO SEM RESOLUÇÃO DO MÉRITO")) %>% 
  filter(nome_movimento %in% c("PROCEDÊNCIA",
                               "IMPROCEDÊNCIA",
                               "PROCEDÊNCIA EM PARTE",
                               "PROVIMENTO",
                               "NÃO-PROVIMENTO",
                               "PROVIMENTO EM PARTE")) %>% 
  mutate(dt_inicio_situacao = as.Date(dt_inicio_situacao,
                                      format = "%Y-%m-%d"),
         dt_fim_situacao = as.Date(dt_fim_situacao,
                                   format = "%Y-%m-%d")) %>%
  group_by(numprocess,
           sigla_grau) %>% 
  slice_min(order_by = dt_inicio_situacao) %>% 
  select(numprocess,
         tribunal,
         sigla_grau,
         dt_inicio_situacao,
         nome_situacao,
         nome_movimento) %>% 
  unique()

## Agrupando os resultados por tribunal

ministpub <- movimentos %>% 
  group_by(tribunal,
           sigla_grau,
           nome_movimento) %>% 
  summarise(freq = n()) %>% 
  rename("grau" = "sigla_grau")

## Juntando com os totais de cada tribunal

ministpub <- left_join(ministpub,
                       tribunal) %>% 
  group_by(tribunal,
           grau,
           nome_movimento) %>% 
  summarise(prop = round(freq/total,4) * 100)

## 2.11. Módulo de Produtividade -------------------------------------------

## Criando a paleta de referência

paleta <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
            "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")

### 2.11.1. Ações ambientais ------------------------------------------------

## Gera o gráfico

plot_magistrsent_ambt <- produtividade_ambt %>% 
  ggplot(aes(x = TotalJuizes,
             y = TotalSentencas)) + 
  geom_point(aes(color = SiglaTribunal),
             size = 5) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,60,10),
                     limits = c(0,61),) +
  scale_y_continuous(breaks = seq(0,2000,500),
                     limits = c(0,2050),
                     labels = function(x) format(x, big.mark = ".", 
                                                 scientific = FALSE),
                     expand = c(0, 0)) +
  labs(x = "Total de juízes",
       y = paste("Total de sentenças")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_text(family="Arial",
                                    size = 23,
                                    face = "bold"),
        legend.text = element_text(size = 23,
                                   family = "Arial"),
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
  scale_color_manual(values = paleta) +
  guides(color = guide_legend(title = "Tribunal",
                              byrow = TRUE,
                              nrow = 2,
                              override.aes = list(
                                size = c(rep(5,10)))))

plot_magistrsent_ambt

## Salva o gráfico

ggsave(filename = "figures/figura 49_sentenças_juizes_unidades jud ambientais_P4.png",
       bg = "white",
       plot = plot_magistrsent_ambt,
       width = 15,
       height = 9,
       dpi = 1000)

### 2.11.2. Ações gerais ----------------------------------------------------

## Gera o gráfico

plot_magistrsent <- produtividade %>% 
  ggplot(aes(x = TotalJuizes,
             y = TotalSentencas,
             color = "#f6cfc3")) + 
  geom_point(size = 3) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,140,30),
                     limits = c(0,142),) +
  scale_y_continuous(breaks = seq(0,2000,500),
                     limits = c(0,2050),
                     labels = function(x) format(x, big.mark = ".", 
                                                 scientific = FALSE),
                     expand = c(0, 0)) +
  labs(x = "Total de juízes",
       y = paste("Total de sentenças")) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_text(family="Arial",
                                    size = 23,
                                    face = "bold"),
        legend.text = element_text(size = 23,
                                   family = "Arial"),
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
                                                   b = 10))) 

plot_magistrsent

## Salva o gráfico

ggsave(filename = "figures/figura xx_sentenças_juizes_unidades jud gerais_P4.png",
       bg = "white",
       plot = plot_magistrsent,
       width = 15,
       height = 9,
       dpi = 1000)

### 2.11.3. Competência ambiental x Competência não-ambiental ---------------

## Gerando o gráfico

plot_competencias <- magistrados_ambt %>% 
  ggplot(aes(x  = TotalJuizes,
             y = TotalSentencas)) + 
  geom_point(aes(color = SiglaTribunal),
             size = 5) +
  facet_wrap(~Tipo) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,60,10),
                     limits = c(0,61),) +
  scale_y_continuous(breaks = seq(0,2000,500),
                     limits = c(0,2050),
                     labels = function(x) format(x, big.mark = ".", 
                                                 scientific = FALSE),
                     expand = c(0, 0)) +
  labs(x = "Total de juízes",
       y = paste("Total de sentenças")) +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_text(family="Arial",
                                    size = 23,
                                    face = "bold"),
        legend.text = element_text(size = 23,
                                   family = "Arial"),
        strip.text = element_text(size = 23,
                                  margin = margin(),
                                  face = "bold",
                                  family = "Arial"),
        panel.spacing = unit(3, "lines"),
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
  scale_color_manual(values = paleta) +
  guides(color = guide_legend(title = "Tribunal",
                              byrow = TRUE,
                              nrow = 2,
                              override.aes = list(
                                size = c(rep(5,10)))))

plot_competencias

## Salva o gráfico

ggsave(filename = "figures/figura 50_competencias_unid jud_P4.png",
       bg = "white",
       plot = plot_competencias,
       width = 19,
       height = 9,
       dpi = 1000)
