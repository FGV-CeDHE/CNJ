
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
  filter(ano_inicio_situacao_novo >= "2020") %>% 
  select(-lat,
         -lon) %>% 
  unique()

## Juntando os dados

datajud2 <- datajud %>% 
  select(numprocess,
         ano_inicio_situacao_novo,
         ano:classe,
         flg_julgamento) %>% 
  unique() %>% 
  mutate(classe = str_to_sentence(classe))
 
# 2. Gráficos -------------------------------------------------------------

## 2.1. Assuntos -----------------------------------------------------------

numacoes_assuntos <- datajud %>% 
  group_by(codigo_01,
           descricao_01,
           descricao_06) %>% 
  summarise(freq = n(),
            prop = round(freq/34986, 4)) %>% 
  arrange(desc(freq)) %>% 
  head(10) %>% 
  mutate(descricao_06 = str_wrap(descricao_06,
                               23))
## Ordenando as categorias

ordem <- numacoes_assuntos$descricao_06

numacoes_assuntos <- numacoes_assuntos %>% 
  mutate(descricao_06 = factor(descricao_06,
                               levels = rev(ordem)))

paleta <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
            "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")[c(2,9)]

## Gerando o gráfico

plot_numacoesassunt <- numacoes_assuntos %>% 
  ggplot(aes(x = descricao_06,
             y = prop,
             fill = descricao_01)) + 
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

ggsave(filename = "figures/figura 12_assuntos_P4.png",
       bg = "white",
       plot = plot_numacoesassunt,
       width = 19,
       height = 12,
       dpi = 1000)

### 2.1.1. Combinações ------------------------------------------------------

## Adequando as informações de assunto e família

datajud <- datajud %>% 
  mutate(assunto = paste0(co_assunto_v2,
                          " - ",
                          descricao_06),
         familia = paste0(codigo_01,
                          " - ",
                          descricao_01))

## Agregando os resultados em combinações

assunto_agg <- aggregate(cbind(assunto, familia) ~ numprocess + grau,
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
  group_by(assunto) %>% 
  summarise(freq = n(),
            prop = round(freq/34986, 4)) %>% 
  arrange(desc(freq)) %>% 
  head(10) %>% 
  mutate(descricao_06 = str_wrap(descricao_06,
                                 23))


## 2.2. Classes ------------------------------------------------------------

numacoes_classes <- datajud2 %>% 
  group_by(codigo_01_classe,
           descricao_01_classe,
           classe) %>% 
  summarise(freq = n(),
            prop = round(freq/34986, 4)) %>% 
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

ggsave(filename = "figures/figura 11_classes processuais_P4.png",
       bg = "white",
       plot = plot_numacoesclas,
       width = 19,
       height = 12,
       dpi = 1000)

## 2.3. Momentos processuais -----------------------------------------------

### 2.3.1. Gráfico de barras ------------------------------------------------

referencia <- readxl::read_xlsx("data/output/SireneJud/média_fases processuais_conhecimento.xlsx")

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

ggsave(filename = "figures/figura 14_fases processuais_g1_conhecimento_P4.png",
       bg = "white",
       plot = plot_nummovimentos,
       width = 19,
       height = 12,
       dpi = 1000)

### 2.3.2. Boxplot ----------------------------------------------------------

referencia <- readxl::read_xlsx("data/output/SireneJud/momentos processuais_sem filtro.xlsx")

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

ggsave(filename = "figures/figura 14_boxplot_fases processuais_g1_sem filtro_P4.png",
       bg = "white",
       plot = plot_momentos,
       width = 19,
       height = 12,
       dpi = 1000)

## 2.4. Fases processuais --------------------------------------------------

referencia <- readxl::read_xlsx("data/output/SireneJud/média_fases processuais.xlsx")

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
                       "326 dias",
                       soma),
         value = ifelse(tribunal == "TJTO",
                        500,
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
                     limits = c(0,2400),
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

ggsave(filename = "figures/figura 14_fases processuais_g1_P4.png",
       bg = "white",
       plot = plot_fases,
       width = 19,
       height = 12,
       dpi = 1000)

## 2.5. Índice de Atendimento a Demanda (IAD) ------------------------------

## Carregando os dados do DataJud

datajud <- datajud <- readRDS("data/output/SireneJud/sirenejud_allvars_filt_v18102022.rds")

## Calculando o número de casos novos por mês

casos_novos <- datajud %>% 
  select(numprocess,
         uf,
         municipio,
         grau,
         tribunal,
         data_ajuizamento,
         dt_inicio_situacao_novo,
         dt_inicio_situacao_novo_v2,
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
  filter(dt_inicio_situacao_novo > "2021-07-31" &
         dt_inicio_situacao_novo <= "2022-07-31") %>% 
  mutate(mes = lubridate::month(dt_inicio_situacao_novo),
         ano = lubridate::year(dt_inicio_situacao_novo),
         ano = ifelse(is.na(ano),
                      dt_inicio_situacao_novo_v2,
                      ano)) %>% 
  group_by(grau,
           ano,
           mes) %>% 
  summarise(freq = n())

## Calculando o número de casos baixados por mês

casos_baixados <- datajud %>% 
  select(numprocess,
         uf,
         municipio,
         grau,
         tribunal,
         data_ajuizamento,
         dt_inicio_situacao_novo,
         dt_inicio_situacao_novo_v2,
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
  filter(dt_inicio_situacao_baixado > "2021-07-31" &
         dt_inicio_situacao_baixado <= "2022-07-31") %>% 
  mutate(mes = lubridate::month(dt_inicio_situacao_baixado),
         ano = lubridate::year(dt_inicio_situacao_baixado)) %>% 
  group_by(grau,
           ano,
           mes) %>% 
  summarise(freq_baixado = n())

## Juntando os dados

iad <- left_join(casos_novos,
                 casos_baixados)

## Calculando o iad por grau

iad_grau <- iad %>% 
  group_by(grau) %>% 
  summarise(freq = sum(freq,
                       na.rm = T),
            freq_baixado = sum(freq_baixado,
                               na.rm = T),
            indice = round((freq/freq_baixado),4) * 100) 

## Índice de Atendimento a Demanda (IAD)

mean(iad_grau$indice)


## 2.6. Início do processo - Primeiro Julgamento ---------------------------

## Carregando a base de movimentações

movimentos <- readRDS("data/output/SireneJud/movimentações_v18102022.rds")

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
  filter(dt_inicio_situacao_novo_v2 >= "2020") %>% 
  filter(classe == "AÇÃO CIVIL PÚBLICA") %>% 
  select(numprocess,
         grau,
         partes_at_desc:tipoPessoa_at) %>% 
  unique() %>% 
  mutate(nome_at = encoder(nome_at),
         nome_at = str_to_upper(rm_accent(nome_at)),
         nome_at = stripWhitespace(trimws(str_squish(nome_at)))) %>%
  filter(grepl("MINISTERIO PUBLICO",
               nome_at,
               fixed = TRUE)) %>% 
  group_by(nome_at) %>% 
  summarise(freq = n(),
            prop = round(freq/3682, 4)) %>% 
  arrange(desc(freq))

### 2.7.2. Amicus Curiae ----------------------------------------------------

poload <- datajud %>% 
  filter(dt_inicio_situacao_novo_v2 >= "2020") %>% 
  filter(classe == "AÇÃO CIVIL PÚBLICA") %>% 
  select(numprocess,
         grau,
         partes_ad_desc:tipoPessoa_ad) %>% 
  filter(!is.na(partes_ad_desc)) %>% 
  unique() %>% 
  mutate(nome_ad = encoder(nome_ad),
         nome_ad = str_to_upper(rm_accent(nome_ad)),
         nome_ad = stripWhitespace(trimws(str_squish(nome_ad))))

## Calculando a proporção

round(nrow(poload)/3682,4) * 100

