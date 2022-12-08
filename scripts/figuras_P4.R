
## TÍTULO: ANÁLISE DESCRITIVA - P4
## DATA: 15/11/2022
## AUTORA: REBECA CARVALHO

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(geobr)
library(ggsci)
library(scales)
library(abjutils)

## OBJETIVOS

#'         - Gerar gráficos e tabelas para a produção do relatório final
#'           do projeto.

## CONFIGURA O AMBIENTE

setwd("CNJ")

windowsFonts("Arial" = windowsFont("Arial"))

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

## Versão sem as variáveis de assunto, que causam repetição de processos

datajud2 <- datajud %>% 
  select(numprocess:classe,
         partes_pa_desc:flg_julgamento) %>% 
  unique() %>% 
  mutate(classe = str_to_sentence(classe),
         descricao_01_classe = str_to_sentence(descricao_01_classe))

## Deixando somente os nomes com cpf/cnj completos

ibama <- ibama %>% 
  mutate(DAT_HORA_AUTO_INFRACAO = as.Date(DAT_HORA_AUTO_INFRACAO,
                                          format = "%d/%m/%Y")) %>% 
  filter(!grepl("\\*",
                CPF_CNPJ_INFRATOR)) %>% 
  mutate(NOME_INFRATOR = str_to_upper(NOME_INFRATOR))

# 2. Gráficos -------------------------------------------------------------

## 2.1. Figura 12: Classes Processuais -------------------------------------

## Figura 12: Ranking das 10 classes processuais mais 
## frequentes nas ações socioambientais dos tribunais 
## atuantes na Amazônia Legal (DataJud)

## Calculando as 10 classes mais recorrentes

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

## 2.2. Figura 13: Assuntos ------------------------------------------------

## Figura 13: Ranking dos 10 assuntos mais frequentes 
## nos tribunais atuantes na Amazônia Legal (DataJud)

## Calculando 10 os assuntos mais recorrentes

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

## 2.3. Figura 21: Infrações Ambientais ------------------------------------

## Figura 21: Número de infrações socioambientais 
## cometidas por indivíduos ajuizados em ações socioambientais 
## nos tribunais atuantes na Amazônia Legal (2020-2022)

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

## 2.4. Figura 49: Número de Sentenças x Número de Juízes ------------------

## Figura 49: Média do número de sentenças x número de magistrados 
## em unidades judiciárias atuantes na Amazônia Legal (2015-2021)

## Criando a paleta de referência

paleta <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", 
            "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")

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

## 2.5. Figura 50: Competência Ambiental x Competência Não-Ambiental -------

## Figura 50: Média do número de sentenças x número de magistrados 
## em unidades judiciárias de competência ambiental e 
## não-ambiental atuantes na Amazônia Legal (2015-2021)

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
