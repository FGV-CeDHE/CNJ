
## TÍTULO: MOMENTOS PROCESSUAIS - DATAJUD
## AUTORA:REBECA CARVALHO
## DATA: 16/09/2022

## PACOTES UTILIZADOS

library(plyr)
library(tidyverse)
library(jsonlite)
library(geobr)
library(data.table)
library(tm)
library(reshape2)

## OBJETIVOS

#'         - Calcular os tempos médios decorridos entre diferentes 
#'           fases processuais.

## PREPARANDO O AMBIENTE

setwd("CNJ")

# 1. Data -----------------------------------------------------------------

## Carregando a base de movimentações

movimentos <- readRDS("data/output/DataJud/movimentações_v26102022.rds")

cod_movimentos <- readRDS("data/output/DataJud/familias_cod_movimentos.rds")

## 1.1. Limpeza ------------------------------------------------------------

## Preparando os dados para o join

cod_movimentos <- cod_movimentos %>% 
  select(codigo_01,
         descricao_01,
         codigo_05,
         arvore_movimento) %>% 
  rename("id_movimento_origem" = "codigo_05",
         "hier_movimento" = "arvore_movimento") %>% 
  unique()

## Juntando os dados

movimentos <- left_join(movimentos,
                        cod_movimentos) %>% 
  unique()

# 2. Momentos -------------------------------------------------------------

## 2.1. Início do Processo -------------------------------------------------

## Selecionando somente as situações e movimentações
## que marcam o início do processo

momento01 <- movimentos %>%  
  unique() %>% 
  filter(nome_situacao %in% c("DISTRIBUÍDO") &
         id_situacao == 24) %>% 
  filter(nome_movimento %in% c("DISTRIBUIÇÃO") &
         id_movimento_origem %in% c(26)) %>% 
  mutate(dt_inicio_situacao = as.Date(dt_inicio_situacao,
                                      format = "%Y-%m-%d"),
         dt_fim_situacao = as.Date(dt_fim_situacao,
                                   format = "%Y-%m-%d"),
         movimento = "Início do Processo") %>%
  group_by(numprocess,
           sigla_grau,
           nome_situacao,
           nome_fase_processual) %>% 
  slice_min(order_by = dt_inicio_situacao) %>% 
  select(numprocess,
         tribunal,
         sigla_grau,
         dt_inicio_situacao,
         dt_fim_situacao,
         nome_situacao,
         nome_fase_processual,
         nome_movimento,
         movimento) %>%
  filter(nome_fase_processual == "CONHECIMENTO") %>% 
  unique()

## Verificando se os resultados estão consistentes

robusto <- fase01 %>% 
  group_by(numprocess, 
           sigla_grau) %>% 
  summarise(freq = n())

## 2.2. Execução do processo -----------------------------------------------

## Selecionando somente as situações e movimentações
## que marcam o início da execução do processo

momento02 <- movimentos %>%  
  unique() %>% 
  filter(nome_situacao %in% c("EXECUÇÃO NÃO CRIMINAL INICIADA",
                              "LIQUIDAÇÃO/EXECUÇÃO INICIADA") &
         id_situacao %in% c(26,
                            91)) %>% 
  filter(nome_movimento %in% c("MUDANÇA DE CLASSE PROCESSUAL",
                               "PETIÇÃO",
                               "EVOLUÇÃO DA CLASSE PROCESSUAL",
                               "LIQUIDAÇÃO INICIADA",
                               "EXECUÇÃO INICIADA") &
          id_movimento_origem %in% c(10966,
                                     12078,
                                     12246,
                                     151,
                                     152,
                                     156,
                                     85,
                                     52,
                                     14739,
                                     11384,
                                     11385)) %>% 
  mutate(dt_inicio_situacao = as.Date(dt_inicio_situacao,
                                      format = "%Y-%m-%d"),
         dt_fim_situacao = as.Date(dt_fim_situacao,
                                   format = "%Y-%m-%d"),
         movimento = "Execução") %>%
  group_by(numprocess,
           sigla_grau,
           nome_situacao,
           nome_fase_processual) %>% 
  slice_min(order_by = dt_inicio_situacao) %>% 
  select(numprocess,
         tribunal,
         sigla_grau,
         dt_inicio_situacao,
         dt_fim_situacao,
         nome_situacao,
         nome_fase_processual,
         nome_movimento,
         movimento) %>% 
  unique()

## Removendo caso duplicado, com dois movimentos 
## em um mesmo dia

fase02 <- fase02[c(-337),]

## Verificando se os resultados estão consistentes

robusto <- fase02 %>% 
  group_by(numprocess, 
           sigla_grau) %>% 
  summarise(freq = n()) 

## Empilhando os dois momentos

momentos <- rbind(momento01,
                  momento02) %>% 
  arrange(sigla_grau, 
          dt_inicio_situacao)

## Agregando os resultados

momentos_agg <- aggregate(nome_fase_processual ~ numprocess + sigla_grau,
                          data = momentos,
                          paste,
                          collapse = " - ") %>% 
  rename("movimento2" = "nome_fase_processual")

## Juntando com as fases

momentos <- left_join(momentos, 
                      momentos_agg) %>% 
  mutate(movimento2 = str_to_title(movimento2),
         movimento = ifelse(!movimento2 %in% c("Conhecimento - Execução"),
                            movimento2,
                            movimento))

## Calculando o tempo entre a distribuição
## e o ínicio da execução

momentos <- momentos %>% 
  mutate(nome_fase_processual = factor(nome_fase_processual,
                                      levels = c("CONHECIMENTO",
                                                 "EXECUÇÃO"))) %>% 
  arrange(numprocess,
          sigla_grau,
          nome_fase_processual) %>% 
  group_by(numprocess,
           sigla_grau) %>% 
  mutate(tempo = dt_fim_situacao - lag(dt_inicio_situacao))

## Removendo as movimentações com data equivocada

momentos <- momentos %>% 
  filter(tempo > 0 |
         !is.na(tempo))

## Calculando o tempo médio para cada um dos tribunais

media_momentos <- momentos %>%
  mutate(movimento = "Início do processo - Execução") %>% 
  group_by(sigla_grau,
           tribunal,
           movimento) %>% 
  summarise(media_fase = mean(tempo,
                              na.rm = T))

# 3. Fases ----------------------------------------------------------------

## 3.1. Investigatória -----------------------------------------------------

## Selecionando somente as movimentações
## que integram a fase investigatória

fase01 <- movimentos %>%  
  unique() %>% 
  filter(!nome_situacao %in% c("PENDENTE",
                               "TRAMITANDO")) %>% 
  filter(nome_fase_processual == "INVESTIGATÓRIA") %>% 
  mutate(dt_inicio_situacao = as.Date(dt_inicio_situacao,
                                      format = "%Y-%m-%d"),
         dt_fim_situacao = as.Date(dt_fim_situacao,
                                   format = "%Y-%m-%d"),
         movimento = "Investigatória - Conhecimento") %>%
  group_by(numprocess,
           sigla_grau,
           nome_fase_processual) %>% 
  slice_min(order_by = dt_inicio_situacao) %>% 
  select(numprocess,
         tribunal,
         sigla_grau,
         dt_inicio_situacao,
         dt_fim_situacao,
         nome_fase_processual,
         movimento) %>%
  group_by(numprocess,
           sigla_grau,
           nome_fase_processual) %>% 
  slice_max(order_by = dt_fim_situacao) %>% 
  unique()

## Verificando se os resultados estão consistentes

robusto <- fase01 %>% 
  group_by(numprocess, 
           sigla_grau) %>% 
  summarise(freq = n())

## 3.2. Conhecimento -------------------------------------------------------

## Selecionando somente as movimentações
## que integram a fase de conhecimento

fase02 <- movimentos %>%  
  unique() %>% 
  filter(!nome_situacao %in% c("PENDENTE",
                               "TRAMITANDO")) %>% 
  filter(nome_fase_processual == "CONHECIMENTO") %>% 
  mutate(dt_inicio_situacao = as.Date(dt_inicio_situacao,
                                      format = "%Y-%m-%d"),
         dt_fim_situacao = as.Date(dt_fim_situacao,
                                   format = "%Y-%m-%d"),
         movimento = "Investigatória - Conhecimento") %>%
  group_by(numprocess,
           sigla_grau,
           nome_fase_processual) %>% 
  slice_min(order_by = dt_inicio_situacao) %>% 
  select(numprocess,
         tribunal,
         sigla_grau,
         dt_inicio_situacao,
         dt_fim_situacao,
         nome_fase_processual,
         movimento) %>%
  group_by(numprocess,
           sigla_grau,
           nome_fase_processual) %>% 
  slice_max(order_by = dt_fim_situacao) %>% 
  unique()

## Verificando se os resultados estão consistentes

robusto <- fase02 %>% 
  group_by(numprocess, 
           sigla_grau) %>% 
  summarise(freq = n())

## 3.3. Execução -----------------------------------------------------------

## Selecionando somente as movimentações
## que integram a fase de execução

fase03 <- movimentos %>%  
  unique() %>% 
  filter(!nome_situacao %in% c("PENDENTE",
                               "TRAMITANDO")) %>% 
  filter(nome_fase_processual == "EXECUÇÃO") %>% 
  mutate(dt_inicio_situacao = as.Date(dt_inicio_situacao,
                                      format = "%Y-%m-%d"),
         dt_fim_situacao = as.Date(dt_fim_situacao,
                                   format = "%Y-%m-%d"),
         movimento = "Conhecimento - Execução") %>%
  group_by(numprocess,
           sigla_grau,
           nome_fase_processual) %>% 
  slice_min(order_by = dt_inicio_situacao) %>% 
  select(numprocess,
         tribunal,
         sigla_grau,
         dt_inicio_situacao,
         dt_fim_situacao,
         nome_fase_processual,
         movimento) %>%
  group_by(numprocess,
           sigla_grau,
           nome_fase_processual) %>% 
  slice_max(order_by = dt_fim_situacao) %>% 
  unique()

## Verificando se os resultados estão consistentes

robusto <- fase03 %>% 
  group_by(numprocess, 
           sigla_grau) %>% 
  summarise(freq = n())

## 3.4. Fim do processo ----------------------------------------------------

## Selecionando somente as movimentações
## que marcam o fim do processo

fase04 <- movimentos %>%  
  unique() %>% 
  filter(id_situacao %in% c(2,10,23,41) |
         (id_situacao == 88 &
          id_situacao_finalizar %in% c(65, 26, 91) &
          dt_fim_situacao != "NULL")) %>% 
  mutate(dt_inicio_situacao = as.Date(dt_inicio_situacao,
                                      format = "%Y-%m-%d"),
         dt_fim_situacao = as.Date(dt_fim_situacao,
                                   format = "%Y-%m-%d"),
         nome_fase_processual = "FIM",
         movimento = "Execução - Fim") %>%
  filter(!is.na(dt_fim_situacao)) %>%
  group_by(numprocess,
           sigla_grau,
           nome_fase_processual) %>% 
  slice_min(order_by = dt_inicio_situacao) %>% 
  select(numprocess,
         tribunal,
         sigla_grau,
         dt_inicio_situacao,
         dt_fim_situacao,
         nome_fase_processual,
         movimento) %>%
  group_by(numprocess,
           sigla_grau,
           nome_fase_processual) %>% 
  slice_max(order_by = dt_fim_situacao) %>% 
  unique()

## Verificando se os resultados estão consistentes

robusto <- fase04 %>% 
  group_by(numprocess, 
           sigla_grau) %>% 
  summarise(freq = n())

## Empilhando as quatro fases

fases <- rbind(fase01,
               fase02,
               fase03,
               fase04) %>% 
  arrange(numprocess,
          sigla_grau, 
          dt_inicio_situacao)

## Agregando os resultados

fases_agg <- aggregate(nome_fase_processual ~ numprocess + sigla_grau,
                       data = fases,
                       paste,
                       collapse = " - ") %>% 
  rename("movimento2" = "nome_fase_processual")

## Juntando com as fases

fases <- left_join(fases, 
                   fases_agg) %>% 
  mutate(movimento2 = str_to_title(movimento2),
         movimento = ifelse(!movimento2 %in% c("Investigatória - Conhecimento - Execução - Fim",
                                               "Investigatória - Conhecimento - Fim - Execução",
                                               "Investigatória - Conhecimento - Execução",
                                               "Conhecimento - Execução - Fim",
                                               "Conhecimento - Execução - Investigatória",
                                               "Fim - Conhecimento - Execução",
                                               "Fim - Investigatória - Conhecimento",
                                               "Investigatória - Conhecimento - Fim",
                                               "Investigatória - Execução - Fim",
                                               "Execução - Fim - Conhecimento",
                                               "Execução - Investigatória - Conhecimento",
                                               "Execução - Investigatória - Conhecimento - Fim"),
                            movimento2,
                            movimento))

## Calculando o tempo entre a distribuição
## e o ínicio da execução

fases <- fases %>% 
  mutate(nome_fase_processual = factor(nome_fase_processual,
                                 levels = c("INVESTIGATÓRIA",
                                            "CONHECIMENTO",
                                            "EXECUÇÃO",
                                            "FIM"))) %>% 
  arrange(numprocess,
          sigla_grau,
          nome_fase_processual) %>% 
  group_by(numprocess,
           sigla_grau) %>% 
  mutate(tempo = dt_fim_situacao - lag(dt_inicio_situacao)) 

## Removendo as movimentações com data equivocada

fases <- fases %>% 
  filter(tempo >= 0 |
         is.na(tempo))

## Calculando o tempo médio para cada um dos tribunais

media_fases <- fases %>%
  group_by(sigla_grau,
           tribunal,
           movimento) %>% 
  summarise(media_fase = mean(tempo,
                              na.rm = T)) %>% 
  filter(!is.na(media_fase)) %>% 
  filter(movimento %in% c("Investigatória - Conhecimento", 
                          "Conhecimento - Execução",
                          "Execução - Fim"))

# 5. Exportar -------------------------------------------------------------

## Salvando o resultado

writexl::write_xlsx(media_fases,
                    "data/output/DataJud/média_fases processuais.xlsx")

writexl::write_xlsx(media_momentos,
                    "data/output/DataJud/média_fases processuais_conhecimento.xlsx")

writexl::write_xlsx(momentos,
                    "data/output/DataJud/momentos processuais_conhecimento.xlsx")

