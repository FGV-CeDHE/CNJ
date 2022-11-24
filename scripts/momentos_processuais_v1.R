
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

# 1. FASE 01: DISTRIBUIÇÃO - JULGAMENTO 1ª INST ---------------------------

movimentos <- readRDS("~/CeDHE/data/output/SireneJud/movimentações_v05092022.rds")

fase01 <- movimentos %>%  
  filter(!numprocess %in% c("1001308-78.2018.4.01.4100",
                            "1002524-06.2020.4.01.4100")) %>% 
  #select(-sigla_grau) %>% 
  unique() %>% 
  filter(nome_situacao %in% c("DISTRIBUÍDO",
                              "JULGADO COM RESOLUÇÃO DO MÉRITO",
                              "JULGADO SEM RESOLUÇÃO DO MÉRITO")) %>% 
  filter(nome_movimento %in% c("DISTRIBUIÇÃO",
                               "PROCEDÊNCIA",
                               "IMPROCEDÊNCIA",
                               "PROCEDÊNCIA EM PARTE")) %>% 
  mutate(dt_inicio_situacao = as.Date(dt_inicio_situacao,
                                      format = "%Y-%m-%d"),
         dt_fim_situacao = as.Date(dt_fim_situacao,
                                   format = "%Y-%m-%d"),
         movimento = "Início do Processo - Julgamento em 1ª instância") %>%
  group_by(numprocess,
           sigla_grau,
           nome_situacao) %>% 
  slice_min(order_by = dt_inicio_situacao) %>% 
  select(numprocess,
         tribunal,
         sigla_grau,
         dt_inicio_situacao,
         nome_situacao,
         nome_movimento,
         movimento) %>% 
  unique()

## Verificando se os resultados estão consistentes
  
robusto <- fase01 %>% 
  group_by(numprocess, 
           sigla_grau
           ) %>% 
  summarise(freq = n()) 

# 2. FASE 02: JULGAMENTO 1ª INST - JULGAMENTO 2ª INST --------------------

fase02 <- movimentos %>%  
  filter(!numprocess %in% c("1001308-78.2018.4.01.4100",
                            "1002524-06.2020.4.01.4100")) %>% 
  filter(!numprocess %in% c("0000525-38.2020.8.11.0021",
                            "1002895-10.2020.8.11.0041",
                            "7009752-50.2017.8.22.0014",
                            "7023875-92.2017.8.22.0001")) %>% 
 # select(-sigla_grau) %>% 
  unique() %>% 
  filter(nome_situacao %in% c("JULGADO COM RESOLUÇÃO DO MÉRITO",
                              "JULGADO SEM RESOLUÇÃO DO MÉRITO")) %>% 
  filter(nome_movimento %in% c("PROVIMENTO",
                               "NÃO-PROVIMENTO",
                               "PROVIMENTO EM PARTE")) %>% 
  mutate(dt_inicio_situacao = as.Date(dt_inicio_situacao,
                                      format = "%Y-%m-%d"),
         dt_fim_situacao = as.Date(dt_fim_situacao,
                                   format = "%Y-%m-%d"),
         movimento = "Julgamento em 1ª instância - Julgamento em 2ª instância") %>%
  group_by(numprocess,
           sigla_grau,
           nome_situacao) %>% 
  slice_min(order_by = dt_inicio_situacao) %>% 
  select(numprocess,
         tribunal,
         sigla_grau,
         dt_inicio_situacao,
         nome_situacao,
         nome_movimento,
         movimento) %>% 
  unique() 

## Verificando se os resultados estão consistentes

robusto <- fase02 %>% 
  group_by(numprocess, 
           sigla_grau
  ) %>% 
  summarise(freq = n()) 

## Empilhando as duas fases

momentos <- rbind(fase01,
                  fase02) %>% 
  arrange(sigla_grau, 
          dt_inicio_situacao)

# 3. FASE 03: JULGAMENTO 1ª/2ª INST - EXECUÇÃO ----------------------------

fase03 <- movimentos %>%  
  filter(!numprocess %in% c("1001308-78.2018.4.01.4100",
                            "1002524-06.2020.4.01.4100")) %>% 
  filter(!numprocess %in% c("0000525-38.2020.8.11.0021",
                            "1002895-10.2020.8.11.0041",
                            "7009752-50.2017.8.22.0014",
                            "7023875-92.2017.8.22.0001")) %>% 
  # select(-sigla_grau) %>% 
  unique() %>% 
  filter(nome_situacao %in% c("JULGADO COM RESOLUÇÃO DO MÉRITO",
                              "JULGADO SEM RESOLUÇÃO DO MÉRITO")) %>% 
  filter(nome_movimento %in% c("EXECUÇÃO/CUMPRIMENTO DE SENTENÇA INICIADA (O) ",
                               "EXTINÇÃO DA EXECUÇÃO OU DO CUMPRIMENTO DA SENTENÇA")) %>% 
  mutate(dt_inicio_situacao = as.Date(dt_inicio_situacao,
                                      format = "%Y-%m-%d"),
         dt_fim_situacao = as.Date(dt_fim_situacao,
                                   format = "%Y-%m-%d"),
         movimento = "Julgamento em 1ª ou 2ª instância - Execução") %>%
  group_by(numprocess,
           sigla_grau,
           nome_situacao) %>% 
  slice_min(order_by = dt_inicio_situacao) %>% 
  select(numprocess,
         tribunal,
         sigla_grau,
         dt_inicio_situacao,
         nome_situacao,
         nome_movimento,
         movimento) %>% 
  unique() 

## Verificando se os resultados estão consistentes

robusto <- fase03 %>% 
  group_by(numprocess, 
           sigla_grau
  ) %>% 
  summarise(freq = n()) 

## Empilhando as duas fases

momentos <- rbind(momentos,
                  fase03) %>% 
  arrange(sigla_grau, 
          dt_inicio_situacao)

# 4. FASE 04: EXECUÇÃO - FIM DO PROCESSO ----------------------------

fase04 <- movimentos %>%  
  filter(!numprocess %in% c("1001308-78.2018.4.01.4100",
                            "1002524-06.2020.4.01.4100")) %>% 
  filter(!numprocess %in% c("0000525-38.2020.8.11.0021",
                            "1002895-10.2020.8.11.0041",
                            "7009752-50.2017.8.22.0014",
                            "7023875-92.2017.8.22.0001")) %>% 
  # select(-sigla_grau) %>% 
  unique() %>% 
  filter(nome_situacao %in% c("TRANSITADO EM JULGADO")) %>% 
  filter(nome_movimento %in% c("TRÂNSITO EM JULGADO")) %>% 
  mutate(dt_inicio_situacao = as.Date(dt_inicio_situacao,
                                      format = "%Y-%m-%d"),
         dt_fim_situacao = as.Date(dt_fim_situacao,
                                   format = "%Y-%m-%d"),
         movimento = "Execução - Fim do processo") %>%
  group_by(numprocess,
           sigla_grau,
           nome_situacao) %>% 
  slice_min(order_by = dt_inicio_situacao) %>% 
  select(numprocess,
         tribunal,
         sigla_grau,
         dt_inicio_situacao,
         nome_situacao,
         nome_movimento,
         movimento) %>% 
  unique() 

## Verificando se os resultados estão consistentes

robusto <- fase04 %>% 
  group_by(numprocess, 
           sigla_grau
  ) %>% 
  summarise(freq = n()) 

## Empilhando as duas fases

momentos <- rbind(momentos,
                  fase04) %>% 
  arrange(sigla_grau, 
          dt_inicio_situacao)

## Calculando o tempo entre a distribuição
## e o primeiro julgamento em 1ª instância

momentos <- momentos %>% 
  mutate(nome_movimento = factor(nome_movimento,
                            levels = c("DISTRIBUIÇÃO",
                                       "PROCEDÊNCIA",
                                       "IMPROCEDÊNCIA",
                                       "PROCEDÊNCIA EM PARTE",
                                       "PROVIMENTO",
                                       "NÃO-PROVIMENTO",
                                       "PROVIMENTO EM PARTE",
                                       "EXECUÇÃO/CUMPRIMENTO DE SENTENÇA INICIADA (O)",
                                       "EXTINÇÃO DA EXECUÇÃO OU DO CUMPRIMENTO DA SENTENÇA",
                                       "TRÂNSITO EM JULGADO"))) %>% 
  arrange(numprocess,
          sigla_grau,
          nome_movimento) %>% 
  group_by(numprocess,
           sigla_grau) %>% 
  mutate(tempo = dt_inicio_situacao - lag(dt_inicio_situacao)) 

## Removendo as movimentações com data equivocada

momentos2 <- momentos %>% 
  filter(tempo >= 0 |
         is.na(tempo))

## Calculando o tempo médio para cada um dos tribunais

media_fases <- momentos2 %>%
  group_by(sigla_grau,
           tribunal,
           movimento) %>% 
  summarise(media_fase = mean(tempo,
                              na.rm = T))

# 5. Exportar -------------------------------------------------------------

## Salvando o resultado

writexl::write_xlsx(media_fases,
                    "data/output/SireneJud/média_fases processuais.xlsx")
