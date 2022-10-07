
stf_comp <- stf_comp %>% 
  rename("Numero" = "numero_processo",
         "SiglaTribunal" = "tribunal",
         "Relator" = "relator",
         "OrgaoJudiciante" = "orgao_judiciante",
         "DataJulgamento" = "data_julgamento",
         "DataPublicacao" = "data_publicacao",
         "TipoDocumento" = "tipo_documento",
         "TipoAcao" = "tipo_ação",
         "TipoRecurso" = "tipo_recurso",
         "URL_InteiroTeor" = "link_pdf_internet",
         "IDPortal" = "id_portal",
         "DadosPublicacao" = "dados_publicacao",
         "Partes" = "partes",
         "Ementa" = "ementa",
         "Decisao" = "decisao",
         "Tema" = "tema",
         "Tese" = "tese",
         "Indexacao" = "indexacao",
         "Legislacao" = "legislacao",
         "Observacao" = "observacao",
         "Doutrina" = "doutrina") %>% 
  select(ID,
         IDPortal,
         Numero,
         DataJulgamento,
         DataPublicacao,
         SiglaTribunal,
         OrgaoJudiciante,
         Relator,
         Partes,
         Tema,
         TipoDocumento,
         TipoAcao,
         TipoRecurso,
         Ementa,
         Decisao,
         DadosPublicacao,
         Indexacao,
         Legislacao,
         Tese,
         Observacao,
         Doutrina,
         URL_InteiroTeor,
         ACP:`recursos naturais/biodiversidade/ecologico`) %>% 
  mutate(DataJulgamento = as.Date(DataJulgamento,
                                  format = "%d/%m/%Y")) %>% 
  filter(DataJulgamento >= "2018-01-01" &
         DataJulgamento <= "2022-04-30")

saveRDS(stf_comp,
        "data/output/STF/acórdãos_STF_03082022_com filtros.rds")  


tjpa_comp <- tjpa_comp %>% 
  rename("NumeroDocumento" = "numero_documento",
         "NumeroAcordao" = "numero_acordao",
         "Numero" = "numero_processo",
         "DataJulgamento" = "data_julgamento",
         "DataPublicacao" = "data_publicacao",
         "OrgaoJudiciante" = "orgao_judiciante",
         "Relator" = "relator",
         "TipoDocumento" = "tipo_documento",
         "TipoAcao" = "tipo_ação",
         "TipoRecurso" = "tipo_recurso",
         "URL_InteiroTeor" = "link_html_internet",
         "Ementa" = "ementa",
         "Secao" = "secao") %>% 
  select(ID,
         Numero,
         NumeroAcordao,
         NumeroDocumento,
         DataJulgamento,
         DataPublicacao,
         OrgaoJudiciante,
         Relator,
         TipoDocumento,
         TipoAcao,
         TipoRecurso,
         Ementa,
         Secao,
         URL_InteiroTeor,
         ACP:`recursos naturais/biodiversidade/ecologico`) %>% 
  mutate(DataJulgamento = as.Date(DataJulgamento,
                                  format = "%d/%m/%Y")) %>% 
  filter(DataJulgamento >= "2018-01-01" &
         DataJulgamento <= "2022-04-30")

saveRDS(tjpa_comp,
        "data/output/TJPA/acórdãos_TJPA_03082022_com filtros.rds")                               
