# 
# # Construyo tabla de indicadores
# indicadores <- tibble(
# dimension = c(rep("com_ext",6),"spr_flot",rep("spr_estancada",6), rep("spr_latente",11)),
# fuente = c(rep("WB",7),rep("ILOSTAT",6), rep("WB",7), rep("ILOSTAT",4)),
# indicator_id = c(
# #com_ext
# #WB
# "TX.MNF.TECH.ZS.UN",
# "TX.VAL.TECH.MF.ZS",
# "TX.VAL.MANF.ZS.UN",
# "TX.VAL.MRCH.CD.WT",
# "BM.GSR.ROYL.CD",
# "BX.GSR.ROYL.CD",
# #spr_flot
# #WB
# "SL.UEM.TOTL.ZS",
# #spr_estancada
# #ILOSTAT
# "EMP_NIFL_SEX_AGE_GEO_RT_A",
# "TRU_DEMP_SEX_AGE_GEO_RT_A",
# "EES_XTMP_SEX_RT_A",
# "EMP_PTER_SEX_RT_A",
# "EMP_NIFL_SEX_STE_RT_A",
# "EMP_PIFL_SEX_STE_RT_A",
# #spr_latente
# #WB
# "SH.H2O.BASW.RU.ZS",
# "SH.STA.HYGN.RU.ZS",
# "SH.STA.BASS.RU.ZS",
# "SL.AGR.EMPL.ZS",
# "SP.RUR.TOTL.ZG",
# "SP.RUR.TOTL.ZS",
# "SP.RUR.TOTL",
# #ILOSTAT
# "EMP_TEMP_SEX_STE_ECO_NB_A",
# "EMP_TEMP_SEX_STE_GEO_NB_A",
# "POP_XWAP_SEX_GEO_LMS_NB_A",
# "EAP_DWAP_SEX_AGE_GEO_RT_A")
# )

library(wbstats)
library(Rilostat)
library(tidyverse)
library(googlesheets4)

indicadores <- read_sheet("https://docs.google.com/spreadsheets/d/1rQh6BYbRDh_4Uv70TsEIWNRjrR449TnqkNoZzJje3UA/edit?gid=423273661#gid=423273661",
                    sheet="tabla_filtro_indicadores")

get_indicator <- function(indicador, fuente){
        if (fuente == "WB"){
                d <- wb_data(
                        indicador, 
                        start_date = 2009, end_date = 2019, 
                        return_wide = FALSE) %>%
                        mutate(sex = "SEX_T",
                               classif1 = "NA",
                               classif2 = "NA") %>%
                        select(indicator_id, indicator, iso3c, country, date, 
                               sex, classif1, classif2, value)
        }

        if (fuente == "ILOSTAT"){
                d <- get_ilostat(id = indicador,
                                 detail = "dataonly",
                                 type="both",
                                 filters=list(timefrom=2009, timeto=2019))
                
                if (!"classif1" %in% names(d)){
                        d <- d %>% mutate(classif1 = "NA",
                                          classif2 = "NA",
                                          classif1.label="NA",
                                          classif2.label="NA")
                        }
                if (!"classif2" %in% names(d)){
                        d <- d %>% mutate(
                                          classif2 = "NA",
                                          classif2.label="NA")
                }
                d <- d %>%
                        select(-c(source, source.label, 
                          sex.label, classif1.label, classif2.label)) %>%
                        rename(iso3c=ref_area,
                               country = ref_area.label,
                               indicator_id = indicator,
                               indicator = indicator.label,
                               date = time,
                               value = obs_value) %>%
                        select(indicator_id, indicator, iso3c, country, date,sex, classif1, classif2, value)
        }
        return(d)
        
}

tictoc::tic()
data <- list()
l <- nrow(indicadores)
for (i in 1:l){
        print(paste0('Descargando indicador ', i, ' de ', l))
        data[[i]] <- get_indicator(
                                indicadores$indicator_id[i],
                                indicadores$fuente[i]
                                   )
}

data <- do.call(rbind, data)

data <- data %>%
        left_join(indicadores)


#mis_ind <- data %>% filter(is.na(dimension)) %>% select(indicator_id) %>% distinct()

data <- data %>%
        mutate(indicator_id = case_when(
                       is.na(dimension) ~ paste0(indicator_id, "_A"),
               TRUE ~ indicator_id)
        ) %>%
        select(-dimension, -fuente) %>%
        left_join(indicadores) 
                
tictoc::toc()

## Agrego tipologia y grupos de países
## AGREGAR VARIABLE CON PAISES A EXCLUIR SEGUN MAIL DE RIC

#clst_pimsa <- read_csv('../PIMSA_pobreza/data/proc/paises_clustering_final.csv') %>%
#        select(iso3c,country, C5) %>%
#        rename(cluster_pimsa = C5)
#country_list <- readxl::read_xlsx('./data/inputs/groups_countries.xlsx', 
#                                  sheet="List of economies") %>%
#        rename(iso3c=Code,
#               country=Economy) %>%
#        janitor::clean_names()

country_classif <- read_csv('./data/ouputs/country_classification.csv')

data <- data %>%
        left_join(country_classif %>% select(-country), by="iso3c")

#data <- data %>%
#        left_join(country_list %>% 
#                          select(iso3c, region, income_group), 
 #                 by="iso3c")


data <- data %>%
        select(indicator_id:country,
               region, income_group, cluster_pimsa, peq_estado, excl_tamaño,
               everything())


data <- data %>%
        mutate(across(region:cluster_pimsa, 
                      ~if_else(is.na(.x), "99_Sin_datos",
                              .x)))



#data <- data %>%
#        mutate(cluster_pimsa = 
#                       case_when(
#                               cluster_pimsa == '1_Alto' ~ 'C1. Cap. avanzado',
#                               cluster_pimsa == '2_Alto A, +PA -RS' ~ 'C2. Cap. extensión reciente c/desarrollo profundidad',
#                               cluster_pimsa == '3_Medio' ~ 'C3. Cap. extensión c/peso campo',
#                               cluster_pimsa == '4_Bajo B +IND,SERV y RS' ~ 'C4. Cap. escasa extensión c/peso campo',
#                               cluster_pimsa == '5_Bajo' ~ 'C5. Pequeña propiedad en el campo'
#                       ))


write_csv(data, 
          paste('./data/proc/', Sys.Date(), '__WB_ILOSTAT_spr_comext_dataset.csv', sep='')
          )

# save_split <- function(df=data){
#         for (s in unique(data$dimension)){
#                 df %>% 
#                         filter(dimension==s) %>%
#                         write_csv(paste0('./data/proc_',
#                                          s,"_",
#                                          "WB_ILOSTAT_spr_comext_dataset_nuevo.csv"))
#         }
# }


#data <- read_csv('./data/proc/WB_ILOSTAT_spr_comext_dataset_nuevo.csv')

for (s in unique(data$dimension)){
        data %>% 
                filter(dimension==s) %>%
                haven::write_sav(paste0('./data/proc/',
                                Sys.Date(),"_",
                                 s,"_",
                                 "WB_ILOSTAT_spr_comext_dataset_nuevo.sav"))
#                write_csv(paste0('./data/proc/',
#                                 s,"_",
#                                 "WB_ILOSTAT_spr_comext_dataset_nuevo.csv"))
}
