## Generate aggs from tables

library(tidyverse)
df <- read_csv('./data/proc/2024-04-04__WB_ILOSTAT_spr_comext_dataset.csv')

df <- df %>%
        mutate(across(classif1:classif2, 
                      ~replace_na(., 'S/Classif')))

df <- df %>%
        mutate(excl_tamaño = if_else(is.na(excl_tamaño), "No excluible", excl_tamaño),
               peq_estado = if_else(is.na(peq_estado), "No peq. estado", peq_estado))

df_agg <- df %>%
        group_by(dimension, iso3c, country, 
                 region, income_group, cluster_pimsa, peq_estado, excl_tamaño,
                 indicator_id, indicator,
                 sex, classif1, classif2) %>%
        summarise(mean = mean(value, na.rm=TRUE),
                  max_value = max(value, na.rm=TRUE),
                  min_value = min(value, na.rm=TRUE),
                  n_years = n(),
                  min_year = min(date),
                  max_year = max(date) 
                  )



write_csv(df_agg, paste0('./data/proc/', 
                         str_replace_all(Sys.Date(), "-", ""),
                         "_WB_ILOSTAT_AGG_spr_comext_dataset.csv")
)

haven::write_sav(df_agg, paste0('./data/proc/', 
                                str_replace_all(Sys.Date(), "-", ""),
                                "_WB_ILOSTAT_AGG_spr_comext_dataset.sav")
)
