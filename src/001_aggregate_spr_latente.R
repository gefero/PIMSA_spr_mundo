library(tidyverse)
df <- read_csv('./data/proc/2024-04-04__WB_ILOSTAT_spr_comext_dataset.csv')

df <- df %>%
        mutate(across(classif1:classif2, 
                      ~replace_na(., 'S/Classif')))

df <- df %>%
        mutate(excl_tamaño = if_else(is.na(excl_tamaño), "No excluible", excl_tamaño),
               peq_estado = if_else(is.na(peq_estado), "No peq. estado", peq_estado))

latente <- c(
        "SP.RUR.TOTL.ZS", 
        "SP.RUR.TOTL",
        "SH.H2O.BASW.RU.ZS",
        "SH.STA.HYGN.RU.ZS", 
        "SH.STA.BASS.RU.ZS",
        "SL.AGR.EMPL.ZS", 
        "EMP_TEMP_SEX_STE_ECO_NB_A"
        )

names <- c(
        "SP.RUR.TOTL.ZS"= "PobrurP", 
        "SP.RUR.TOTL"= "PobrurN",
        "SH.H2O.BASW.RU.ZS" = "Pobruragua",
        "SH.STA.HYGN.RU.ZS" = "Pobrurmano", 
        "SH.STA.BASS.RU.ZS" = "Pobrursanit",
        "SL.AGR.EMPL.ZS" = "EmpagrP"
        ) %>% 
        enframe(name="indicator_id", value="newname")


df_latente <- df %>%
        filter(indicator_id %in% latente) %>%
        filter(indicator_id != "EMP_TEMP_SEX_STE_ECO_NB_A")


df_latente <- df_latente %>%
        left_join(names, by="indicator_id") %>%
        select(newname, indicator:fuente)

#test <- df_latente %>%
#        filter(iso3c == "ARG" & indicator_id == "SH.STA.HYGN.RU.ZS")

df_latente_agg <- df_latente %>%
        select(-c(sex, classif1, classif2)) %>%
        group_by(dimension, iso3c, country, 
                 region, income_group, cluster_pimsa, peq_estado, excl_tamaño,
                 newname, indicator) %>%
#        group_by(iso3c, newname, indicator) %>%
        summarise(mean_value = mean(value, na.rm = TRUE),
                  max_value = max(value, na.rm = TRUE),
                  min_value = min(value, na.rm = TRUE),
                  max_value_year = date[which.max(value)],
                  min_value_year = date[which.min(value)],
                  n = sum(!is.na(value))
        ) %>%
        left_join(
                df_latente %>%
                        select(-c(sex, classif1, classif2)) %>%
                        drop_na(value) %>%
                        group_by(iso3c,
                                 newname, indicator) %>%
                        summarise(max_year = max(date),
                                  min_year = min(date))
        ) %>%
        ungroup()

df_latente_agg_pivot <- df_latente_agg %>%
        select(-indicator) %>%
        pivot_wider(names_from = newname,
                    values_from = mean_value:min_year,
                    names_glue = "{newname}_{.value}")

test <- df_latente_agg_pivot %>%
        filter(iso3c=="ARG")

#####

df_cat_ocup <- df %>%
        filter(indicator_id %in% "EMP_TEMP_SEX_STE_ECO_NB_A") %>%
        filter(sex=="SEX_T") %>%
        filter(str_detect(classif1, "STE_ICSE93"))  %>%
        filter(
                str_detect(classif2, "ECO_SECTOR_TOTAL") |
                str_detect(classif2, "ECO_SECTOR_AGR")
        )


cat_ocup <- c(
        "STE_ICSE93_TOTAL" = "Total",
        "STE_ICSE93_1" = "Employers",
        "STE_ICSE93_2" = "Employees",
        "STE_ICSE93_3" = "Own account workers",
        "STE_ICSE93_4" = "Members of producers cooperatives",
        "STE_ICSE93_5" = "Contributing family workers",
        "STE_ICSE93_6" = "Workers non classifiable") %>%
        enframe(name="classif1",
                value="label") %>%
        mutate(label_agg = case_when(
                classif1 %in% c("STE_ICSE93_3", "STE_ICSE93_4", "STE_ICSE93_5") ~ "PeqpropagrN",
                TRUE ~ label
        ))

df_cat_ocup <- df_cat_ocup %>%
        left_join(cat_ocup, by="classif1") %>% 
        select(indicator_id:classif1, label, label_agg, classif2:fuente)

agr_total_nac <- df_cat_ocup %>%
        filter(
                str_detect(classif1, "TOTAL")
        ) %>%
        select(-fuente, -dimension) %>%
        pivot_wider(names_from=classif2,
                    values_from = value) %>%
        rename(EmptotalN=ECO_SECTOR_TOTAL,
               EmpagrnacN=ECO_SECTOR_AGR) %>%
        mutate(EmpagrnacP = EmpagrnacN/EmptotalN*100)

agr_total_nac <- agr_total_nac %>%
        #select(-label_agg,-label, -indicator, -indicator_id) %>%
        pivot_longer(EmptotalN:EmpagrnacP,
                     names_to="classif2") %>%
        select(iso3c, country, 
               region, income_group, cluster_pimsa, peq_estado, excl_tamaño,
               classif2, date, value)

agr_total_nac_agg <- agr_total_nac %>%
        group_by(iso3c, classif2) %>%
        summarise(mean_value = mean(value, na.rm = TRUE),
                  max_value = max(value, na.rm = TRUE),
                  min_value = min(value, na.rm = TRUE),
                  max_value_year = date[which.max(value)],
                  min_value_year = date[which.min(value)],
                  n = sum(!is.na(value)),
                  max_year = max(date),
                  min_year = min(date)
        ) %>%
        ungroup()


test <- agr_total_nac %>% filter(iso3c=="AFG")

agr_total_nac_agg_pivot <- agr_total_nac_agg %>%
        pivot_wider(names_from = classif2,
                    values_from = mean_value:min_year,
                    names_glue = "{classif2}_{.value}")


### Trab. indepte.

indeptes_agro <- df_cat_ocup %>%
        filter(label_agg %in% c("PeqpropagrN", "Total") &
                       classif2=="ECO_SECTOR_AGR")


indeptes_agro_agg <- indeptes_agro %>%
        select(-fuente, -dimension, -classif1,
               -classif2, -sex) %>%
        group_by(iso3c, date, label_agg) %>%
        summarise(value=sum(value)) %>%
        ungroup() %>%
        pivot_wider(names_from=label_agg,
                    values_from = value) %>%
        mutate(PeqpropagrP = PeqpropagrN / Total *100) %>%
        select(-Total) %>%
        pivot_longer(PeqpropagrN:PeqpropagrP,
             names_to="label_agg")

indeptes_agro_agg_pivot <- indeptes_agro_agg %>%
        group_by(iso3c, label_agg) %>%
        summarise(mean_value = mean(value, na.rm = TRUE),
                  max_value = max(value, na.rm = TRUE),
                  min_value = min(value, na.rm = TRUE),
                  max_value_year = date[which.max(value)],
                  min_value_year = date[which.min(value)],
                  n = sum(!is.na(value)),
                  max_year = max(date),
                  min_year = min(date)
        ) %>%
        ungroup() %>%
        pivot_wider(names_from = label_agg,
                    values_from = mean_value:min_year,
                    names_glue = "{label_agg}_{.value}")

test <- indeptes_agro %>% filter(iso3c=="AFG")

df_latente_final <- df_latente_agg_pivot %>%
        left_join(agr_total_nac_agg_pivot) %>%
        left_join(indeptes_agro_agg_pivot)

## Agrego totales absolutos
abs <- read_csv('./data/raw/Datos centralizados (SPR latente)- Países.csv') %>%
        janitor::clean_names() %>%
        select(codigo, empleo_agricola_miles) %>%
        mutate(empleo_agricola_miles=empleo_agricola_miles) %>%
        rename(iso3c = codigo,
               EmpagrnacN_mean_tablajulian = empleo_agricola_miles)

df_latente_final <- df_latente_final %>%
        left_join(abs)

names(df_latente_final)

df_latente_final <- df_latente_final %>%
        select(dimension:excl_tamaño, 
               contains("mean_value"), contains("min_value"), contains("max_value"),
               contains("max_value_year"), contains("min_value_year"),
               contains("max_year"), contains("min_year"),
               contains("n"))

df_latente_final %>% 
        select(c("EmpagrP_mean_value", "EmpagrnacP_mean_value", "EmpagrnacN_mean_tablajulian"))


write_csv(df_latente_final, './data/proc/tablas_finales/spr_latente_ind_final.csv')
haven::write_sav(df_latente_final, './data/proc/tablas_finales/spr_latente_ind_final.sav')
openxlsx::write.xlsx(df_latente_final, './data/proc/tablas_finales/spr_latente_ind_final.xlsx')
#var_dict <- tibble(
#        var_names = names(df_latente_final),
#       1 var_labels = NA)
#var_dict %>% write_csv('./data/proc/tablas_finales/dict_spr_latente_ind_final.csv')
