library(tidyverse)
library(Rilostat)
library(wbstats)
latente <- c(
        "SP.RUR.TOTL.ZS", 
        "SP.RUR.TOTL",
        "SP.POP.TOTL",
        "SH.H2O.BASW.RU.ZS",
        "SH.STA.BASS.RU.ZS",
        "SL.AGR.EMPL.ZS", 
        "EMP_TEMP_SEX_STE_ECO_NB_A"
)


names <- c(
        "SP.RUR.TOTL.ZS"= "Pob_Rural_Prop", 
        "SP.RUR.TOTL"= "Pob_Rural_N",
        "SP.POP.TOTL" = "Pob_Total_N",
        "SH.H2O.BASW.RU.ZS" = "Pob_Rural_Agua_Prop",
        "SH.STA.BASS.RU.ZS" = "Pob_Sanit_Agua_Prop",
        "SL.AGR.EMPL.ZS" = "Ocup_Agro_Prop",
        "EMP_TEMP_SEX_STE_ECO_NB_A" = "Ocup_Rama_CatOcup_N"
) %>% 
        enframe(name="indicator_id", value="newname") %>%
        mutate(source = case_when(
                              indicator_id == "EMP_TEMP_SEX_STE_ECO_NB_A" ~ "ILOStat",
                              TRUE ~ "WBData"
                      ))


## Uso la librería wbstats para descargar los datos
latente_wb <- wb_data(names %>% 
                              filter(source == "WBData") %>% 
                              select(indicator_id) %>%
                              pull(), 
                      start_date = 2009, end_date = 2019, return_wide = FALSE)

pobreza_rural <- latente_wb %>%
        group_by(iso3c, country, indicator_id) %>%
        summarise(value_raw = mean(value, na.rm=TRUE)) %>%
        pivot_wider(names_from=indicator_id, 
                    values_from = value_raw) %>%
        drop_na() %>%
        ungroup() %>% 
        mutate(SH.H2O.BASW.RU.ZS = 100 - SH.H2O.BASW.RU.ZS,
               SH.STA.BASS.RU.ZS = 100 - SH.STA.BASS.RU.ZS)

country_classif <- read_csv('./data/ouputs/country_classification.csv') 

pobreza_rural_final <- pobreza_rural %>%
        left_join(country_classif) %>%
        select(iso3c, country, region:ocde, everything())

### CAT_OCUP_RAMA
df_catocup <- get_ilostat(id="EMP_TEMP_SEX_STE_ECO_NB_A",
                       filters=list(
                               timefrom=2009,
                               timeto=2019,
                               sex="SEX_T")
                       )

df_catocup_ <- df_catocup %>%
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
                classif1 %in% c("STE_ICSE93_3", "STE_ICSE93_4", "STE_ICSE93_5") ~ "Peq_prod_Agro_N",
                TRUE ~ label
        ))

df_catocup_ <- df_catocup_ %>%
        left_join(cat_ocup, by="classif1") %>% 
        select(ref_area, indicator:classif1, label, label_agg, classif2:obs_value)

## CALCULO LOS TOTALES DE POB AGRICOLA A NIVEL PAÍS
agr_total_nac <- df_catocup_ %>%
        filter(
                str_detect(classif1, "TOTAL")
        ) %>%
        select(-label, -label_agg) %>%
        pivot_wider(names_from=classif2,
                    values_from = obs_value) %>%
        #rename(EmptotalN=ECO_SECTOR_TOTAL,
        #       EmpagrnacN=ECO_SECTOR_AGR) %>%
        mutate(ECO_SECTOR_AGR_PROP = ECO_SECTOR_AGR/ECO_SECTOR_TOTAL*100)


agr_total_nac <- agr_total_nac %>%
        #select(-label_agg,-label, -indicator, -indicator_id) %>%
        pivot_longer(ECO_SECTOR_TOTAL:ECO_SECTOR_AGR_PROP,
                     names_to="classif2") %>%
        select(ref_area, time, classif2, value)

agr_total_nac_agg <- agr_total_nac %>%
        group_by(ref_area, classif2) %>%
        summarise(mean_value = mean(value, na.rm = TRUE)
        ) %>%
        ungroup()

agr_total_nac_agg_pivot <- agr_total_nac_agg %>%
        pivot_wider(names_from = classif2,
                    values_from = mean_value)

## CÁLCULO DE PESO DE PEQUEÑA PROPIEDAD A NIVEL PAIS
indeptes_agro <- df_catocup_ %>%
        filter(label_agg %in% c("Peq_prod_Agro_N", "Total") &
                       classif2=="ECO_SECTOR_AGR")

indeptes_agro_agg <- indeptes_agro %>%
        group_by(ref_area, time, label_agg) %>%
        summarise(value=sum(obs_value, na.rm=TRUE)) %>%
        ungroup() %>%
        pivot_wider(names_from=label_agg,
                    values_from = value) %>%
        mutate(Peq_prod_Agro_P = Peq_prod_Agro_N / Total *100) %>%
        select(-Total) %>%
        pivot_longer(Peq_prod_Agro_N:Peq_prod_Agro_P,
                     names_to="label_agg")

indeptes_agro_agg_pivot <- indeptes_agro_agg %>%
        group_by(ref_area, label_agg) %>%
        summarise(mean_value = mean(value, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        pivot_wider(names_from = label_agg,
                    values_from = mean_value)

## UNION DE TABLAS DE TOTAL Y DE PEQUEÑA PROPIEDAD
df_peqprop_final <- agr_total_nac_agg_pivot %>%
        left_join(indeptes_agro_agg_pivot) %>%
        rename(iso3c = ref_area,
               Pob_Agric_N = ECO_SECTOR_AGR,
               Pob_Agric_P = ECO_SECTOR_AGR_PROP,
               Pob_Ocup_N = ECO_SECTOR_TOTAL) %>%
        select(iso3c, everything())

df_peqprop_final <- df_peqprop_final %>%
        left_join(country_classif) %>%
        select(iso3c, country:ocde, everything()) 
        

## UNION TABLA SPR LATENTE FINAL
spr_latente <- df_peqprop_final %>%
        left_join(pobreza_rural_final %>% 
                          select(iso3c, SH.H2O.BASW.RU.ZS:SP.RUR.TOTL.ZS)
                  )

write_csv(spr_latente, './data/tablas_finales/spr_latente_ind_final.csv')
haven::write_sav(spr_latente, './data/tablas_finales/spr_latente_ind_final.sav')
openxlsx::write.xlsx(spr_latente, './data/tablas_finales/spr_latente_ind_final.xlsx')

## Testeo media pesada Pequeña prop. agrícola (peso población ocupada total)
spr_latente %>%
        filter(
                peq_estado != "Peq. estado"
                ) %>%
        group_by(cluster_pimsa) %>%
        summarise(
                mean_peq_prop_N = mean(Peq_prod_Agro_P, na.rm=TRUE), #media comun
                w_mean_peq_prop_N = weighted.mean(Peq_prod_Agro_P, w=Pob_Ocup_N, , na.rm=TRUE), #media pesada
                  n = n()
                  )


## Testeo media pesada indicadores de pobreza rural (peso población total)
spr_latente %>%
        filter(
                peq_estado != "Peq. estado"
                ) %>%
        group_by(cluster_pimsa) %>%
        summarise(across(SH.H2O.BASW.RU.ZS:SL.AGR.EMPL.ZS,
                         list(
                                 mean = function(x) mean(x, na.rm=TRUE),
                                 w_mean = function(x) weighted.mean(x, w=SP.POP.TOTL, na.rm=TRUE),
                                 n = function(x) n()
                         ))
        )
