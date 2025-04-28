library(tidyverse)
library(Rilostat)
library(wbstats)
latente <- c(
        "SP.RUR.TOTL.ZS", 
        "SP.RUR.TOTL",
        "SH.H2O.BASW.RU.ZS",
        "SH.STA.BASS.RU.ZS",
        "SL.AGR.EMPL.ZS", 
        "EMP_TEMP_SEX_STE_ECO_NB_A"
)

names <- c(
        "SP.RUR.TOTL.ZS"= "PobrurP", 
        "SP.RUR.TOTL"= "PobrurN",
        "SH.H2O.BASW.RU.ZS" = "Pobruragua",
        "SH.STA.BASS.RU.ZS" = "Pobrursanit",
        "SL.AGR.EMPL.ZS" = "EmpagrP"
) %>% 
        enframe(name="indicator_id", value="newname")


## Uso la librería wbstats para descargar los datos
latente_wb <- wb_data(latente[1:4], start_date = 2009, end_date = 2019, return_wide = FALSE)

pobreza_rural <-latente_wb %>%
        group_by(iso3c, country, indicator_id) %>%
        summarise(value_raw = mean(value, na.rm=TRUE)) %>%
        pivot_wider(names_from=indicator_id, 
                    values_from = value_raw) %>%
        drop_na() %>%
        ungroup() %>%
        mutate(SH.H2O.BASW.RU.ZS = 100 - SH.H2O.BASW.RU.ZS,
               SH.STA.BASS.RU.ZS = 100 - SH.STA.BASS.RU.ZS) 


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
                classif1 %in% c("STE_ICSE93_3", "STE_ICSE93_4", "STE_ICSE93_5") ~ "PeqpropagrN",
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

indeptes_agro <- df_catocup_ %>%
        filter(label_agg %in% c("PeqpropagrN", "Total") &
                       classif2=="ECO_SECTOR_AGR")

indeptes_agro_agg <- indeptes_agro %>%
        group_by(ref_area, time, label_agg) %>%
        summarise(value=sum(obs_value)) %>%
        ungroup() %>%
        pivot_wider(names_from=label_agg,
                    values_from = value) %>%
        mutate(PeqpropagrP = PeqpropagrN / Total *100) %>%
        select(-Total) %>%
        pivot_longer(PeqpropagrN:PeqpropagrP,
                     names_to="label_agg")

indeptes_agro_agg_pivot <- indeptes_agro_agg %>%
        group_by(ref_area, label_agg) %>%
        summarise(mean_value = mean(value, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        pivot_wider(names_from = label_agg,
                    values_from = mean_value)

df_peqprop_final <- agr_total_nac_agg_pivot %>%
        left_join(indeptes_agro_agg_pivot) %>%
        rename(iso3c = ref_area) %>%
        left_join(pobreza_rural) %>%
        select(iso3c, country, everything())
