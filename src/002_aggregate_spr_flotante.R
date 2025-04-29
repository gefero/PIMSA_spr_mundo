library(Rilostat)
library(tidyverse)
library(googlesheets4)
library(wbstats)
library(gt)

## Uso la librería wbstats para descargar los datos
desoc <- wb_data(c("SL.UEM.TOTL.ZS","SL.TLF.TOTL.IN"),
                 start_date=1991,
                 end_date=2019)



desoc <- desoc %>%
        filter(!is.na(SL.UEM.TOTL.ZS)) %>%
        #filter(!is.na())
        #select(-c(unit:last_updated)) %>%
        rename(p_desoc = SL.UEM.TOTL.ZS,
               pea = SL.TLF.TOTL.IN) %>%
        mutate(abs_desoc = p_desoc*pea/100)

country_classif <- read_csv('./data/ouputs/country_classification.csv') 


desoc <- desoc %>%
        left_join(country_classif %>% select(-country), by="iso3c") %>%
        select(iso3c:country, region:ocde, everything())

desoc_agg <- desoc %>%
        arrange(iso3c, date) %>%
        filter(date >= 2009) %>% # Filtra los años 2009-2019 para que sea consistente con el resto de los análisis
        group_by(iso3c, country, region, income_group, cluster_pimsa,
                 peq_estado, excl_tamaño) %>%
        summarise(mean_desoc_w = weighted.mean(p_desoc,pea),
                  mean_desoc = mean(p_desoc),
                  sd = sd(p_desoc),
                  pea = mean(pea),
                  #max_value = max(p_desoc),
                  #min_value = min(p_desoc),
                  #first_year_value = p_desoc[which.min(date)],
                  #last_year_value = p_desoc[which.max(date)],
                  #max_value_year = date[which.max(p_desoc)],
                  #min_value_year = date[which.min(p_desoc)],
                  #serie = list(p_desoc)
        ) %>%
        ungroup() 
#%>%
#        mutate(evolution = (((last_year_value / first_year_value) - 1)),
#               range = max_value - min_value)

################################################################################

## CALCULAR PUNTO INTERMEDIO LA CANTIDAD TOTAL DE DESOC PARA 2010 y 2019
##



desoc %>%
        filter(date >= 2009) %>%
        group_by(cluster_pimsa) %>%
        summarise(mean_w =  weighted.mean(p_desoc,pea),
                  mean_sw = mean(p_desoc)
        )









desoc %>%
#        mutate(abs_desoc = p_desoc*pea) %>%
        arrange(iso3c, date) %>%
        group_by(cluster_pimsa, date) %>%
        summarise(mean_w = weighted.mean(p_desoc, pea),
                  mean_sw = mean(p_desoc)) %>%
        #filter(income_group != "99_Sin_datos") %>%
        pivot_longer(
                cols = c(mean_w, mean_sw),
                names_to = "indicador") %>%
        ungroup() %>%
        ggplot() + 
                geom_line(aes(x=date, y=value, color=cluster_pimsa)) +
                facet_wrap(~indicador)

desoc_agg %>%
        select(-serie) %>%
        write_csv('./data/tablas_finales/spr_flotante_desoc.csv')

desoc_agg %>%
        select(-serie) %>%
        haven::write_sav('./data/tablas_finales/spr_flotante_desoc.sav')

desoc_agg %>%
        select(-serie) %>%
        openxlsx::write.xlsx('./data/tablas_finales/spr_flotante_desoc.xlsx')

########

desoc_agg %>%
        arrange(cluster_pimsa, iso3c) %>%
        gt() %>% 
        cols_label(
                iso3c = "Country code",
                mean_value = 'Tasa desoc. (media)',
                max_value = 'Tasa desoc. (valor max)',
                min_value = 'Tasa desoc. (valor min)',
                max_value_year = 'Tasa desoc. (año con valor max)',
                min_value_year = 'Tasa desoc. (año con valor min)'
        ) %>%
        fmt_percent(columns = mean_value:min_value) %>%
        gtExtras::gt_plt_sparkline(column = serie,
                                   fig_dim = c(20, 40)
        )


plot <- desoc %>%
        ggplot() +
        geom_line(aes(x=date, y=p_desoc, color=income_group_2, group=country)) +
        scale_color_viridis_d() +
        theme_minimal() +
        facet_wrap(~cluster_pimsa)

plotly::ggplotly(plot)


## Hacer tasa desoc (ponderada y simple) por región, cluster y grupo de ingresos
## % de desocupados (distribución mundial) por región, cluster y grupo de ingresos
#https://docs.google.com/document/d/1rnUBeGKQgC4VGMxAqBbovCrYaEopJDXkg3NvSR2s_fQ/edit?tab=t.0
 

desoc %>%
        group_by(region) %>%
        summarise(`Tasa de desoc. (pond. PEA)` = weighted.mean(p_desoc, pea),
                  `Tasa de desoc.` = mean(p_desoc)) %>%
        writexl::write_xlsx('./data/tabs/sprflotante_tasa_desoc_region.xlsx')

desoc %>%
        group_by(income_group_2) %>%
        summarise(`Tasa de desoc. (pond. PEA)` = weighted.mean(p_desoc, pea),
                  `Tasa de desoc.` = mean(p_desoc)) %>%
        writexl::write_xlsx('./data/tabs/sprflotante_tasa_desoc_income.xlsx')


desoc %>%
        group_by(cluster_pimsa) %>%
        summarise(`Tasa de desoc. (pond. PEA)` = weighted.mean(p_desoc, pea),
                  `Tasa de desoc.` = mean(p_desoc)
        ) %>%
        writexl::write_xlsx('./data/tabs/sprflotante_tasa_desoc_clstpimsa.xlsx')


desoc %>%
        filter(date %in% c(2010, 2019)) %>%
        group_by(date, region) %>%
        summarise(n_desoc = sum(abs_desoc)) %>%
        mutate(p_desoc = n_desoc / sum(n_desoc)) %>%
        ungroup() %>%
        pivot_longer(cols = c(n_desoc, p_desoc)) %>%
        pivot_wider(names_from = c(date, name),
                    values_from=value) %>%
        select(contains("n"), contains("p")) %>%
        writexl::write_xlsx('./data/tabs/sprflotante_20102019_desoc_region.xlsx')

        

desoc %>%
        filter(date %in% c(2010, 2019)) %>%
        group_by(date, income_group_2) %>%
        summarise(n_desoc = sum(abs_desoc)) %>%
        mutate(p_desoc = n_desoc / sum(n_desoc)) %>%
        ungroup() %>%
        pivot_longer(cols = c(n_desoc, p_desoc)) %>%
        pivot_wider(names_from = c(date, name),
                    values_from=value) %>%
        select(contains("n"), contains("p")) %>%
        writexl::write_xlsx('./data/tabs/sprflotante_20102019_desoc_income.xlsx')

 desoc %>%
        filter(date %in% c(2010, 2019)) %>%
        group_by(date, cluster_pimsa) %>%
        summarise(n_desoc = sum(abs_desoc)) %>%
        mutate(p_desoc = n_desoc / sum(n_desoc)) %>%
        ungroup() %>%
        pivot_longer(cols = c(n_desoc, p_desoc)) %>%
        pivot_wider(names_from = c(date, name),
                    values_from=value) %>%
        select(cluster_pimsa, contains("n"), contains("p")) %>%
         writexl::write_xlsx('./data/tabs/sprflotante_20102019_desoc_clstpimsa.xlsx')
 


#https://rfortherestofus.com/2024/02/sparklines-gt