library(Rilostat)
library(tidyverse)
library(googlesheets4)
library(wbstats)
library(gt)



## Uso la librería wbstats para descargar los datos
desoc <- wb_data(c("SL.UEM.TOTL.ZS","SL.TLF.TOTL.IN"),
                 start_date=1991,
                 end_date=2023)



desoc <- desoc %>%
        filter(!is.na(SL.UEM.TOTL.ZS)) %>%
        #filter(!is.na())
        #select(-c(unit:last_updated)) %>%
        rename(p_desoc = SL.UEM.TOTL.ZS,
               pea = SL.TLF.TOTL.IN) %>%
        mutate(abs_desoc = p_desoc*pea/100)

# country_classif <- read_csv('./data/ouputs/country_classification.csv') 


desoc <- desoc %>%
        left_join(country_classif %>% select(-country), by="iso3c") %>%
        select(iso3c:country, region:ocde, everything()) 

# desoc_agg <- desoc %>%
#        arrange(iso3c, date) %>%
#        filter(date >= 2009) %>% # Filtra los años 2009-2019 para que sea consistente con el resto de los análisis
#        group_by(iso3c, country, region, income_group, cluster_pimsa,
#                 peq_estado, excl_tamaño) %>%
#        summarise(mean_desoc_w = weighted.mean(p_desoc,pea),
#                  mean_desoc = mean(p_desoc),
#                  sd = sd(p_desoc),
#                  pea = mean(pea),
#                  #max_value = max(p_desoc),
#                  #min_value = min(p_desoc),
#                  #first_year_value = p_desoc[which.min(date)],
#                  #last_year_value = p_desoc[which.max(date)],
#                  #max_value_year = date[which.max(p_desoc)],
#                  #min_value_year = date[which.min(p_desoc)],
#                  #serie = list(p_desoc)
#        ) %>%
        ungroup() 
#%>%
#        mutate(evolution = (((last_year_value / first_year_value) - 1)),
#               range = max_value - min_value)

# Desocupación por año, promedio simple y ponderado
desocgraf1 <- desoc %>%
        filter(!is.na(p_desoc), !is.na(pea), income_group_2 != "99_Sin_datos",
        peq_estado != "Peq. estado",   excl_tamaño != "Excluible") %>%
        group_by(date) %>%
        summarise(
                desoc_simple = mean(p_desoc, na.rm = TRUE),  # Promedio simple
                desoc_ponderada = weighted.mean(p_desoc, pea, na.rm = TRUE)  # Promedio ponderado
        ) %>%
        ggplot() +
        geom_line(aes(x = date, y = desoc_simple, color = "Promedio simple"), linewidth = 1, linetype = "solid") +  # Línea promedio simple
        geom_line(aes(x = date, y = desoc_ponderada, color = "Promedio ponderado"), linewidth = 1, linetype = "dashed") +  # Línea promedio ponderado
        labs(
                x = "Año",
                y = "Tasa de desocupación",
                title = "Tasa de desocupación. Promedio simple y ponderado. 1991-2023",
                color = "Tipo de Promedio"
        ) +
        theme_minimal() +
        scale_color_manual(values = c("Promedio simple" = "steelblue", "Promedio ponderado" = "orange")) +
        scale_y_continuous(
                limits = c(0, NA),
                breaks = seq(0, 10, by = 0.5)
        ) +
        theme(legend.position = "bottom")

ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desocgraf1.jpg", 
       plot = desocgraf1, width = 8, height = 6, dpi = 300)

# Desocupación por año y región, promedio simple y ponderado
desocgraf2 <- desoc %>%
        filter(!is.na(p_desoc), !is.na(pea), !is.na(region), income_group_2 != "99_Sin_datos",
        peq_estado != "Peq. estado",   excl_tamaño != "Excluible") %>%
        group_by(date, region) %>%
        summarise(
                `Promedio simple` = mean(p_desoc, na.rm = TRUE),
                `Promedio ponderado` = weighted.mean(p_desoc, pea, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        pivot_longer(cols = c(`Promedio simple`, `Promedio ponderado`),
                     names_to = "Tipo", values_to = "Desocupación") %>%
        ggplot(aes(x = date, y = Desocupación, color = region)) +
        geom_line(linewidth = 1) +
        facet_wrap(~ Tipo, nrow = 1) +
        scale_y_continuous(limits = c(0, NA), breaks = seq(0, 10, by = 1)) +
        scale_color_manual(
                values = c(
                        "East Asia & Pacific" = "#1f77b4",
                        "Latin America & Caribbean" = "#ff7f0e",
                        "North America" = "#2ca02c",
                        "Sub-Saharan Africa" = "#d62728",
                        "Europe & Central Asia" = "#9467bd",
                        "Middle East & North Africa" = "#8c564b",
                        "South Asia" = "#e377c2"
                ),
                labels = c(
                        "East Asia & Pacific" = "Asia Oriental y Pacífico",
                        "Latin America & Caribbean" = "Latinoamérica y Caribe",
                        "North America" = "Norteamérica",                                         
                        "Sub-Saharan Africa" = "África Subsahariana",
                        "Europe & Central Asia" = "Europa y Asia Central",
                        "Middle East & North Africa" = "Medio Oriente y Norte de África",
                        "South Asia" = "Sur de Asia"
                )
        ) +
        labs(
                x = "Año",
                y = "Tasa de desocupación",
                title = "Tasa de desocupación. Promedio simple y ponderado según regiones. 1991-2023",
                color = "Región"
        ) +
                theme_minimal() +
                theme(
                        legend.position = "bottom",
                )

ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desocgraf2.jpg", 
       plot = desocgraf2, width = 8, height = 6, dpi = 300)

desoc_tabla1 <- desoc %>%
        filter(
                !is.na(p_desoc), !is.na(pea), !is.na(region),
                income_group_2 != "99_Sin_datos",
                peq_estado != "Peq. estado",
                excl_tamaño != "Excluible"
        ) %>%
        group_by(date, region) %>%
        summarise(
                simple = mean(p_desoc, na.rm = TRUE),
                ponderado = weighted.mean(p_desoc, pea, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        pivot_longer(cols = c(ponderado, simple), names_to = "tipo", values_to = "valor") %>%  # ordenamos aquí
        group_by(region, tipo) %>%
        summarise(
                promedio = mean(valor, na.rm = TRUE),
                minimo = min(valor, na.rm = TRUE),
                maximo = max(valor, na.rm = TRUE),
                rango = maximo - minimo,
                .groups = "drop"
        ) %>%
        pivot_wider(
                names_from = tipo,
                values_from = c(promedio, minimo, maximo, rango),
                names_glue = "{tipo}_{.value}"
        ) %>%
        select(
                region,
                starts_with("ponderado_"),
                starts_with("simple_")
        )



install.packages("gt")
library(gt)

desoc_tabla1_gt <- desoc_tabla1 %>%
        gt() %>%
        tab_header(
                title = "Resumen de la tasa de desocupación por región",
                subtitle = "Promedios, mínimos, máximos y rangos para promedios ponderados y simples"
        ) %>%
        cols_label(
                region = "Región",
                ponderado_promedio = "Pond. Promedio",
                ponderado_minimo = "Pond. Mínimo",
                ponderado_maximo = "Pond. Máximo",
                ponderado_rango = "Pond. Rango",
                simple_promedio = "Simple Promedio",
                simple_minimo = "Simple Mínimo",
                simple_maximo = "Simple Máximo",
                simple_rango = "Simple Rango"
        ) %>%
        fmt_number(
                columns = where(is.numeric),
                decimals = 1
        ) %>%
        tab_options(
                table.font.size = "small",
                heading.title.font.size = 14,
                heading.subtitle.font.size = 12
        )

# Mostrar en RStudio Viewer
desoc_tabla1_gt

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