library(Rilostat)
library(tidyverse)
library(googlesheets4)
library(wbstats)
library(gt)
library(writexl)

library(scales)

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

# Desocupación. Indicadores seleccionados por regiones
desoc_tabla1 <- desoc %>%
        filter(!is.na(p_desoc), !is.na(pea), !is.na(region),
               income_group_2 != "99_Sin_datos",
               peq_estado != "Peq. estado",
               excl_tamaño != "Excluible") %>%
        group_by(region, date) %>%
        summarise(
                `Promedio simple` = mean(p_desoc, na.rm = TRUE),
                `Promedio ponderado` = weighted.mean(p_desoc, pea, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        group_by(region) %>%
        summarise(
                ponderado_promedio = mean(`Promedio ponderado`, na.rm = TRUE),
                ponderado_minimo = min(`Promedio ponderado`, na.rm = TRUE),
                ponderado_maximo = max(`Promedio ponderado`, na.rm = TRUE),
                ponderado_rango = ponderado_maximo - ponderado_minimo,
                simple_promedio = mean(`Promedio simple`, na.rm = TRUE),
                simple_minimo = min(`Promedio simple`, na.rm = TRUE),
                simple_maximo = max(`Promedio simple`, na.rm = TRUE),
                simple_rango = simple_maximo - simple_minimo,
                .groups = "drop"
        ) %>%
        mutate(
                region = recode(region,
                                "East Asia & Pacific" = "Asia Oriental y Pacífico",
                                "Latin America & Caribbean" = "Latinoamérica y Caribe",
                                "North America" = "Norteamérica",
                                "Sub-Saharan Africa" = "África Subsahariana",
                                "Europe & Central Asia" = "Europa y Asia Central",
                                "Middle East & North Africa" = "Medio Oriente y Norte de África",
                                "South Asia" = "Sur de Asia"
                )
        ) %>%
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

desoc_tabla1_df <- as.data.frame(desoc_tabla1)
write_xlsx(desoc_tabla1_df, "C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desoc_tabla1.xlsx")


# Tasas de desocupación promedio simmple y ponderado por ingresos
desocgraf3 <- desoc %>%
        filter(
                !is.na(p_desoc), !is.na(pea), !is.na(income_group_2),
                income_group_2 != "99_Sin_datos",
                peq_estado != "Peq. estado",
                excl_tamaño != "Excluible"
        ) %>%
        group_by(date, income_group_2) %>%
        summarise(
                `Promedio simple` = mean(p_desoc, na.rm = TRUE),
                `Promedio ponderado` = weighted.mean(p_desoc, pea, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        pivot_longer(cols = c(`Promedio simple`, `Promedio ponderado`),
                     names_to = "Tipo", values_to = "Desocupación") %>%
        ggplot(aes(x = date, y = Desocupación, color = income_group_2)) +
        geom_line(linewidth = 1) +
        facet_wrap(~ Tipo, nrow = 1) +
        scale_y_continuous(limits = c(0, NA), breaks = seq(0, 12, by = 1)) +
        scale_color_manual(
                values = c(
                        "01 Altos ingresos" = "#1f77b4",
                        "02 Medios-altos ingresos" = "#ff7f0e",
                        "03 Medios-bajos ingresos" = "#2ca02c",
                        "04 Bajos ingresos" = "#d62728"
                ),
                labels = c(
                        "01 Altos ingresos" = "Altos",
                        "02 Medios-altos ingresos" = "Medio-altos",
                        "03 Medios-bajos ingresos" = "Medio-bajos",
                        "04 Bajos ingresos" = "Bajos"
                )
        ) +
        labs(
                x = "Año",
                y = "Tasa de desocupación",
                title = "Tasa de desocupación. Promedio simple y ponderado según grupo de ingreso. 1991-2023",
                color = "Grupo de ingreso"
        ) +
        theme_minimal() +
        theme(
                legend.position = "bottom",
                panel.grid.minor = element_blank()
        )

ggsave(
        filename = "C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desocgraf3.jpg",
        plot = desocgraf3,
        width = 8,
        height = 6,
        dpi = 300
)

# Desocupación. Indicadores seleccionados según grupo de ingreso
desoc_tabla2_data <- desoc %>%
        filter(
                !is.na(p_desoc), !is.na(pea), !is.na(income_group_2),
                income_group_2 != "99_Sin_datos",
                peq_estado != "Peq. estado",
                excl_tamaño != "Excluible"
        ) %>%
        group_by(income_group_2, date) %>%
        summarise(
                simple = mean(p_desoc, na.rm = TRUE),
                ponderado = weighted.mean(p_desoc, pea, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        group_by(income_group_2) %>%
        summarise(
                `Promedio ponderado` = mean(ponderado, na.rm = TRUE),
                `Mínimo ponderado` = min(ponderado, na.rm = TRUE),
                `Máximo ponderado` = max(ponderado, na.rm = TRUE),
                `Diferencia ponderado` = max(ponderado, na.rm = TRUE) - min(ponderado, na.rm = TRUE),
                `Promedio simple` = mean(simple, na.rm = TRUE),
                `Mínimo simple` = min(simple, na.rm = TRUE),
                `Máximo simple` = max(simple, na.rm = TRUE),
                `Diferencia simple` = max(simple, na.rm = TRUE) - min(simple, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        mutate(`Grupo de ingreso` = case_when(
                income_group_2 == "01 Altos ingresos" ~ "Altos ingresos",
                income_group_2 == "02 Medios-altos ingresos" ~ "Medios-altos ingresos",
                income_group_2 == "03 Medios-bajos ingresos" ~ "Medios-bajos ingresos",
                income_group_2 == "04 Bajos ingresos" ~ "Bajos ingresos"
        )) %>%
        select(`Grupo de ingreso`, everything(), -income_group_2)

write_xlsx(desoc_tabla2_data,
           path = "C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desoc_tabla2.xlsx")

desoc_tabla2 <- desoc_tabla2_data %>%
        gt() %>%
        fmt_number(decimals = 1) %>%
        tab_header(
                title = "Tasa de desocupación por grupo de ingreso",
                subtitle = "Promedio simple y ponderado. 1991–2023"
        )

# Tasas de desocupación promedio simmple y ponderado por cluster pimsa
desocgraf4 <- desoc %>%
        filter(!is.na(p_desoc), !is.na(pea), !is.na(cluster_pimsa), income_group_2 != "99_Sin_datos",
               peq_estado != "Peq. estado", excl_tamaño != "Excluible") %>%
        group_by(date, cluster_pimsa) %>%
        summarise(
                `Promedio simple` = mean(p_desoc, na.rm = TRUE),
                `Promedio ponderado` = weighted.mean(p_desoc, pea, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        pivot_longer(cols = c(`Promedio simple`, `Promedio ponderado`),
                     names_to = "Tipo", values_to = "Desocupación") %>%
        ggplot(aes(x = date, y = Desocupación, color = cluster_pimsa)) +
        geom_line(linewidth = 1) +
        facet_wrap(~ Tipo, nrow = 1) +
        scale_y_continuous(limits = c(0, NA), breaks = seq(0, 12, by = 1)) +
        scale_color_manual(
                values = c(
                        "C4. Cap. escasa extensión c/peso campo" = "#1f77b4",  # Cambia estos colores según los clusters
                        "C1. Cap. avanzado" = "#ff7f0e",
                        "C3. Cap. extensión c/peso campo" = "#2ca02c",
                        "C5. Pequeña propiedad en el campo" = "#d62728",
                        "C2. Cap. extensión reciente c/desarrollo profundidad" = "#9467bd"
                ),
                labels = c(
                        "C4. Cap. escasa extensión c/peso campo" = "Grupo 4",
                        "C1. Cap. avanzado" = "Grupo 1",
                        "C3. Cap. extensión c/peso campo" = "Grupo 3",
                        "C5. Pequeña propiedad en el campo" = "Grupo 5",
                        "C2. Cap. extensión reciente c/desarrollo profundidad" = "Grupo 2"
                )
        ) +
        labs(
                x = "Año",
                y = "Tasa de desocupación",
                title = "Tasa de desocupación.\n Promedio simple y ponderado según desarrollo en extensión y profundidad. 1991-2023",
                color = "Tipología"
        ) +
        theme_minimal() +
        theme(
                legend.position = "bottom"
        )

ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desocgraf4.jpg", 
       plot = desocgraf4, width = 8, height = 6, dpi = 300)

# Desocupación. Indicadores seleccionados según cluster pimsa


desoc_tabla3 <- desoc %>%
        filter(
                !is.na(p_desoc), !is.na(pea), !is.na(cluster_pimsa),
                income_group_2 != "99_Sin_datos",
                peq_estado != "Peq. estado",
                excl_tamaño != "Excluible"
        ) %>%
        group_by(cluster_pimsa, date) %>%
        summarise(
                ponderado = weighted.mean(p_desoc, pea, na.rm = TRUE),
                simple = mean(p_desoc, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        group_by(cluster_pimsa) %>%
        summarise(
                `Promedio ponderado` = mean(ponderado, na.rm = TRUE),
                `Mínimo ponderado` = min(ponderado, na.rm = TRUE),
                `Máximo ponderado` = max(ponderado, na.rm = TRUE),
                `Diferencia ponderado` = max(ponderado, na.rm = TRUE) - min(ponderado, na.rm = TRUE),
                `Promedio simple` = mean(simple, na.rm = TRUE),
                `Mínimo simple` = min(simple, na.rm = TRUE),
                `Máximo simple` = max(simple, na.rm = TRUE),
                `Diferencia simple` = max(simple, na.rm = TRUE) - min(simple, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        mutate(`Cluster PIMSA` = cluster_pimsa) %>%
        select(`Cluster PIMSA`, everything(), -cluster_pimsa) 

write_xlsx(desoc_tabla3,
           path = "C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desoc_tabla3.xlsx")

desoc_tabla_cluster_data_final %>%
        gt() %>%
        fmt_number(decimals = 1) %>%
        tab_header(
                title = "Tasa de desocupación por cluster PIMSA",
                subtitle = "Promedio simple y ponderado. 1991–2023"
        )

# Desocupación según  clusters pimsa. Bloxplot y puntos.1999 y 2010

desocgraf5 <- desoc %>%
        filter(
                date %in% c(1999, 2010),
                !is.na(p_desoc), !is.na(pea), !is.na(cluster_pimsa),
                income_group_2 != "99_Sin_datos",
                peq_estado != "Peq. estado",
                excl_tamaño != "Excluible"
        ) %>%
        mutate(cluster_pimsa = case_when(
                str_detect(cluster_pimsa, "^C1") ~ "Grupo 1",
                str_detect(cluster_pimsa, "^C2") ~ "Grupo 2",
                str_detect(cluster_pimsa, "^C3") ~ "Grupo 3",
                str_detect(cluster_pimsa, "^C4") ~ "Grupo 4",
                str_detect(cluster_pimsa, "^C5") ~ "Grupo 5",
                TRUE ~ cluster_pimsa
        )) %>%
        ggplot(aes(x = cluster_pimsa, y = p_desoc)) +
        geom_boxplot(outlier.shape = NA, fill = "gray90") +
        geom_jitter(aes(size = pea, color = cluster_pimsa), width = 0.2, alpha = 0.6) +
        facet_wrap(~ date) +
        coord_flip() +
        labs(
                title = "Tasa de desocupación por tipología de desarrollo capitalista\n en extensión y profundidad. 1999 y 2010",
                x = "Tipología",
                y = "Tasa de desocupación (%)"
        ) +
        scale_size_continuous(
                name = "PEA (millones)",
                labels = label_number(scale = 1e-6, suffix = " M", accuracy = 0.1)
        ) +
        scale_color_manual(
                name = "Grupo PIMSA",
                values = c(
                        "Grupo 1" = "#ff7f0e",
                        "Grupo 2" = "#9467bd",
                        "Grupo 3" = "#2ca02c",
                        "Grupo 4" = "#1f77b4",
                        "Grupo 5" = "#d62728"
                )
        ) +
        theme_minimal() +
        theme(
                legend.position = "bottom",
                legend.box = "vertical",  # Asegura que las leyendas se apilen verticalmente
                legend.box.spacing = unit(0.5, "cm"),  # Espacio entre las leyendas
                legend.title = element_text(hjust = 0.5)  # Alinea el título de las leyendas
        ) +
        guides(
                color = guide_legend(title = "Tipología"),
                size = guide_legend(title = "PEA (millones)")
        )
ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desocgraf5.jpg", 
       plot = desocgraf5, width = 8, height = 6, dpi = 300)

# Desocupación según  clusters pimsa. Indicadores seleccionados.1999 y 2010 
desoc_tabla4 <- desoc %>%
        filter(
                date %in% c(1999, 2010),
                !is.na(p_desoc), !is.na(pea), !is.na(cluster_pimsa),
                income_group_2 != "99_Sin_datos",
                peq_estado != "Peq. estado",
                excl_tamaño != "Excluible"
        ) %>%
        mutate(cluster_pimsa = case_when(
                str_detect(cluster_pimsa, "^C1") ~ "Grupo 1",
                str_detect(cluster_pimsa, "^C2") ~ "Grupo 2",
                str_detect(cluster_pimsa, "^C3") ~ "Grupo 3",
                str_detect(cluster_pimsa, "^C4") ~ "Grupo 4",
                str_detect(cluster_pimsa, "^C5") ~ "Grupo 5",
                TRUE ~ cluster_pimsa
        )) %>%
        group_by(cluster_pimsa, date) %>%
        summarise(
                media_pond = weighted.mean(p_desoc, pea, na.rm = TRUE),
                desvio_pond = sqrt(sum(pea * (p_desoc - weighted.mean(p_desoc, pea, na.rm = TRUE))^2, na.rm = TRUE) / sum(pea, na.rm = TRUE)),
                promedio = mean(p_desoc, na.rm = TRUE),
                desvio_std = sd(p_desoc, na.rm = TRUE),
                p25 = quantile(p_desoc, 0.25, na.rm = TRUE),
                mediana = median(p_desoc, na.rm = TRUE),
                p75 = quantile(p_desoc, 0.75, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        arrange(date, cluster_pimsa)
write_xlsx(desoc_tabla4,
           path = "C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desoc_tabla4.xlsx")


############

# Distribución de desocupados

write_xlsx( list(
        "Por región" = desoc %>%
                filter(!is.na(region), !is.na(abs_desoc), !is.na(country)) %>%
                group_by(region) %>%
                summarise(
                        `Desocupados absolutos` = sum(abs_desoc, na.rm = TRUE),
                        `Cantidad de países` = n_distinct(country),
                        .groups = "drop"
                ) %>%
                mutate(
                        `Distribución % desocupados` = round(100 * `Desocupados absolutos` / sum(`Desocupados absolutos`), 1),
                        `Distribución % países` = round(100 * `Cantidad de países` / sum(`Cantidad de países`), 1)
                ) %>%
                select(region, `Desocupados absolutos`, `Distribución % desocupados`, `Cantidad de países`, `Distribución % países`),
        
        "Por grupo de ingreso" = desoc %>%
                filter(!is.na(income_group_2), income_group_2 != "99_Sin_datos", !is.na(abs_desoc), !is.na(country)) %>%
                group_by(income_group_2) %>%
                summarise(
                        `Desocupados absolutos` = sum(abs_desoc, na.rm = TRUE),
                        `Cantidad de países` = n_distinct(country),
                        .groups = "drop"
                ) %>%
                mutate(
                        `Distribución % desocupados` = round(100 * `Desocupados absolutos` / sum(`Desocupados absolutos`), 1),
                        `Distribución % países` = round(100 * `Cantidad de países` / sum(`Cantidad de países`), 1)
                ) %>%
                select(income_group_2, `Desocupados absolutos`, `Distribución % desocupados`, `Cantidad de países`, `Distribución % países`),
        
        "Por cluster PIMSA" = desoc %>%
                filter(!is.na(cluster_pimsa), !is.na(abs_desoc), !is.na(country)) %>%
                group_by(cluster_pimsa) %>%
                summarise(
                        `Desocupados absolutos` = sum(abs_desoc, na.rm = TRUE),
                        `Cantidad de países` = n_distinct(country),
                        .groups = "drop"
                ) %>%
                mutate(
                        `Distribución % desocupados` = round(100 * `Desocupados absolutos` / sum(`Desocupados absolutos`), 1),
                        `Distribución % países` = round(100 * `Cantidad de países` / sum(`Cantidad de países`), 1)
                ) %>%
                select(cluster_pimsa, `Desocupados absolutos`, `Distribución % desocupados`, `Cantidad de países`, `Distribución % países`)
                ),
                path = "C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desoc_absoluto.xlsx"
                )


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