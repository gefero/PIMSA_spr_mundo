library(Rilostat)
library(tidyverse)
library(googlesheets4)
library(wbstats)
library(gt)
library(writexl)

library(scales)
library(Rilostat)

library(plotly)
library(stringr)

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

# Cantidad de países según cluster pimsa y rangos de desocupación, 1999 y 2010
desoc_tabla5 <- desoc %>%
        filter(
                date %in% c(1999, 2010),
                !is.na(p_desoc), !is.na(pea), !is.na(cluster_pimsa),
                income_group_2 != "99_Sin_datos",
                peq_estado != "Peq. estado",
                excl_tamaño != "Excluible"
        ) %>%
        mutate(
                rango_desoc = case_when(
                        p_desoc <= 5 ~ "0-5%",
                        p_desoc <= 10 ~ "5,1-10%",
                        p_desoc <= 15 ~ "10,1-15%",
                        p_desoc <= 20 ~ "15,1-20%",
                        p_desoc > 20 ~ "Más de 20%"
                ),
                rango_desoc = factor(rango_desoc, levels = c("0-5%", "5,1-10%", "10,1-15%", "15,1-20%", "Más de 20%"))
        ) %>%
        distinct(date, country, cluster_pimsa, rango_desoc) %>%
        count(date, cluster_pimsa, rango_desoc, name = "n_paises") %>%
        tidyr::pivot_wider(
                names_from = rango_desoc,
                values_from = n_paises,
                values_fill = 0
        ) %>%
        arrange(date, cluster_pimsa)
write_xlsx(desoc_tabla5,
           path = "C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desoc_tabla5.xlsx")

# Enumeración de países según cluster pimsa y rangos de desocupación, 1999 y 2010
desoc_tabla6 <- desoc %>%
        filter(
                date %in% c(1999, 2010),
                !is.na(p_desoc), !is.na(pea), !is.na(cluster_pimsa),
                income_group_2 != "99_Sin_datos",
                peq_estado != "Peq. estado",
                excl_tamaño != "Excluible"
        ) %>%
        mutate(
                rango_desoc = case_when(
                        p_desoc <= 10 ~ "hasta 10%",
                        p_desoc <= 20 ~ "10,1-20%",
                        p_desoc > 20 ~ "Más de 20%"
                ),
                rango_desoc = factor(rango_desoc, levels = c("hasta 10%", "10,1-20%", "Más de 20%"))
        ) %>%
        distinct(date, country, cluster_pimsa, rango_desoc) %>%
        group_by(date, cluster_pimsa, rango_desoc) %>%
        summarise(
                paises = paste(sort(unique(country)), collapse = ", "),
                .groups = "drop"
        ) %>%
        tidyr::pivot_wider(
                names_from = rango_desoc,
                values_from = paises,
                values_fill = ""
        ) %>%
        arrange(date, cluster_pimsa)
write_xlsx(desoc_tabla6,
           path = "C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desoc_tabla6.xlsx")

 

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

###### INCORPORAR DATOS DE OCUPADOS POR RAMA PARA RELACIONAR*******************************

rama <- get_ilostat("EMP_2EMP_SEX_ECO_NB_A")

# 1. Filtrar y preparar `rama`
rama <- rama %>%
        filter(
                sex == "SEX_T",
                classif1 %in% c("ECO_SECTOR_TOTAL", "ECO_SECTOR_AGR", "ECO_SECTOR_IND", "ECO_SECTOR_SER")
        ) %>%
        mutate(time = as.numeric(time)) %>%
        select(ref_area, time, classif1, obs_value)

# 2. Pivotear: pasar sectores a columnas
rama <- rama %>%
        pivot_wider(
                names_from = classif1,
                values_from = obs_value,
                names_prefix = "empleo_"
        )

# 3. Unir con `desoc`
desocyrama  <- desoc %>%
        left_join(rama, by = c("iso3c" = "ref_area", "date" = "time"))

# 4. Chequeo
desocyrama <- desocyrama %>%
        mutate(
                empleo_calculado = pea - abs_desoc,
                diferencia = empleo_calculado - (empleo_ECO_SECTOR_TOTAL * 1000)
        )
desocyrama <- desocyrama %>%
        mutate(
                dif_rel = round(100 * (diferencia / empleo_calculado), 2),
                grupo_dif = case_when(
                        abs(dif_rel) <= 1 ~ "≤1%",
                        abs(dif_rel) <= 5 ~ "≤5%",
                        TRUE ~ "resto"
                )
        )
desocyrama %>%
        count(grupo_dif) %>%
        mutate(porc = round(100 * n / sum(n), 1))
# Resultado
#  ±1%        5488  89  
#  ±5%         511   8.3
#  resto       168   2.7 (pendiente de revisión)

options(tibble.print_max = Inf)
desocyrama %>%
        count(date, grupo_dif) %>%
        pivot_wider(
                names_from = grupo_dif,
                values_from = n,
                values_fill = 0
        )

desocyrama %>%
        count(iso3c, grupo_dif) %>%
        pivot_wider(
                names_from = grupo_dif,
                values_from = n,
                values_fill = 0
        )

# Cambio de nombres de variables y generación variables de proporción por ramas
desocyrama <- desocyrama %>%
        rename(
                abs_AGR = empleo_ECO_SECTOR_AGR,
                abs_IND = empleo_ECO_SECTOR_IND,
                abs_SER = empleo_ECO_SECTOR_SER,
                abs_TOTAL = empleo_ECO_SECTOR_TOTAL
        )
desocyrama <- desocyrama %>%
        mutate(
                porc_AGR = round(100 * abs_AGR / abs_TOTAL, 2),
                porc_IND = round(100 * abs_IND / abs_TOTAL, 2),
                porc_SER = round(100 * abs_SER / abs_TOTAL, 2)
        )

# Porcentaje de población industrial por cluster pimsa 1999 y 2010
desocgraf6 <- desocyrama %>%
        filter(
                date %in% c(1999, 2010),
                !is.na(porc_IND), !is.na(abs_IND), !is.na(cluster_pimsa),
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
        ggplot(aes(x = cluster_pimsa, y = porc_IND)) +
        geom_boxplot(outlier.shape = NA, fill = "gray90") +
        geom_jitter(aes(size = abs_IND, color = cluster_pimsa), width = 0.2, alpha = 0.6) +
        facet_wrap(~ date) +
        coord_flip() +
        labs(
                title = "Participación del empleo industrial por tipología de desarrollo\ncapitalista en extensión y profundidad. 1999 y 2010",
                x = "Tipología",
                y = "Empleo industrial (% del total)"
        ) +
        scale_size_continuous(
                name = "Empleo industrial (millones)",
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
                legend.box = "vertical",
                legend.box.spacing = unit(0.5, "cm"),
                legend.title = element_text(hjust = 0.5)
        ) +
        guides(
                color = guide_legend(title = "Tipología"),
                size = guide_legend(title = "Empleo industrial (millones)")
        )

ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desocgraf6.jpg",
       plot = desocgraf6, width = 8, height = 6, dpi = 300)


# Relación entre desocupación y población industrial

desocgraf7 <- desocyrama %>%
        filter(
                date %in% c(1999, 2010),
                grupo_dif != "resto", 
                !is.na(p_desoc),
                !is.na(porc_IND),
                !is.na(cluster_pimsa),
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
        ggplot(aes(x = porc_IND, y = p_desoc, color = cluster_pimsa)) +
        geom_point(aes(size = pea), alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
        facet_wrap(~ date) +
        labs(
                title = "Relación entre tasa desocupación y empleo industrial (%)\n según tipología de desarrollo en extensión y profundidad. 1999 y 2010",
                x = "Empleo industrial (% del total)",
                y = "Tasa de desocupación (%)",
                color = "Tipología",
                size = "PEA (millones)"
        ) +
        scale_size_continuous(
                labels = label_number(scale = 1e-6, suffix = " M", accuracy = 0.1)
        ) +
        scale_color_manual(
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
                legend.direction = "horizontal",      
                legend.box = "horizontal",              
                legend.text = element_text(size = 8),
                legend.title = element_text(size = 9)
        ) +
        guides(
                color = guide_legend(title = "Tipología", title.position = "top", override.aes = list(size = 3)),
                size = guide_legend(title = "PEA (millones)", title.position = "top", override.aes = list(size = 3))
        )
ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desocgraf7.jpg",
       plot = desocgraf7, width = 8, height = 6, dpi = 300)

desoc_tabla7 <- desocyrama %>%
        filter(
                date %in% c(1999, 2010),
                grupo_dif != "resto",
                !is.na(porc_AGR),
                !is.na(porc_IND),
                !is.na(porc_SER),
                !is.na(p_desoc),
                !is.na(cluster_pimsa),
                income_group_2 != "99_Sin_datos",
                peq_estado != "Peq. estado",
                excl_tamaño != "Excluible"
        ) %>%
        group_by(cluster_pimsa, date) %>%
        summarize(
                promedio_AGR = mean(porc_AGR, na.rm = TRUE),
                promedio_IND = mean(porc_IND, na.rm = TRUE),
                promedio_SER = mean(porc_SER, na.rm = TRUE),
                promedio_desoc = mean(p_desoc, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        mutate(across(starts_with("promedio"), ~ round(.x, 1))) %>%
        pivot_wider(
                names_from = date,
                values_from = c(promedio_AGR, promedio_IND, promedio_SER, promedio_desoc),
                names_sep = "_"
        )
write_xlsx(desoc_tabla7,
           path = "C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desoc_tabla7.xlsx")

# Relación cuadrática entre empleo industrial y desocupación


lm(p_desoc ~ porc_IND + I(porc_IND^2), 
   data = desocyrama %>%
           filter(date == 1999, 
                  grupo_dif != "resto", 
                  !is.na(p_desoc), 
                  !is.na(porc_IND),
                  !is.na(cluster_pimsa),
                  income_group_2 != "99_Sin_datos",
                  peq_estado != "Peq. estado",
                  excl_tamaño != "Excluible"
                  ))
lm(p_desoc ~ porc_IND + I(porc_IND^2), 
                  data = desocyrama %>%
                          filter(date == 2010, 
                                 grupo_dif != "resto", 
                                 !is.na(p_desoc), 
                                 !is.na(porc_IND),
                                 !is.na(cluster_pimsa),
                                 income_group_2 != "99_Sin_datos",
                                 peq_estado != "Peq. estado",
                                 excl_tamaño != "Excluible"                                 
                                 ))

desocgraf8 <- desocyrama %>%
        filter(
                date %in% c(1999, 2010),
                grupo_dif != "resto", 
                !is.na(p_desoc),
                !is.na(porc_IND),
                !is.na(cluster_pimsa),
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
        ggplot(aes(x = porc_IND, y = p_desoc, color = cluster_pimsa)) +
        geom_point(aes(size = pea), alpha = 0.6) +
        geom_smooth(method = "lm", formula = y ~ x + I(x^2),
                    se = FALSE, color = "red", fullrange = FALSE) +        
        facet_wrap(~ date) +
        labs(
                title = "Relación entre tasa desocupación y empleo industrial (%)\nsegún tipología. 1999 y 2010",
                x = "Empleo industrial (% del total)",
                y = "Tasa de desocupación (%)",
                color = "Tipología",
                size = "PEA (millones)"
        ) +
        scale_size_continuous(
                labels = label_number(scale = 1e-6, suffix = " M", accuracy = 0.1)
        ) +
        scale_color_manual(
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
                legend.direction = "horizontal",
                legend.box = "horizontal",
                legend.text = element_text(size = 8),
                legend.title = element_text(size = 9)
        ) +
        guides(
                color = guide_legend(title = "Tipología", title.position = "top", override.aes = list(size = 3)),
                size = guide_legend(title = "PEA (millones)", title.position = "top", override.aes = list(size = 3))
        )

ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desocgraf8.jpg",
       plot = desocgraf8, width = 8, height = 6, dpi = 300)

# Relación entre agro/servicios y desocupación

# 1. Definimos rango sin extremos por percentiles 1% y 99%, por cada año
rangos_sin_extremos <- desocyrama %>%
        filter(
                date %in% c(1999, 2010),
                grupo_dif != "resto", 
                !is.na(p_desoc),
                !is.na(razon_AGR_SER),
                !is.na(cluster_pimsa),
                income_group_2 != "99_Sin_datos",
                peq_estado != "Peq. estado",
                excl_tamaño != "Excluible"
        ) %>%
        group_by(date) %>%
        summarise(
                min_r = quantile(razon_AGR_SER, 0.01, na.rm = TRUE),
                max_r = quantile(razon_AGR_SER, 0.99, na.rm = TRUE),
                .groups = "drop"
        )

# 2. Creamos predicciones sólo en ese rango “sin extremos”
predicciones <- rangos_sin_extremos %>%
        rowwise() %>%
        mutate(
                razon_AGR_SER = list(seq(min_r, max_r, length.out = 200))
        ) %>%
        unnest(cols = c(razon_AGR_SER)) %>%
        mutate(
                p_desoc_pred = predict(modelo4, newdata = data.frame(razon_AGR_SER = razon_AGR_SER))
        )

# 3. Graficamos todo junto, incluyendo los puntos originales (con casos extremos)
desocgraf9 <- desocyrama %>%
        filter(
                date %in% c(1999, 2010),
                grupo_dif != "resto", 
                !is.na(p_desoc),
                !is.na(razon_AGR_SER),
                !is.na(cluster_pimsa),
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
        ggplot(aes(x = razon_AGR_SER, y = p_desoc, color = cluster_pimsa)) +
        geom_point(aes(size = pea), alpha = 0.6) +
        geom_line(data = predicciones, aes(x = razon_AGR_SER, y = p_desoc_pred),
                  inherit.aes = FALSE, color = "red", size = 1) +
        facet_wrap(~ date) +
        labs(
                title = "Relación entre tasa desocupación y razón empleo agro/servicios\nsegún tipología. 1999 y 2010",
                x = "Empleo agropecuario / Empleo en servicios",
                y = "Tasa de desocupación (%)",
                color = "Tipología",
                size = "PEA (millones)"
        ) +
        scale_size_continuous(
                labels = label_number(scale = 1e-6, suffix = " M", accuracy = 0.1)
        ) +
        scale_color_manual(
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
                legend.direction = "horizontal",
                legend.box = "horizontal",
                legend.text = element_text(size = 8),
                legend.title = element_text(size = 9)
        ) +
        guides(
                color = guide_legend(title = "Tipología", title.position = "top", override.aes = list(size = 3)),
                size = guide_legend(title = "PEA (millones)", title.position = "top", override.aes = list(size = 3))
        )

# 4. Guardar el gráfico
ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desocgraf9.jpg",
       plot = desocgraf9, width = 8, height = 6, dpi = 300)



################# # Relación entre agro/servicios (escala logarítimica) y desocupación. 


# 1. Definimos rango sin extremos por percentiles 1% y 99%, por cada año
rangos_sin_extremos <- desocyrama %>%
        filter(
                date %in% c(1999, 2010),
                grupo_dif != "resto", 
                !is.na(p_desoc),
                !is.na(razon_AGR_SER),
                !is.na(cluster_pimsa),
                income_group_2 != "99_Sin_datos",
                peq_estado != "Peq. estado",
                excl_tamaño != "Excluible"
        ) %>%
        group_by(date) %>%
        summarise(
                min_r = quantile(razon_AGR_SER, 0.01, na.rm = TRUE),
                max_r = quantile(razon_AGR_SER, 0.99, na.rm = TRUE),
                .groups = "drop"
        )

# 2. Creamos predicciones sólo en ese rango “sin extremos”
predicciones <- rangos_sin_extremos %>%
        rowwise() %>%
        mutate(
                razon_AGR_SER = list(seq(min_r, max_r, length.out = 200))
        ) %>%
        unnest(cols = c(razon_AGR_SER)) %>%
        mutate(
                p_desoc_pred = predict(modelo4, newdata = data.frame(razon_AGR_SER = razon_AGR_SER))
        )

# 3. Graficamos todo junto, con eje X logarítmico
desocgraf10 <- desocyrama %>%
        filter(
                date %in% c(1999, 2010),
                grupo_dif != "resto", 
                !is.na(p_desoc),
                !is.na(razon_AGR_SER),
                !is.na(cluster_pimsa),
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
        ggplot(aes(x = razon_AGR_SER, y = p_desoc, color = cluster_pimsa)) +
        geom_point(aes(size = pea), alpha = 0.6) +
        geom_line(data = predicciones, aes(x = razon_AGR_SER, y = p_desoc_pred),
                  inherit.aes = FALSE, color = "red", size = 1) +
        scale_x_log10() +
        facet_wrap(~ date) +
        labs(
                title = "Relación entre tasa desocupación y razón empleo agro/servicios\nsegún tipología. 1999 y 2010",
                x = "Empleo agropecuario / Empleo en servicios (escala logarítmica)",
                y = "Tasa de desocupación (%)",
                color = "Tipología",
                size = "PEA (millones)"
        ) +
        scale_size_continuous(
                labels = label_number(scale = 1e-6, suffix = " M", accuracy = 0.1)
        ) +
        scale_color_manual(
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
                legend.direction = "horizontal",
                legend.box = "horizontal",
                legend.text = element_text(size = 8),
                legend.title = element_text(size = 9)
        ) +
        guides(
                color = guide_legend(title = "Tipología", title.position = "top", override.aes = list(size = 3)),
                size = guide_legend(title = "PEA (millones)", title.position = "top", override.aes = list(size = 3))
        )

# 4. Guardamos
ggsave(
        "C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desocgraf10.jpg",
        plot = desocgraf10, width = 8, height = 6, dpi = 300)

###
        

# Cuartiles por cluster pimisa. Evolución 1991-2023.
cuartiles_por_cluster <- desocyrama %>%
        filter(
                !is.na(p_desoc),
                !is.na(cluster_pimsa),
                income_group_2 != "99_Sin_datos",
                peq_estado != "Peq. estado",
                excl_tamaño != "Excluible"
        ) %>%
        group_by(cluster_pimsa, date) %>%
        summarize(
                q25 = quantile(p_desoc, 0.25, na.rm = TRUE),
                q50 = quantile(p_desoc, 0.50, na.rm = TRUE),
                q75 = quantile(p_desoc, 0.75, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        pivot_longer(cols = starts_with("q"), names_to = "cuartil", values_to = "p_desoc")

# Cuartiles globales (para todos los clusters juntos)
cuartiles_totales <- desocyrama %>%
        filter(
                !is.na(p_desoc),
                !is.na(cluster_pimsa),
                income_group_2 != "99_Sin_datos",
                peq_estado != "Peq. estado",
                excl_tamaño != "Excluible"
        ) %>%
        group_by(date) %>%
        summarize(
                q25 = quantile(p_desoc, 0.25, na.rm = TRUE),
                q50 = quantile(p_desoc, 0.50, na.rm = TRUE),
                q75 = quantile(p_desoc, 0.75, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        pivot_longer(cols = starts_with("q"), names_to = "cuartil", values_to = "p_desoc") %>%
        mutate(cluster_pimsa = "Total")  # etiqueta para usar en facet_wrap

# Unir ambos
cuartiles_completo <- bind_rows(cuartiles_por_cluster, cuartiles_totales)

# Gráfico

desocgraf11 <- ggplot(cuartiles_completo, aes(x = date, y = p_desoc, color = cuartil)) +
        geom_line(size = 0.8) +
        facet_wrap(
                ~ cluster_pimsa,
                labeller = as_labeller(c(
                        "C1. Cap. avanzado" = "Grupo 1",
                        "C2. Cap. extensión reciente c/desarrollo profundidad" = "Grupo 2",
                        "C3. Cap. extensión c/peso campo" = "Grupo 3",
                        "C4. Cap. escasa extensión c/peso campo" = "Grupo 4",
                        "C5. Pequeña propiedad en el campo" = "Grupo 5",
                        "Total" = "Total"
                ))
        ) +
        labs(
                title = "Evolución de los cuartiles de la tasa de desocupación \npor tipología de desarrollo en extensión y profundidad. 1991-2023",
                x = "Año", y = "Tasa de desocupación (%)",
                color = "Cuartil"
        ) +
        theme_minimal()+
        theme(
                strip.text = element_text(size = 11)  
        )

ggsave(
        "C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Flotante/desocgraf11.jpg",
        plot = desocgraf11, width = 8, height = 6, dpi = 300)
########

desocbm %>%
 #       filter(
  #              date %in% c(1999, 2010),
   #             !is.na(p_desoc), !is.na(pea), !is.na(cluster_pimsa),
    #            income_group_2 != "99_Sin_datos",
     #           peq_estado != "Peq. estado",
      #          excl_tamaño != "Excluible"
#        ) %>%
        distinct(iso3c) %>%
        count()

desocbm <- wb_data(c("SL.UEM.TOTL.ZS","SL.TLF.TOTL.IN"),
                 start_date=1991,
                 end_date=2023)


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