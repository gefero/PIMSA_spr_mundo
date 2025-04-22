


library(Rilostat)
library(tidyverse)

library(googlesheets4)
library(gt)
library(ggplot2)
library(scales)
library(dplyr)
library(grid)
library(gridExtra)
library(cowplot)


# Cantidad de países con datos

graf1 <- asal %>%
  filter(!is.na(informal_p_asal) | !is.na(temporario_p_asal) | !is.na(tparcial_p_asal), 
         income_group_2 != "99_Sin_datos") %>%
  pivot_longer(cols = c(informal_p_asal, temporario_p_asal, tparcial_p_asal), 
               names_to = "Fuente", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
  mutate(time = as.numeric(as.character(time))) %>%
  filter(!is.na(time)) %>%  # Eliminar filas con 'time' NA
  group_by(Fuente, time, income_group_2) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = time, y = n, color = income_group_2, group = income_group_2)) +
  geom_line() +
  facet_wrap(~ Fuente, scales = "free_y", 
  labeller = labeller(Fuente = c(informal_p_asal = "Informal", 
                                 temporario_p_asal = "Temporario", 
                                 tparcial_p_asal = "Parcial"))) +  # Títulos personalizados
  scale_y_continuous(limits = c(0, 45)) +  # Establecer el máximo de Y en 40
        scale_color_discrete(labels = c("01 Altos ingresos" = "Alto", 
                                        "02 Medios-altos ingresos" = "Medio alto", 
                                        "03 Medios-bajos ingresos" = "Medio bajo",                                         
                                        "04 Bajos ingresos" = "Bajo")) +  # Solo cambia las etiquetas
  labs(x = "Año", y = "Cantidad de países con información", color = "Grupo de ingresos", 
       caption = "Fuente: elaboración propia a partir de OIT-ILOSTAT.") +
  ggtitle("Cantidad de países con información. Indicadores seleccionados. 1980-2024") +
  theme_minimal() + 
  theme(strip.text = element_text(size = 12), # Tamaño de los títulos de facetas
  legend.position = "bottom", 
  legend.text = element_text(size = 12), # Leyenda abajo
        plot.margin = margin(10, 10, 10, 10))  # Aumentar margen inferior (10 unidades para los demás márgenes, 50 para el inferior)


ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Estancada/graf1.jpg", plot = graf1, width = 8, height = 6, dpi = 300)

# Cantidad de países con datos, pero excluyendo pequeños estados y excluibles por población
asal %>%
  filter(!is.na(informal_p_asal) | !is.na(temporario_p_asal) | !is.na(tparcial_p_asal), 
         income_group_2 != "99_Sin_datos",
         peq_estado != "Peq. estado",  # Excluir "Peq. estado"
         excl_tamaño != "Excluible") %>%  # Excluir "Excluible"
  pivot_longer(cols = c(informal_p_asal, temporario_p_asal, tparcial_p_asal), 
               names_to = "Fuente", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
  mutate(time = as.numeric(as.character(time))) %>%
  filter(!is.na(time)) %>%  # Eliminar filas con 'time' NA
  group_by(Fuente, time, income_group_2) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = time, y = n, color = income_group_2, group = income_group_2)) +
  geom_line() +
  facet_wrap(~ Fuente, scales = "free_y", 
             labeller = labeller(Fuente = c(informal_p_asal = "Informal", 
                                            temporario_p_asal = "Temporario", 
                                            tparcial_p_asal = "Parcial"))) +  # Títulos personalizados
  scale_y_continuous(limits = c(0, 45)) +  # Establecer el máximo de Y en 45
  labs(x = "Año", y = "Cantidad de países con información", color = "Grupo de ingresos",
       caption = "Fuente: elaboración propia a partir de OIT-ILOSTAT.") +
  ggtitle("Cantidad de países con datos (sin pequeños estados y escasa población). Indicadores seleccionados") +
        theme_minimal() + 
        theme(strip.text = element_text(size = 12), # Tamaño de los títulos de facetas
              legend.position = "bottom", 
              legend.text = element_text(size = 12), # Leyenda abajo
              plot.margin = margin(10, 10, 10, 10))  # Aumentar margen inferior


# Desde aquí ya selecciono el periodo 2009 / 2019

# Promedio simple de indicadores / asalariados
graf2 <- asal %>%
  filter(!is.na(informal_p_asal) | !is.na(temporario_p_asal) | !is.na(tparcial_p_asal), 
         income_group_2 != "99_Sin_datos",
         peq_estado != "Peq. estado",  # Excluir "Peq. estado"
         excl_tamaño != "Excluible") %>%  # Excluir "Excluible"
  pivot_longer(cols = c(informal_p_asal, temporario_p_asal, tparcial_p_asal), 
               names_to = "Fuente", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
        mutate(time = as.character(time)) %>%
  filter(!is.na(time), time >= 2009, time <= 2019) %>%  # Filtrar el período entre 2009 y 2019
  group_by(Fuente, time, income_group_2) %>%
  summarise(promedio = mean(Valor, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = time, y = promedio, color = income_group_2, group = income_group_2)) +
  geom_line() +
  facet_wrap(~ Fuente, scales = "free_y", 
             labeller = labeller(Fuente = c(informal_p_asal = "Informal", 
                                            temporario_p_asal = "Temporario", 
                                            tparcial_p_asal = "Parcial"))) +  # Títulos personalizados
  scale_y_continuous(limits = c(0, 100)) +  # Ajustar el máximo de Y a 40
        scale_color_discrete(labels = c("01 Altos ingresos" = "Alto", 
                                        "02 Medios-altos ingresos" = "Medio alto", 
                                        "03 Medios-bajos ingresos" = "Medio bajo",                                         
                                        "04 Bajos ingresos" = "Bajo")) +  # Solo cambia las etiquetas
          labs(x = "Año", y = "Promedio", color = "Grupo de ingresos",
          caption = "Fuente: elaboración propia a partir de OIT-ILOSTAT.") +
  ggtitle("Indicadores seleccionados. Promedio simple sobre asalariados. 2009-2019") +
        theme_minimal() + 
        theme(strip.text = element_text(size = 12), # Tamaño de los títulos de facetas
              legend.position = "bottom", 
                axis.text.x = element_text(angle = 90, hjust = 1),  # Rotar las etiquetas del eje X 
                legend.text = element_text(size = 12), # Leyenda abajo
              plot.margin = margin(10, 10, 10, 10))  # Aumentar margen inferior


ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Estancada/graf2.jpg", plot = graf2, width = 8, height = 6, dpi = 300)


# Promedio ponderado de indicadores sobre asalariados
graf3 <-asal %>%
  filter(!is.na(informal_p_asal) | !is.na(temporario_p_asal) | !is.na(tparcial_p_asal), 
         income_group_2 != "99_Sin_datos",
         peq_estado != "Peq. estado",  # Excluir "Peq. estado"
         excl_tamaño != "Excluible") %>%  # Excluir "Excluible"
  pivot_longer(cols = c(informal_p_asal, temporario_p_asal, tparcial_p_asal), 
               names_to = "Fuente", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
        mutate(time = as.character(time)) %>%
  filter(!is.na(time), time >= 2009, time <= 2019) %>%  # Filtrar el período entre 2009 y 2019
  group_by(Fuente, time, income_group_2) %>%
  summarise(promedio_ponderado = sum(Valor * asal_n, na.rm = TRUE) / sum(asal_n, na.rm = TRUE), 
            .groups = "drop") %>%
  ggplot(aes(x = time, y = promedio_ponderado, color = income_group_2, group = income_group_2)) +
  geom_line() +
  facet_wrap(~ Fuente, scales = "free_y", 
             labeller = labeller(Fuente = c(informal_p_asal = "Informal", 
                                            temporario_p_asal = "Temporario", 
                                            tparcial_p_asal = "Parcial"))) +  # Títulos personalizados
  scale_y_continuous(limits = c(0, 100)) +  # Ajustar el máximo de Y a 100
        scale_color_discrete(labels = c("01 Altos ingresos" = "Alto", 
                                        "02 Medios-altos ingresos" = "Medio alto", 
                                        "03 Medios-bajos ingresos" = "Medio bajo",                                         
                                        "04 Bajos ingresos" = "Bajo")) +  # Solo cambia las etiquetas
  labs(x = "Año", y = "Promedio Ponderado", color = "Grupo de ingresos",
        caption = "Fuente: elaboración propia a partir de OIT-ILOSTAT.") +
  ggtitle("Indicadores seleccionados. Promedio ponderado sobre asalariados. 2009-2019") +
        theme_minimal() + 
        theme(strip.text = element_text(size = 12), # Tamaño de los títulos de facetas
              legend.position = "bottom", 
              axis.text.x = element_text(angle = 90, hjust = 1),  # Rotar las etiquetas del eje X 
              legend.text = element_text(size = 12), # Leyenda abajo
              plot.margin = margin(10, 10, 10, 10))  # Aumentar margen inferior

ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Estancada/graf3.jpg", plot = graf3, width = 8, height = 6, dpi = 300)


# Cantidad de países con información sobre total ocupados.

asal %>%
        filter(!is.na(asal_p), income_group_2 != "99_Sin_datos",
               peq_estado != "Peq. estado",  # Excluir "Peq. estado"
               excl_tamaño != "Excluible") %>%  # Excluir "Excluible"
        mutate(time = as.numeric(as.character(time))) %>%
        filter(!is.na(time)) %>%  # Eliminar filas con 'time' NA
        group_by(time, income_group_2) %>%
        summarise(n = n(), .groups = "drop") %>%
        ggplot(aes(x = time, y = n, color = income_group_2, group = income_group_2)) +
        geom_line() +
               scale_y_continuous(limits = c(0, 45)) +  # Establecer el máximo de Y en 40
        labs(x = "Año", y = "Cantidad de países con información", color = "Grupo de ingresos") +
        ggtitle("Cantidad de países con dato de ocupados (sin pequeños estados y escasa población)") +
        theme_minimal() +
        theme(legend.position = "bottom")


# Promedio simple asalariados 

graf4a <-asal %>%
  filter(!is.na(asal_p), income_group_2 != "99_Sin_datos",
         peq_estado != "Peq. estado",  # Excluir "Peq. estado"
         excl_tamaño != "Excluible") %>%  # Excluir "Excluible"
        mutate(time = as.character(time)) %>%
  filter(!is.na(time), time >= 2009, time <= 2019) %>%  # Filtrar el período entre 2009 y 2019
  group_by(time, income_group_2) %>%
  summarise(promedio = mean(asal_p, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = time, y = promedio, color = income_group_2, group = income_group_2)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 100)) +  # Ajustar el máximo de Y a 100
        scale_color_discrete(labels = c("01 Altos ingresos" = "Alto", 
                                        "02 Medios-altos ingresos" = "Medio alto", 
                                        "03 Medios-bajos ingresos" = "Medio bajo",                                         
                                        "04 Bajos ingresos" = "Bajo")) +  # Solo cambia las etiquetas
  labs(x = "Año", y = "Promedio", color = "Grupo de ingresos",
        caption = "Fuente: elaboración propia a partir de OIT-ILOSTAT.") +        
  ggtitle("Asalariados. \n Promedio simple sobre ocupados. \n 2009-2019") +
        theme_minimal() + 
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 90, hjust = 1),  # Rotar las etiquetas del eje X 
              legend.text = element_text(size = 10), # Leyenda abajo
              plot.margin = margin(10, 10, 10, 10)) + # Aumentar margen inferior
                guides(color = guide_legend(label.position = "left", title.position = "top", ncol = 2))

ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Estancada/graf4a.jpg", plot = graf4a, width = 4, height = 6, dpi = 300)



# Promedio ponderado asalariados 
graf4b <-asal %>%
  filter(!is.na(asal_p), income_group_2 != "99_Sin_datos",
         peq_estado != "Peq. estado",  # Excluir "Peq. estado"
         excl_tamaño != "Excluible") %>%  # Excluir "Excluible"
        mutate(time = as.character(time)) %>%
  filter(!is.na(time), time >= 2009, time <= 2019) %>%  # Filtrar el período entre 2009 y 2019
  group_by(time, income_group_2) %>%
  summarise(promedio_ponderado = weighted.mean(asal_p, ocup_n, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = time, y = promedio_ponderado, color = income_group_2, group = income_group_2)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 100)) +  # Ajustar el máximo de Y a 100
        scale_color_discrete(labels = c("01 Altos ingresos" = "Alto", 
                                        "02 Medios-altos ingresos" = "Medio alto", 
                                        "03 Medios-bajos ingresos" = "Medio bajo",                                         
                                        "04 Bajos ingresos" = "Bajo")) +  # Solo cambia las etiquetas
        labs(x = "Año", y = "Promedio", color = "Grupo de ingresos",
             caption = "Fuente: elaboración propia a partir de OIT-ILOSTAT.") + 
  ggtitle("Asalariados. \n Promedio ponderado sobre ocupados. \n 2009-2019") +
        theme_minimal() + 
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 90, hjust = 1),  # Rotar las etiquetas del eje X 
              legend.text = element_text(size = 10), # Leyenda abajo
              plot.margin = margin(10, 10, 10, 10)) +  # Aumentar margen inferior
                guides(color = guide_legend(label.position = "left", title.position = "top", ncol = 2))



ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Estancada/graf4b.jpg", plot = graf4b, width = 4, height = 6, dpi = 300)



# Promedio simple de indicadores sobre / ocupados

graf5a <- asal %>%
  filter(!is.na(informal_p_asal) | !is.na(temporario_p_asal) | !is.na(tparcial_p_asal), 
         income_group_2 != "99_Sin_datos",
         peq_estado != "Peq. estado",  # Excluir "Peq. estado"
         excl_tamaño != "Excluible") %>%  # Excluir "Excluible"
  pivot_longer(cols = c(informal_p_ocup, temporario_p_ocup, tparcial_p_ocup), 
               names_to = "Fuente", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
        mutate(time = as.character(time)) %>%
  filter(!is.na(time), time >= 2009, time <= 2019) %>%  # Filtrar el período entre 2009 y 2019
  group_by(Fuente, time, income_group_2) %>%
  summarise(promedio = mean(Valor, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = time, y = promedio, color = income_group_2, group = income_group_2)) +
  geom_line() +
  facet_wrap(~ Fuente, scales = "free_y", 
             labeller = labeller(Fuente = c(informal_p_ocup = "Informal", 
                                            temporario_p_ocup = "Temporario", 
                                            tparcial_p_ocup = "Parcial"))) +  # Títulos personalizados
  scale_y_continuous(limits = c(0, 100)) +  # Ajustar el máximo de Y a 40
  labs(x = "Año", y = "Promedio", color = "Grupo de ingresos",
        caption = "Fuente: elaboración propia a partir de OIT-ILOSTAT.") + 
        scale_color_discrete(labels = c("01 Altos ingresos" = "Alto", 
                                        "02 Medios-altos ingresos" = "Medio alto", 
                                        "03 Medios-bajos ingresos" = "Medio bajo",                                         
                                        "04 Bajos ingresos" = "Bajo")) +  # Solo cambia las etiquetas
  ggtitle("Indicadores seleccionados. Promedio simple sobre ocupados. 2009-2019") +
        theme_minimal() +
        theme(strip.text = element_text(size = 12), # Tamaño de los títulos de facetas
         axis.text.x = element_text(angle = 90, hjust = 1),  # Rotar las etiquetas del eje X 
        legend.position = "bottom", 
        legend.text = element_text(size = 12), # Leyenda abajo
        plot.margin = margin(10, 10, 10, 10))  # Aumentar margen inferior (10 unidades para los demás márgenes, 50 para el inferior)

ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Estancada/graf5a.jpg", plot = graf5a, width = 8, height = 6, dpi = 300)


 

# Promedio ponderado de indicadores sobre ocupados
graf5b <- asal %>%
  filter(!is.na(informal_p_asal) | !is.na(temporario_p_asal) | !is.na(tparcial_p_asal), 
         income_group_2 != "99_Sin_datos",
         peq_estado != "Peq. estado",  # Excluir "Peq. estado"
         excl_tamaño != "Excluible") %>%  # Excluir "Excluible"
  pivot_longer(cols = c(informal_p_ocup, temporario_p_ocup, tparcial_p_ocup), 
               names_to = "Fuente", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
        mutate(time = as.character(time)) %>%
  filter(!is.na(time), time >= 2009, time <= 2019) %>%  # Filtrar el período entre 2009 y 2019
  group_by(Fuente, time, income_group_2) %>%
  summarise(promedio_ponderado = sum(Valor * ocup_n, na.rm = TRUE) / sum(ocup_n, na.rm = TRUE), 
            .groups = "drop") %>%
  ggplot(aes(x = time, y = promedio_ponderado, color = income_group_2, group = income_group_2)) +
  geom_line() +
  facet_wrap(~ Fuente, scales = "free_y", 
             labeller = labeller(Fuente = c(informal_p_ocup = "Informal", 
                                            temporario_p_ocup = "Temporario", 
                                            tparcial_p_ocup = "Parcial"))) +  # Títulos personalizados
  scale_y_continuous(limits = c(0, 100)) +  # Ajustar el máximo de Y a 100
        scale_color_discrete(labels = c("01 Altos ingresos" = "Alto", 
                                        "02 Medios-altos ingresos" = "Medio alto", 
                                        "03 Medios-bajos ingresos" = "Medio bajo",                                         
                                        "04 Bajos ingresos" = "Bajo")) +  # Solo cambia las etiquetas
  labs(x = "Año", y = "Promedio Ponderado", color = "Grupo de ingresos",
        caption = "Fuente: elaboración propia a partir de OIT-ILOSTAT.") + 
  ggtitle("Indicadores seleccionados. Promedio ponderado sobre ocupados. 2009-2019") +
        theme_minimal() +
        theme(strip.text = element_text(size = 12), # Tamaño de los títulos de facetas
              axis.text.x = element_text(angle = 90, hjust = 1),  # Rotar las etiquetas del eje X 
              legend.position = "bottom", 
              legend.text = element_text(size = 12), # Leyenda abajo
              plot.margin = margin(10, 10, 10, 10))  # Aumentar margen inferior (10 unidades para los demás márgenes, 50 para el inferior)


ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Estancada/graf5b.jpg", plot = graf5b, width = 8, height = 6, dpi = 300)

# Los indicadores ponderados dan como resultado:
# informal en países medios y bajos (que posiblemente incluye temporarios)
# trabajo parcial en altos ingresos (que puede superponerse temporarios)
# Ver posibilidad de seleccionar informales para medios y bajos y parcial para altos?


# # Promedio ponderado de indicadores sobre ocupadospor grupo de ingresos (total 2009-2019)
graf6 <- asal %>%
  filter(!is.na(informal_p_asal) | !is.na(temporario_p_asal) | !is.na(tparcial_p_asal), 
         income_group_2 != "99_Sin_datos",
         peq_estado != "Peq. estado",  # Excluir "Peq. estado"
         excl_tamaño != "Excluible") %>%  # Excluir "Excluible"
  pivot_longer(cols = c(informal_p_ocup, temporario_p_ocup, tparcial_p_ocup), 
               names_to = "Fuente", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
  mutate(time = as.numeric(as.character(time)),
         Fuente = recode(Fuente, 
                         informal_p_ocup = "Informal", 
                         temporario_p_ocup = "Temporario", 
                         tparcial_p_ocup = "Parcial"),
         Fuente = factor(Fuente, levels = c("Informal", "Temporario", "Parcial"))) %>%  # Definir el orden
  filter(!is.na(time), time >= 2009, time <= 2019) %>%  # Filtrar el período entre 2009 y 2019
  group_by(Fuente, income_group_2) %>%  # Agrupar por Fuente y grupo de ingresos
  summarise(promedio_ponderado = sum(Valor * ocup_n, na.rm = TRUE) / sum(ocup_n, na.rm = TRUE), 
            .groups = "drop") %>%
  ggplot(aes(x = Fuente, y = promedio_ponderado, fill = income_group_2, group = income_group_2)) +
  geom_bar(stat = "identity", position = "dodge") +  # Barra separada por grupo de ingresos
  scale_y_continuous(limits = c(0, 30)) +  # Ajustar el máximo de Y a 100
        scale_fill_discrete(name = "Grupo de ingresos",  # Cambiar el nombre en la leyenda
                            labels = c("01 Altos ingresos" = "Alto", 
                                       "02 Medios-altos ingresos" = "Medio alto", 
                                       "03 Medios-bajos ingresos" = "Medio bajo",                                         
                                       "04 Bajos ingresos" = "Bajo")) +
        scale_x_discrete(labels = c("Informal" = "Asalariado informal", 
                                    "Temporario" = "Asalariado temporario", 
                                    "Parcial" = "Asalariado parcial")) +  # Cambiar nombres de las categorías de "Fuente"
  labs(x = "Fuente", y = "Promedio Ponderado", color = "Grupo de ingresos",
        caption = "Fuente: elaboración propia a partir de OIT-ILOSTAT.") + 
  ggtitle("Indicadores seleccionados. \n Promedio ponderado sobre ocupados por grupo de ingresos. 2009-2019") +
  theme_minimal() +
  theme(legend.position = "bottom",  # Opcional: mover la leyenda
        axis.title.x = element_blank(),# Eliminar título del eje X
        axis.text.x = element_text(size = 12))  # Aumentar tamaño de las etiquetas de las categorías de "Fuente"

ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Estancada/graf6.jpg", plot = graf6, width = 8, height = 6, dpi = 300)

# # Promedio ponderado de indicadores sobre ocupados por región (total 2009-2019)
graf7 <- asal %>%
  filter(!is.na(informal_p_asal) | !is.na(temporario_p_asal) | !is.na(tparcial_p_asal), 
         income_group_2 != "99_Sin_datos",
         peq_estado != "Peq. estado",  # Excluir "Peq. estado"
         excl_tamaño != "Excluible") %>%  # Excluir "Excluible"
  pivot_longer(cols = c(informal_p_ocup, temporario_p_ocup, tparcial_p_ocup), 
               names_to = "Fuente", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
  mutate(time = as.numeric(as.character(time)),
         Fuente = recode(Fuente, 
                         informal_p_ocup = "Informal", 
                         temporario_p_ocup = "Temporario", 
                         tparcial_p_ocup = "Parcial"),
         Fuente = factor(Fuente, levels = c("Informal", "Temporario", "Parcial"))) %>%  # Definir el orden
  filter(!is.na(time), time >= 2009, time <= 2019) %>%  # Filtrar el período entre 2009 y 2019
  group_by(Fuente, region) %>%  # Agrupar por Fuente y región
  summarise(promedio_ponderado = sum(Valor * ocup_n, na.rm = TRUE) / sum(ocup_n, na.rm = TRUE), 
            .groups = "drop") %>%
        complete(Fuente, region, fill = list(promedio_ponderado = 0)) %>% 
  ggplot(aes(x = Fuente, y = promedio_ponderado, fill = region, group = region)) +  # Cambiar a 'region' en el gráfico
  geom_bar(stat = "identity", position = "dodge") +  # Barra separada por región
  scale_y_continuous(limits = c(0, 40)) +  # Ajustar el máximo de Y a 30
        scale_fill_discrete(labels = c("East Asia & Pacific" = "Asia Oriental y Pacífico", 
                                       "Latin America & Caribbean" = "Latinoamérica y Caribe", 
                                       "North America" = "Norteamérica",                                         
                                       "Sub-Saharan Africa" = "África Subsahariana",
                                        "Europe & Central Asia" = "Europa y Asia Central",
                                        "Middle East & North Africa" = "Medio Oriente y Norte de África",
                                        "South Asia" = "Sur de Asia")) +
        scale_x_discrete(labels = c("Informal" = "Asalariado informal", 
                                    "Temporario" = "Asalariado temporario", 
                                    "Parcial" = "Asalariado parcial")) +  # Cambiar nombres de las categorías de "Fuente"
  labs(x = "Fuente", y = "Promedio Ponderado", fill = "Región",  # Cambiar 'color' por 'fill' y etiqueta 'Región'
        caption = "Fuente: elaboración propia a partir de OIT-ILOSTAT.") + 
          ggtitle("Indicadores seleccionados. Promedio ponderado sobre ocupados por región. 2009-2019") +
        theme_minimal() +
        theme(legend.position = "bottom",  # Opcional: mover la leyenda
        axis.title.x = element_blank(),# Eliminar título del eje X
        axis.text.x = element_text(size = 12))  # Aumentar tamaño de las etiquetas de las categorías de "Fuente"

ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Estancada/graf7.jpg", plot = graf7, width = 8, height = 6, dpi = 300)


# # Promedio ponderado de indicadores sobre ocupados por pertenencia a la OCDE (total 2009-2019)
asal %>%
  filter(!is.na(informal_p_asal) | !is.na(temporario_p_asal) | !is.na(tparcial_p_asal), 
         income_group_2 != "99_Sin_datos",
         peq_estado != "Peq. estado",  # Excluir "Peq. estado"
         excl_tamaño != "Excluible") %>%  # Excluir "Excluible"
  pivot_longer(cols = c(informal_p_ocup, temporario_p_ocup, tparcial_p_ocup), 
               names_to = "Fuente", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
  mutate(time = as.numeric(as.character(time)),
         Fuente = recode(Fuente, 
                         informal_p_ocup = "Informal", 
                         temporario_p_ocup = "Temporario", 
                         tparcial_p_ocup = "Parcial"),
         Fuente = factor(Fuente, levels = c("Informal", "Temporario", "Parcial"))) %>%  # Definir el orden
  filter(!is.na(time), time >= 2009, time <= 2019) %>%  # Filtrar el período entre 2009 y 2019
  group_by(Fuente, ocde) %>%  # Agrupar por Fuente y región
  summarise(promedio_ponderado = sum(Valor * ocup_n, na.rm = TRUE) / sum(ocup_n, na.rm = TRUE), 
            .groups = "drop") %>%
  ggplot(aes(x = Fuente, y = promedio_ponderado, fill = ocde, group = ocde)) +  
  geom_bar(stat = "identity", position = "dodge") +  # Barra separada por región
  scale_y_continuous(limits = c(0, 30)) +  # Ajustar el máximo de Y a 30
  labs(x = "Fuente", y = "Promedio Ponderado", fill = "OCDE") +  # Cambiar 'color' por 'fill' y etiqueta 'Región'
  ggtitle("Indicadores seleccionados. Promedio ponderado sobre ocupados según pertenecia a la OCDE. 2009-2019") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Opcional: mover la leyenda

# # Promedio ponderado de indicadores sobre ocupados por grado de desarrollo (total 2009-2019)
graf8 <- asal %>%
  filter(!is.na(informal_p_asal) | !is.na(temporario_p_asal) | !is.na(tparcial_p_asal), 
         income_group_2 != "99_Sin_datos",
         peq_estado != "Peq. estado",  # Excluir "Peq. estado"
         excl_tamaño != "Excluible") %>%  # Excluir "Excluible"
  pivot_longer(cols = c(informal_p_ocup, temporario_p_ocup, tparcial_p_ocup), 
               names_to = "Fuente", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
  mutate(time = as.numeric(as.character(time)),
         Fuente = recode(Fuente, 
                         informal_p_ocup = "Informal", 
                         temporario_p_ocup = "Temporario", 
                         tparcial_p_ocup = "Parcial"),
         Fuente = factor(Fuente, levels = c("Informal", "Temporario", "Parcial"))) %>%  # Definir el orden
  filter(!is.na(time), time >= 2009, time <= 2019) %>%  # Filtrar el período entre 2009 y 2019
  group_by(Fuente, cluster_pimsa) %>%  # Agrupar por Fuente y región
  summarise(promedio_ponderado = sum(Valor * ocup_n, na.rm = TRUE) / sum(ocup_n, na.rm = TRUE), 
            .groups = "drop") %>%
        filter(!is.na(cluster_pimsa)) %>%  # Excluir casos donde cluster_pimsa es NA
  ggplot(aes(x = Fuente, y = promedio_ponderado, fill = cluster_pimsa, group = cluster_pimsa)) +  
  geom_bar(stat = "identity", position = "dodge") +  # Barra separada por región
  scale_y_continuous(limits = c(0, 30)) +  # Ajustar el máximo de Y a 30
        scale_fill_discrete(name = "Grupos según extensión y profundidad",  # Cambiar el nombre en la leyenda
                        labels = c("C1. Cap. avanzado" = "Grupo 1", 
                                       "C2. Cap. extensión reciente c/desarrollo profundidad" = "Grupo 2", 
                                       "C3. Cap. extensión c/peso campo" = "Grupo 3",                                         
                                       "C4. Cap. escasa extensión c/peso campo" = "Grupo 4",
                                       "C5. Pequeña propiedad en el campo" = "Grupo 5")) +
        scale_x_discrete(labels = c("Informal" = "Asalariado informal", 
                                    "Temporario" = "Asalariado temporario", 
                                    "Parcial" = "Asalariado parcial")) +  # Cambiar nombres de las categorías de "Fuente"
  labs(x = "Fuente", y = "Promedio Ponderado", fill = "Grado", # Cambiar 'color' por 'fill' y etiqueta 'Región'
        caption = "Fuente: elaboración propia a partir de OIT-ILOSTAT.") + 
  ggtitle("Indicadores seleccionados. \n Promedio ponderado sobre ocupados \n según extensión y profundidad de relaciones capitalista. \n 2009-2019") +
  theme_minimal() +
        theme(legend.position = "bottom",  # Opcional: mover la leyenda
      axis.title.x = element_blank(),# Eliminar título del eje X
      axis.text.x = element_text(size = 12))  # Aumentar tamaño de las etiquetas de las categorías de "Fuente"

# Mismo dato anterior pero en tabla
tabla8 <- asal %>%
        filter(!is.na(informal_p_asal) | !is.na(temporario_p_asal) | !is.na(tparcial_p_asal), 
               income_group_2 != "99_Sin_datos",
               peq_estado != "Peq. estado",  # Excluir "Peq. estado"
               excl_tamaño != "Excluible") %>%  # Excluir "Excluible"
        pivot_longer(cols = c(informal_p_ocup, temporario_p_ocup, tparcial_p_ocup), 
                     names_to = "Fuente", values_to = "Valor") %>%
        filter(!is.na(Valor)) %>%
        mutate(
                time = as.numeric(as.character(time)),
                Fuente = recode(Fuente, 
                                informal_p_ocup = "Asalariado informal", 
                                temporario_p_ocup = "Asalariado temporario", 
                                tparcial_p_ocup = "Asalariado parcial")
        ) %>%
        filter(!is.na(time), time >= 2009, time <= 2019) %>%  # Filtrar el período entre 2009 y 2019
        group_by(Fuente, cluster_pimsa) %>%
        summarise(promedio_ponderado = sum(Valor * ocup_n, na.rm = TRUE) / sum(ocup_n, na.rm = TRUE), 
                  .groups = "drop") %>%
        filter(!is.na(cluster_pimsa)) %>%
        mutate(
                cluster_pimsa = recode(cluster_pimsa,
                                       "C1. Cap. avanzado" = "Grupo 1", 
                                       "C2. Cap. extensión reciente c/desarrollo profundidad" = "Grupo 2", 
                                       "C3. Cap. extensión c/peso campo" = "Grupo 3",                                         
                                       "C4. Cap. escasa extensión c/peso campo" = "Grupo 4",
                                       "C5. Pequeña propiedad en el campo" = "Grupo 5"
                )
        )

tabla8_ancha <- tabla8 %>%
        pivot_wider(names_from = Fuente, values_from = promedio_ponderado)

# Países que componen cada grupo
tabla_paises <- asal %>%
        filter(!is.na(cluster_pimsa),
               income_group_2 != "99_Sin_datos",
               peq_estado != "Peq. estado",
               excl_tamaño != "Excluible") %>%
        select(country, cluster_pimsa) %>%
        distinct() %>%
        arrange(cluster_pimsa, country)
tabla_paises_listada <- tabla_paises %>%
        group_by(cluster_pimsa) %>%
        summarise(Países = paste(sort(country), collapse = ", "), .groups = "drop")

ggsave("C:/Users/Ric/Documents/Ric/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/Estancada/graf8.jpg", plot = graf8, width = 8, height = 6, dpi = 300)


# Para conclusiones. Hipótesis
# 1 Tiempo parcial como formam propia de países de desarrollo capitalista, posiblemente
# con sistema institucional desarrollado y luego adaptado a la precariedad legal
# 2 Informalidad asalariada mayor en capitalismo en extensión con peso del campo
# Por afluencia de campo a ciudad en un sistema institucional no extendido para el asalariado?
# 3 Menor peso de informalidad asalariada en grupos 4 y 5 tal vez por incidencia de menor peso de asalariado
# 4 ¿Peso de informalidad en países de extensión reciente, una combinación de ambas situaciones?
# 5 No leer mecánicamente, no se trata de que todos llegaran a "tiempo parcial" reconocido como parte de
# desarrollo capitalista, sino que movimiento de repulsión presiona hacia formas no reconocidas desciudadanizadas
# y erosión de las formas reconocidas institucionalmente

na_cases <- asal %>%
  filter(is.na(region) | is.na(income_group)  | 
           is.na(peq_estado) | is.na(excl_tamaño))

# Ver los casos con NA en las variables especificadas
na_cases

# Explorar algún indicador de cuentapropismo
# Ver si se pueden combinar las siguientes

# Ocupación según actividad económica y ocupación (miles) -- Anual
# Id: EMP_TEMP_ECO_OCU_NB_A

# Ocupación según sexo, situación en la ocupación y actividad económica (miles) -- Anual
# EMP_TEMP_SEX_STE_ECO_NB_A

# Ocupación según sexo, situación en la ocupación y ocupación (miles) -- Anual
# Id: EMP_TEMP_SEX_STE_OCU_NB_A

# Ocupación distingue entre tres niveles de competencia
#1. Directores y gerentes, 2. Profesionales y 3. Técnicos y profesionales de nivel medio	 (Alto 3 y 4)
#4. Personal de apoyo administrativo, 5. Trabajadores de los servicios y vendedores
#        6. Agricultores y trabajadores calificados de agropecuaria, forestal y pesquera,
#        7. Oficiales, operarios y artesanos de artes mecánicas y de otros oficios,
#        8. Operadores de instalaciones y maquinaria y ensambladores	(Medio 2)
# 9. Ocupaciones elementales (Bajo 1)

# Con limitaciones se podría distinguir entre nivel bajo respecto del resto y agro/no agro
# Y TCP no agro

n_distinct(asal$time)
n_distinct(asal$ref_area)