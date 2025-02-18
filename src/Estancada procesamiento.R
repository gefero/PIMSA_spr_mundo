


library(Rilostat)
library(tidyverse)

library(googlesheets4)
library(gt)
library(ggplot2)
library(scales)
library(dplyr)



# Cantidad de países con datos
asal %>%
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
  labs(x = "Año", y = "Cantidad de países con información", color = "Grupo de ingresos") +
  ggtitle("Cantidad de países con datos. Indicadores seleccionados") +
  theme_minimal()

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
  scale_y_continuous(limits = c(0, 45)) +  # Establecer el máximo de Y en 40
  labs(x = "Año", y = "Cantidad de países con información", color = "Grupo de ingresos") +
  ggtitle("Cantidad de países con datos (sin pequeños estados y escasa población). Indicadores seleccionados") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Desde aquí ya selecciono el periodo 2009 / 2019

# Promedio simple de indicadores / asalariados
asal %>%
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
  labs(x = "Año", y = "Promedio", color = "Grupo de ingresos") +
  ggtitle("Indicadores seleccionados. Promedio simple sobre asalariados") +
        theme_minimal() +
        theme(legend.position = "bottom")

# Promedio ponderado de indicadores sobre asalariados
asal %>%
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
  labs(x = "Año", y = "Promedio Ponderado", color = "Grupo de ingresos") +
  ggtitle("Indicadores seleccionados. Promedio ponderado sobre asalariados") +
        theme_minimal() +
        theme(legend.position = "bottom")

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

asal %>%
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
  labs(x = "Año", y = "Promedio", color = "Grupo de ingresos") +
  ggtitle("Asalariados. Promedio simple sobre ocupados") +
        theme_minimal() +
        theme(legend.position = "bottom")

# Promedio ponderado asalariados 
asal %>%
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
  labs(x = "Año", y = "Promedio ponderado", color = "Grupo de ingresos") +
  ggtitle("Asalariados. Promedio ponderado sobre ocupados") +
        theme_minimal() +
        theme(legend.position = "bottom")

# Promedio simple de indicadores sobre / ocupados

asal %>%
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
  labs(x = "Año", y = "Promedio", color = "Grupo de ingresos") +
  ggtitle("Indicadores seleccionados. Promedio simple sobre ocupados") +
        theme_minimal() +
        theme(legend.position = "bottom")

# Promedio ponderado de indicadores sobre ocupados
asal %>%
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
  labs(x = "Año", y = "Promedio Ponderado", color = "Grupo de ingresos") +
  ggtitle("Indicadores seleccionados. Promedio ponderado sobre ocupados") +
        theme_minimal() +
        theme(legend.position = "bottom")

# Los indicadores ponderados dan como resultado:
# informal en países medios y bajos (que posiblemente incluye temporarios)
# trabajo parcial en altos ingresos (que puede superponerse temporarios)
# Ver posibilidad de seleccionar informales para medios y bajos y parcial para altos?


# # Promedio ponderado de indicadores sobre ocupadospor grupo de ingresos (total 2009-2019)
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
  group_by(Fuente, income_group_2) %>%  # Agrupar por Fuente y grupo de ingresos
  summarise(promedio_ponderado = sum(Valor * ocup_n, na.rm = TRUE) / sum(ocup_n, na.rm = TRUE), 
            .groups = "drop") %>%
  ggplot(aes(x = Fuente, y = promedio_ponderado, fill = income_group_2, group = income_group_2)) +
  geom_bar(stat = "identity", position = "dodge") +  # Barra separada por grupo de ingresos
  scale_y_continuous(limits = c(0, 30)) +  # Ajustar el máximo de Y a 100
  labs(x = "Fuente", y = "Promedio Ponderado", color = "Grupo de ingresos") +
  ggtitle("Indicadores seleccionados. Promedio ponderado sobre ocupados por grupo de ingresos. 2009-2019") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Opcional: mover la leyenda

# # Promedio ponderado de indicadores sobre ocupados por región (total 2009-2019)
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
  group_by(Fuente, region) %>%  # Agrupar por Fuente y región
  summarise(promedio_ponderado = sum(Valor * ocup_n, na.rm = TRUE) / sum(ocup_n, na.rm = TRUE), 
            .groups = "drop") %>%
        complete(Fuente, region, fill = list(promedio_ponderado = 0)) %>% 
  ggplot(aes(x = Fuente, y = promedio_ponderado, fill = region, group = region)) +  # Cambiar a 'region' en el gráfico
  geom_bar(stat = "identity", position = "dodge") +  # Barra separada por región
  scale_y_continuous(limits = c(0, 40)) +  # Ajustar el máximo de Y a 30
  labs(x = "Fuente", y = "Promedio Ponderado", fill = "Región") +  # Cambiar 'color' por 'fill' y etiqueta 'Región'
  ggtitle("Indicadores seleccionados. Promedio ponderado sobre ocupados por región. 2009-2019") +
        theme_minimal() +
        theme(legend.position = "bottom")  # Opcional: mover la leyenda

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

# # Promedio ponderado de indicadores sobre ocupados por grado (total 2009-2019)
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
  group_by(Fuente, cluster_pimsa) %>%  # Agrupar por Fuente y región
  summarise(promedio_ponderado = sum(Valor * ocup_n, na.rm = TRUE) / sum(ocup_n, na.rm = TRUE), 
            .groups = "drop") %>%
        filter(!is.na(cluster_pimsa)) %>%  # Excluir casos donde cluster_pimsa es NA
  ggplot(aes(x = Fuente, y = promedio_ponderado, fill = cluster_pimsa, group = cluster_pimsa)) +  
  geom_bar(stat = "identity", position = "dodge") +  # Barra separada por región
  scale_y_continuous(limits = c(0, 30)) +  # Ajustar el máximo de Y a 30
  labs(x = "Fuente", y = "Promedio Ponderado", fill = "Grado") +  # Cambiar 'color' por 'fill' y etiqueta 'Región'
  ggtitle("Indicadores seleccionados. Promedio ponderado sobre ocupados según grado. 2009-2019") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Opcional: mover la leyenda

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