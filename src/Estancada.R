

library(Rilostat)
library(tidyverse)

library(googlesheets4)
library(gt)
library(ggplot2)
library(scales)
library(dplyr)

#OJO HAY QUE REHACER ASAL DE NUEVO PORQUE TOMÓ OCUPADOS TOTAL SOLO PARA INFORMAL Y NO PARA EL RESTO!!!!
#OJO HAY QUE REHACER ASAL DE NUEVO PORQUE TOMÓ OCUPADOS TOTAL SOLO PARA INFORMAL Y NO PARA EL RESTO!!!!
#OJO HAY QUE REHACER ASAL DE NUEVO PORQUE TOMÓ OCUPADOS TOTAL SOLO PARA INFORMAL Y NO PARA EL RESTO!!!!
#OJO HAY QUE REHACER ASAL DE NUEVO PORQUE TOMÓ OCUPADOS TOTAL SOLO PARA INFORMAL Y NO PARA EL RESTO!!!!

#Creación de base  con datos de 
#- trabajo temporario
#- trabajo tiempo parcial
#- trabajo informal
# sobre el universo de asalariados para evaluar indicadores

###############################################################
#### ASALARIADOS A TIEMPO PARCIAL
##############################################################

# Employees by sex and weekly hours actually worked (thousands) -- Annual

employeeshours <- get_ilostat(id = 'EES_TEES_SEX_HOW_NB_A', segment = 'indicator') 


country_classif <- read_csv('C:/Users/Ricardo/Documents/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/PIMSA_spr_mundo/data/ouputs/country_classification.csv')


# Construcción porcentajes porque aparece en absolutos de asalariados por bandas horarias

employeeshoursnyp <- employeeshours %>%
  filter(sex == "SEX_T") %>%  # Filtrar sexo total
  group_by(time, classif1, ref_area) %>%   
  summarize(total_obs_value = sum(obs_value, na.rm = TRUE), .groups = 'drop') %>%  # Sumar obs_value por clase
  # Calcular el total sin "HOW_BANDS_TOTAL" por cada año
  group_by(time, ref_area) %>%
  mutate(total_sin_total = sum(total_obs_value[classif1 != "HOW_BANDS_TOTAL"], na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(porcentaje = if_else(classif1 == "HOW_BANDS_TOTAL", 100,
                              (total_obs_value / total_sin_total) * 100)) %>%
  mutate(porcentaje = as.numeric(porcentaje))

options(scipen = 999) #Para que el resultado no aparezca como notación científica

# Selecciono y sumo bandas de menos de 35 hs y calculo porcentaje y absolutos

#Tiempo parcial absoluto y porcentaje y asalariados total por país y año 
tparcial <- employeeshoursnyp %>%
  # Sumar las categorías específicas de classif1
  filter(classif1 %in% c("HOW_BANDS_H00", "HOW_BANDS_H01-14", "HOW_BANDS_H15-29", "HOW_BANDS_H30-34")) %>%
  group_by(time, ref_area) %>%
  summarize(tparcial_p = sum(porcentaje, na.rm = TRUE),
         tparcial_n = sum(total_obs_value, na.rm = TRUE),
            asal_n = first(total_sin_total), .groups = 'drop')

# Incorporar categorías de grupos de países
tparcial <- tparcial %>%
  left_join(country_classif, by = c("ref_area" ="iso3c"))

# Cálculo promedios por grupo de ingreso
tparcialxingreso <- tparcial %>%
  group_by(income_group, time) %>%
  summarize(
    tparcial_proms = mean(tparcial_p, na.rm = TRUE),
    tparcial_promp = sum(tparcial_p * asal_n, na.rm = TRUE) / sum(asal_n, na.rm = TRUE),
    cantidad_paises = n_distinct(ref_area),  
    .groups = 'drop'  # Asegurarse de que el agrupamiento se elimine después de la summarización
  )

# Cantidad de países
ggplot(tparcialxingreso, aes(x = time, y = cantidad_paises, color = income_group, group = income_group)) +
  geom_line() +  # Para las líneas
  geom_point() +  # Si quieres agregar puntos en cada observación
  labs(title = "Asalariados a  tiempo parcial. Evolución de la cantidad de países con datos por grupo de ingresos",
       x = "Año",
       y = "Cantidad de países") +
  theme_minimal() +
  theme(legend.title = element_blank()) 
# La cantidad de países con información aumenta según grupo de ingreso, 2009 implica un salto en la inforamción.
# Altos ingresos, 40 países aprox
# Medio altos, entre 30 y 40 países aprox
# Medio bajos, en torno a 20 países, más oscilante
# Bajos, en tron a 5 países

#Promedio simmple
ggplot(tparcialxingreso, aes(x = time, y = tparcial_proms, color = income_group, group = income_group)) +
  geom_line() +  # Para las líneas
  geom_point() +  # Si quieres agregar puntos en cada observación
  labs(title = "% Tiempo parcial sobre asalariados. Promedio simple",
       x = "Año",
       y = "% Tiempo parcial") +
  theme_minimal() +
  theme(legend.title = element_blank()) 

#Promedio ponderado
ggplot(tparcialxingreso, aes(x = time, y = tparcial_promp, color = income_group, group = income_group)) +
  geom_line() +  # Para las líneas
  geom_point() +  # Si quieres agregar puntos en cada observación
  labs(title = "% Tiempo parcial sobre asalariados. Promedio ponderado",
       x = "Año",
       y = "% Tiempo parcial") +
  theme_minimal() +
  theme(legend.title = element_blank()) 

# Promedio 2009-2019
tparcialxingreso0919 <- tparcial %>%
  filter(!is.na(income_group) & income_group != "99_Sin_datos") %>%
  group_by(income_group) %>%
  filter(time >= 2009 & time <= 2019)  %>%
  summarize(
    tparcial_proms = mean(tparcial_p, na.rm = TRUE),
    tparcial_promp = sum(tparcial_p * asal_n, na.rm = TRUE) / sum(asal_n, na.rm = TRUE),
    .groups = 'drop'  # Asegurarse de que el agrupamiento se elimine después de la summarización
  )

# Promedio ponderado 
# Altos ingresos 31.2%
# Medio altos 15,4%
# Medio bajos 15,7%
# Bajos 32,1%
# Poner en relación con porcentaje de asalariados y cantidad de países con información
# Los países de bajos ingresos tiene tanto como los de altos 
# pero su población asalariada es menor y la cantidad de información baja.


###############################################################
###### ASALARIADOS CON CONTRATOS TEMPORARIOS
##################################################################

# Hay dos tablas, ambos con absolutos
employeestemporary <- get_ilostat(id = 'EES_XTMP_SEX_RT_A', segment = 'indicator') # Como porcentnaje de los asalariados
employeescontract <- get_ilostat(id = 'EES_TEES_SEX_AGE_JOB_NB_A', segment = 'indicator')# Absolutos según temporario/permanente y edades
# La segudna tiene total de asalariados por edades y distingue entre permanente y temporario

unique(employeescontract$classif1)

employeescontractnyp <- employeescontract %>%
  filter(sex == "SEX_T", classif1 == "AGE_YTHADULT_YGE15") %>%  # Selecciono 15 o más y total
  group_by(time, classif2, ref_area) %>%   
  summarize(total_obs_value = sum(obs_value, na.rm = TRUE), .groups = 'drop') %>%  
  group_by(time, ref_area) %>%
  mutate(total_sin_total = sum(total_obs_value[classif2 != "JOB_CONTRACT_TOTAL"], na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(porcentaje = if_else(classif2 == "JOB_CONTRACT_TOTAL", 100,
                              (total_obs_value / total_sin_total) * 100)) %>%
  mutate(porcentaje = as.numeric(porcentaje))

options(scipen = 999) #Para que el resultado no aparezca como notación científica

temporario <- employeescontractnyp %>%
  filter(classif2 == "JOB_CONTRACT_TEMP") %>%
  rename(
    temporario_p = porcentaje,
    temporario_n = total_obs_value,
    asal_n = total_sin_total
      )

# Incorporar categorías de grupos de países
temporario <- temporario %>%
  left_join(country_classif, by = c("ref_area" ="iso3c"))

# Cálculo promedios por grupo de ingreso
temporarioxingreso <- temporario %>%
  group_by(income_group, time) %>%
  summarize(
    temporario_proms = mean(temporario_p, na.rm = TRUE),
    temporario_promp = sum(temporario_p * asal_n, na.rm = TRUE) / sum(asal_n, na.rm = TRUE),
    cantidad_paises = n_distinct(ref_area),  
    .groups = 'drop'  # Asegurarse de que el agrupamiento se elimine después de la summarización
  )
  
# Cantidad de países
ggplot(temporarioxingreso, aes(x = time, y = cantidad_paises, color = income_group, group = income_group)) +
  geom_line() +  
  geom_point() +  
  labs(title = "Asalariados temporarios. Evolución de la cantidad de países con datos por grupo de ingresos",
       x = "Año",
       y = "Cantidad de países") +
  theme_minimal() +
  theme(legend.title = element_blank()) 
# La cantidad de países con información aumenta según grupo de ingreso, 
# hacia 2000 ya aumenta en los países de altos ingresos, hacia 2010 en el resto.
# Altos ingresos, entre 30 y 40 países aprox
# Medio altos, entre 15 y 25 países aprox
# Medio bajos, entre poco menos de 10 y poco más de 20 aprox, más oscilante
# Bajos, en torno a 5 países


#Promedio simmple
ggplot(temporarioxingreso, aes(x = time, y = temporario_proms, color = income_group, group = income_group)) +
  geom_line() + 
  geom_point() 
  labs(title = "% Temporario sobre asalariados. Promedio simple",
       x = "Año",
       y = "% Temporario") +
  theme_minimal() +
  theme(legend.title = element_blank()) 

#Promedio ponderado
ggplot(temporarioxingreso, aes(x = time, y = temporario_promp, color = income_group, group = income_group)) +
  geom_line() +  # Para las líneas
  geom_point() +  # Si quieres agregar puntos en cada observación
  labs(title = "% Temporario sobre asalariados. Promedio ponderado",
       x = "Año",
       y = "% Tiempo parcial") +
  theme_minimal() +
  theme(legend.title = element_blank()) 

# Promedio 2009-2019
temporarioxingreso0919 <- temporario %>%
  filter(!is.na(income_group) & income_group != "99_Sin_datos") %>%
  group_by(income_group) %>%
  filter(time >= 2009 & time <= 2019)  %>%
  summarize(
    temporario_proms = mean(temporario_p, na.rm = TRUE),
    temporario_promp = sum(temporario_p * asal_n, na.rm = TRUE) / sum(asal_n, na.rm = TRUE),
    .groups = 'drop'  # Asegurarse de que el agrupamiento se elimine después de la summarización
  )

# Promedio ponderado 
# Altos ingresos 17,8%
# Medio altos 22,1%
# Medio bajos 44,3%
# Bajos 37,7%
# Poner en relación con porcentaje de asalariados y cantidad de países con información
# Los países de ingresos bajos y medio bajos tiene proporción más alta pero con datos más oscilantes
# pero su población asalariada es menor y la cantidad de información baja.



#######################
###ASALARIADOS INFORMALES
#######################

# El dataset "Informal employment by sex and status in employment (thousands) -- Annual"
# contiene datos de empleo informal total según categoría

informalcat_ocup <- get_ilostat(id = 'EMP_NIFL_SEX_STE_NB_A', segment = 'indicator') 
unique(informalcat_ocup$classif1)

informalcat_ocup <- informalcat_ocup %>%
  filter(sex == "SEX_T", classif1 %in% c("STE_ICSE93_TOTAL", "STE_ICSE93_1", "STE_ICSE93_2", "STE_ICSE93_3", "STE_ICSE93_5", "STE_ICSE93_6", "STE_ICSE93_4")) %>%  
  mutate(classif1 = recode(classif1,
                           "STE_ICSE93_1" = "Asalariados",
                           "STE_ICSE93_2" = "Empleadores",
                           "STE_ICSE93_3" = "Trabajadores por cuenta propia (TCP)",
                           "STE_ICSE93_4" = "En cooperativas",
                           "STE_ICSE93_5" = "Trabajo familiar",
                           "STE_ICSE93_6" = "No clasificable",
                           "STE_ICSE93_TOTAL" = "Total")) 

# Commplemento con empleo por categoría ocupacional para calcular porcentaje
# Ocupación según sexo y situación en la ocupación (miles) -- Anual
# Los datos deben calculase sobre el total de ocupados de cada categoría

cat_ocup <- get_ilostat(id = 'EMP_TEMP_SEX_STE_NB_A', segment = 'indicator') 
 
unique(cat_ocup$classif1)

cat_ocup <- cat_ocup %>%
  filter(sex == "SEX_T", classif1 %in% c("STE_ICSE93_TOTAL", "STE_ICSE93_1", "STE_ICSE93_2", "STE_ICSE93_3", "STE_ICSE93_5", "STE_ICSE93_6", "STE_ICSE93_4")) %>%  
  mutate(classif1 = recode(classif1,
                           "STE_ICSE93_1" = "Asalariados",
                           "STE_ICSE93_2" = "Empleadores",
                           "STE_ICSE93_3" = "Trabajadores por cuenta propia (TCP)",
                           "STE_ICSE93_4" = "En cooperativas",
                           "STE_ICSE93_5" = "Trabajo familiar",
                           "STE_ICSE93_6" = "No clasificable",
                           "STE_ICSE93_TOTAL" = "Total")) 


informalcat_ocupnyp <- informalcat_ocup %>%
  inner_join(cat_ocup, by = c("time", "ref_area", "classif1"), suffix = c(".informal", ".ocup"))

informal <- informalcat_ocupnyp  %>%
  select (time, classif1, ref_area, obs_value.informal, obs_value.ocup) %>%
  mutate(
    informal_p = obs_value.informal * 100 / obs_value.ocup  # Calcular el porcentaje
  ) %>%
  rename(
    informal_n = obs_value.informal,
    ocup_n = obs_value.ocup
  )

# Chequeo de casos erróneos 

informal %>%
  select (ref_area, classif1, time, informal_n, ocup_n) %>%
  filter( informal_n > ocup_n) %>%  
  group_by(classif1) %>% 
  summarize(casos_erroneos = n())
# Cantidad de casos en los que informalidad es mayor que categoría ocupacional
#1 Asalariados                                       2
#2 Empleadores                                       3
#3 En cooperativas                                   1
#4 No clasificable                                  16
#5 Trabajadores por cuenta propia (TCP)             13
#6 Trabajo familiar                                113

informal %>%
  select (ref_area, classif1, time, informal_n, ocup_n) %>%
  filter( informal_n > ocup_n) %>%  
  group_by(ref_area) %>% 
  summarize(casos_erroneos = n())  %>%
  arrange(desc(casos_erroneos))

informal %>%
  select ( time, informal_n, ocup_n) %>%
  filter( informal_n > ocup_n) %>%  
  group_by(time) %>% 
  summarize(casos_erroneos = n())  %>%
  arrange(desc(casos_erroneos))
# El error está más distribuido por año

# Incorporar categorías de grupos de países
informal <- informal  %>%
  left_join(country_classif, by = c("ref_area" ="iso3c"))

# Selecciono asalariados y cálculo promedios por grupo de ingreso
ainformalxingreso <- informal  %>%
  filter (classif1 == "Asalariados") %>%
  group_by(income_group, time) %>%
  summarize(
    ainformal_proms = mean(informal_p, na.rm = TRUE),
    ainformal_promp = sum(informal_p * ocup_n, na.rm = TRUE) / sum(ocup_n, na.rm = TRUE),
    cantidad_paises = n_distinct(ref_area),  
    .groups = 'drop'  # Asegurarse de que el agrupamiento se elimine después de la summarización
  )

# Cantidad de países
ggplot(ainformalxingreso, aes(x = time, y = cantidad_paises, color = income_group, group = income_group)) +
  geom_line() +  
  geom_point() +  
  labs(title = "Asalariados informales. Evolución de la cantidad de países con datos por grupo de ingresos",
       x = "Año",
       y = "Cantidad de países") +
  theme_minimal() +
  theme(legend.title = element_blank()) 
# La cantidad de países con información aumenta según grupo de ingreso, 
# hacia 2007 ya aumenta en los países de altos ingresos, hacia 2010 en el resto.
# Altos ingresos, entre 25 y 30 países aprox
# Medio altos, entre 20 y 25 países aprox
# Medio bajos, entre  10 y 20 aprox, más oscilante
# Bajos, no supera los 10, oscilante


#Promedio simmple
ggplot(ainformalxingreso, aes(x = time, y = ainformal_proms, color = income_group, group = income_group)) +
  geom_line() + 
  geom_point() 
labs(title = "% Informal sobre asalariados. Promedio simple",
     x = "Año",
     y = "% Temporario") +
  theme_minimal() +
  theme(legend.title = element_blank()) 

#Promedio ponderado
ggplot(ainformalxingreso, aes(x = time, y = ainformal_promp, color = income_group, group = income_group)) +
  geom_line() +  
  geom_point() +  
  labs(title = "% Informal sobre asalariados. Promedio ponderado",
       x = "Año",
       y = "% Tiempo parcial") +
  theme_minimal() +
  theme(legend.title = element_blank()) 

# Promedio 2009-2019
ainformalxingreso0919 <- informal %>%
  filter(!is.na(income_group) & income_group != "99_Sin_datos" & classif1== "Asalariados" ) %>%
  group_by(income_group) %>%
  filter(time >= 2009 & time <= 2019)  %>%
  summarize(
    ainformal_proms = mean(informal_p, na.rm = TRUE),
    ainformal_promp = sum(informal_p * ocup_n, na.rm = TRUE) / sum(ocup_n, na.rm = TRUE),
    .groups = 'drop'  # Asegurarse de que el agrupamiento se elimine después de la summarización
  )

# Promedio ponderado 
# Altos ingresos 5,3%
# Medio altos 27,6%
# Medio bajos 62,1%
# Bajos 79,3%
# Poner en relación con porcentaje de asalariados y cantidad de países con información
# Los países de ingresos altos tiene proporción mínima con una población asalariada mucho mayor


##################################################
# Comparar si coincide el total de asalariados en 
# en datos de temporarios y trabajo parcial en datos orginarios
#################################################

# Unir las bases por ref_area y time
compasal1 <- tparcial %>%
        select(ref_area, time, asal_n) %>%
        rename(asal_n_tparcial = asal_n) %>%
        full_join(
                temporario %>% 
                        select(ref_area, time, asal_n) %>%
                        rename(asal_n_temporario = asal_n),
                by = c("ref_area", "time")
        )

# Filtrar para el rango de años entre 2009 y 2019
compasal1 <- compasal1 %>%
        filter(time >= 2009 & time <= 2019)

# Crear columnas para verificar coincidencias y margen de diferencia
compasal1 <- compasal1 %>%
        mutate(
                coincide = ifelse(is.na(asal_n_tparcial) | is.na(asal_n_temporario), 
                                  NA, 
                                  asal_n_tparcial == asal_n_temporario),
                diferencia = asal_n_tparcial - asal_n_temporario,
                dentro_margen = ifelse(
                        is.na(diferencia) | coincide, 
                        FALSE, 
                        abs(diferencia) / asal_n_tparcial <= 0.01  # Margen de ±1%
                ),
                fuera_margen = ifelse(
                        !is.na(diferencia) & !coincide & abs(diferencia) / asal_n_tparcial > 0.01, 
                        TRUE, 
                        FALSE
                )
        )

# Resumen de coincidencias, diferencias y márgenes
resumenasal1 <- compasal1 %>%
        summarize(
                total_casos = n(),
                coincidencias_exactas = sum(coincide, na.rm = TRUE),
                diferencias_total = sum(!coincide & !is.na(diferencia), na.rm = TRUE),
                dentro_margen = sum(dentro_margen, na.rm = TRUE),
                fuera_margen = sum(fuera_margen, na.rm = TRUE),
                sin_datos = sum(is.na(coincide))
        )
        

# Mostrar el resultado
list(compasal1 = compasal1, resumenasal1 = resumenasal1)

# Filtrar los casos fuera del margen y mostrar la diferencia
casos_fuera_margen1 <- compasal1 %>%
        filter(fuera_margen == TRUE) %>%
        select(ref_area, time, asal_n_tparcial, asal_n_temporario, diferencia) %>%
        mutate(
                margen_diferencia = round(abs(diferencia) / asal_n_tparcial * 100, 2)  # Calcular el margen de diferencia
        )

# Ver el listado de los casos fuera del margen
casos_fuera_margen1

# Calcular la frecuencia del margen de diferencia según ref_area y time
frecuencia_margen_ref_area1 <- casos_fuera_margen1 %>%
        group_by(ref_area) %>%
        summarize(frecuencia_ref_area = n(), .groups = 'drop')

frecuencia_margen_time1 <- casos_fuera_margen1 %>%
        group_by(time) %>%
        summarize(frecuencia_time = n(), .groups = 'drop')

# Mostrar los resultados
frecuencia_margen_ref_area1
frecuencia_margen_time1



# casos
# total          1071
# sin datos       291
# coinciden       211
# no coinciden    569, de los cuales
# dentro del margen +-1%        549
# fuera del margen               20, de los cuales
# diferencia >5%                  4
# AUS GTM DOM PSE concentran     17
# 2009, 2014, 2010 y 2013        14


##################################################
# Comparar si coincide el total de asalariados en 
# en datos de temporarios e informal en datos originarios
#################################################

compasal2 <- informal %>%
        filter(classif1 == "Asalariados") %>%
        select(ref_area, time, ocup_n) %>%
        rename(asal_n_informal = ocup_n) %>%
        full_join(
                temporario %>% 
                        select(ref_area, time, asal_n) %>%
                        rename(asal_n_temporario = asal_n),
                by = c("ref_area", "time")
        )

# Filtrar para el rango de años entre 2009 y 2019
compasal2 <- compasal2 %>%
        filter(time >= 2009 & time <= 2019)

# Crear columnas para verificar coincidencias y margen de diferencia
compasal2 <- compasal2 %>%
        mutate(
                coincide = ifelse(is.na(asal_n_informal) | is.na(asal_n_temporario), 
                                  NA, 
                                  asal_n_informal == asal_n_temporario),
                diferencia = asal_n_informal - asal_n_temporario,
                dentro_margen = ifelse(
                        is.na(diferencia) | coincide, 
                        FALSE, 
                        abs(diferencia) / asal_n_informal <= 0.01  # Margen de ±1%
                ),
                fuera_margen = ifelse(
                        !is.na(diferencia) & !coincide & abs(diferencia) / asal_n_informal > 0.01, 
                        TRUE, 
                        FALSE
                )
        )

# Resumen de coincidencias, diferencias y márgenes
resumenasal2 <- compasal2 %>%
        summarize(
                total_casos = n(),
                coincidencias_exactas = sum(coincide, na.rm = TRUE),
                diferencias_total = sum(!coincide & !is.na(diferencia), na.rm = TRUE),
                dentro_margen = sum(dentro_margen, na.rm = TRUE),
                fuera_margen = sum(fuera_margen, na.rm = TRUE),
                sin_datos = sum(is.na(coincide))
        )


# Mostrar el resultado
list(compasal2 = compasal2, resumenasal2 = resumenasal2)

# Filtrar los casos fuera del margen y mostrar la diferencia
casos_fuera_margen2 <- compasal2 %>%
        filter(fuera_margen == TRUE) %>%
        select(ref_area, time, asal_n_informal, asal_n_temporario, diferencia) %>%
        mutate(
                margen_diferencia = round(abs(diferencia) / asal_n_informal * 100, 2)  # Calcular el margen de diferencia
        )

# Ver el listado de los casos fuera del margen
casos_fuera_margen2

# Calcular la frecuencia del margen de diferencia según ref_area y time
frecuencia_margen_ref_area2 <- casos_fuera_margen2 %>%
        group_by(ref_area) %>%
        summarize(frecuencia_ref_area = n(), .groups = 'drop')

frecuencia_margen_time2 <- casos_fuera_margen2 %>%
        group_by(time) %>%
        summarize(frecuencia_time = n(), .groups = 'drop')

# Mostrar los resultados
frecuencia_margen_ref_area2
frecuencia_margen_time2



326
351
338
13

# casos
# total           914
# sin datos       237
# coinciden       326
# no coinciden    351, de los cuales
# dentro del margen +-1%        338
# fuera del margen               13, de los cuales
# diferencia >5%                  3
# AUS  concentran                11
# 2011, 2014                     4

######################################################
# UNIFICAR BASES 
#####################################################

# Filtrar y renombrar en la base informal
informal_asal <- informal %>%
        filter(classif1 == "Asalariados") %>% # Filtrar asalariados
        select(-classif1) %>%      # Eliminar classif1 y classif2
        rename(asal_n_inf = ocup_n)           # Renombrar asal_n con sufijo _inf

# Renombrar asal_n en las otras bases
temporario_asal <- temporario %>%
        select(-classif2) %>%      # Eliminar classif1 y classif2 si existen
        rename(asal_n_temp = asal_n)

tparcial_asal <- tparcial %>%
        rename(asal_n_tp = asal_n)

# Unir las bases
asal <- informal_asal %>%
        full_join(temporario_asal, by = c("ref_area", "time", "country", "region", 
                                                "income_group", "income_group_2", "cluster_pimsa", "peq_estado", "excl_tamaño", "ocde")) %>%
        full_join(tparcial_asal, by = c("ref_area", "time", "country", "region", 
                                                 "income_group", "income_group_2", "cluster_pimsa", "peq_estado", "excl_tamaño", "ocde")) %>%
        # Reordenar columnas para que country a excl_tamaño estén al final
        select(-country, -region, -income_group, -income_group_2, -cluster_pimsa, -peq_estado, -excl_tamaño, -ocde,
               country, region, income_group, income_group_2, cluster_pimsa, peq_estado, excl_tamaño, ocde)


# Chequear que datos de asalariados coincidan


asal <- asal %>%
        mutate(
                # Calcular la diferencia porcentual respecto a asal_n_inf, manejando NAs
                dif_temp = if_else(!is.na(asal_n_inf) & !is.na(asal_n_temp), 
                                   (asal_n_temp - asal_n_inf) / asal_n_inf * 100, NA_real_),
                dif_tp   = if_else(!is.na(asal_n_inf) & !is.na(asal_n_tp),   
                                   (asal_n_tp - asal_n_inf) / asal_n_inf * 100, NA_real_),
                
                # Clasificar los casos según los criterios indicados
                categoria = case_when(
                        is.na(asal_n_inf) & is.na(asal_n_temp) & is.na(asal_n_tp) ~ "sin datos",
                        
                        # Si solo dos tienen datos, comparar entre ellas
                        is.na(asal_n_inf) ~ if_else(abs(asal_n_temp - asal_n_tp) / asal_n_temp * 100 <= 1, 
                                                    "b) Dentro del margen", "c) Fuera del margen"),
                        is.na(asal_n_temp) ~ if_else(abs(asal_n_inf - asal_n_tp) / asal_n_inf * 100 <= 1, 
                                                     "b) Dentro del margen", "c) Fuera del margen"),
                        is.na(asal_n_tp) ~ if_else(abs(asal_n_inf - asal_n_temp) / asal_n_inf * 100 <= 1, 
                                                   "b) Dentro del margen", "c) Fuera del margen"),
                        
                        # Si las tres tienen datos
                        asal_n_inf == asal_n_temp & asal_n_inf == asal_n_tp ~ "a) Coinciden",
                        abs(dif_temp) <= 1 & abs(dif_tp) <= 1 ~ "b) Dentro del margen",
                        TRUE ~ "c) Fuera del margen"
                )
        ) %>%
        # Convertir cualquier NA en "sin datos"
        mutate(categoria = replace_na(categoria, "sin datos"))

asal %>%
        count(categoria)

# a) Coinciden           191
# b) Dentro del margen  1627
# c) Fuera del margen     53
# sin datos              471

asal %>%
        filter(categoria == "c) Fuera del margen") %>%
        count(country)
# De los 51
# Australia 20
# Bolivia 8 
# Guatemala 5
# Panama 4
# El resto se distribuye entre 9 países.

asal %>%
        filter(categoria == "c) Fuera del margen") %>%
        count(time)
# No se concentran en un año

asal %>%
        filter(categoria == "c) Fuera del margen") %>%
        count(excl_tamaño)
# Todos incluibles

asal %>%
        filter(categoria == "c) Fuera del margen") %>%
        count(peq_estado)
# Ninguno pequeño estado


# Unir con datos de cantidad de ocupados.
asal <- asal %>%
        left_join(informal %>%
                          filter(classif1 == "Total") %>%
                          select(ref_area, time, ocup_n), 
                  by = c("ref_area", "time"))

# Ordenar la base
asal <- asal %>%
        select(time, ref_area, ocup_n, asal_n_inf, informal_n, informal_p, temporario_n, temporario_p, tparcial_n, tparcial_p, everything())
# Excluyo los 53 casos fuera de margen
asal <- asal %>%
        filter(categoria != "c) Fuera del margen")
# Tomo solo la variable de número de asalariados que aparecía en la base de informalidad
# porque había sido tomada de cat_ocup y porque ya fueron excluidos los valores que no coincidían procedentes de las otras bases
asal <- asal %>%
        rename(asal_n = asal_n_inf)
# Elimino variables superfluas
asal <- asal %>%
        select(-dif_temp, -dif_tp, -categoria, -asal_n_temp, -asal_n_tp)
# Renombrar variables para posterior recálculo sobre ocupados
asal <- asal %>%
        rename(
                informal_n_asal = informal_n,
                informal_p_asal = informal_p,
                temporario_n_asal = temporario_n,
                temporario_p_asal = temporario_p,
                tparcial_n_asal = tparcial_n,
                tparcial_p_asal = tparcial_p       )
# Generar recálculo sobre total de ocupados
asal <- asal %>%
        mutate(
                informal_p_ocup = informal_n_asal * 100 / ocup_n,
                temporario_p_ocup = temporario_n_asal * 100 / ocup_n,
               tparcial_p_ocup = tparcial_n_asal * 100 / ocup_n
        )
# Generar proporción de asalariados sobre total
asal <- asal %>%
        mutate(
                asal_p = asal_n *100 / ocup_n)
# Ordenar base
asal <- asal %>%
        select(time, ref_area, ocup_n, asal_n, asal_p, 
               informal_n_asal, informal_p_asal, informal_p_ocup, 
               temporario_n_asal, temporario_p_asal, temporario_p_ocup, 
               tparcial_n_asal, tparcial_p_asal, tparcial_p_ocup, everything())


