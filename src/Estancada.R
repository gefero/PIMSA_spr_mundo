

library(Rilostat)
library(tidyverse)

library(googlesheets4)
library(gt)
library(ggplot2)
library(scales)
library(dplyr)

###############################################################
#### ASALARIADOS A TIEMPO PARCIAL
##############################################################

# Employees by sex and weekly hours actually worked (thousands) -- Annual

employeeshours <- get_ilostat(id = 'EES_TEES_SEX_HOW_NB_A', segment = 'indicator') 
country_classif <- read_csv('C:/Users/Ricardo/Documents/PIMSA/Estructura/Equipo Estructura/Proyecto Superpoblación/Equipo con Brasil/country_classification.csv')

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


###Pendiente, 
# Comparar asalariados total entre indicadores
# Calcular sobre % de asalariados en ocupados
# Explorar algún indicador de cuentapropismo
