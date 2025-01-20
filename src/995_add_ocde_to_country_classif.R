library(tidyverse)

count <- read_csv('./data/ouputs/country_classification.csv')

ocde <- tibble(iso3c=c("AUS", "JPN",
"AUT", "KOR",
"BEL", "LVA",
"CAN", "LTU",
"CHL", "LUX",
"COL", "MEX",
"CZE", "NLD",
"CRI", "NZL",
"DNK", "NOR",
"EST", "POL",
"FIN", "PRT",
"FRA", "SVK",
"DEU", "SVN",
"GRC", "ESP",
"HUN", "SWE",
"ISL", "CHE",
"IRL", "TUR",
"ISR", "GBR",
"ITA", "USA"),
ocde = rep("OCDE", 38))

count <- count %>%
        left_join(ocde) %>%
        mutate(ocde = if_else(is.na(ocde), "No OCDE", ocde),
               income_group_2 = case_when(
                       income_group == "High income" ~ "01 Altos ingresos",
                       income_group == "Upper middle income" ~ "02 Medios-altos ingresos",
                       income_group == "Lower middle income" ~ "03 Medios-bajos ingresos",
                       income_group == "Low income" ~ "04 Bajos ingresos",
                       TRUE ~ income_group
               )) %>%
        select(iso3c, country, region, income_group, income_group_2, everything())

write_csv(count, './data/ouputs/country_classification.csv')
