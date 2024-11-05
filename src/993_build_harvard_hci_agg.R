library(tidyverse)
hs<-read_delim('./data/inputs/HAR_hs_product.tab', delim="\t",
               col_names = c("product_id", 
                             "hs_product_code",
                             "hs_product_name_short_en",
                             "level",
                             "parent_id"))

hs <- hs %>%
        mutate(parent_id = case_when(
                hs_product_code == "services" ~ 10,
                hs_product_code == "travel" ~ 11,
                hs_product_code == "transport" ~ 12,
                hs_product_code == "ict" ~ 13,
                hs_product_code == "financial" ~ 14,
                hs_product_code == "unspecified" ~ 99,
                #hs_product_code %in% as.character(c(0:9)) ~ as.double(hs_product_code),
                TRUE ~ parent_id
        ))

data <- read_delim('./data/inputs/HAR_country_hsproduct2digit_year.tab',
                   delim="\t")

data <- data %>%
        left_join(hs)
        


agg_data <- data %>%
        group_by(location_code, year, parent_id) %>%
        summarise(across(c(export_value,
                           import_value,
                           hs_eci,
                           hs_coi,
                           pci
                           ), ~sum(.x, na.rm = TRUE))
        ) %>% ungroup()

labels_final<-hs %>%
        select(parent_id, hs_product_name_short_en) %>%
        filter(is.na(parent_id) | parent_id %in% c(10:14, 99)) %>%
        distinct() %>%
        mutate(parent_id = case_when(
                hs_product_name_short_en == "Textiles" ~ 0,
                hs_product_name_short_en == "Agriculture" ~ 1,
                hs_product_name_short_en == "Stone" ~ 2,
                hs_product_name_short_en == "Minerals" ~ 3,
                hs_product_name_short_en == "Metals" ~ 4,
                hs_product_name_short_en == "Chemicals" ~ 5,
                hs_product_name_short_en == "Vehicles" ~ 6,
                hs_product_name_short_en == "Machinery" ~ 7,
                hs_product_name_short_en == "Electronics" ~ 8,
                hs_product_name_short_en == "Other" ~ 9,
                TRUE ~ parent_id
        ))

agg_data <- agg_data %>%
        left_join(labels_final) %>%
        select(location_code, year, 
               parent_id, hs_product_name_short_en,
               everything()) %>%
        rename(iso3c = location_code,
               hs_product_name = hs_product_name_short_en)

write_csv(agg_data, './data/proc/harvard_eci_1digit.csv')


agg_data <- read_csv('./data/proc/harvard_eci_1digit.csv')
haven::write_sav(agg_data, './data/proc/harvard_eci_1digit.sav')
