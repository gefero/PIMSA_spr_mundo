library(wbstats)
library(tidyverse)
library(googlesheets4)

indic <- read_sheet("https://docs.google.com/spreadsheets/d/1LILDeqcwOJkT3w4tFAGlbGWgLnT7MBcGSMU-7OyHisw/edit#gid=1507970422",
                    sheet="seleccionados")

inds <- indic %>% select(indicator_id) %>% pull()
#splits <- split(inds, ceiling(seq_along(inds)/50))
#it <- 0
#for (s in splits){
#        it <- it + 1
#        files_list <- list.files("./data/proc",full.names = TRUE)
#        file_name <- paste0('./data/proc/split_', it, ".csv")
#        if (file_name %in% files_list){
#                next
#        }
#        else{
#                print(paste0("Getting data from split ", it))
#                d <- wb_data(s, start_date = 2000, end_date = 2022, return_wide = FALSE)
#                write_csv(d, file_name)   
#        }
#        print(paste0("Saved split ", it))
#}

country_list <- readxl::read_xlsx('./data/inputs/groups_countries.xlsx', sheet="List of economies")

clst_pimsa <- read_csv('../PIMSA_pobreza/data/proc/paises_clustering_final.csv') %>%
        select(iso3c, C5) %>%
        rename(cluster_pimsa = C5)

country_list <- country_list %>% 
        rename(iso3c=Code) %>% 
        janitor::clean_names()

country_list <- country_list %>% left_join(clst_pimsa)

country_list <- country_list %>%
        mutate(across(c(income_group, region, cluster_pimsa),
                      ~if_else(is.na(.x), "S/D", .x)))


splits <- list.files("./data/proc/", pattern = "split_", full.names = TRUE)

indicators_full <-splits %>%
        map_df(~read_csv(.x))

indicators_full <- indicators_full %>% drop_na(value)

indicators_full <- indicators_full %>%
        left_join(country_list %>% select(iso3c, region, income_group, cluster_pimsa)) %>%
        select(indicator_id, indicator, region, income_group, cluster_pimsa, everything())

indic_country <- indicators_full %>%
        group_by(indicator_id, iso3c, country, income_group, region, cluster_pimsa) %>%
        summarise(min_year = min(date),
                  max_year = max(date),
                  range_year = max_year -  min_year + 1,
                  n_years = n(),
                  ratio_n_range = n_years / range_year) %>% 
        ungroup()

# indicator_coverage <- indic %>%
#         left_join( indic_country %>%
#                           group_by(indicator_id) %>%
#                           reframe(n_country=n(),
#                                     country_list = paste0(country, collapse=", "),
#                                     years = sum(n_years),
#                                     ratio = years/(22*223)))



indicator_coverage <- indic %>%
        left_join(indic_country %>%
                           group_by(indicator_id) %>%
                           summarise(
                                   n_country=n(),
                                   country_list = paste0(country, collapse=", "),
                                   p_country = n_country / 223,
                                   years = mean(ratio_n_range)
                                   ))

indicator_coverage <- indicator_coverage %>%
        left_join(
                indic_country %>%
                        group_by(indicator_id, income_group) %>%
                        summarise(
                                n_country=n(),
                                years = mean(ratio_n_range)
                        ) %>%
                        pivot_wider(names_from=income_group,
                                    values_from = c(n_country, years)),
                by=join_by(indicator_id)
        ) %>%
        left_join(
                indic_country %>%
                        group_by(indicator_id, region) %>%
                        summarise(
                                n_country=n(),
                                years = mean(ratio_n_range)
                        ) %>%
                        pivot_wider(names_from=region,
                                    values_from = c(n_country, years)),
                by=join_by(indicator_id)
        ) %>% 
        left_join(
                indic_country %>%
                        group_by(indicator_id, cluster_pimsa) %>%
                        summarise(
                                n_country=n(),
                                years = mean(ratio_n_range)
                        ) %>%
                        pivot_wider(names_from=cluster_pimsa,
                                    values_from = c(n_country, years)),
                by=join_by(indicator_id)
        )

indicator_coverage %>% write_csv('./data/inputs/wb_indicators_cobert.csv')
indicator_coverage %>% openxlsx::write.xlsx('./data/inputs/wb_indicators_cobert.xlsx')
ss <- gs4_create("indicators_WB_coverage", sheets = indicator_coverage)


