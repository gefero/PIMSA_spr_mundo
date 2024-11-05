library(wbstats)
library(tidyverse)
indicators <- wb_indicators() %>%
        #unnest(cols=topics) %>%
        select(-unit)

indicators <- indicators %>%
        unnest(topics, keep_empty = TRUE) %>%
        rename(topic_id = id,
               topic_desc = value) %>%
        distinct(indicator_id, .keep_all = TRUE)

write_csv(indicators, './data/inputs/wb_indicators.csv')


filter_words <- c("Women",
"Students",
"Adults",
"Youth",
"Labor force",
"Children",
"Child",
"Population",
"People",
"Female",
"Male",
"Young",
"Employment",
"Children",
"Adolescents",
"Age",
"Capita",
"Households")


indicators %>% 
        mutate(indicator = str_to_lower(indicator)) %>%
        filter(indicator %in% "poverty")

grepl(indicators$indicator, filter_words, fixed = TRUE)
