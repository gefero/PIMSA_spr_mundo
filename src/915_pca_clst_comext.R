## Tareas:

library(tidyverse)
peq_estados <- c("ASM","DMA","GRD","XKX","MHL","KIR","FSM","AND","ATG","ABW",
                 "BMU","VGB","CYM","CUW","FRO","GIB","GRL","IMN","LIE","MCO",
                 "NRU","MNP","SMR","SYC","SXM","KNA","MAF","TCA")

excl_tamaño <- c("TUV",
                 "NRU",
                 "PLW",
                 "VGB",
                 "MAF",
                 "GIB",
                 "SMR",
                 "MCO",
                 "LIE",
                 "MHL",
                 "SXM",
                 "ASM",
                 "TCA",
                 "KNA",
                 "MNP",
                 "FRO",
                 "GRL",
                 "BMU",
                 "CYM",
                 "DMA",
                 "AND",
                 "IMN",
                 "ATG",
                 "VCT",
                 "VIR",
                 "ABW",
                 "TON",
                 "FSM",
                 "SYC",
                 "GRD",
                 "KIR",
                 "CUW",
                 "GUM",
                 "CHI",
                 "LCA",
                 "WSM",
                 "STP",
                 "NCL",
                 "BRB",
                 "PYF",
                 "VUT")


#p <- read_csv('./data/proc/com_ext_WB_ILOSTAT_spr_comext_dataset_nuevo.csv') %>%
#        filter(!indicator_id %in% c("BM.GSR.ROYL.CD", "BX.GSR.ROYL.CD")) %>%
#        filter(country=="Panama")    %>%
#        mutate(excl_tamaño = if_else(iso3c %in% excl_tamaño, 'Excluíble', "No excluible")) %>%
#        select(-classif1, -classif2)

#p %>% filter(indicator_id=="TX.VAL.MRCH.CD.WT") %>% select(value)

comext <- read_csv('./data/proc/com_ext_WB_ILOSTAT_spr_comext_dataset_nuevo.csv') %>%
        filter(!indicator_id %in% c("BM.GSR.ROYL.CD", "BX.GSR.ROYL.CD")) %>%
        filter(country!="Panama") %>% 
        mutate(peq_estado = if_else(iso3c %in% peq_estados, 'Peq. estado', "No peq. estado")) %>%
        mutate(excl_tamaño = if_else(iso3c %in% excl_tamaño, 'Excluíble', "No excluible")) %>%
        select(-classif1, -classif2) %>%
        mutate(value = if_else(
                indicator_id == "TX.VAL.MRCH.CD.WT" & country == "Panama", value/100, value)
        )

country_data <- comext %>% 
        select(iso3c, country, region, income_group, cluster_pimsa, peq_estado, excl_tamaño) %>%
        distinct()

#write_csv(country_data, './data/ouputs/country_classification.csv')
comext %>%
        select(indicator_id, indicator) %>%
        distinct()

x <- comext %>% filter(country=="Panama" & indicator_id=="TX.VAL.TECH.MF.ZS")

comext_agg <- comext %>%
        filter(excl_tamaño=="No excluible") %>%
        group_by(iso3c, country, indicator_id) %>%
        summarise(mean_value = mean(value, na.rm = TRUE),
        ) %>%
        #drop_na(mean_value) %>%
        pivot_wider(names_from=indicator_id,
                    values_from=mean_value) %>%
        ungroup() %>%
        mutate(prop_market_exp = 100 * TX.VAL.MRCH.CD.WT / sum(TX.VAL.MRCH.CD.WT, na.rm = TRUE)
        ) %>%
        rename(medhi_tech_exp = TX.MNF.TECH.ZS.UN,
               hi_tech_exp = TX.VAL.TECH.MF.ZS,
               manuf_exp = TX.VAL.MANF.ZS.UN ,
               merch_exp = TX.VAL.MRCH.CD.WT) 


#%>%
#        filter(iso3c != "PAN") %>%
        #mutate(med_tech_exp = medhi_tech_exp - hi_tech_exp) %>%
#        drop_na()

### Limpio med y medhi
#x <- comext %>%
#        filter(iso3c == "GNB" & indicator_id %in% c("TX.MNF.TECH.ZS.UN", "TX.VAL.TECH.MF.ZS") )



comext_agg<-comext_agg %>%
        mutate(medhi_tech_exp = case_when(
                is.na(medhi_tech_exp) & !is.na(hi_tech_exp) ~ hi_tech_exp,
                is.na(medhi_tech_exp) & is.na(hi_tech_exp) ~ 0,
                TRUE ~ medhi_tech_exp
        ),
        hi_tech_exp = case_when(
                !is.na(medhi_tech_exp) & is.na(hi_tech_exp) ~ 0,
                is.na(medhi_tech_exp) & is.na(hi_tech_exp) ~ 0,
                TRUE ~ hi_tech_exp 
        ))

## Asumo que NaN es cero en exportaciones
comext_agg <- comext_agg %>%
        mutate(manuf_exp = replace_na(manuf_exp, 0),
               merch_exp = replace_na(merch_exp, 0),
               prop_market_exp = replace_na(prop_market_exp, 0)
        )

### Chequeo NAS
#setdiff(unique(comext$country), comext_agg$country)
#nas <- setdiff(unique(comext$iso3c), comext_agg$iso3c)

#country_data %>%
#        filter(iso3c %in% nas) %>%
#        select(iso3c, country, income_group, cluster_pimsa, peq_estado) %>%
#        arrange(cluster_pimsa, income_group, excl_tamaño, iso3c) %>%
#        write_csv('./data/ouputs/NAs_comext.csv')

### PCA
library(tidymodels)
recipe_pca <- comext_agg %>%
        recipe(~.) %>%
        update_role(c(iso3c, country), new_role = "id variable") %>%
        step_rm(merch_exp) %>%
        step_normalize(all_numeric()) %>%
        step_pca(all_numeric(), num_comp = 4)

pca_estimates <- prep(recipe_pca, training = comext_agg)
pca_data <- bake(pca_estimates, comext_agg)

tidy(pca_estimates, 3, type = "coef") %>%
        filter(component %in% c("PC1", "PC2", "PC3", "PC4")) %>%
        ggplot(aes(value, terms, fill = terms)) + 
        geom_col(show.legend = FALSE) + geom_text(aes(label = round(value,
                                                                                                                    +     2))) + labs(title = "Cargas factoriales (comp. 1 y 2)", x = "Valo", y = "Variable") +
        facet_wrap(~component, nrow = 1) + theme_minimal()

tidy(pca_estimates, 3, type = "variance") %>%
        filter(terms == "cumulative percent variance") %>%
        mutate(component = paste0("PC", component)) %>%
        ggplot(aes(x = component, y = value, group = terms)) + 
        geom_col() + 
        ylim(0, 100) +
        labs(title = "% varianza acum.", x = "Componente", y = "Valor") + 
        theme_minimal()

# Generamos un vector con la secuencia de clústers
centers <- 1:8
# Incializamos dos tibbles vacías para llenar con las asignaciones de clusters
# y con algunas métricas
assignments <- tibble()
clusterings <- tibble()

# Iteramos sobre cada uno de los elementos de centers
for (i in centers) {
        # Corremos la receta, seleccionamos las variables para clusterizar y
        # corremos el k-medias con el
        km <- recipe_pca %>%
                prep() %>%
                bake(comext_agg) %>%
                select(-iso3c, -country) %>%
                kmeans(x = ., centers = i)
        
        # Ejecutamos glance sobre km para extraer las métricas de variabilidad
        # intra y extracluster y las agregamos a una de las tibbles
        clusterings <- clusterings %>%
                bind_rows(glance(km) %>%
                mutate(k = i) %>%
                select(k, everything()))
        # Ejecutamos augment sobre km y df para agregar las pertenencias a los
        # clusters a la tabla original; luego, apilamos todas las tablas en una
        # sola
        assignments <- assignments %>%
                bind_rows(augment(km, comext_agg) %>%
                mutate(k = i) %>%
                select(k, everything()))
}

ggplot(clusterings, aes(x = as.factor(k), y = tot.withinss, group = 1)) + 
        geom_line() +
        geom_point() + 
        geom_vline(xintercept = 6, linetype = "dashed") + 
        theme_minimal() +
        labs(x = "Cantidad clústers (k)", y = "Variabilidad intra-cluster")

km_2clst <- recipe_pca %>%
        prep() %>%
        bake(comext_agg) %>%
        select(-iso3c, -country) %>%
        kmeans(x = ., centers = 6)



#cl <- assignments %>%
#        filter(k == 3 & .cluster==3)

clst_iso3c <- assignments %>%
        filter(k %in% c(3,4,5,6)) %>%
        select(k, iso3c, .cluster) %>%
        pivot_wider(id_cols=iso3c,
                    names_from = k,
                    names_prefix = "clst_",
                    values_from= .cluster) %>%
        mutate(across(starts_with("clst_"), ~as.numeric(as.character(.x))))


comext_agg <- comext_agg %>%
        left_join(clst_iso3c)


assignments %>%
        filter(k == 4) %>%
        group_by(.cluster) %>%
        summarise(n=n())

#table(comext_agg$clst_5)

#comext_agg %>% filter(clst_5 == 4)

assignments %>%
        filter(k == 4) %>% 
        select(k, iso3c, country, .cluster,medhi_tech_exp:prop_market_exp,
               -merch_exp) %>%
        pivot_longer(cols = medhi_tech_exp:prop_market_exp) %>%
        ggplot(aes(y = value, fill = .cluster)) + 
        geom_boxplot() + 
        #ylim(0,100) +
        theme_minimal() +
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_blank(), 
              axis.ticks.x = element_blank()) +
        facet_wrap(~name, scales = "free_y") 


comext_agg %>%
        write_csv('./data/proc/20240315_clusters_comext.csv')

comext_agg %>%
        haven::write_sav('./data/proc/20240315_clusters_comext.sav')


