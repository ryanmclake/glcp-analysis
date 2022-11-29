library(tidyverse)



d <- vroom::vroom("/Volumes/SeagateBackupPlusDrive/A2_glcp_filtered_significant_sens_slopes_for_modeling.csv")

dec <- d %>% dplyr::filter(sens.slope<0)
inc <- d %>% dplyr::filter(sens.slope>0)

length(unique(dec$hybas_id))

length(unique(inc$hybas_id))

source("./code/cluster.reg.R")

Y <- inc %>% filter(country == "China") %>%
  select(hylak_id, total_km2) %>%
  group_by(hylak_id) %>%
  mutate(total_km2 = scale(total_km2) %>% as.vector(.)) %>%
  mutate(id = row_number()) %>%
  pivot_wider(names_from = hylak_id, values_from = total_km2, id_cols = id) %>%
  dplyr::mutate_all(funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  select(-id)

X <- inc %>% filter(country == "China") %>%
  select(hylak_id, pop_sum, mean_annual_temp_k, total_precip_mm) %>%
  group_by(hylak_id) %>%
  dplyr::mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  mutate(id = row_number()) %>%
  ungroup(.) %>%
  group_by(id) %>%
  summarise(pop_sum = median(pop_sum),
            total_precip_mm = median(total_precip_mm),
            mean_annual_temp_k = median(mean_annual_temp_k)) %>%
  dplyr::mutate_at(vars(total_precip_mm),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  dplyr::mutate_at(vars(mean_annual_temp_k),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  select(-id)


run<-cluster.reg(Y,X)
run

