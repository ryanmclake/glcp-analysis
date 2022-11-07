set.seed(0)
library(reshape2)
library(ggplot2)
library(dplyr)
library(imputeTS)
library(caret)
library(zoo)
library(tidyverse)
library(randomForest)
library(trend)



d <- vroom::vroom("./output/A3_glcp_filtered_biomes_joined.csv") %>%
  group_by(hylak_id) %>%
  dplyr::mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  dplyr::mutate_at(vars(total_km2),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  dplyr::mutate(total_km2 = scale(total_km2) %>% as.vector()) %>%
  ungroup(.) %>%
  dplyr::mutate_at(vars(total_km2),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  dplyr::mutate(total_precip_mm = log(total_precip_mm+1) %>% as.vector()) %>%
  dplyr::mutate(mean_annual_temp_k = log(mean_annual_temp_k+1) %>% as.vector()) %>%
  dplyr::mutate(pop_sum = log(pop_sum+1) %>% as.vector()) %>%
  dplyr::mutate(ice_cover_median = log(ice_cover_median+1) %>% as.vector()) %>%
  dplyr::mutate(mean_spec_humidity = log(mean_spec_humidity+1) %>% as.vector()) %>%
  dplyr::mutate(mean_sw_wm2 = log(mean_sw_wm2+1) %>% as.vector()) %>%
  dplyr::mutate_at(vars(ice_cover_median),funs(imputeTS::na_interpolation(., option = "linear")))






data <- d %>% ungroup(.) %>%
  select(hylak_id, total_km2, total_precip_mm, mean_annual_temp_k, mean_spec_humidity, mean_sw_wm2,
         pop_sum, elevation, shore_dev, slope_100, ice_cover_median, biome_type) %>%
  dplyr::group_by(hylak_id, elevation, shore_dev, slope_100, biome_type) %>%
  summarise(across(c(1:7),  ~list(sens.slope(ts(.))))) %>%
  mutate(sens.slope.area = unlist(purrr::map(total_km2, "estimates")),
         sens.slope.precip = unlist(purrr::map(total_precip_mm, "estimates")),
         sens.slope.temp = unlist(purrr::map(mean_annual_temp_k, "estimates")),
         sens.slope.pop = unlist(purrr::map(pop_sum, "estimates")),
         sens.slope.hum = unlist(purrr::map(mean_spec_humidity, "estimates")),
         sens.slope.ice = unlist(purrr::map(ice_cover_median, "estimates")),
         sens.slope.sw = unlist(purrr::map(mean_sw_wm2, "estimates"))) %>%
  select(-total_km2, -total_precip_mm, -mean_annual_temp_k, -pop_sum,
          -mean_spec_humidity, -mean_sw_wm2)

data_boreal <- data %>% mutate(slope_100 = ifelse(slope_100 <= 0, 0, slope_100)) %>%
  filter(biome_type == "TEMPERATE")

library(rpart)
library(rpart.plot)

glcp.tree=rpart(sens.slope.area ~ sens.slope.precip + shore_dev + slope_100 + sens.slope.ice +
                   sens.slope.temp + sens.slope.pop + elevation + sens.slope.hum, method = "anova",  data=data_boreal)
printcp(glcp.tree) # display the results
plotcp(glcp.tree) # visualize cross-validation results
summary(glcp.tree) # detailed summary of splits
rpart.plot(glcp.tree)

#Prune the Tree

p_glcp.tree<- prune(glcp.tree, cp=glcp.tree$cptable[which.min(glcp.tree$cptable[,"xerror"]),"CP"])
summary(p_glcp.tree)
rpart.plot(p_glcp.tree)
print(p_glcp.tree)









basin_rf_shrink_desert <- list()
basins <- unique(shrink_desert$hybas_id)

for(i in 1:length(basins)){
  f <- shrink_desert %>% filter(hybas_id == basins[i]) %>%
  select(hybas_id, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum) %>%
  group_by(hybas_id) %>%
  dplyr::do(model = randomForest::randomForest(formula = total_km2 ~
                                                 total_precip_mm +
                                                 mean_annual_temp_k +
                                                 pop_sum,
                                                 data = ., na.action=na.exclude)) %>%
  dplyr::ungroup(.)

e <- as.data.frame(f$model[[1]]$importance)
e$predictor <- row.names(e)
#e$NSE = hydroGOF::NSE(shrink_desert$model[[1]]$predicted, shrink_desert$model[[1]]$y)
e$basin = basins[i]
e <- e %>% arrange(-IncNodePurity)
basin_rf_shrink_desert[[i]] <- e
}

basin_level_rf_shrinking_desert = do.call(rbind, basin_rf_shrink_desert) %>%
  group_by(basin) %>%
  filter(IncNodePurity == max(IncNodePurity))%>%
  ggplot(.)+
  geom_bar(aes(x = predictor), stat = "count")

swell_temperate <- d %>% filter(sens.slope > 0) %>% filter(biome_type == "TEMPERATE")
basin_rf_swell_tropical <- list()
basins <- unique(swell_temperate$hybas_id)

for(i in 1:length(basins)){
  f <- swell_temperate %>% filter(hybas_id == basins[i]) %>%
    select(hybas_id, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum) %>%
    group_by(hybas_id) %>%
    dplyr::do(model = randomForest::randomForest(formula = total_km2 ~
                                                   total_precip_mm +
                                                   mean_annual_temp_k +
                                                   pop_sum,
                                                 data = ., na.action=na.exclude)) %>%
    dplyr::ungroup(.)
  
  e <- as.data.frame(f$model[[1]]$importance)
  e$predictor <- row.names(e)
  #e$NSE = hydroGOF::NSE(swell_temperate$model[[1]]$predicted, swell_temperate$model[[1]]$y)
  e$basin = basins[i]
  e <- e %>% arrange(-IncNodePurity)
  basin_rf_swell_temperate[[i]] <- e
}

basin_level_rf_swelling_temperate = do.call(rbind, basin_rf_swell_temperate) %>%
  group_by(basin) %>%
  filter(IncNodePurity == max(IncNodePurity))%>%
  ggplot(.)+
  geom_bar(aes(x = predictor), stat = "count")


shrink_temperate <- d %>% filter(sens.slope < 0) %>% filter(biome_type == "TEMPERATE")
basin_rf_shrink_temperate <- list()
basins <- unique(shrink_temperate$hybas_id)

for(i in 1:length(basins)){
  f <- shrink_temperate %>% filter(hybas_id == basins[i]) %>%
    select(hybas_id, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum) %>%
    group_by(hybas_id) %>%
    dplyr::do(model = randomForest::randomForest(formula = total_km2 ~
                                                   total_precip_mm +
                                                   mean_annual_temp_k +
                                                   pop_sum,
                                                 data = ., na.action=na.exclude)) %>%
    dplyr::ungroup(.)
  
  e <- as.data.frame(f$model[[1]]$importance)
  e$predictor <- row.names(e)
  #e$NSE = hydroGOF::NSE(shrink_temperate$model[[1]]$predicted, shrink_temperate$model[[1]]$y)
  e$basin = basins[i]
  e <- e %>% arrange(-IncNodePurity)
  basin_rf_shrink_temperate[[i]] <- e
}

basin_level_rf_shrinking_temperate = do.call(rbind, basin_rf_shrink_temperate) %>%
  group_by(basin) %>%
  filter(IncNodePurity == max(IncNodePurity))%>%
  ggplot(.)+
  geom_bar(aes(x = predictor), stat = "count")




swell_tropical <- d %>% filter(sens.slope > 0) %>% filter(biome_type == "TROPICAL")
basin_rf_swell_tropical <- list()
basins <- unique(swell_tropical$hybas_id)

for(i in 1:length(basins)){
  f <- swell_tropical %>% filter(hybas_id == basins[i]) %>%
    select(hybas_id, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum) %>%
    group_by(hybas_id) %>%
    dplyr::do(model = randomForest::randomForest(formula = total_km2 ~
                                                   total_precip_mm +
                                                   mean_annual_temp_k +
                                                   pop_sum,
                                                 data = ., na.action=na.exclude)) %>%
    dplyr::ungroup(.)
  
  e <- as.data.frame(f$model[[1]]$importance)
  e$predictor <- row.names(e)
  e$NSE = hydroGOF::NSE(swell_tropical$model[[1]]$predicted, swell_tropical$model[[1]]$y)
  e$basin = basins[i]
  e <- e %>% arrange(-IncNodePurity)
  basin_rf_swell_tropical[[i]] <- e
}

basin_level_rf_swelling_tropical = do.call(rbind, basin_rf_swell_tropical) %>%
  group_by(basin) %>%
  filter(IncNodePurity == max(IncNodePurity))%>%
  ggplot(.)+
  geom_bar(aes(x = predictor), stat = "count")

group_by(hylak_id) %>%
  do(mod = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
  mutate(res_std = summary(mod)$sigma,
         intercept = summary(mod)$coefficients[1],
         precip = summary(mod)$coefficients[2],
         temp = summary(mod)$coefficients[3],
         pop = summary(mod)$coefficients[4],
         r_sqr = summary(mod)$adj.r.squared)%>%
  select(-mod) %>%
  ungroup(.) %>%
  filter(r_sqr >= 0.3) %>%
  summarize_all(funs(median), na.rm = T) %>%
  mutate(biome = "Swelling/Desert")

shrink_desert <- d %>% filter(biome_type == "DESERT") %>% filter(sens.slope < 0) %>%
  group_by(hylak_id) %>%
  do(mod = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
  mutate(res_std = summary(mod)$sigma,
         intercept = summary(mod)$coefficients[1],
         precip = summary(mod)$coefficients[2],
         temp = summary(mod)$coefficients[3],
         pop = summary(mod)$coefficients[4],
         r_sqr = summary(mod)$adj.r.squared)%>%
  select(-mod) %>%
  ungroup(.) %>%
  filter(r_sqr >= 0.3) %>%
  summarize_all(funs(median), na.rm = T) %>%
  mutate(biome = "Shrinking/Desert")


swell_tropical <- d %>% filter(sens.slope > 0) %>% filter(biome_type == "TROPICAL") %>%
  group_by(hylak_id) %>%
  do(mod = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
  mutate(res_std = summary(mod)$sigma,
         intercept = summary(mod)$coefficients[1],
         precip = summary(mod)$coefficients[2],
         temp = summary(mod)$coefficients[3],
         pop = summary(mod)$coefficients[4],
         r_sqr = summary(mod)$adj.r.squared)%>%
  select(-mod) %>%
  ungroup(.) %>%
  filter(r_sqr >= 0.3) %>%
  summarize_all(funs(median), na.rm = T) %>%
  mutate(biome = "Swelling/Tropical")

shrink_tropical <- d %>% filter(sens.slope < 0) %>% filter(biome_type == "TROPICAL") %>%
  group_by(hylak_id) %>%
  do(mod = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
  mutate(res_std = summary(mod)$sigma,
         intercept = summary(mod)$coefficients[1],
         precip = summary(mod)$coefficients[2],
         temp = summary(mod)$coefficients[3],
         pop = summary(mod)$coefficients[4],
         r_sqr = summary(mod)$adj.r.squared)%>%
  select(-mod) %>%
  ungroup(.) %>%
  filter(r_sqr >= 0.3) %>%
  summarize_all(funs(median), na.rm = T) %>%
  mutate(biome = "Shrinking/Tropical")

swell_temperate <- d %>% filter(biome_type == "TEMPERATE") %>%
  group_by(hybas_id,  centr_lat, centr_lon) %>%
  do(mod = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
  mutate(res_std = summary(mod)$sigma,
         intercept = summary(mod)$coefficients[1],
         precip = summary(mod)$coefficients[2],
         temp = summary(mod)$coefficients[3],
         pop = summary(mod)$coefficients[4],
         r_sqr = summary(mod)$adj.r.squared)%>%
  select(-mod) %>%
  ungroup(.) %>%
  summarize_all(funs(median), na.rm = T) %>%
  mutate(biome = "Swelling/Temperate")

shrink_temperate <- d %>% filter(sens.slope < 0) %>% filter(biome_type == "TEMPERATE") %>%
  select(hylak_id, centr_lat, centr_lon, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum) %>%
  group_by(hylak_id,  centr_lat, centr_lon) %>%
  dplyr::mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  mutate(total_km2 = scale(total_km2),
         total_precip_mm = scale(total_precip_mm),
         mean_annual_temp_k = scale(mean_annual_temp_k),
         pop_sum = scale(pop_sum)) %>%
  na.omit(.)%>%
  do(mod = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
  mutate(res_std = summary(mod)$sigma,
         intercept = summary(mod)$coefficients[1],
         precip = summary(mod)$coefficients[2],
         temp = summary(mod)$coefficients[3],
         pop = summary(mod)$coefficients[4],
         r_sqr = summary(mod)$adj.r.squared) %>%
  select(-mod) %>%
  ungroup(.) %>%
  summarize_all(funs(median), na.rm = T) %>%
  mutate(biome = "Shrinking/Temperate")











shrink_desert_pop <- d %>% filter(sens.slope < 0) %>% filter(biome_type == "DESERT") %>%
  group_by(hylak_id) %>%
  do(mod_pop = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
  mutate(res_std_pop = summary(mod_pop)$sigma,
         intercept = summary(mod_pop)$coefficients[1],
         precip = summary(mod_pop)$coefficients[2],
         temp = summary(mod_pop)$coefficients[3],
         pop = summary(mod_pop)$coefficients[4],
         r_sqr = summary(mod_pop)$adj.r.squared,
         sd_resid_pop = sd(summary(mod_pop)$residuals)) %>%
  select(-mod_pop) %>%
  ungroup(.) %>%
  mutate(biome = "Shrink Desert") %>%
  select(hylak_id, res_std_pop, biome)


shrink_desert_nopop <- d %>% filter(sens.slope < 0) %>% filter(biome_type == "DESERT") %>%
  group_by(hylak_id) %>%
  do(mod_nopop = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k, data = ., na.action = na.exclude))%>%
  mutate(res_std_nopop = summary(mod_nopop)$sigma,
         intercept = summary(mod_nopop)$coefficients[1],
         precip = summary(mod_nopop)$coefficients[2],
         temp = summary(mod_nopop)$coefficients[3],
         r_sqr = summary(mod_nopop)$adj.r.squared,
         sd_resid_nopop = sd(summary(mod_nopop)$residuals)) %>%
  select(-mod_nopop) %>%
  ungroup(.) %>%
  mutate(biome = "Shrink Desert") %>%
  select(hylak_id, res_std_nopop, biome)

shrink_desert <- left_join(shrink_desert_nopop, shrink_desert_pop, by = c("hylak_id","biome")) %>%
  mutate(std_res_diff = res_std_nopop - res_std_pop)
















shrink_boreal <- d %>% filter(sens.slope <= 0) %>% filter(biome_type == "BOREAL/TUNDRA/ICE") %>%
  select(hylak_id, centr_lat, centr_lon, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum) %>%
  group_by(hylak_id,  centr_lat, centr_lon) %>%
  dplyr::mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  mutate(total_km2 = scale(total_km2),
         total_precip_mm = scale(total_precip_mm),
         mean_annual_temp_k = scale(mean_annual_temp_k),
         pop_sum = scale(pop_sum)) %>%
  do(mod = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
  mutate(res_std = summary(mod)$sigma,
         intercept = summary(mod)$coefficients[1],
         precip = summary(mod)$coefficients[2],
         temp = summary(mod)$coefficients[3],
         pop = summary(mod)$coefficients[4],
         r_sqr = summary(mod)$adj.r.squared)%>%
  select(-mod) %>%
  ungroup(.) %>%
  summarize_all(funs(median), na.rm = T) %>%
  mutate(biome = "Shrinking/Boreal")







heat_map_coefficients <- rbind(shrink_desert, swell_desert, shrink_temperate, swell_temperate, 
             shrink_tropical, swell_tropical, shrink_boreal, swell_boreal) %>%
  select(biome, intercept, precip, temp, pop) %>%
  rename(Intercept = intercept, Precipitation = precip, Temperature = temp, Population = pop) %>%
  melt(., id.vars = c("biome")) %>%
  rename(Biome = biome, `Coefficient Values` = value, `Coefficient` = variable)



# p1 <- ggplot(heat_map_coefficients, aes(y=Coefficient, x=`Coefficient Values`)) + 
#   geom_boxplot(outlier.shape = NA, aes(fill = Biome)) +
#   scale_fill_viridis_d(option = "B")+
#   coord_flip() +
#   xlim(c(-1.5,1.5))+
#   facet_wrap(~Biome, ncol = 4)+
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 35,size = 12, color = "black", hjust = 1),
#         axis.text.y.left = element_text(size = 12, color = "black"),
#         legend.position = "top")
# 
# 
# ggsave(p1, path = ".",
#        filename = "./output/coefficient_values.jpg",
#        width = 12, height = 8, device='jpg', dpi=1000)


p2 <- ggplot(heat_map_coefficients, aes(`Coefficient`, Biome)) +
  geom_tile(aes(fill = `Coefficient Values`)) +
  scale_fill_gradient2(midpoint=0, low="orange1", mid="white",
                       high="cyan", space ="Lab", na.value="grey",
                       name = "Trend/Biome median coefficient values")+
  theme_classic()+
  theme(axis.text.y = element_text(angle = 16),
        legend.position = "top", 
        axis.text.y.left = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.title = element_blank(),
        legend.title = element_text(size = 12))

ggsave(p2, path = ".",
       filename = "./output/coefficient_medians.jpg",
       width = 12, height = 6, device='jpg', dpi=1000)

#p.val.pop = summary(mod)$coefficients["pop_sum", "Pr(>|t|)"],

for(i in 1:length(indexes)){
  b <- area_hexes_avg %>% filter(index == indexes[i]) %>%
    dplyr::do(model = randomForest::randomForest(formula = lsa_tau ~
                                                   precip_tau +
                                                   temp_tau +
                                                   pop_tau +
                                                   snow_tau,
                                                 data = ., na.action=na.roughfix)) %>%
    dplyr::collect() %>%
    dplyr::ungroup(.)
  
  e <- as.data.frame(b$model[[1]]$importance)
  e$predictor <- row.names(e)
  e$NSE = hydroGOF::NSE(b$model[[1]]$predicted, b$model[[1]]$y)
  e$index = indexes[i]
  
  e <- e %>% arrange(-IncNodePurity)
  
  hex_rf[[i]] <- e
  
}

hex_level_rf = do.call(rbind, hex_rf) %>%
  group_by(index) %>%
  slice(1) %>%
  mutate(strong_NSE = ifelse(NSE>=0, "STRONG","WEAK"))%>%
  right_join(grid, by = "index")%>%
  st_sf()

lake_change_predictors <- hex_level_rf %>%
  mutate(predictor_new = case_when(
    predictor == "pop_tau" ~ "Δ Population",
    predictor == "precip_tau" ~ "Δ Precipitation",
    predictor == "snow_tau" ~ "Δ Snowfall",
    predictor == "temp_tau" ~ "Δ Temperature",
    TRUE ~ NA_character_))
