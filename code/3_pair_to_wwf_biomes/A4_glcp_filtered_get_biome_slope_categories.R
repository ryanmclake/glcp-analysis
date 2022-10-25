
library(reshape2)
library(ggplot2)
library(dplyr)
library(imputeTS)
library(caret)

d <- vroom::vroom("./output/A3_glcp_filtered_biomes_joined.csv")

swell_desert <- d %>% filter(sens.slope > 0) %>% filter(biome_type == "DESERT") %>%
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
  mutate(biome = "Swelling/Desert")

shrink_desert <- d %>% filter(sens.slope < 0) %>% filter(biome_type == "DESERT") %>%
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
  mutate(biome = "Shrinking/Desert")


swell_tropical <- d %>% filter(sens.slope > 0) %>% filter(biome_type == "TROPICAL") %>%
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
         r_sqr = summary(mod)$adj.r.squared) %>%
  select(-mod) %>%
  ungroup(.) %>%
  summarize_all(funs(median), na.rm = T) %>%
  mutate(biome = "Swelling/Tropical")

shrink_tropical <- d %>% filter(sens.slope < 0) %>% filter(biome_type == "TROPICAL") %>%
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
         r_sqr = summary(mod)$adj.r.squared) %>%
  select(-mod) %>%
  ungroup(.) %>%
  summarize_all(funs(median), na.rm = T) %>%
  mutate(biome = "Shrinking/Tropical")

swell_temperate <- d %>% filter(sens.slope > 0) %>% filter(biome_type == "TEMPERATE") %>%
  select(hylak_id, centr_lat, centr_lon, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum) %>%
  group_by(hylak_id,  centr_lat, centr_lon) %>%
  dplyr::mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  mutate(total_km2 = scale(total_km2),
         total_precip_mm = scale(total_precip_mm),
         mean_annual_temp_k = scale(mean_annual_temp_k),
         pop_sum = scale(pop_sum)) %>%
  na.omit(.) %>%
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

swell_boreal <- d %>% filter(sens.slope > 0) %>% filter(biome_type == "BOREAL/TUNDRA/ICE") %>%
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
         r_sqr = summary(mod)$adj.r.squared) %>%
  select(-mod) %>%
  ungroup(.) %>%
  summarize_all(funs(median), na.rm = T) %>%
  mutate(biome = "Swelling/Boreal")

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

