
d <- vroom::vroom("./output/A3_glcp_filtered_biomes_joined.csv")


swell_desert <- d %>% filter(sens.slope > 0) %>% filter(biome_type == "DESERT") %>%
  select(hylak_id, hybas_id, centr_lat, centr_lon, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum) %>%
  group_by(hybas_id, hylak_id, centr_lat, centr_lon) %>%
  mutate(total_km2 = scale(total_km2),
         total_precip_mm = scale(total_precip_mm),
         mean_annual_temp_k = scale(mean_annual_temp_k),
         pop_sum = scale(pop_sum)) %>%
  do(mod = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
  mutate(res_std = summary(mod)$sigma,
         intercept = summary(mod)$coefficients[1],
         precip = summary(mod)$coefficients[2],
         p.val.precip = summary(mod)$coefficients["total_precip_mm", "Pr(>|t|)"],
         temp = summary(mod)$coefficients[3],
         p.val.temp = summary(mod)$coefficients["mean_annual_temp_k", "Pr(>|t|)"],
         pop = summary(mod)$coefficients[4],
         p.val.pop = summary(mod)$coefficients["pop_sum", "Pr(>|t|)"],
         r_sqr = summary(mod)$adj.r.squared)%>%
  select(-mod)

shrink_desert <- d %>% filter(sens.slope < 0) %>% filter(biome_type == "DESERT") %>%
  select(hylak_id, hybas_id, centr_lat, centr_lon, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum) %>%
  group_by(hybas_id, hylak_id, centr_lat, centr_lon) %>%
  mutate(total_km2 = scale(total_km2),
         total_precip_mm = scale(total_precip_mm),
         mean_annual_temp_k = scale(mean_annual_temp_k),
         pop_sum = scale(pop_sum)) %>%
  do(mod = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
  mutate(res_std = summary(mod)$sigma,
         intercept = summary(mod)$coefficients[1],
         precip = summary(mod)$coefficients[2],
         p.val.precip = summary(mod)$coefficients["total_precip_mm", "Pr(>|t|)"],
         temp = summary(mod)$coefficients[3],
         p.val.temp = summary(mod)$coefficients["mean_annual_temp_k", "Pr(>|t|)"],
         pop = summary(mod)$coefficients[4],
         p.val.pop = summary(mod)$coefficients["pop_sum", "Pr(>|t|)"],
         r_sqr = summary(mod)$adj.r.squared)%>%
  select(-mod)


swell_tropical <- d %>% filter(sens.slope > 0) %>% filter(biome_type == "TROPICAL") %>%
  select(hylak_id, hybas_id, centr_lat, centr_lon, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum) %>%
  group_by(hybas_id, hylak_id, centr_lat, centr_lon) %>%
  mutate(total_km2 = scale(total_km2),
         total_precip_mm = scale(total_precip_mm),
         mean_annual_temp_k = scale(mean_annual_temp_k),
         pop_sum = scale(pop_sum)) %>%
  do(mod = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
  mutate(res_std = summary(mod)$sigma,
         intercept = summary(mod)$coefficients[1],
         precip = summary(mod)$coefficients[2],
         p.val.precip = summary(mod)$coefficients["total_precip_mm", "Pr(>|t|)"],
         temp = summary(mod)$coefficients[3],
         p.val.temp = summary(mod)$coefficients["mean_annual_temp_k", "Pr(>|t|)"],
         pop = summary(mod)$coefficients[4],
         p.val.pop = summary(mod)$coefficients["pop_sum", "Pr(>|t|)"],
         r_sqr = summary(mod)$adj.r.squared)%>%
  select(-mod)

shrink_tropical <- d %>% filter(sens.slope < 0) %>% filter(biome_type == "TROPICAL") %>%
  select(hylak_id, hybas_id, centr_lat, centr_lon, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum) %>%
  group_by(hybas_id, hylak_id, centr_lat, centr_lon) %>%
  mutate(total_km2 = scale(total_km2),
         total_precip_mm = scale(total_precip_mm),
         mean_annual_temp_k = scale(mean_annual_temp_k),
         pop_sum = scale(pop_sum)) %>%
  do(mod = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
  mutate(res_std = summary(mod)$sigma,
         intercept = summary(mod)$coefficients[1],
         precip = summary(mod)$coefficients[2],
         p.val.precip = summary(mod)$coefficients["total_precip_mm", "Pr(>|t|)"],
         temp = summary(mod)$coefficients[3],
         p.val.temp = summary(mod)$coefficients["mean_annual_temp_k", "Pr(>|t|)"],
         pop = summary(mod)$coefficients[4],
         p.val.pop = summary(mod)$coefficients["pop_sum", "Pr(>|t|)"],
         r_sqr = summary(mod)$adj.r.squared)%>%
  select(-mod)

swell_temperate <- d %>% filter(sens.slope > 0) %>% filter(biome_type == "TEMPERATE") %>%
  select(hylak_id, hybas_id, centr_lat, centr_lon, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum) %>%
  group_by(hybas_id, hylak_id, centr_lat, centr_lon) %>%
  mutate(total_km2 = scale(total_km2),
         total_precip_mm = scale(total_precip_mm),
         mean_annual_temp_k = scale(mean_annual_temp_k),
         pop_sum = scale(pop_sum)) %>%
  do(mod = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
  mutate(res_std = summary(mod)$sigma,
         intercept = summary(mod)$coefficients[1],
         precip = summary(mod)$coefficients[2],
         p.val.precip = summary(mod)$coefficients["total_precip_mm", "Pr(>|t|)"],
         temp = summary(mod)$coefficients[3],
         p.val.temp = summary(mod)$coefficients["mean_annual_temp_k", "Pr(>|t|)"],
         pop = summary(mod)$coefficients[4],
         p.val.pop = summary(mod)$coefficients["pop_sum", "Pr(>|t|)"],
         r_sqr = summary(mod)$adj.r.squared)%>%
  select(-mod)

shrink_temperate <- d %>% filter(sens.slope < 0) %>% filter(biome_type == "TEMPERATE") %>%
  select(hylak_id, hybas_id, centr_lat, centr_lon, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum) %>%
  group_by(hybas_id, hylak_id, centr_lat, centr_lon) %>%
  mutate(total_km2 = scale(total_km2),
         total_precip_mm = scale(total_precip_mm),
         mean_annual_temp_k = scale(mean_annual_temp_k),
         pop_sum = scale(pop_sum)) %>%
  do(mod = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
  mutate(res_std = summary(mod)$sigma,
         intercept = summary(mod)$coefficients[1],
         precip = summary(mod)$coefficients[2],
         p.val.precip = summary(mod)$coefficients["total_precip_mm", "Pr(>|t|)"],
         temp = summary(mod)$coefficients[3],
         p.val.temp = summary(mod)$coefficients["mean_annual_temp_k", "Pr(>|t|)"],
         pop = summary(mod)$coefficients[4],
         p.val.pop = summary(mod)$coefficients["pop_sum", "Pr(>|t|)"],
         r_sqr = summary(mod)$adj.r.squared)%>%
  select(-mod)

swell_boreal <- d %>% filter(sens.slope > 0) %>% filter(biome_type == "BOREAL/TUNDRA/ICE") %>%
  select(hylak_id, hybas_id, centr_lat, centr_lon, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum) %>%
  group_by(hybas_id, hylak_id, centr_lat, centr_lon) %>%
  mutate(total_km2 = scale(total_km2),
         total_precip_mm = scale(total_precip_mm),
         mean_annual_temp_k = scale(mean_annual_temp_k),
         pop_sum = scale(pop_sum)) %>%
  do(mod = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
  mutate(res_std = summary(mod)$sigma,
         intercept = summary(mod)$coefficients[1],
         precip = summary(mod)$coefficients[2],
         p.val.precip = summary(mod)$coefficients["total_precip_mm", "Pr(>|t|)"],
         temp = summary(mod)$coefficients[3],
         p.val.temp = summary(mod)$coefficients["mean_annual_temp_k", "Pr(>|t|)"],
         pop = summary(mod)$coefficients[4],
         p.val.pop = summary(mod)$coefficients["pop_sum", "Pr(>|t|)"],
         r_sqr = summary(mod)$adj.r.squared)%>%
  select(-mod)

# shrink_boreal <- d %>% filter(sens.slope < 0) %>% filter(biome_type == "BOREAL/TUNDRA/ICE") %>%
#   select(hylak_id, hybas_id, centr_lat, centr_lon, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum) %>%
#   group_by(hybas_id, hylak_id, centr_lat, centr_lon) %>%
#   mutate(total_km2 = scale(total_km2),
#          total_precip_mm = scale(total_precip_mm),
#          mean_annual_temp_k = scale(mean_annual_temp_k),
#          pop_sum = scale(pop_sum)) %>%
#   do(mod = lm(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, data = ., na.action = na.exclude))%>%
#   mutate(res_std = summary(mod)$sigma,
#          intercept = summary(mod)$coefficients[1],
#          precip = summary(mod)$coefficients[2],
#          p.val.precip = summary(mod)$coefficients["total_precip_mm", "Pr(>|t|)"],
#          temp = summary(mod)$coefficients[3],
#          p.val.temp = summary(mod)$coefficients["mean_annual_temp_k", "Pr(>|t|)"],
#          pop = summary(mod)$coefficients[4],
#          p.val.pop = summary(mod)$coefficients["pop_sum", "Pr(>|t|)"],
#          r_sqr = summary(mod)$adj.r.squared)%>%
#   select(-mod)
