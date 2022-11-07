# REGRESSION TREE ANALYSIS ACROSS BIOMES

library(tidyverse)
library(trend)
library(ggparty)
library(sf)
library(ggplot2)

source("./code/functions.R")

biomes <- c("TEMPERATE","TROPICAL","DESERT","BOREAL/ICE/TUNDRA")

for(i in 1:length(biomes)){
d <- vroom::vroom("./output/A3_glcp_filtered_biomes_joined.csv") %>%
  group_by(hylak_id) %>%
  dplyr::mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  dplyr::mutate_at(vars(total_km2),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  dplyr::mutate(total_km2 = scale(total_km2) %>% as.vector()) %>%
  dplyr::mutate_at(vars(total_km2),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  dplyr::mutate(total_precip_mm = log(total_precip_mm+1) %>% as.vector()) %>%
  dplyr::mutate(mean_annual_temp_k = log(mean_annual_temp_k+1) %>% as.vector()) %>%
  dplyr::mutate(pop_sum = log(pop_sum+1) %>% as.vector()) %>%
  dplyr::mutate(ice_cover_median = log(ice_cover_median+1) %>% as.vector()) %>%
  dplyr::mutate(mean_spec_humidity = log(mean_spec_humidity+1) %>% as.vector()) %>%
  dplyr::mutate(mean_sw_wm2 = log(mean_sw_wm2+1) %>% as.vector()) %>%
  ungroup(.) %>%
  dplyr::mutate_at(vars(ice_cover_median),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  select(hylak_id, total_km2, total_precip_mm, mean_annual_temp_k, mean_spec_humidity,mean_sw_wm2,
         pop_sum, elevation, shore_dev, slope_100, ice_cover_median, biome_type, centr_lat, centr_lon) %>%
  dplyr::group_by(hylak_id, elevation, shore_dev, slope_100, biome_type, centr_lat, centr_lon) %>%
  summarise(across(c(1:7),  ~list(sens.slope(ts(.))))) %>%
  mutate(sens.slope.area = unlist(purrr::map(total_km2, "estimates")),
         sens.slope.precip = unlist(purrr::map(total_precip_mm, "estimates")),
         p.value.precip = unlist(purrr::map(total_precip_mm, "p.value")),
         sens.slope.sw = unlist(purrr::map(mean_sw_wm2, "estimates")),
         p.value.sw = unlist(purrr::map(mean_sw_wm2, "p.value")),
         sens.slope.temp = unlist(purrr::map(mean_annual_temp_k, "estimates")),
         p.value.temp = unlist(purrr::map(mean_annual_temp_k, "p.value")),
         sens.slope.pop = unlist(purrr::map(pop_sum, "estimates")),
         p.value.pop = unlist(purrr::map(pop_sum, "p.value")),
         sens.slope.hum = unlist(purrr::map(mean_spec_humidity, "estimates")),
         p.value.hum = unlist(purrr::map(mean_spec_humidity, "p.value")),
         sens.slope.ice = unlist(purrr::map(ice_cover_median, "estimates")),
         p.value.ice = unlist(purrr::map(ice_cover_median, "p.value"))) %>%
  select(-total_km2, -total_precip_mm, -mean_annual_temp_k, -pop_sum,
         -mean_spec_humidity, -ice_cover_median) %>%
  mutate(slope_100 = ifelse(slope_100 <= 0, 0, slope_100)) 
#%>%
 # filter(biome_type == biomes[i]) 

model_input = d %>%
  ungroup(.) %>%
  select(hylak_id,
         biome_type,
    sens.slope.area,
    sens.slope.precip,
    shore_dev,
    slope_100,
    sens.slope.ice,
    sens.slope.temp,
    sens.slope.pop,
    elevation,
    sens.slope.hum,
    sens.slope.sw)

model_input %>% 
  select(-hylak_id, -biome_type) %>%
  cor() %>% 
  corrplot::corrplot(.)

fitted_raw = rpart(sens.slope.area ~ sens.slope.sw + sens.slope.precip + slope_100 + sens.slope.ice + sens.slope.temp + sens.slope.pop + elevation + sens.slope.hum,
                   data = model_input, control = rpart.control(cp = 0.01))

p_fitted_raw<- prune(fitted_raw, cp=fitted_raw$cptable[which.min(fitted_raw$cptable[,"xerror"]),"CP"])

model_input %>% 
  mutate(.pred = predict(p_fitted_raw, newdata = model_input),
         dif = .pred - sens.slope.area) %>% 
  summarise(
    rmse = sqrt(mean(dif^2)),
    mae = mean(abs(dif)),
    r2 = 1 - (sum(dif^2)) / (sum((sens.slope.area - mean(sens.slope.area))^2))
  ) %>% 
  ungroup()

fitted = as.party.rpart(obj = p_fitted_raw) ## convert to party object
plot(fitted)

lake_latlon = d %>% ungroup(.) %>%
  select(centr_lat, centr_lon, biome_type, hylak_id) %>%
  as_tibble()

terminal_node_ids = nodeids(fitted, terminal = T)

model_input = model_input %>% left_join(lake_latlon, by = c("hylak_id","biome_type"))
for (i in terminal_node_ids) {
  temp = fitted[i]$data %>% as_tibble %>% 
    left_join(model_input %>% 
                select(centr_lon, centr_lat, elevation,sens.slope.area, slope_100, sens.slope.pop, sens.slope.hum,sens.slope.temp)) %>% 
    mutate(id = i)
  if(i == terminal_node_ids[1]) {
    pred = temp
  } else if (i != terminal_node_ids[1]) {
    pred = bind_rows(pred, temp)
  }
}

bw = 1.5

pred %>% count(id)

node_blue_ratio = pred %>% group_by(id) %>% 
  summarise(blue_ratio = sum(sens.slope.area <= 0) / n()) %>% 
  ungroup() %>% 
  arrange(desc(blue_ratio))

node_blue_ratio

terminal_node_ids
nodeName = tibble(node_id = terminal_node_ids,
                  node_name = c("dry, less rain, more SW",
                                "dry, less rain, less SW, high elev.",
                                "dry, less rain, less SW, low elev.",
                                "dry, more rain, low temp",
                                "dry, more rain, inc. temp",
                                "wet, high elev., shallow WS",
                                "wet, high elev., steep WS",
                                "wet, low elev., more SW",
                                "wet, low elev., less SW",
                                "wet")) %>%
  mutate(order = 1:length(terminal_node_ids))

pred_sf = pred %>% st_as_sf(coords = c("centr_lon", "centr_lat"), crs = 4326)

world_sf = st_as_sf(rnaturalearthdata::coastline110)

strip_text = pred %>% 
  left_join(nodeName, by = c("id" = "node_id")) %>% 
  group_by(id) %>% 
  summarise(
    nlakes = n(),
    nperlakes = nlakes / nrow(pred) * 100,
    id_text = paste0(node_name, ": (", format(nperlakes, digits = 3), "%)"),
    order = first(order)) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(id_text = fct_reorder(id_text, order))

node_maps = pred_sf %>% 
  left_join(strip_text, by = "id") %>% 
  ggplot() +
  geom_sf(data = world_sf, color = "darkgrey", lwd = 0.2) +
  geom_sf(aes(color = as.character(id_text)), cex = 0.5, alpha = 1) +
  coord_sf(ylim = c(-60, 85)) +
  theme_bw() +
  scale_color_viridis_d(option = "D")+
  labs(color = "Lake Sen Slope") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(color = "grey80"))

den_all = pred %>% pull(sens.slope.area) %>% density(bw = bw)

color_fill_all = den_all[1:2] %>% as_tibble() %>% rename(sens.slope.area = x, d = y) %>% 
  mutate(d = d * nrow(pred))
maxd = max(color_fill_all$d) / 2
color_fill = tibble(id = terminal_node_ids) %>% group_by(id) %>% 
  do({
    dat = .
    node_id = dat$id[1]
    this_pred = pred %>% filter(id == node_id)
    den = this_pred %>% pull(sens.slope.area) %>% density(bw = bw)
    den[1:2] %>% as_tibble() %>% rename(sens.slope.area = x, d = y) %>% 
      mutate(d = d * nrow(this_pred))
  }) %>% 
  ungroup()



fitted %>% 
  ggparty(terminal_space = 0.4) +
  geom_edge(aes(lwd = nodesize)) +
  geom_edge_label(mapping = aes(label = paste(substr(breaks_label, start = 1, stop = 15)))) +
  geom_node_splitvar(color = "grey10", fill = "white", label.padding = unit(0.2, "lines"), angle = 90, fontface = "bold") +
  geom_node_info(
    aes(label = paste0("Node ", id)),
    fontface = "bold",
    ids = "terminal",
    size = 3,
    nudge_y = 0.01
  ) +
  geom_node_plot(gglist = list(geom_violin(aes(x = "", y = sens.slope.area, color = sens.slope.area), trim = F,
                                           draw_quantiles = c(0.25, 0.5, 0.75),
                                           fill = "grey80", colour = "#3366FF"),
                               scale_y_continuous(limits = c(-0.3, 0.3)),
                               scale_color_viridis_c(option = "D"),
                               theme_minimal()),
  shared_axis_labels = T) +
  theme(legend.position = "none",
        text = element_text(size = 10))







+geom_node_plot(gglist = list(
    geom_segment(data = color_fill, aes(x = -d, xend = d, y = sens.slope.area, yend = sens.slope.area, color = sens.slope.area), lwd = 1, show.legend = FALSE),
    scale_x_continuous(limits = c(-maxd, maxd)),
    scale_y_continuous(limits = c(-0.25, 0.25)),
    scale_color_viridis_c(option = "D"),
      theme_void(), 
    labs(
      x = NULL,
      y = NULL)
  ),
  shared_axis_labels = T) +
  theme(legend.position = "none",
        text = element_text(size = 10))

ggparty_tree
ggparty_tree %>% ggsave(
  filename = "figs/figure2_ggparty_tree_with_histogram.png",
  width = 6.5,
  height = 4.5)

}




ggparty_tree = fitted %>% 
  ggparty(terminal_space = 0.4) +
  geom_edge(aes(lwd = nodesize)) +
  geom_edge_label(mapping = aes(label = paste(substr(breaks_label, start = 1, stop = 15)))) +
  geom_node_splitvar(color = "grey10", fill = "white", label.padding = unit(0.2, "lines"), angle = 90, fontface = "bold") +
  geom_node_info(
    aes(label = paste0("Node ", id)),
    fontface = "bold",
    ids = "terminal",
    size = 3,
    nudge_y = 0.01
  ) +
  geom_node_plot(gglist = list(
    geom_segment(data = color_fill, aes(x = -d, xend = d, y = sens.slope.area, yend = sens.slope.area, color = sens.slope.area), lwd = 1, show.legend = FALSE),
    scale_x_continuous(limits = c(-maxd, maxd)),
    scale_y_continuous(limits = c(-0.3, 0.3)),
    
    theme_void(), 
    labs(
      x = NULL,
      y = NULL)
  ),
  shared_axis_labels = T) +
  theme(legend.position = "none",
        text = element_text(size = 10))

)