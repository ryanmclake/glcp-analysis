library(sf)
library(dplyr)
library(ggplot2)
library(maps)
library(patchwork)
library(hexbin)
library(rnaturalearth)
library(units)
library(zoo)
library(randomForest)
library(viridis)
library(rpart)
library(rpart.plot)
library(grid)
library(outliers)


slope_data_all <- vroom::vroom("./output/A1_glcp_filtered_add_sens_slope.csv", delim = " ", col_names = T)

slope_data_all <- slope_data_all %>% select(hylak_id, centr_lat, centr_lon, sens.slope) %>%
  group_by(hylak_id, centr_lat, centr_lon) %>%
  summarize(lake_sens_slope = mean(sens.slope))

slope_data_all <- slope_data_all %>%
  st_as_sf(coords = c("centr_lon", "centr_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext")

grid_spacing <- 400000 # CRS units in meters (100000 m = 111 km & 111 km ~ 1 Decimal degree)

grid <- st_make_grid(
  world,
  cellsize = c(grid_spacing, grid_spacing),
  #n = c(200, 200), # grid granularity
  crs = st_crs(world),
  what = "polygons",
  flat_topped = T,
  square = F) %>%
  st_intersection(world)

grid <- st_sf(index = 1:length(lengths(grid)), grid)

area_hexes <- st_join(slope_data_all, grid, join = st_intersects)

area_hexes_avg <- area_hexes %>%
  st_drop_geometry() %>%
  group_by(index) %>%
  mutate(bin_count = n()) %>%
  summarise(lake_sens_slope = median(lake_sens_slope, na.rm = TRUE),
            bin_count = median(bin_count)) %>%
  right_join(grid, by="index") %>%
  st_sf()

lake_area_change <-
  ggplot() +
  geom_sf(data = world, lwd = 0.5, color = "black")+
  geom_sf(data = area_hexes_avg,lwd = 0.05,
    aes(fill = lake_sens_slope))+
  geom_sf_text(data = area_hexes_avg, aes(label = bin_count), size = 1.5)+
  scale_fill_gradient2(midpoint=0, low="tan1", mid="white",
                       high="turquoise1", space ="Lab", na.value="black",
                       name = "**Δ Lake Surface Area** <br>Sen's Slope (km/yr)") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))


ggplot(area_hexes_avg) +
  geom_point(aes(y = bin_count, x=lake_sens_slope),pch = 21, size = 2, fill = "grey50")+
  ylab("Number of Lakes in Hexagon")+
  xlab("Lake Area Sen's Slope")+
  theme_classic()

ggsave(lake_area_change, path = ".",
       filename = "./output/figures/sens_slope_lake_area_global_plot_NEW.jpg",
       width = 14, height = 8, device='jpg', dpi=2000)




# 
# 
# lake_area_change <-
#   ggplot() +
#   geom_sf(data = world, lwd = 0.5, color = "black")+
#   geom_sf(data = slope_data,lwd = 0.05,
#           aes(fill = lsa_tau))+
#   scale_fill_gradient2(midpoint=0, low="tan1", mid="white",
#                        high="turquoise1", space ="Lab", na.value="black",
#                        name = "**Δ Lake Surface Area** <br>Kendall tau estimate") +
#   coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
#   guides(fill = guide_colourbar(title.position = "top"))+
#   theme_void()+
#   theme(legend.position = c(0.11, 0.35),
#         legend.direction = "vertical",
#         legend.title = ggtext::element_markdown(size = 10),
#         legend.text = element_text(size=9),
#         legend.key.height  = unit(.5, 'cm'),
#         legend.key.width =  unit(.3, 'cm'))
# 
# ggsave(lake_area_change, path = ".",
#        filename = "./output/figures/tau_lake_area_global_plot2.jpg",
#        width = 14, height = 8, device='jpg', dpi=1000)
# 
# 
# 
# 
# precip_change <-
#   ggplot() +
#   geom_sf(data = world, lwd = 0.75, color = "black")+
#   geom_sf(data = area_hexes_avg,lwd = 0.05,
#           aes(fill = precip_tau))+
#   scale_fill_gradient2(midpoint=0, low="orange3", mid="white",
#                        high="violet", space ="Lab", na.value="grey",
#                        name = "**Δ Rainfall** <br>Kendall tau estimate") +
#   coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
#   guides(fill = guide_colourbar(title.position = "top"))+
#   theme_void()+
#   theme(legend.position = c(0.11, 0.35),
#         legend.direction = "vertical",
#         legend.title = ggtext::element_markdown(size = 10),
#         legend.text = element_text(size=9),
#         legend.key.height  = unit(.5, 'cm'),
#         legend.key.width =  unit(.3, 'cm'))
# 
# ggsave(precip_change, path = ".",
#        filename = "./output/figures/tau_precip_change_plot.jpg",
#        width = 14, height = 8, device='jpg', dpi=1000)
# 
# 
# temp_change <-
#   ggplot() +
#   geom_sf(data = world, lwd = 0.75, color = "black")+
#   geom_sf(data = area_hexes_avg,lwd = 0.05,
#           aes(fill = temp_tau))+
#   scale_fill_gradient2(midpoint=0, low="dodgerblue4", mid="white",
#                        high="darkred", space ="Lab", na.value="grey",
#                        name = "**Δ Temperature** <br>Kendall tau estimate") +
#   coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
#   guides(fill = guide_colourbar(title.position = "top"))+
#   theme_void()+
#   theme(legend.position = c(0.11, 0.35),
#         legend.direction = "vertical",
#         legend.title = ggtext::element_markdown(size = 10),
#         legend.text = element_text(size=9),
#         legend.key.height  = unit(.5, 'cm'),
#         legend.key.width =  unit(.3, 'cm'))
# 
# ggsave(temp_change, path = ".",
#        filename = "./output/figures/slope_global_plot_1_degree_temp.jpg",
#        width = 14, height = 8, device='jpg', dpi=1000)
# 
# 
# snow_change <-
#   ggplot() +
#   geom_sf(data = world, lwd = 0.75, color = "black")+
#   geom_sf(data = area_hexes_avg,lwd = 0.05,
#           aes(fill = snow_tau))+
#   scale_fill_gradient2(midpoint=0, low="purple3", mid="white",
#                        high="cyan", space ="Lab", na.value="grey",
#                        name = "**Δ Snow** <br>Kendall tau estimate") +
#   coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
#   guides(fill = guide_colourbar(title.position = "top"))+
#   theme_void()+
#   theme(legend.position = c(0.11, 0.35),
#         legend.direction = "vertical",
#         legend.title = ggtext::element_markdown(size = 10),
#         legend.text = element_text(size=9),
#         legend.key.height  = unit(.5, 'cm'),
#         legend.key.width =  unit(.3, 'cm'))
# 
# ggsave(snow_change, path = ".",
#        filename = "./output/figures/slope_global_plot_1_degree_snow.jpg",
#        width = 14, height = 8, device='jpg', dpi=300)
# 
# 
# pop_change <-
#   ggplot() +
#   geom_sf(data = world, lwd = 0.75, color = "black")+
#   geom_sf(data = area_hexes_avg,lwd = 0.05,
#           aes(fill = pop_tau))+
#   scale_fill_gradient2(midpoint=0, low="green3", mid="white",
#                        high="yellow3", space ="Lab", na.value="grey",
#                        name = "**Δ Population** <br>Kendall tau estimate") +
#   coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
#   guides(fill = guide_colourbar(title.position = "top"))+
#   theme_void()+
#   theme(legend.position = c(0.11, 0.35),
#         legend.direction = "vertical",
#         legend.title = ggtext::element_markdown(size = 10),
#         legend.text = element_text(size=9),
#         legend.key.height  = unit(.5, 'cm'),
#         legend.key.width =  unit(.3, 'cm'))
# 
# library(patchwork)
# 
# big_fig <- (lake_area_change + temp_change)/(precip_change + pop_change)
# 
# ggsave(big_fig, path = ".",
#        filename = "./output/figures/slope_global_all_compare.jpg",
#        width = 26, height = 14, device='jpg', dpi=1500, limitsize = FALSE)
