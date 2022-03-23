library(sf)
library(dplyr)
library(ggplot2)
library(maps)
library(patchwork)
library(hexbin)

world <- map_data("world")

plot1 <- readRDS("./output/hylak_id_slopes.rds") %>%
  rename(`Lake Area Change` = fit_total_slope) %>%
  #st_as_sf(coords = c("pour_long", "pour_lat"), crs = 4326) %>%
  ggplot(data = ., aes(color = `Lake Area Change`)) +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),
           color = "black", fill = NA, size = 0.1)+
  labs(x = "Longitude", y = "Latitude")+
  geom_hex(aes(x = pour_long, y = pour_lat), bins = 30, alpha = 0.8)+
  #geom_sf(pch = 15, size = 0.001, inherit.aes = T)+
  scale_color_gradient2(low="orange4", high="green4", space ="Lab") +
  theme_classic()+
  theme(legend.position = "top",
        legend.key.width= unit(3, 'cm'))

plot2 <- readRDS("./output/hylak_id_slopes.rds") %>%
  rename(`Regression R-square` = fit_total_rsq) %>%
  st_as_sf(coords = c("pour_long", "pour_lat"), crs = 4326) %>%
  ggplot(data = ., aes(color = `Regression R-square`)) +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),
           color = "black", fill = NA, size = 0.1)+
  labs(x = "Longitude", y = "Latitude")+
  geom_sf(pch = 15, size = 0.001, inherit.aes = T)+
  scale_color_gradient(low="blue",high="red")+
  theme_classic()+
  theme(legend.position = "top",
        legend.key.width= unit(3, 'cm'))

plot3 = plot1 + plot2

ggsave(plot3, path = ".", filename = "./output/figures/slope_global_plot.jpg", width = 20, height = 8, device='jpg', dpi=2000)
