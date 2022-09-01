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


# shp_boreal <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
#   mutate(biome_type = case_when(
#     BIOME == 1 ~ "TROPICAL MOIST FOREST",
#     BIOME == 2 ~ "TROPICAL DRY FOREST",
#     BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
#     BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
#     BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
#     BIOME == 6 ~ "BOREAL FOREST",
#     BIOME == 7 ~ "TROPICAL GRASSLAND",
#     BIOME == 8 ~ "TEMPERATE GRASSLAND",
#     BIOME == 9 ~ "FLOODED GRASSLAND",
#     BIOME == 10 ~ "MONTANE GRASSLAND",
#     BIOME == 11 ~ "TUNDRA",
#     BIOME == 12 ~ "MEDITERRANIAN FOREST",
#     BIOME == 13 ~ "DESERT",
#     BIOME == 14 ~ "MANGROVES",
#     BIOME == 98 ~ "LAKE",
#     BIOME == 99 ~ "ROCK & ICE",
#     TRUE ~ NA_character_))%>%
#   st_transform("+proj=eqearth +wktext") %>%
#   filter(biome_type %in% c("BOREAL FOREST","ROCK & ICE","TUNDRA"))
#
# shp_desert <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
#   mutate(biome_type = case_when(
#     BIOME == 1 ~ "TROPICAL MOIST FOREST",
#     BIOME == 2 ~ "TROPICAL DRY FOREST",
#     BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
#     BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
#     BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
#     BIOME == 6 ~ "BOREAL FOREST",
#     BIOME == 7 ~ "TROPICAL GRASSLAND",
#     BIOME == 8 ~ "TEMPERATE GRASSLAND",
#     BIOME == 9 ~ "FLOODED GRASSLAND",
#     BIOME == 10 ~ "MONTANE GRASSLAND",
#     BIOME == 11 ~ "TUNDRA",
#     BIOME == 12 ~ "MEDITERRANIAN FOREST",
#     BIOME == 13 ~ "DESERT",
#     BIOME == 14 ~ "MANGROVES",
#     BIOME == 98 ~ "LAKE",
#     BIOME == 99 ~ "ROCK & ICE",
#     TRUE ~ NA_character_))%>%
#   st_transform("+proj=eqearth +wktext") %>%
#   filter(biome_type == "DESERT")
#
#
# shp_temperate <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
#   mutate(biome_type = case_when(
#     BIOME == 1 ~ "TROPICAL MOIST FOREST",
#     BIOME == 2 ~ "TROPICAL DRY FOREST",
#     BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
#     BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
#     BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
#     BIOME == 6 ~ "BOREAL FOREST",
#     BIOME == 7 ~ "TROPICAL GRASSLAND",
#     BIOME == 8 ~ "TEMPERATE GRASSLAND",
#     BIOME == 9 ~ "FLOODED GRASSLAND",
#     BIOME == 10 ~ "MONTANE GRASSLAND",
#     BIOME == 11 ~ "TUNDRA",
#     BIOME == 12 ~ "MEDITERRANIAN FOREST",
#     BIOME == 13 ~ "DESERT",
#     BIOME == 14 ~ "MANGROVES",
#     BIOME == 98 ~ "LAKE",
#     BIOME == 99 ~ "ROCK & ICE",
#     TRUE ~ NA_character_))%>%
#   st_transform("+proj=eqearth +wktext") %>%
#   filter(biome_type %in% c("TEMPERATE GRASSLAND","TEMPERATE CONIFEROUS FOREST","MEDITERRANIAN FOREST","FLOODED GRASSLAND",
#                            "MONTANE GRASSLAND","LAKE","TEMPERATE BROADLEAF FOREST"))
#
# shp_tropical <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
#   mutate(biome_type = case_when(
#     BIOME == 1 ~ "TROPICAL MOIST FOREST",
#     BIOME == 2 ~ "TROPICAL DRY FOREST",
#     BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
#     BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
#     BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
#     BIOME == 6 ~ "BOREAL FOREST",
#     BIOME == 7 ~ "TROPICAL GRASSLAND",
#     BIOME == 8 ~ "TEMPERATE GRASSLAND",
#     BIOME == 9 ~ "FLOODED GRASSLAND",
#     BIOME == 10 ~ "MONTANE GRASSLAND",
#     BIOME == 11 ~ "TUNDRA",
#     BIOME == 12 ~ "MEDITERRANIAN FOREST",
#     BIOME == 13 ~ "DESERT",
#     BIOME == 14 ~ "MANGROVES",
#     BIOME == 98 ~ "LAKE",
#     BIOME == 99 ~ "ROCK & ICE",
#     TRUE ~ NA_character_))%>%
#   st_transform("+proj=eqearth +wktext") %>%
#   filter(biome_type %in% c("TROPICAL MOIST FOREST","TROPICAL DRY FOREST","TROPICAL GRASSLAND","MANGROVES",
#                            "TROPICAL CONIFEROUS FOREST"))

slope_data_ones <- vroom::vroom("./output/slopes/hylak_id_kendall.csv", delim = " ", col_names = T) %>%
  rename(lsa_pval = p.value,
         lsa_tau = statistic) %>%
  mutate_all(funs(as.numeric(.))) %>%
  filter(!(hylak_id %% 1)) %>%
  na.omit(.) %>%
  filter(lsa_tau == 1.000000000000) %>%
  st_as_sf(coords = c("centr_lon", "centr_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")


slope_data <- vroom::vroom("./output/slopes/hylak_id_kendall2.csv", delim = " ", col_names = T) %>%
  rename(lsa_pval = p.value,
         lsa_tau = statistic) %>%
  mutate_all(funs(as.numeric(.))) %>%
  filter(!(hylak_id %% 1)) %>%
  na.omit(.) %>%
  filter(lsa_tau < 1.000000000000) %>%
  filter(lsa_tau != 0.000000000000) %>%
  st_as_sf(coords = c("centr_lon", "centr_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

dam_data <- vroom::vroom("./data/GeoDAR_v11_dams_beta_pr1.csv", delim = ",", col_names = T) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

dam_glcp_link <- dam_data %>%
  cbind(slope_data[st_nearest_feature(dam_data, slope_data),]) %>%
  mutate(dist = st_distance(geometry, geometry.1, by_element = T))
#
reservoirs <- c(unique(dam_glcp_link$hylak_id))

`%!in%` <- Negate(`%in%`)

slope_data_2 <- slope_data %>%
  filter(hylak_id %!in% reservoirs)

world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext")

grid_spacing <- 222000 # CRS units in meters (100000 m = 111 km & 111 km ~ 1 Decimal degree)

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

area_hexes <- st_join(slope_data_2, grid, join = st_intersects)
area_hexes_avg <- area_hexes %>%
  st_drop_geometry() %>%
  group_by(index) %>%
  summarise(lsa_tau = median(lsa_tau, na.rm = TRUE)) %>%
  right_join(grid, by="index") %>%
  st_sf()

lake_area_change <-
  ggplot() +
  geom_sf(data = world, lwd = 0.5, color = "black")+
  geom_sf(data = area_hexes_avg,lwd = 0.05,
    aes(fill = lsa_tau))+
  scale_fill_gradient2(midpoint=0, low="tan1", mid="white",
                       high="turquoise1", space ="Lab", na.value="black",
                       name = "**Δ Lake Surface Area** <br>Kendall tau estimate") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

ggsave(lake_area_change, path = ".",
       filename = "./output/figures/tau_lake_area_global_plot.jpg",
       width = 14, height = 8, device='jpg', dpi=1000)

lake_area_change <-
  ggplot() +
  geom_sf(data = world, lwd = 0.5, color = "black")+
  geom_sf(data = slope_data,lwd = 0.05,
          aes(fill = lsa_tau))+
  scale_fill_gradient2(midpoint=0, low="tan1", mid="white",
                       high="turquoise1", space ="Lab", na.value="black",
                       name = "**Δ Lake Surface Area** <br>Kendall tau estimate") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

ggsave(lake_area_change, path = ".",
       filename = "./output/figures/tau_lake_area_global_plot2.jpg",
       width = 14, height = 8, device='jpg', dpi=1000)




precip_change <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes_avg,lwd = 0.05,
          aes(fill = precip_tau))+
  scale_fill_gradient2(midpoint=0, low="orange3", mid="white",
                       high="violet", space ="Lab", na.value="grey",
                       name = "**Δ Rainfall** <br>Kendall tau estimate") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

ggsave(precip_change, path = ".",
       filename = "./output/figures/tau_precip_change_plot.jpg",
       width = 14, height = 8, device='jpg', dpi=1000)


temp_change <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes_avg,lwd = 0.05,
          aes(fill = temp_tau))+
  scale_fill_gradient2(midpoint=0, low="dodgerblue4", mid="white",
                       high="darkred", space ="Lab", na.value="grey",
                       name = "**Δ Temperature** <br>Kendall tau estimate") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

ggsave(temp_change, path = ".",
       filename = "./output/figures/slope_global_plot_1_degree_temp.jpg",
       width = 14, height = 8, device='jpg', dpi=1000)


snow_change <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes_avg,lwd = 0.05,
          aes(fill = snow_tau))+
  scale_fill_gradient2(midpoint=0, low="purple3", mid="white",
                       high="cyan", space ="Lab", na.value="grey",
                       name = "**Δ Snow** <br>Kendall tau estimate") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

ggsave(snow_change, path = ".",
       filename = "./output/figures/slope_global_plot_1_degree_snow.jpg",
       width = 14, height = 8, device='jpg', dpi=300)


pop_change <-
  ggplot() +
  geom_sf(data = world, lwd = 0.75, color = "black")+
  geom_sf(data = area_hexes_avg,lwd = 0.05,
          aes(fill = pop_tau))+
  scale_fill_gradient2(midpoint=0, low="green3", mid="white",
                       high="yellow3", space ="Lab", na.value="grey",
                       name = "**Δ Population** <br>Kendall tau estimate") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

library(patchwork)

big_fig <- (lake_area_change + temp_change)/(precip_change + pop_change)

ggsave(big_fig, path = ".",
       filename = "./output/figures/slope_global_all_compare.jpg",
       width = 26, height = 14, device='jpg', dpi=1500, limitsize = FALSE)
