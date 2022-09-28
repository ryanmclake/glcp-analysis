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


rf_data <- vroom::vroom("./output/slopes/hylak_id_random_forest.csv", delim = " ", col_names = T) %>%
  mutate(rf_fit = case_when(
    predictor == "pop_sum" ~ "POP.",
    predictor == "mean_sw_wm2" ~ "CLIM.",
    predictor == "mean_temp_k" ~ "CLIM.",
    predictor == "mean_lw_wm2" ~ "CLIM.",
    predictor == "mean_spec_humidity" ~ "CLIM.",
    predictor == "total_precip_mm" ~ "CLIM.",
    predictor == "ice_cover_mean"  ~ "CLIM.",
    predictor == "snow_km2" ~ "CLIM.",
    TRUE ~ NA_character_))%>%
  st_as_sf(coords = c("centr_lon", "centr_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")


world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext")

grid_spacing <- 111000 # CRS units in meters (100000 m = 111 km & 111 km ~ 1 Decimal degree)

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

area_hexes <- st_join(rf_data, grid, join = st_intersects)

# test <- area_hexes %>%
#   cbind(EcoregionMask_hex[st_nearest_feature(area_hexes, EcoregionMask_hex),]) %>%
#   mutate(dist = st_distance(geometry, geometry.1, by_element = T))
#
# test$dist <- drop_units(test$dist)
# test <- test %>% filter(dist == 0) %>% arrange(hylak_id)

area_hexes_avg <- area_hexes %>%
  st_drop_geometry() %>%
  group_by(index) %>%
  arrange(rf_fit) %>%
  summarise(NSE = mean(NSE, na.rm = TRUE),
            rf_fit = paste(unique(rf_fit), collapse=', ')) %>%
  right_join(grid, by="index") %>%
  st_sf()

lake_hex_predictors <-
  ggplot() +
  geom_sf(data = world, lwd = 0.5, color = "black")+
  geom_sf(data = area_hexes_avg,lwd = 0.05,
          aes(fill = rf_fit))+
  scale_fill_viridis(option = "C", na.value = "white",
                     direction = -1, discrete = T, name = "**Predictors** <br> Random Forest")+
  #scale_color_manual(values = c("blue", NA, "black"), na.value = "black",name = "**Predictive Skill** <br> NSE") +
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  #guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

ggsave(lake_hex_predictors, path = ".",
       filename = "./output/figures/slope_global_rf_predictor.jpg",
       width = 14, height = 8, device='jpg', dpi=300)

