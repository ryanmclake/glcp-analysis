library(sf)
library(dplyr)
library(ggplot2)
library(maps)
library(patchwork)
library(hexbin)
library(rnaturalearth)
library(units)

#### I AM STILL IN PROGRESS #####

shp_boreal <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
  mutate(biome_type = case_when(
    BIOME == 1 ~ "TROPICAL MOIST FOREST",
    BIOME == 2 ~ "TROPICAL DRY FOREST",
    BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
    BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
    BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
    BIOME == 6 ~ "BOREAL FOREST",
    BIOME == 7 ~ "TROPICAL GRASSLAND",
    BIOME == 8 ~ "TEMPERATE GRASSLAND",
    BIOME == 9 ~ "FLOODED GRASSLAND",
    BIOME == 10 ~ "MONTANE GRASSLAND",
    BIOME == 11 ~ "TUNDRA",
    BIOME == 12 ~ "MEDITERRANIAN FOREST",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "MANGROVES",
    BIOME == 98 ~ "LAKE",
    BIOME == 99 ~ "ROCK & ICE",
    TRUE ~ NA_character_))%>%
  st_transform("+proj=eqearth +wktext") %>%
  filter(biome_type %in% c("BOREAL FOREST","ROCK & ICE","TUNDRA"))

shp_desert <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
  mutate(biome_type = case_when(
    BIOME == 1 ~ "TROPICAL MOIST FOREST",
    BIOME == 2 ~ "TROPICAL DRY FOREST",
    BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
    BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
    BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
    BIOME == 6 ~ "BOREAL FOREST",
    BIOME == 7 ~ "TROPICAL GRASSLAND",
    BIOME == 8 ~ "TEMPERATE GRASSLAND",
    BIOME == 9 ~ "FLOODED GRASSLAND",
    BIOME == 10 ~ "MONTANE GRASSLAND",
    BIOME == 11 ~ "TUNDRA",
    BIOME == 12 ~ "MEDITERRANIAN FOREST",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "MANGROVES",
    BIOME == 98 ~ "LAKE",
    BIOME == 99 ~ "ROCK & ICE",
    TRUE ~ NA_character_))%>%
  st_transform("+proj=eqearth +wktext") %>%
  filter(biome_type == "DESERT")


shp_temperate <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
  mutate(biome_type = case_when(
    BIOME == 1 ~ "TROPICAL MOIST FOREST",
    BIOME == 2 ~ "TROPICAL DRY FOREST",
    BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
    BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
    BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
    BIOME == 6 ~ "BOREAL FOREST",
    BIOME == 7 ~ "TROPICAL GRASSLAND",
    BIOME == 8 ~ "TEMPERATE GRASSLAND",
    BIOME == 9 ~ "FLOODED GRASSLAND",
    BIOME == 10 ~ "MONTANE GRASSLAND",
    BIOME == 11 ~ "TUNDRA",
    BIOME == 12 ~ "MEDITERRANIAN FOREST",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "MANGROVES",
    BIOME == 98 ~ "LAKE",
    BIOME == 99 ~ "ROCK & ICE",
    TRUE ~ NA_character_))%>%
  st_transform("+proj=eqearth +wktext") %>%
  filter(biome_type %in% c("TEMPERATE GRASSLAND","TEMPERATE CONIFEROUS FOREST","MEDITERRANIAN FOREST","FLOODED GRASSLAND",
                           "MONTANE GRASSLAND","LAKE","TEMPERATE BROADLEAF FOREST"))

shp_tropical <- read_sf(paste0("./data/shapes/WWF_ecoregions/official/wwf_terr_ecos.shp")) %>%
  mutate(biome_type = case_when(
    BIOME == 1 ~ "TROPICAL MOIST FOREST",
    BIOME == 2 ~ "TROPICAL DRY FOREST",
    BIOME == 3 ~ "TROPICAL CONIFEROUS FOREST",
    BIOME == 4 ~ "TEMPERATE BROADLEAF FOREST",
    BIOME == 5 ~ "TEMPERATE CONIFEROUS FOREST",
    BIOME == 6 ~ "BOREAL FOREST",
    BIOME == 7 ~ "TROPICAL GRASSLAND",
    BIOME == 8 ~ "TEMPERATE GRASSLAND",
    BIOME == 9 ~ "FLOODED GRASSLAND",
    BIOME == 10 ~ "MONTANE GRASSLAND",
    BIOME == 11 ~ "TUNDRA",
    BIOME == 12 ~ "MEDITERRANIAN FOREST",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "MANGROVES",
    BIOME == 98 ~ "LAKE",
    BIOME == 99 ~ "ROCK & ICE",
    TRUE ~ NA_character_))%>%
  st_transform("+proj=eqearth +wktext") %>%
  filter(biome_type %in% c("TROPICAL MOIST FOREST","TROPICAL DRY FOREST","TROPICAL GRASSLAND","MANGROVES",
                           "TROPICAL CONIFEROUS FOREST"))