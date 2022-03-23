#### ECOREGIONS (for ecoregional borders)

library(multidplyr)
library(dplyr)
library(sf)
library(ggplot2)
library(units)
library(reshape2)

cluster <- multidplyr::new_cluster(6)

data <- readRDS("./data/glcp_condensed.rds")

d2 <- data %>%
  dplyr::group_by(hybas_id) %>%
  dplyr::filter(lake_type == 1) %>%
  mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear")))%>%
  dplyr::mutate(Z_total_km2 = scale(total_km2, center = TRUE, scale = TRUE),
                Z_precip = scale(sum_precip_mm, center = TRUE, scale = TRUE),
                Z_temp = scale(mean_temp_k, center = TRUE, scale = TRUE),
                Z_pop = scale(pop_sum, center = TRUE, scale = TRUE)) %>%
  dplyr::select(hybas_id, pour_long, pour_lat, year, Z_total_km2, Z_precip, Z_temp, Z_pop) %>%
  na.omit(.)%>%
  dplyr::group_by(hybas_id, pour_long, pour_lat) %>%
  mutate(change_total_km2 = Z_total_km2-Z_total_km2[1]) %>%
  group_by(hybas_id, year) %>%
  summarize_all(funs(mean)) %>%
  st_as_sf(coords = c("pour_long", "pour_lat"), crs = 4326)



Dir.Base <- getwd() 						# identifying the current directory
Dir.Data <- file.path(Dir.Base, "data") 			# folder path for data
Dir.Shapes <- file.path(Dir.Data, "shapes") 		# folder path for shapefiles


if (!file.exists(file.path(Dir.Shapes, "WWF_ecoregions"))) {
  download.file("http://assets.worldwildlife.org/publications/15/files/original/official_teow.zip",
                destfile = file.path(Dir.Shapes, "wwf_ecoregions.zip")
  ) 																						# download regions
  unzip(file.path(Dir.Shapes, "wwf_ecoregions.zip"), exdir = file.path(Dir.Shapes, "WWF_ecoregions"))
}

EcoregionMask <- read_sf(file.path(Dir.Shapes, "WWF_ecoregions", "official", "wwf_terr_ecos.shp")) 			# read
EcoregionMask <- st_make_valid(EcoregionMask)


YEARS <- c(1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)
biome_to_lakeyear <- list()

for(i in 1:length(YEARS)){
  b <- d2 %>% filter(year == YEARS[i])
  b2 <- EcoregionMask %>%
    cbind(b[st_nearest_feature(EcoregionMask, b),]) %>%
    mutate(dist = st_distance(geometry, geometry.1, by_element = T))
  b2$dist <- drop_units(b2$dist)
  b2 <- b2 %>% filter(dist == 0) %>% arrange(hybas_id)
  biome_to_lakeyear[[i]] <- b2
}

biome_to_lakeyear = do.call(rbind, biome_to_lakeyear)

biome_to_lakeyear <- biome_to_lakeyear %>%
  mutate(BIOME_TYPE = case_when(
    BIOME == 1 ~ "TROPICAL MOIST FOREST",
    BIOME == 2 ~ "TROPICAL DRY FOREST",
    BIOME == 4 ~ "TEMPERATE CONIFEROUS FOREST",
    BIOME == 5 ~ "TEMPERATE CONIFER FOREST",
    BIOME == 6 ~ "BOREAL FOREST",
    BIOME == 7 ~ "TROPICAL GRASSLAND",
    BIOME == 8 ~ "TEMPERATE GRASSLAND",
    BIOME == 9 ~ "FLOODED GRASSLAND",
    BIOME == 10 ~ "MONTANE GRASSLAND",
    BIOME == 11 ~ "TUNDRA",
    BIOME == 12 ~ "MEDITERRANIAN FOREST",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "MANGROVES",
    TRUE ~ NA_character_)) %>%
  st_drop_geometry(.) %>%
  select(BIOME_TYPE, year, hybas_id, change_total_km2)%>%
  melt(., id.vars = c("BIOME_TYPE","hybas_id", "year"))

ggp <- ggplot(biome_to_lakeyear, aes(as.character(year), as.character(hybas_id), group = as.character(BIOME_TYPE))) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = value))+
  scale_fill_gradient(low="blue", high="red")+
  theme_classic()
ggp



BIOMES <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 98, 99)
lake_to_biome <- list()

for(i in 1:length(BIOMES)){
  b <- EcoregionMask %>% filter(BIOME == BIOMES[i])
  b2 <- slopes %>%
    cbind(b[st_nearest_feature(slopes, b),]) %>%
    mutate(dist = st_distance(geometry, geometry.1, by_element = T))
  b2$dist <- drop_units(b2$dist)
  b2 <- b2 %>% filter(dist == 0) %>% arrange(hybas_id)
  lake_to_biome[[i]] <- b2
}

lake_to_biome = do.call(rbind, lake_to_biome)

biome_change <- lake_to_biome %>%
  mutate(BIOME_TYPE = case_when(
    BIOME == 1 ~ "TROPICAL MOIST FOREST",
    BIOME == 2 ~ "TROPICAL DRY FOREST",
    BIOME == 4 ~ "TEMPERATE CONIFEROUS FOREST",
    BIOME == 5 ~ "TEMPERATE CONIFER FOREST",
    BIOME == 6 ~ "BOREAL FOREST",
    BIOME == 7 ~ "TROPICAL GRASSLAND",
    BIOME == 8 ~ "TEMPERATE GRASSLAND",
    BIOME == 9 ~ "FLOODED GRASSLAND",
    BIOME == 10 ~ "MONTANE GRASSLAND",
    BIOME == 11 ~ "TUNDRA",
    BIOME == 12 ~ "MEDITERRANIAN FOREST",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "MANGROVES",
    TRUE ~ NA_character_))

biome_change_plot <- ggplot(biome_change, aes(BIOME_TYPE, total_slope, group = BIOME_TYPE))+
  geom_boxplot(fill = "grey70", color = "black", lwd = 0.6, alpha = 0.6, show.legend = FALSE) +
  labs(x = "WWF Biomes", y = "Change in total lake area (km2)") +
  stat_summary(
    fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., color = "Mean"),
    width = .75, linetype = "dashed"
  ) +
  stat_summary(
    fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., color = "Median"),
    width = .75, linetype = "solid"
  ) +
  scale_colour_manual("", values = c(Median = "dodgerblue4", Mean = "darkred")) +
  theme_minimal() +
  theme(text = element_text(size = 14),legend.position = "top",
        panel.grid.major.y = element_line(c(0, 25, 50, 75, 100),color = "black",size = 0.2),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"))

biome_change_plot

ggsave(path = Dir.Base, filename = "./output/figures/WWF_biome_type_trends.jpg", width = 10, height = 8, device='jpg', dpi=700)
