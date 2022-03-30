library(units)
library(sf)
library(dplyr)

Dir.Base <- getwd() 						# identifying the current directory
Dir.Data <- file.path(Dir.Base, "data") 			# folder path for data
Dir.Shapes <- file.path(Dir.Data, "shapes") 		# folder path for shapefiles

if (!file.exists(file.path(Dir.Shapes, "WWF_ecoregions"))) {
  download.file("http://assets.worldwildlife.org/publications/15/files/original/official_teow.zip",
                destfile = file.path(Dir.Shapes, "wwf_ecoregions.zip")
  ) 																						# download regions
  unzip(file.path(Dir.Shapes, "wwf_ecoregions.zip"), exdir = file.path(Dir.Shapes, "WWF_ecoregions"))
}

data <- readRDS("./output/hylak_id_slopes.rds") %>%
  st_as_sf(coords = c("pour_long", "pour_lat"), crs = 4326)

EcoregionMask <- read_sf(file.path(Dir.Base,"data","shapes", "WWF_ecoregions", "official", "wwf_terr_ecos.shp")) 			# read
EcoregionMask <- st_make_valid(EcoregionMask)

BIOME <- c(as.numeric(unique(EcoregionMask$BIOME)))
lake_to_biome <- list()

  for(i in 1:(BIOME)){
     b <- EcoregionMask %>% filter(BIOME == BIOME[i])
     b2 <- data %>%
       cbind(b[st_nearest_feature(data, b),]) %>%
       mutate(dist = st_distance(geometry, geometry.1, by_element = T))
     b2$dist <- drop_units(b2$dist)
     b2 <- b2 %>% filter(dist == 0) %>% arrange(hylak_id)
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

biome_change_plot <- ggplot(biome_change, aes(BIOME_TYPE, , group = BIOME_TYPE))+
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
