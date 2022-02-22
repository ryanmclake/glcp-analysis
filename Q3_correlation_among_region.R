
BIOMES <- c("TROPICAL MOIST FOREST",
             "TROPICAL DRY FOREST",
             "TEMPERATE CONIFEROUS FOREST",
             "TEMPERATE CONIFER FOREST",
             "BOREAL FOREST",
             "TROPICAL GRASSLAND",
             "TEMPERATE GRASSLAND",
             "FLOODED GRASSLAND",
             "MONTANE GRASSLAND",
             "TUNDRA",
             "MEDITERRANIAN FOREST",
             "DESERT",
             "MANGROVES")

for(i in 1:length(BIOMES)){

  b <- biome_change %>% filter(BIOME_TYPE == BIOMES[i]) %>%
    ungroup(.) %>% st_drop_geometry(.) %>%
    select(total_slope, precip_slope, temp_slope, pop_slope)

  chart.Correlation(b, histogram = TRUE, method = c("pearson"))

}


