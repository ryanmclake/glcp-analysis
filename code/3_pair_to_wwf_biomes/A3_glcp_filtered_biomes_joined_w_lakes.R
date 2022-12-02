
Dir.Base <- getwd()

ecoregion <- read_sf(file.path(Dir.Base,"data","shapes", "WWF_ecoregions", "official", "wwf_terr_ecos.shp")) %>%
  st_make_valid(.) %>%
  st_transform("+proj=eqearth +wktext") %>%
  select(BIOME, geometry) %>%
  mutate(biome_type = case_when(
    BIOME == 1 ~ "TROPICAL",
    BIOME == 2 ~ "TROPICAL",
    BIOME == 3 ~ "TROPICAL",
    BIOME == 4 ~ "TEMPERATE",
    BIOME == 5 ~ "TEMPERATE",
    BIOME == 6 ~ "BOREAL/TUNDRA/ICE",
    BIOME == 7 ~ "TROPICAL",
    BIOME == 8 ~ "TEMPERATE",
    BIOME == 9 ~ "TEMPERATE",
    BIOME == 10 ~ "TEMPERATE",
    BIOME == 11 ~ "BOREAL/TUNDRA/ICE",
    BIOME == 12 ~ "TEMPERATE",
    BIOME == 13 ~ "DESERT",
    BIOME == 14 ~ "TROPICAL",
    BIOME == 98 ~ "LAKE",
    BIOME == 99 ~ "BOREAL/TUNDRA/ICE",
    TRUE ~ NA_character_)) %>%
  filter(BIOME != "LAKE") %>%
  select(-BIOME)

year_in_dat <- c("2000","2001", "2002", "2003", "2004", "2005", "2006", "2007",
                 "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")

d <- vroom::vroom("/Volumes/SeagateBackupPlusDrive/A2_glcp_filtered_significant_sens_slopes_for_modeling 1.csv") %>%
     filter(year %in% year_in_dat)

d2 <- d %>%
  select(hylak_id, centr_lon, centr_lat) %>%
  na.omit(.) %>%
  st_as_sf(coords = c("centr_lon", "centr_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

# takes a while! Sit tight. 
link <- d2 %>%
  cbind(ecoregion[st_nearest_feature(d2, ecoregion),]) %>%
  mutate(dist = st_distance(geometry, geometry.1, by_element = T)) %>%
  st_drop_geometry(.) %>%
  select(-geometry.1, -dist) %>%
  unique()

left_join(d, link, by = "hylak_id") %>%
  write.table(., file = paste0("./output/A3_glcp_filtered_biomes_joined.csv"),
              append = T,
              row.names = F,
              col.names = !file.exists("./output/A3_glcp_filtered_biomes_joined.csv"))
