
#### initial time for script start #### 
s = Sys.time()

d <- vroom::vroom("./output/D3_glcp_slim_kendall_add_cutoff.csv")

d2 <- d %>% select(hylak_id, centr_lon, centr_lat) %>%
  st_as_sf(coords = c("centr_lon", "centr_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

dam_data <- vroom::vroom("./data/GeoDAR_v11_dams_beta_pr1.csv", delim = ",", col_names = T) %>%
  select(longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

dam_glcp_link <- d2 %>%
  cbind(dam_data[st_nearest_feature(d2, dam_data),]) %>%
  mutate(dist = st_distance(geometry, geometry.1, by_element = T))

# units are in meters --> Removing the units so I can work with the column in the left_join function
dam_glcp_link$dist <- drop_units(dam_glcp_link$dist)

dams_to_remove <- dam_glcp_link %>% 
  st_drop_geometry() %>%
  select(-geometry.1) %>% unique()

left_join(d, dams_to_remove, by = "hylak_id") %>%
  mutate(water_body_type = ifelse(dist <= 5000, "RESERVOIR", "LAKE")) %>%
  select(-dist) %>% 
  write.table(., file = paste0("./output/D4_glcp_slim_kendall_cutoff_add_reservoir.csv"),
              append = T,
              row.names = F,
              col.names = !file.exists("./output/D4_glcp_slim_kendall_cutoff_add_reservoir.csv"))

#### Time check ####
e <- Sys.time()
t=e-s
print(t)
