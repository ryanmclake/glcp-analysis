world <- map_data("world")

plot1 <- readRDS("./output/hylak_id_predictors.rds") %>%
  group_by(hylak_id, continent) %>%
  mutate(drivers = paste(predictor, collapse=", ")) %>%
  group_by(hylak_id, continent, drivers) %>%
  summarize_all(funs(mean)) %>%
  ggplot(data = ., aes(color = drivers)) +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),
           color = "black", fill = NA, size = 0.1)+
  geom_hex(aes(x = long, y = lat), bins = 100)+
  labs(x = "Longitude", y = "Latitude")+
  scale_color_gradient2(low="orange4", high="green4", space ="Lab") +
  theme_classic()+
  theme(legend.position = "top",
        legend.key.width= unit(3, 'cm'))
