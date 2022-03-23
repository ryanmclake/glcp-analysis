library(multidplyr)
library(vroom)
library(sf)
library(dplyr)
library(ggplot2)
library(maps)

cluster <- multidplyr::new_cluster(16)


d2 <- vroom::vroom("./data/glcp_hydro_and_climate_pop_subset_all_vars.csv", delim = ",") %>%
    dplyr::group_by(hybas_id) %>%
    dplyr::filter(lake_type == 1) %>%
    dplyr::mutate(Z_total_km2 = scale(total_km2, center = TRUE, scale = TRUE),
                  Z_precip = scale(sum_precip_mm, center = TRUE, scale = TRUE),
                  Z_temp = scale(mean_temp_k, center = TRUE, scale = TRUE),
                  Z_pop = scale(pop_sum, center = TRUE, scale = TRUE))
    dplyr::group_by(year) %>%
    dplyr::mutate(SD_precip = sd(sum_precip_mm),
                  SD_temp = sd(mean_temp_k),
                  SD_pop = scale(pop_sum)) %>%
    dplyr::ungroup(.) %>%
    dplyr::select(hybas_id, year, Z_total_km2, Z_precip, Z_temp, Z_pop) %>%
    na.omit(.)%>%
    dplyr::group_by(hybas_id, pour_long, pour_lat) %>%
    multidplyr::partition(cluster) %>%
    dplyr::do(fit_total = lm(Z_total_km2 ~ year, data = ., na.action = na.exclude),
              fit_precip = lm(Z_precip ~ year, data = ., na.action = na.exclude),
              fit_temp = lm(Z_temp ~ year, data = ., na.action = na.exclude),
              fit_pop = lm(Z_pop ~ year, data = ., na.action = na.exclude))%>%
    collect()

  slopes_total <- matrix(ncol=1, nrow=length(d2$hybas_id))
  slopes_precip<- matrix(ncol=1, nrow=length(d2$hybas_id))
  slopes_temp <- matrix(ncol=1, nrow=length(d2$hybas_id))
  slopes_pop <- matrix(ncol=1, nrow=length(d2$hybas_id))

  for(i in 1:length(d2$hybas_id)){
    slopes_total[i,] <- d2$fit_total[[i]]$coefficients[2]
    slopes_precip[i,] <- d2$fit_precip[[i]]$coefficients[2]
    slopes_temp[i,] <- d2$fit_temp[[i]]$coefficients[2]
    slopes_pop[i,] <- d2$fit_pop[[i]]$coefficients[2]
  }

  slopes_total <- as.data.frame(slopes_total) %>% rename(., total_slope = V1)
  slopes_precip <- as.data.frame(slopes_precip) %>% rename(., precip_slope = V1)
  slopes_temp <- as.data.frame(slopes_temp) %>% rename(., temp_slope = V1)
  slopes_pop <- as.data.frame(slopes_pop) %>% rename(., pop_slope = V1)

  slopes <- bind_cols(as.data.frame(d2$hybas_id),
                      as.data.frame(d2$pour_long),
                      as.data.frame(d2$pour_lat),
                      slopes_total,
                      slopes_precip,
                      slopes_temp,
                      slopes_pop) %>%
    rename(hybas_id = `d2$hybas_id`,
           longitude = `d2$pour_long`,
           latitude = `d2$pour_lat`)

saveRDS(slopes, "./output/hybas_id_slopes.rds")

rsq_total <- matrix(ncol=1, nrow=length(d2$hybas_id))
rsq_precip<- matrix(ncol=1, nrow=length(d2$hybas_id))
rsq_temp <- matrix(ncol=1, nrow=length(d2$hybas_id))
rsq_pop <- matrix(ncol=1, nrow=length(d2$hybas_id))

for(i in 1:length(d2$hybas_id)){
  rsq_total[i,] <- summary(d2$fit_total[[i]])$r.square
  rsq_precip[i,] <- summary(d2$fit_precip[[i]])$r.square
  rsq_temp[i,] <- summary(d2$fit_temp[[i]])$r.square
  rsq_pop[i,] <- summary(d2$fit_pop[[i]])$r.square
}

rsq_total <- as.data.frame(rsq_total) %>% rename(., total_rsq = V1)
rsq_precip <- as.data.frame(rsq_precip) %>% rename(., precip_rsq = V1)
rsq_temp <- as.data.frame(rsq_temp) %>% rename(., temp_rsq = V1)
rsq_pop <- as.data.frame(rsq_pop) %>% rename(., pop_rsq = V1)

rsq <- bind_cols(as.data.frame(d2$hybas_id),
                    as.data.frame(d2$pour_long),
                    as.data.frame(d2$pour_lat),
                    rsq_total,
                    rsq_precip,
                    rsq_temp,
                    rsq_pop) %>%
  rename(hybas_id = `d2$hybas_id`,
         longitude = `d2$pour_long`,
         latitude = `d2$pour_lat`)

saveRDS(rsq, "./output/hybas_id_rsq.rds")

world <- map_data("world")

slopes <- slopes %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mid_slope<-mean(slopes$total_slope)

rsq <- rsq %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

slope_plot <- ggplot() +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),
           color = "black", fill = NA, size = 0.1)+
  labs(x = "Longitude", y = "Latitude")+
  geom_sf(data = slopes, aes(color = total_slope), pch = 15, size = 0.1, inherit.aes = F)+
  scale_color_gradient2(midpoint=mid_slope, low="blue", mid="green3",
                        high="red", space ="Lab") +
  theme_classic()
slope_plot
ggsave(path = ".", filename = "./output/figures/slope_global_plot.jpg", width = 10, height = 8, device='jpg', dpi=700)

rsq_plot <- ggplot() +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),
           color = "black", fill = NA, size = 0.1)+
  labs(x = "Longitude", y = "Latitude")+
  geom_sf(data = rsq, aes(color = total_rsq), pch = 15, size = 0.1, inherit.aes = F)+
  scale_color_gradient(low="blue",
                        high="red")+
  theme_classic()
rsq_plot
ggsave(path = ".", filename = "./output/figures/rsq_global_plot.jpg", width = 10, height = 8, device='jpg', dpi=700)
