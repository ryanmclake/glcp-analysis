library(DBI)
library(RSQLite)
library(imputeTS)
library(tidyverse)
library(parallel)
library(dplyr)

con <- dbConnect(RSQLite::SQLite(), dbname = "./data/glcp_extended_SQLdb.sqlite")


lakes <- as.data.frame(tbl(con, sql("SELECT hylak_id FROM GLCP_SQLdb"))) %>%
  group_by(hylak_id) %>%
  summarize_all(funs(mean)) %>%
  t(.) %>%
  c(.) %>%
  factor(.)

dbDisconnect(con)


slope_function <- function(x){

  con <- dbConnect(RSQLite::SQLite(), dbname = "./data/glcp_extended_SQLdb.sqlite")

  d <- DBI::dbGetQuery(con, paste0("SELECT * FROM GLCP_SQLdb WHERE hylak_id == ",x,""))

  dbDisconnect(con)

  test <- d %>% group_by(year, hylak_id, hybas_id) %>%
    dplyr::summarize_all(funs(mean), na.rm = T) %>%
    #dplyr::filter(year >= "2000") %>%
    ungroup(.) %>%
    dplyr::mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
    dplyr::mutate(total_km2 = ifelse(is.nan(total_km2),0,total_km2),
                  total_precip_mm = ifelse(is.nan(total_precip_mm),0,total_precip_mm),
                  mean_temp_k = ifelse(is.nan(mean_temp_k),0,mean_temp_k),
                  pop_sum = ifelse(is.nan(pop_sum),0,pop_sum),
                  mean_spec_humidity = ifelse(is.nan(mean_spec_humidity),0,mean_spec_humidity),
                  mean_totcloud_pct = ifelse(is.nan(mean_totcloud_pct),0,mean_totcloud_pct),
                  mean_sw_wm2 = ifelse(is.nan(mean_sw_wm2),0,mean_sw_wm2),
                  mean_lw_wm2 = ifelse(is.nan(mean_lw_wm2),0,mean_lw_wm2),
                  ice_cover_count = ifelse(is.nan(ice_cover_count),0,ice_cover_count)) %>%
    dplyr::mutate(scaled_total_km2 = ((total_km2 - total_km2[1])/total_km2[1])*100,
                  scaled_total_precip_mm = ((total_precip_mm - total_precip_mm[1])/total_precip_mm[1])*100,
                  scaled_mean_temp_k = ((mean_temp_k - mean_temp_k[1])/mean_temp_k[1])*100,
                  scaled_pop_sum = ((pop_sum - pop_sum[1])/pop_sum[1])*100,
                  scaled_mean_spec_humidity = ((mean_spec_humidity - mean_spec_humidity[1])/mean_spec_humidity[1])*100,
                  scaled_mean_totcloud_pct = ((mean_totcloud_pct - mean_totcloud_pct[1])/mean_totcloud_pct[1])*100,
                  scaled_mean_sw_wm2 = ((mean_sw_wm2 - mean_sw_wm2[1])/mean_sw_wm2[1])*100,
                  scaled_mean_lw_wm2 = ((mean_lw_wm2 - mean_lw_wm2[1])/mean_lw_wm2[1])*100,
                  scaled_ice_cover_count = ((ice_cover_count - ice_cover_count[1])/ice_cover_count[1])*100,
                  scaled_ice_cover_count = ifelse(is.nan(scaled_ice_cover_count),0,scaled_ice_cover_count)) %>%
    dplyr::group_by(hylak_id, hybas_id, pour_long, pour_lat, lake_type,
                    lake_area, shore_dev, vol_total, depth_avg, res_time,
                    elevation, slope_100, wshd_area) %>%
    dplyr::do(fit_total_km2 = lm(scaled_total_km2 ~ year, data = ., na.action = na.exclude),
              fit_precip_mm = lm(scaled_total_precip_mm ~ year, data = ., na.action = na.exclude),
              fit_mean_temp = lm(scaled_mean_temp_k ~ year, data = ., na.action = na.exclude),
              fit_pop_sum = lm(scaled_pop_sum ~ year, data = ., na.action = na.exclude),
              fit_spec_hum = lm(scaled_mean_spec_humidity ~ year, data = ., na.action = na.exclude),
              fit_total_cloud = lm(scaled_mean_totcloud_pct ~ year, data = ., na.action = na.exclude),
              fit_sw = lm(scaled_mean_sw_wm2 ~ year, data = ., na.action = na.exclude),
              fit_lw = lm(scaled_mean_lw_wm2 ~ year, data = ., na.action = na.exclude),
              fit_ice_cov = lm(scaled_ice_cover_count ~ year, data = ., na.action = na.exclude)) %>%
    dplyr::collect() %>%
    dplyr::ungroup(.) %>%
    dplyr::mutate(fit_total_slope = .$fit_total_km2[[1]]$coefficients[2],
                  fit_total_rsq = summary(.$fit_total_km2[[1]])$r.square,
                  fit_precip_slope = .$fit_precip_mm[[1]]$coefficients[2],
                  fit_precip_rsq = summary(.$fit_precip_mm[[1]])$r.square,
                  fit_temp_slope = .$fit_mean_temp[[1]]$coefficients[2],
                  fit_temp_rsq = summary(.$fit_mean_temp[[1]])$r.square,
                  fit_pop_slope = .$fit_pop_sum[[1]]$coefficients[2],
                  fit_pop_rsq = summary(.$fit_pop_sum[[1]])$r.square,
                  fit_humid_slope = .$fit_spec_hum[[1]]$coefficients[2],
                  fit_humid_rsq = summary(.$fit_spec_hum[[1]])$r.square,
                  fit_cloud_slope = .$fit_total_cloud[[1]]$coefficients[2],
                  fit_cloud_rsq = summary(.$fit_total_cloud[[1]])$r.square,
                  fit_sw_slope = .$fit_sw[[1]]$coefficients[2],
                  fit_sw_rsq = summary(.$fit_sw[[1]])$r.square,
                  fit_lw_slope = .$fit_lw[[1]]$coefficients[2],
                  fit_lw_rsq = summary(.$fit_lw[[1]])$r.square,
                  fit_ice_slope = .$fit_ice_cov[[1]]$coefficients[2],
                  fit_ice_rsq = summary(.$fit_ice_cov[[1]])$r.square) %>%
    dplyr::select(-fit_total_km2,
                  -fit_precip_mm,
                  -fit_mean_temp,
                  -fit_pop_sum,
                  -fit_spec_hum,
                  -fit_total_cloud,
                  -fit_sw,
                  -fit_lw,
                  -fit_ice_cov) %>%
    write.table(., file = paste0("./output/hylak_id_slopes.csv"),
                append = T,
                row.names = F,
                col.names = !file.exists("./output/hylak_id_slopes.csv"))

  return(unique(d$hylak_id))

}

no_cores <- detectCores()-2

cl <- makeCluster(no_cores)

clusterExport(cl, list("%>%","collect", "vars","con","dbConnect","funs","dbDisconnect","ungroup","group_by"))

mcLapply(lakes, function(x) slope_function(x), cores = no_cores)



stopCluster(cl)

vroom::vroom("./output/hylak_id_slopes.csv") %>% saveRDS(., file = "./output/hylak_id_slopes.rds")

