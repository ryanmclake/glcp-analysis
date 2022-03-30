library(DBI)
library(RSQLite)
library(imputeTS)
library(tidyverse)
library(parallel)
library(dplyr)
library(randomForest)
library(hydroGOF)

con <- dbConnect(RSQLite::SQLite(), dbname = "./data/glcp_extended_SQLdb.sqlite")

lakes <- as.data.frame(tbl(con, sql("SELECT hybas_id FROM GLCP_SQLdb"))) %>%
  group_by(hybas_id) %>%
  summarize_all(funs(mean)) %>%
  t(.) %>%
  c(.) %>%
  factor(.)

driver_function <- function(x, con){

  con <- dbConnect(RSQLite::SQLite(), dbname = "./data/glcp_extended_SQLdb.sqlite")

  d <- DBI::dbGetQuery(con, paste0("SELECT * FROM GLCP_SQLdb WHERE hybas_id == ",x,""))

  dbDisconnect(con)

    d %>%
    dplyr::mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
    dplyr::mutate(Z_total_km2 = scale(total_km2, center = TRUE, scale = TRUE),
                  Z_sum_precip_mm = scale(sum_precip_mm, center = TRUE, scale = TRUE),
                  Z_mean_temp_k = scale(mean_temp_k, center = TRUE, scale = TRUE),
                  Z_pop_sum = scale(pop_sum, center = TRUE, scale = TRUE),
                  Z_mean_spec_humidity = scale(mean_spec_humidity, center = TRUE, scale = TRUE),
                  Z_mean_totcloud_pct = scale(mean_totcloud_pct, center = TRUE, scale = TRUE),
                  Z_mean_sw_wm2 = scale(mean_sw_wm2, center = TRUE, scale = TRUE),
                  Z_mean_lw_wm2 = scale(mean_lw_wm2, center = TRUE, scale = TRUE)) %>%
    dplyr::mutate(Z_total_km2 = ifelse(is.nan(Z_total_km2),0,Z_total_km2)) %>%
    dplyr::group_by(year)%>%
    dplyr::mutate(lag_Z_total_km2 = lag(Z_total_km2)) %>%
    dplyr::mutate_at(vars(lag_Z_total_km2,
                          Z_sum_precip_mm,
                          Z_mean_temp_k,
                          Z_mean_spec_humidity,
                          Z_mean_totcloud_pct,
                          Z_mean_sw_wm2,
                          Z_mean_lw_wm2),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
    dplyr::ungroup(.) %>%
    select(hylak_id, year, pour_long, pour_lat, Z_total_km2, lag_Z_total_km2, Z_sum_precip_mm,Z_mean_temp_k,Z_pop_sum,Z_mean_spec_humidity,Z_mean_totcloud_pct,
           Z_mean_sw_wm2,Z_mean_lw_wm2,elevation,continent,slope_100,shore_dev,res_time,sub_area) %>%
    dplyr::group_by(hylak_id,pour_long,pour_lat,elevation,continent,slope_100,shore_dev,res_time,sub_area) %>%
    dplyr::do(model = randomForest::randomForest(formula = Z_total_km2 ~  Z_sum_precip_mm +
                                                                          Z_mean_temp_k +
                                                                          Z_pop_sum +
                                                                          Z_mean_spec_humidity +
                                                                          Z_mean_totcloud_pct +
                                                                          Z_mean_sw_wm2 +
                                                                          Z_mean_lw_wm2, data = ., na.action=na.roughfix)) %>%
    dplyr::collect() %>%
    dplyr::ungroup(.)

  e <- as.data.frame(d$model[[1]]$importance)
  e$hylak_id = d$hylak_id
  e$predictor <- row.names(e)
  e$long = d$pour_long
  e$lat = d$pour_long
  e$elevation = d$elevation
  e$continent = d$continent
  e$slope_100 = d$slope_100
  e$shore_dev = d$shore_dev
  e$res_time = d$res_time
  e$sub_area = d$sub_area
  e$NSE = hydroGOF::NSE(d$model[[1]]$predicted, d$model[[1]]$y)

  return(e)

}

no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)
clusterExport(cl, list("%>%","collect", "vars","con","dbConnect","funs","select","na.roughfix","mutate_all"))
parLapply(cl, lakes, driver_function) %>% do.call(rbind, .) %>%
  group_by(hylak_id,long,lat,elevation,continent,slope_100,shore_dev,res_time,sub_area,NSE) %>%
  arrange(hylak_id, -IncNodePurity) %>% saveRDS(., file = "./output/hylak_id_predictors.rds")
stopCluster(cl)
