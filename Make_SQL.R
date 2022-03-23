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

slope_function <- function(x, con){

  con <- dbConnect(RSQLite::SQLite(), dbname = "./data/glcp_extended_SQLdb.sqlite")
    d <- DBI::dbGetQuery(con, paste0("SELECT * FROM GLCP_SQLdb WHERE hylak_id == ",x,"")) %>%
         dplyr::mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
         dplyr::mutate(Z_total_km2 = scale(total_km2, center = TRUE, scale = TRUE)) %>%
         dplyr::group_by(year) %>%
         dplyr::summarize_all(funs(mean)) %>%
         dplyr::ungroup(.) %>%
         dplyr::select(hylak_id, pour_long, pour_lat, year, Z_total_km2) %>%
         dplyr::mutate(Z_total_km2 = ifelse(is.nan(Z_total_km2),0,Z_total_km2)) %>%
         dplyr::group_by(hylak_id, pour_long, pour_lat) %>%
         dplyr::do(fit_total = lm(Z_total_km2 ~ year, data = ., na.action = na.exclude)) %>%
         dplyr::collect() %>%
         dplyr::ungroup(.) %>%
         dplyr::mutate(fit_total_slope = .$fit_total[[1]]$coefficients[2],
                       fit_total_rsq = summary(.$fit_total[[1]])$r.square) %>%
         dplyr::select(-fit_total)

    return(d)

}

no_cores <- detectCores()

cl <- makeCluster(no_cores)

clusterExport(cl, list("%>%","collect", "vars","con","dbConnect","funs"))

dat_out <- parLapply(cl, lakes, slope_function) %>% do.call(rbind, .)


