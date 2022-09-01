s = Sys.time()


#devtools::install_github("zmjones/edarf", subdir = "pkg")

library(arrow, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(doParallel, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(Kendall, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(randomForest, warn.conflicts = FALSE)
library(edarf, warn.conflicts = FALSE)


country <- list.files(path = "./output/slopes/countries")
country <- gsub("\\..*", "", country)

perfectPartialPlot <- function(df, x, y){

  # Need string for aes_string()
  centile <- "centile"

  # Save marginal probabilities as separate data frame
  vote_prop <- df %>%
    select(y) %>%
    mutate(row = row_number())

  # Gather predictor centiles into a single column and join vote_prop
  pd_tidy <- df %>%
    select(x) %>%
    gather(x, key = "predictor", value = "centile") %>%
    na.omit() %>%
    mutate(row = row_number()) %>%
    left_join(vote_prop, by = "row")

  # Create the perfect partial plot
  ggplot(pd_tidy, aes_string(x = centile, y = y)) +
    geom_line(lwd = 1.25) +
    labs(title = paste0(country[137]," Partial Dependence Plot"),
         x = "",
         y = paste("Trend in", y)) +
    facet_wrap(~predictor) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
    theme(plot.title = element_text(hjust = 0.5))

}

analysis_function <- function(i){

d <- read_csv_arrow(
    paste0("./output/slopes/countries/",country[137],".csv"),
    quote = "\"",
    escape_double = TRUE,
    escape_backslash = FALSE,
    schema = NULL,
    col_names = F,
    col_types = NULL,
    col_select = NULL,
    na = c("", "NA"),
    quoted_na = TRUE,
    skip_empty_rows = TRUE,
    skip = 0L,
    parse_options = NULL,
    convert_options = NULL,
    read_options = NULL,
    as_data_frame = TRUE,
    timestamp_parsers = NULL) %>% rename(year = f0,
                                         month=f1,
                                         hylak_id=f2,
                                         centr_lat=f3,
                                         centr_lon=f4,
                                         continent=f5,
                                         country=f6,
                                         bsn_lvl=f7,
                                         hybas_id=f8,
                                         mean_monthly_precip_mm=f9,
                                         total_precip_mm=f10,
                                         mean_annual_temp_k=f11,
                                         pop_sum=f12,
                                         seasonal_km2=f13,
                                         permanent_km2=f14,
                                         total_km2=f15,
                                         lake_name=f16,
                                         lake_type=f17,
                                         lake_area=f18,
                                         shore_dev=f19,
                                         vol_total=f20,
                                         vol_res=f21,
                                         vol_src=f22,
                                         depth_avg=f23,
                                         res_time=f24,
                                         elevation=f25,
                                         slope_100=f26,
                                         wshd_area=f27,
                                         pour_long=f28,
                                         pour_lat=f29,
                                         sub_area=f30,
                                         mean_spec_humidity=f31,
                                         mean_precip_mm=f32,
                                         sum_precip_mm=f33,
                                         mean_temp_k=f34,
                                         mean_totcloud_pct=f35,
                                         mean_sw_wm2=f36,
                                         mean_lw_wm2=f37,
                                         above_ratio_cutoff=f38,
                                         ice_cover_min=f39,
                                         ice_cover_max=f40,
                                         ice_cover_mean=f41,
                                         ice_cover_median=f42,
                                         ice_cover_binary_min=f43,
                                         ice_cover_binary_max=f44,
                                         ice_cover_binary_mean=f45,
                                         ice_cover_binary_median=f46,
                                         ice_cover_count=f47,
                                         snow_km2=f48) %>%
  group_by(year, hylak_id, centr_lat, centr_lon, elevation) %>%
  collect() %>%
  summarize(total_km2 = mean(total_km2, na.rm = T),
            total_precip_mm = sum(total_precip_mm, na.rm = T),
            mean_temp_k = mean(mean_temp_k, na.rm = T),
            pop_sum = mean(pop_sum, na.rm = T),
            ice_cover_mean = mean(ice_cover_mean, na.rm = T)) %>%
  ungroup(.) %>%
  select(-year) %>%
  group_by(hylak_id) %>%
  mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear"))) %>%
  mutate(across(c(4:8), ~ scale(.))) %>%
  mutate(across(c(4:8), ~ if_else(is.na(.),0,.))) %>%
  na.omit(.) %>%
  ungroup(.) %>%
  st_as_sf(coords = c("centr_lon", "centr_lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") %>%
      collect()

dam_data <- vroom::vroom("./data/GeoDAR_v11_dams_beta_pr1.csv", delim = ",", col_names = T) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

dam_glcp_link <- dam_data %>%
  cbind(d[st_nearest_feature(dam_data, d),]) %>%
  mutate(dist = st_distance(geometry, geometry.1, by_element = T))
#
reservoirs <- c(unique(dam_glcp_link$hylak_id))

`%!in%` <- Negate(`%in%`)

d_2 <- d %>%
  filter(hylak_id %!in% reservoirs)

model = randomForest::randomForest(formula = total_km2 ~
                                             total_precip_mm +
                                             mean_temp_k +
                                             pop_sum +
                                             ice_cover_mean,
                                         data = d_2,
                                         na.action=na.roughfix,
                                         ntree = 50)

      d_2 <- d_2 %>%
        mutate(predicted_total_km2 = predict(model, type = "class")) %>%
        st_drop_geometry()

      imp_df <- as.data.frame(model$importance) %>%
        mutate(names = rownames(.)) %>%
        arrange(desc(IncNodePurity))

      nm <- as.character(imp_df$names)[1:4]

      pd_df <- partial_dependence(fit = model,
                                  vars = nm,
                                  data = d_2,
                                  n = c(100, 200))

      p <- perfectPartialPlot(df = pd_df, x = nm, y = "total_km2")

      ggsave(p, path = ".",
             filename = paste0("./output/figures/",country[137],"_PDP_plot.jpg"),
             width = 8, height = 6, device='jpg', dpi=200, limitsize = FALSE)




    # imp_DFhylak_ids <- c(unique(d$hylak_id))
    # rf_output <- list()
    #
    # for(s in 1:length(hylak_ids)){
    #
    #   rf_out <- as.data.frame(d$model[[s]]$importance) %>%
    #     mutate(hylak_id = hylak_ids[s],
    #            centr_lat = d$centr_lat[s],
    #            centr_lon = d$centr_lon[s],
    #            predictor = row.names(.),
    #            NSE = hydroGOF::NSE(d$model[[s]]$predicted, d$model[[s]]$y)) %>%
    #     arrange(-IncNodePurity) %>%
    #     slice(1)
    #
    #   rf_output[[s]] <- rf_out
    #
    # }
    #
    # rf_predict = do.call(rbind, rf_output) %>%
    #   write.table(., file = paste0("./output/slopes/hylak_id_random_forest.csv"),
    #               append = T,
    #               row.names = F,
    #               col.names = !file.exists("./output/slopes/hylak_id_random_forest.csv"))
}


no_cores <- detectCores()-2
cl <- makeCluster(no_cores, type="FORK")
registerDoParallel(cl)
foreach(i=country) %dopar% analysis_function(i)

e <- Sys.time()
t=e-s
print(t)
