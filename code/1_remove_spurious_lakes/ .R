# script selects columns of interest from GLCP 2.0 
   # selects columns 
        #columns of interest are:  
            # year,
            # hylak_id,
            # centr_lat,
            # centr_lon,
            # continent,
            # country,
            # bsn_lvl,
            # hybas_id,
            # mean_monthly_precip_mm,
            # total_precip_mm,
            # mean_annual_temp_k,
            # pop_sum,
            # seasonal_km2,
            # permanent_km2,
            # total_km2,
            # lake_area
            # elevation, 
            # sub_area,
            # ice_cover_min,
            # ice_cover_max,
            # ice_cover_mean,
            # ice_cover_median,
            # ice_cover_count,
            # snow_km2

   # calculates yearly median 
   # exports

# =======================================================================
#------------------------------------------------------------------------
#### initial time for script start #### 
s = Sys.time()

#### Libraries #### 
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(vroom, warn.conflicts = FALSE)
library(sf, warn.conflicts = FALSE)
library(units, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(Kendall, warn.conflicts = FALSE)
library(arrow, warn.conflicts = FALSE)
library(doParallel, warn.conflicts = FALSE)

#### country partition for running in parallel if needed ####
country <- list.files(path = "./data/countries")
country <- gsub("\\..*", "", country)

#### Import, select, summarize, export ####
analysis_function <- function(x){
#imports using 'arrow' 
    # calls columns f#. We maintain this until we rename before exporting
read_csv_arrow(paste0("./data/countries/",x,".csv"),   
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
    group_by(year, hylak_id, centr_lat, centr_lon,continent, country, bsn_lvl, hybas_id, 
             elevation, sub_area, lake_area, slope_100, depth_avg, shore_dev) %>%
    filter(year>=2000) %>%
#summarizing observations at each lake by year 
   #using 'median' as our summary statistic 
  summarize(mean_monthly_precip_mm = median(mean_monthly_precip_mm, na.rm = T),
            total_precip_mm = median(total_precip_mm, na.rm = T),
            mean_annual_temp_k = median(mean_annual_temp_k, na.rm = T),
            pop_sum = median(pop_sum, na.rm = T),
            seasonal_km2 = median(seasonal_km2, na.rm = T),
            permanent_km2 = median(permanent_km2, na.rm = T),
            total_km2 = median(total_km2, na.rm = T),
            ice_cover_min = median(ice_cover_min, na.rm = T),
            ice_cover_max = median(ice_cover_max, na.rm = T),
            ice_cover_mean = median(ice_cover_mean, na.rm = T),
            ice_cover_median = median(ice_cover_median, na.rm = T),
            ice_cover_count = median(ice_cover_count, na.rm = T),
            mean_spec_humidity = median(mean_spec_humidity, na.rm = T),
            mean_sw_wm2 = median(mean_sw_wm2, na.rm = T),
            mean_lw_wm2 = median(mean_lw_wm2, na.rm = T)) %>%
#renaming columns back to sensible names
#collecting so it is in arrow table format
   collect() %>%
#exporting file
   write.table(., file = paste0("./output/D1_glcp_slim_yearly_median.csv"),
               append = T,
               row.names = F,
               col.names = !file.exists("./output/D1_glcp_slim_yearly_median.csv"))
}

no_cores <- detectCores()-2
cl <- makeCluster(no_cores, type="FORK")
registerDoParallel(cl)
foreach(x=country) %dopar% analysis_function(x)

#### Time check ####
e <- Sys.time()
t=e-s
print(t)
