s = Sys.time()

library(arrow, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

# country <- list.files(path = "./data/countries")
# country <- gsub("\\..*", "", country)

read_csv_arrow(paste0("./data/countries/glcp_extended.csv"),
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
    timestamp_parsers = NULL) %>%
  collect() %>% 
  select(f0, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12,
                    f13, f14, f15, f25, f38, f39, f40, f41, f42, f47, f48)%>% 
  group_by(f0, f2, f3, f4, f5, f6, f7, f8, f25) %>%
  summarize(f9 = median(f9, na.rm = T),
            f10 = median(f10, na.rm = T),
            f11 = median(f11, na.rm = T),
            f12 = median(f12, na.rm = T),
            f13 = median(f13, na.rm = T),
            f14 = median(f14, na.rm = T),
            f15 = median(f15, na.rm = T),
            f38 = median(f38, na.rm = T),
            f39 = median(f39, na.rm = T),
            f40 = median(f40, na.rm = T),
            f41 = median(f41, na.rm = T),
            f42 = median(f42, na.rm = T),
            f47 = median(f47, na.rm = T),
            f48 = median(f48, na.rm = T)) %>%
  rename(year = f0,
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
         elevation=f25,
         above_ratio_cutoff=f38,
         ice_cover_min=f39,
         ice_cover_max=f40,
         ice_cover_mean=f41,
         ice_cover_median=f42,
         ice_cover_count=f47,
         snow_km2=f48) %>%
      collect() %>%
      write.table(., file = paste0("./output/glcp_slim_yearly_median.csv"),
                  append = T,
                  row.names = F,
                  col.names = !file.exists("./output/glcp_slim_yearly_median.csv"))

e <- Sys.time()
t=e-s
print(t)