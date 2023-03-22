# script selects columns of interest from GLCP 2.0 

# selects columns 
# column numbers correspond with these variables
# 1-year
# 2-month
# 3-hylak_id
# 4-centr_lat
# 5-centr_lon
# 6-continent
# 7-country
# 8-bsn_lvl
# 9-pop_sum
# 10-permanant_km2
# 11-lake_type
# 12-sum_precip_mm
# 13-mean_temp_k
# 14-above_cutoff

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
library(units, warn.conflicts = FALSE) 
library(sf, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(Kendall, warn.conflicts = FALSE) 
library(arrow, warn.conflicts = FALSE)
library(trend, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)

  #imports using 'arrow' 
  # calls columsn f#. We maintain this until we rename before exporting
  
read_csv_arrow(paste0("/central/groups/carnegie_poc/rmcclure/GLCP_analysis_mfm/data/derived_products/glcp_extended_thin2.csv"),   
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
                                                      pop_sum=f8,
                                                      permanent_km2=f9,
                                                      lake_type=f10,
                                                      sum_precip_mm=f11,
                                                      mean_temp_k=f12,
                                                      above_ratio_cutoff=f13) %>%
    group_by(year, hylak_id, centr_lat, centr_lon, continent, country, bsn_lvl, lake_type, above_ratio_cutoff) %>%
    filter(year>=1995) %>%
    #filter(year>=2000) %>%
    #summarizing observations at each lake by year 
    #using 'median' as our summary statistic 
    summarize(sum_precip_mm = mean(sum_precip_mm, na.rm = T),
              permanent_km2 = median(permanent_km2, na.rm = T),
              mean_temp_k = median(mean_temp_k, na.rm = T),
              pop_sum = median(pop_sum, na.rm = T)) %>%
    write_csv(., "/central/groups/carnegie_poc/rmcclure/glcp-analysis/output/D1_glcp_slim_yearly_mean.csv")
    #renaming columns back to sensible names
    #collecting so it is in arrow table format
    #exporting file
    # write.table(., file = paste0("/central/groups/carnegie_poc/rmcclure/glcp-analysis/output/D1_glcp_slim_yearly_mean.csv"),
    #             append = T,
    #             row.names = F,
    #             col.names = !file.exists("/central/groups/carnegie_poc/rmcclure/glcp-analysis/output/D1_glcp_slim_yearly_median2.csv"))


#### Time check ####
e <- Sys.time()
t=e-s
print(t)
