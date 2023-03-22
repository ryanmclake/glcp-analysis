
# script calculates sens slope and p value for lake change, based on the GLCP_glim_kendall_cutoff_reservoir_filtered dataset
  # glcp slim kendall cutoff reservoir filtered is the GLCP 2.0 with only the columns of interest, and the spurious lakes based on kendall tau, size, and reservoir status removed
  # script: 
     # loads dataset
     # selects lake id and total km surface area columns
     # calculates sens slope and p values
     # joins with origional data set 
     # exports

# =======================================================================
#------------------------------------------------------------------------

#### Load libraries ####
library(trend, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

#### Bringingin the data set ####
# Load in the dataset that has all of the spurious lakes and reservoirs removed
d <- vroom::vroom("./output/D5_glcp_slim_kendall_cutoff_reservoir_filtered_new.csv")

#### Calculating Senes slope and p value ####
# Quantify the sen's slope (non-parametric)
# Only selecting the hylak_id and total_km2
s <- d %>% 
  dplyr::select(hylak_id, permanent_km2) %>%
  # Grouping by hylak_id
  dplyr::group_by(hylak_id) %>%
  mutate(permanent_km2 = scale(permanent_km2)) %>%
  na.omit(.) %>%
  # Summarizing and calculating the sens slope from the trend package
  summarise(across(c(1),  ~list(sens.slope(ts(.))))) %>%
  dplyr::group_by(hylak_id) %>%
  mutate(sens.slope = unlist(purrr::map(permanent_km2, "estimates")),
         p.value = unlist(purrr::map(permanent_km2, "p.value"))) %>%
  select(-permanent_km2) %>%
  select(hylak_id, p.value, sens.slope) %>%
  # Collect makes it run faster
  collect() 

#### Joining and exporting ####
# Join with the original DF and make a new column that specifies what sens slopes
# are significant. These significant slopes will be filtered in the next script
left_join(d, s, by = "hylak_id") %>%
  mutate(sig_sens_slope = ifelse(p.value < 0.0500000, "S", "NS")) %>%
  readr::write_csv(., file = paste0("./output/A1_glcp_filtered_add_sens_slope_new.csv"))

