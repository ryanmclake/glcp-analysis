
# Load libraries
library(trend, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)


# Load in the dataset that has all of the spurious lakes and reservoirs removed
d <- vroom::vroom("./output/D5_glcp_slim_kendall_cutoff_reservoir_filtered.csv")

# Quantify the sen's slope (non-parametric)
# Only selecting the hylak_id and total_km2
s <- d %>% 
  dplyr::select(hylak_id, total_km2) %>%
  # Grouping by hylak_id
  dplyr::group_by(hylak_id) %>%
  # Summarizing and calculating the sens slope from the trend package
  summarise(across(c(1),  ~list(sens.slope(ts(.))))) %>%
  dplyr::group_by(hylak_id) %>%
  mutate(sens.slope = unlist(purrr::map(total_km2, "estimates")),
         p.value = unlist(purrr::map(total_km2, "p.value"))) %>%
  select(-total_km2) %>%
  select(hylak_id, p.value, sens.slope) %>%
  # Collect makes it run faster
  collect() 

# Join with the original DF and make a new column that specifies what sens slopes
# are significant. These significant slopes will be filtered in the next script
left_join(d, s, by = "hylak_id") %>%
  mutate(sig_sens_slope = ifelse(p.value < 0.0500000, "S", "NS")) %>%
  write.table(., file = paste0("./output/A1_glcp_filtered_add_sens_slope.csv"),       
              append = T,
              row.names = F,
              col.names = !file.exists("./output/A1_glcp_filtered_add_sens_slope.csv"))

