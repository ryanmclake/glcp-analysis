library(trend, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tidyverse)

d <- vroom::vroom("./output/D5_glcp_slim_kendall_cutoff_reservoir_filtered.csv")

s <- d %>% select(hylak_id, total_km2) %>%
  group_by(hylak_id) %>%
  summarise(across(c(1),  ~list(sens.slope(ts(.)) %>%
                                  glance(.)))) %>%
  ungroup(.) %>%
  unnest(c(2), names_repair = "minimal") %>%
  mutate(sens.slope = (conf.low+conf.high)/2) %>%
  select(hylak_id, p.value, sens.slope) %>%
  collect() 


left_join(d, s, by = "hylak_id") %>%
  mutate(sig_sens_slope = ifelse(p.value < 0.0500000, "S", "NS")) %>%
  write.table(., file = paste0("./output/A1_glcp_filtered_add_sens_slope.csv"),       
              append = T,
              row.names = F,
              col.names = !file.exists("./output/A1_glcp_filtered_add_sens_slope.csv"))

