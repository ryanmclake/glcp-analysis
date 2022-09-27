
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(vroom, warn.conflicts = FALSE)

d <- vroom::vroom("./output/D2_glcp_slim_add_kendall_tau.csv")

c <- d %>% select(hylak_id, bsn_lvl, lake_area, sub_area) %>%
  group_by(hylak_id, bsn_lvl) %>%
  mutate(log_ratio = log(lake_area/sub_area)) %>%
  ungroup(.) %>%
  group_by(bsn_lvl) %>%
  mutate(q05 = quantile(log_ratio, 0.05)) %>%
  mutate(area_cutoff = ifelse(log_ratio > q05, "KEEP", "REMOVE")) %>%
  ungroup(.) %>%
  select(hylak_id, area_cutoff) %>% unique()

l <- left_join(d, c, by = "hylak_id") %>%
  write.table(., file = paste0("./output/D3_glcp_slim_kendall_add_cutoff.csv"),
              append = T,
              row.names = F,
              col.names = !file.exists("./output/D3_glcp_slim_kendall_add_cutoff.csv"))
