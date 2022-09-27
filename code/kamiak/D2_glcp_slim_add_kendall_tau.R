
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(vroom, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(Kendall, warn.conflicts = FALSE)

d <- vroom::vroom("./output/glcp_slim_yearly_median.csv")

k <- d %>% select(hylak_id, permanent_km2) %>%
  group_by(hylak_id) %>%
  summarise(across(c(1),  ~list(MannKendall(.) %>%
                                  tidy %>%
                                  select(p.value, statistic)))) %>%
  ungroup(.) %>%
  unnest(c(2), names_repair = "minimal") %>%
  rename(kendall_tau = statistic) %>%
  select(-p.value)

left_join(d, k, by = "hylak_id") %>%
  write.table(., file = paste0("./output/D2_glcp_slim_add_kendall_tau.csv"),
              append = T,
              row.names = F,
              col.names = !file.exists("./output/D2_glcp_slim_add_kendall_tau.csv"))
