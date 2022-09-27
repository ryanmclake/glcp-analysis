# script calculates kendall tau on GLCP slim data set 
  # glcp slim is the GLCP 2.0 with only the columns of interest
  #script:
     # selects columns 
     # calculates kendall tau
     # joins to original data set
     # exports

# =======================================================================
#------------------------------------------------------------------------

#### Libraries ####
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(vroom, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(Kendall, warn.conflicts = FALSE)

#### Bringing in 'glcp slim' data set ####
d <- vroom::vroom("./output/glcp_slim_yearly_median.csv")

#### Calculating kendall tau for each lake ####

k <- d %>% 
#selecting columns
select(hylak_id, permanent_km2) %>%
#grouping by lake id
  group_by(hylak_id) %>%
#calculating kendall tau
  summarise(across(c(1),  ~list(MannKendall(.) %>%
                                  tidy %>%
                                  select(p.value, statistic)))) %>%
#unnest so p.value and kendall tau statistic are separate columns
  ungroup(.) %>%
  unnest(c(2), names_repair = "minimal") %>%
#rename 'statistic' to 'kendall_tau'
  rename(kendall_tau = statistic) %>%
#remove 'p.value' column
  select(-p.value)

#### Joining and exporting ####
#join
left_join(d, k, by = "hylak_id") %>%
#export
  write.table(., file = paste0("./output/D2_glcp_slim_add_kendall_tau.csv"),
              append = T,
              row.names = F,
              col.names = !file.exists("./output/D2_glcp_slim_add_kendall_tau.csv"))
