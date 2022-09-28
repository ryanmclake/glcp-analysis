#### initial time for script start #### 
s = Sys.time()

vroom::vroom("./output/D4_glcp_slim_kendall_cutoff_add_reservoir.csv") %>%
  filter(kendall_tau < 1) %>%
  filter(area_cutoff == "KEEP") %>%
  filter(water_body_type == "LAKE") %>%
  write.table(., file = paste0("./output/D5_glcp_slim_kendall_cutoff_reservoir_filtered.csv"),
              append = T,
              row.names = F,
              col.names = !file.exists("./output/D5_glcp_slim_kendall_cutoff_reservoir_filtered.csv"))

#### Time check ####
e <- Sys.time()
t=e-s
print(t)
