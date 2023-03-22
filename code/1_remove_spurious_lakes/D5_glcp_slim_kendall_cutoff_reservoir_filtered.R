

# script removes lakes based on their kendall tau, area cutoff, and reservoir tag from the GCLP slim kendall cutoff reservoir data set
  # glcp slim kendall cutoff reservoir is the GLCP 2.0 with only the columns of interest, plus the column of kendal tau of permanent water, cutoff, and reservoir tag
  #script:
     # imports data
     # selects lakes with Kendall Tau's of less than 1 
     # selects lakes above the lake area/basin area cutoff
     # exports

# =======================================================================
#------------------------------------------------------------------------

#### initial time for script start #### 
s = Sys.time()

#### Bringing in and filtering data ####
#import
vroom::vroom("./output/D4_glcp_slim_kendall_cutoff_add_reservoir_new.csv") %>%
#select lakes with kendall tau < 1
  filter(kendall_tau < 1) %>%
#select lakes above area cutoff
  #filter(area_cutoff == "KEEP") %>%
#select lakes tagged as LAKE
  filter(water_body_type == "LAKE") %>%
  write.table(., file = paste0("./output/D5_glcp_slim_kendall_cutoff_reservoir_filtered_new.csv"),
              append = T,
              row.names = F,
              col.names = !file.exists("./output/D5_glcp_slim_kendall_cutoff_reservoir_filtered_new.csv"))

#### Time check ####
e <- Sys.time()
t=e-s
print(t)
