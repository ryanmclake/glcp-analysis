# script selects lakes which are changing (swelling or shrinking) from the GLCP filtered add sense slope data
  # GLCP filtered add sense slope is the analysis data set with sense slope and p value calculated
  # script: 
     # loads dataset
     # selects lakes which have significant slopes based on P value (sis_sens_slope column is categorical column identifying significant slopes) 
     # exports

# =======================================================================
#------------------------------------------------------------------------

# Filter only the significant sen's slopes to link with the WWF global Ecoregions

# Pull in DF using vroom::vroom (its faster)
vroom::vroom("./output/A1_glcp_filtered_add_sens_slope.csv") %>% 
  # Filter only the significant sen's slopes (identified in the previous A1 script)
  dplyr::filter(sig_sens_slope == "S") %>%
  # Write it to a new table
  write.table(., file = paste0("./output/A2_glcp_filtered_significant_sens_slopes_for_modeling.csv"),       
              append = T,
              row.names = F,
              col.names = !file.exists("./output/A2_glcp_filtered_significant_sens_slopes_for_modeling.csv"))
