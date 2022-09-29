
# Filter only the significant sen's slopes to link with the WWF global Ecoregions

# Pull in DF using vroom::vroom (its faster)
vroom::vroom("./output/A1_glcp_filtered_add_sens_slope.csv") %>% 
  # Filter only the significant sen's slopes (identified in the previous A1 script)
  dplyr::filter(sig_sens_slope == "S") %>%
  # Write it to a new table
  write.table(., file = paste0("./output/A2_glcp_filtered_significant_sens_slopes.csv"),       
              append = T,
              row.names = F,
              col.names = !file.exists("./output/A2_glcp_filtered_significant_sens_slopes.csv"))