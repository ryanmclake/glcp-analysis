
condense_glcp <- function(directory) {
  d1 <- vroom::vroom("./data/glcp_hydro_and_climate_pop_subset_all_vars.csv")
  saveRDS(d1, file = directory)

  glcp_data <- readRDS(directory)
  return(glcp_data)
}

