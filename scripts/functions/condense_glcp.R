
condense_glcp <- function(directory) {
  d1 <- vroom::vroom("./data/glcp_extended.csv")
  saveRDS(d1, file = directory)

  glcp_data <- readRDS(directory)
  return(glcp_data)
}

