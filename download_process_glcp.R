### PACKAGE DOWNLOAD FOR GLCP ###

install.packages("dplyr")
install.packages("vroom")
install.packages("doParallel")
install.packages("foreach")
install.packages("iterators")
install.packages("parallel")

library(dplyr)
library(vroom)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)

directory <- "./data/glcp_condensed.rds"
d1 <- vroom::vroom("./data/glcp_hydro_and_climate_pop_subset_all_vars.csv")
saveRDS(d1, file = directory)
