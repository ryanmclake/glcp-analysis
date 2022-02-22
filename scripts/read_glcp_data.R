### CONDENSE THE GLCP DATASET ###
source("./scripts/functions/condense_glcp.R")
directory = "./data/glcp_condensed.rds"

data <- condense_glcp(directory = directory)
