library(flexmix)
library(tidyverse)
library(vroom)

# Upload new DF
d1 <- vroom::vroom("./output/subset_GLCP_for_model_testing_40_lakes.csv")

# Get varaibles of interest and remove spurious NA's Why is there still one? -RPM
d2 <- d1 %>% select(hylak_id, total_km2, total_precip_mm, mean_annual_temp_k, pop_sum, biome_type) %>%
  na.omit(.) %>%
  mutate(hylak_id = as.character(hylak_id))

# Set up DF to run in Flexmix
attach(d2)
l3 <- flexmix(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, 
              data = d2, 
              k = 3,
              model = FLXMRglm(family = "gaussian"))

# Summary report -->
summary(l3)
plot(l3)
parameters(l3, component = 1)
parameters(l3, component = 2)
parameters(l3, component = 3)
table(clusters(l3))
table(d2$biome_type, clusters(l3))

# refit the model
rl3 <- refit(l3)
summary(rl3)