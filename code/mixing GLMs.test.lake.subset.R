setwd("C:/Users/Katz/Dropbox/Global Lakes")

getwd()

library("flexmix")
library("imputeTS")

#  Import test data of made up lake data
# There are three groups of 4, 4 & 3 for a total of 13 lakes.
# In each case there is a variable called Area and then Var1, Var2 and so on.

NLake.LN <- read.csv("Lake.Area.Truth.LN.csv")

head(NLake.LN)


# Now do for a Gaussian distribution of errors

l2 <- flexmix(Area ~ Var1 + Var2 + Var3 + Var4 + Var5, 
		  data = NLake.LN, 
		  k = 3,
		  model = FLXMRglm(family = "gaussian"))

summary(l2)
plot(l2)

parameters(l2, component = 1)

parameters(l2, component = 2)

parameters(l2, component = 3)


table(NLake.LN$Class, clusters(l2))

# Now check if the coefficients are significant

rl2 <- refit(l2)

summary(rl2)


#  Things work OK - the call correctly identifies 3 classes, and put about the 
# and puts things in the right group.


# Now import data from subset of lakes
# 10 lakes of unknown classification

Lake.SS <- read.csv("glcp_analysis_tiny.csv")

head(Lake.SS, 20)

attach(Lake.SS)

plot(pop_sum~year)

# Now interpolate the population as a linear fit between censuses.

Lake.SS$pop.adj <- na_interpolation(pop_sum, option = "linear")

head(Lake.SS, 10)		# Make sure it added a column that is the interpolated data

# Now check in the interpolated data looks correct.

plot(pop.adj~year, pch=19, col="black")	# The real data in black
points(pop_sum~year, pch=19, col="green")	# The interpolated data in green


d1 <- vroom::vroom("./output/subset_GLCP_for_model_testing_40_lakes.csv")

# Now fit the lakes to clusters
d2 <- d1 %>% select(total_km2, total_precip_mm, mean_annual_temp_k, pop_sum, biome_type) %>%
  na.omit(.)

attach(d2)
l3 <- flexmix(total_km2 ~ total_precip_mm + mean_annual_temp_k + pop_sum, 
		  data = d2, 
		  k = 3,
		  model = FLXMRglm(family = "gaussian"))

summary(l3)

plot(l3)

parameters(l3, component = 1)

parameters(l3, component = 2)

parameters(l3, component = 3)

table(clusters(l3))

table(d2$biome_type, clusters(l3))

rl3 <- refit(l3)

summary(rl3)

#  WTF?!?!?!  Now it thinks that there are 159 clusters - for only 10 lakes.  It thinks 
# all the data points are independent and not data in a glm.  
#  How did it get it right in the made up data and not with the real data?

# I even tried to do it with fewer columns in case flexmix was having trouble 
# identifying the name column.  It didn't change anything.  The package still think that
# each data point is its own lake.

new.dat <- Lake.SS[c('hylak_id', 'year', 'permanent_km2', 'total_precip_mm', 'mean_annual_temp_k', 'pop.adj')]
new.dat$hylak_id <- as.character(new.dat$hylak_id)

head(new.dat)
attach(new.dat)
str(new.dat)

l4 <- flexmix(permanent_km2 ~ total_precip_mm + mean_annual_temp_k + pop.adj, 
		  data = new.dat, 
		  k = 3,
		  model = FLXMRglm(family = "gaussian"))

summary(l4)

plot(l4)



