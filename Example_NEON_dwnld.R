install.packages("neonUtilities") # download the NEON package

library(neonUtilities) # load the package

byFileAOP(dpID = "DP3.30024.001", # specify the data products to download you can also concatenate e.g., dpID = c("","","")
          site="BART",            # specify the site to download
          year="2019",            # specify the year to download
          check.size = F,         # TRUE or FALSE to output the size of the DP downloaded. I set to FALSE :)
          savepath="working directoy I want my NEON data to be saved to") # make sure it has a lot of space!

# example savepaths for mac "/Users/ryanmcclure/Documents/"
# example savepaths for windows "C:/Users/ryan.mcclure/Documents/"
# if you're doing this in linux you understand directories ;)

# use this link: https://data.neonscience.org/data-products/explore
# to explore different sites and data products related to NEON's remote sensing data
