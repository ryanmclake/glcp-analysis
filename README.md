# GLCP-analysis

-----

:busts_in_silhouette: Ryan McClure, Alli Cramer, Steve Katz

:busts_in_silhouette: Special thanks to: Michael Meyer, Matt Brousil, and Stephanie Hampton

Questions?  :email: ryan.mcclure@wsu.edu

## Motivation

Thank you for checking out the GLCP analysis workflow. Lakes globally are both increasing and decreasing in size as a result of changing climate and human pressure. However, both the magnitude and spatial variation of how waterbody area is changing has been restricted to regional assesments and has not yet been quantified for lakes globally.

We developed a workflow that analyzes the magnitude and direction of lake area change for 1.4+ million lakes globally. We then isolated lakes that were increasing and decreasing in area across four WWF ecoregions and then quantified the importance of drivers. This is a first globally-scaled attempt to partition how lake area is chaning globally and to isolate the most important predictors of that change. 

### There are 4 major scripts currently in production. 

1. <i>Q1.1_calc_hylak_kendall_tau.R</i>
--> Calcualtes the kendall tau using the permanent_km2 variable. All taus == 1 are flagged to be removed from from dataset

2. <i>Q1.2_hylak_kendall_sens_slope.R</i>
--> Filters the lakes with taus < 1 and then calcualtes the sens slope using the trend package (Currently only does this for lake area now).

3. <i>Q2.1_hylak_predict_swell.R</i>   !! STILL IN PRODUCTION !!
--> Filter lakes with tau < 1 and reservoirs. Then use a simple first order model to determine the drivers.  

4. <i>Q2.2_hylak_predict_shrink.R</i>  !! STILL IN PRODUCTION !!
--> Filter lakes with tau < 1 and reservoirs. Then use a simple first order model to determine the drivers.  

1 and 2 are both completed and work. 3 and 4 are both still in development. 

The workflow in kamiak runs through each country. The directory to run these scripts in kamiak is:

<i>cd /data/lab/katz/projects/glcp-analysis/</i>
