s = Sys.time()

# Pull in packages needed
library(arrow, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(doParallel, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(trend, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

# Identify the permanent_km2 values that give a tau of 1 to remove as flags from the data
kendall_tau_ones <-
  vroom::vroom("./output/slopes/hylak_id_kendall_tau.csv", delim = " ", col_names = T) %>%
  mutate_all(funs(as.numeric(.))) %>%
  filter(!(hylak_id %% 1)) %>%
  na.omit(.) %>%
  filter(kendall_tau == 1.000000000000)
kendall_tau_ones <- c(unique(kendall_tau_ones$hylak_id))
`%!in%` <- Negate(`%in%`)

# Specify the list of countries.csv that will be run
# Note - Canada is not in this list because it is so big it exhausts the Katz partition memory
country <- list.files(path = "./data/countries")
country <- gsub("\\..*", "", country)

# Function that runs through each country.csv file and performs the tasks described

analysis_function <- function(x){

  system.time(read_csv_arrow(                                                   # Read data in using arrow. It is the fastest so far
    paste0("./data/countries/",x,".csv"),
    quote = "\"",
    escape_double = TRUE,
    escape_backslash = FALSE,
    schema = NULL,
    col_names = F,
    col_types = NULL,
    col_select = NULL,
    na = c("", "NA"),
    quoted_na = TRUE,
    skip_empty_rows = TRUE,
    skip = 0L,
    parse_options = NULL,
    convert_options = NULL,
    read_options = NULL,
    as_data_frame = TRUE,
    timestamp_parsers = NULL) %>% rename(year = f0,                             # Pull in data and rename it for ONLY columns we need
                                         hylak_id=f2,
                                         centr_lat=f3,
                                         centr_lon=f4,
                                         pop_sum=f12,
                                         total_km2=f15,
                                         wshd_area=f27) %>%
      filter(hylak_id %!in% kendall_tau_ones) %>%                               # Remove all of the lakes with a Kendall tau == 1
      group_by(year, hylak_id, centr_lat, centr_lon) %>%                        # Group by year to aggregate to annual lake area
      summarize(total_km2 = median(total_km2, na.rm = T),                       # Summarize the median total lake area and population
                pop_sum = median(pop_sum, na.rm = T),
                lake_area = median(lake_area, na.rm = T),
                wshd_area = median(wshd_area, na.rm = T)) %>%
      ungroup(.) %>%
      group_by(hylak_id) %>%
      mutate_at(vars(pop_sum),funs(imputeTS::na_interpolation(., option = "linear"))) %>% # Fill in the missing population data
      mutate(pop_diff = last(pop_sum) - first(pop_sum),                         # calculate population diff, lake area to wshd area ratio
             ratio = lake_area/wshd_area) %>%
      select(-lake_area, -wshd_area, -pop_sum, -year) %>%
      mutate(across(c(3), ~ scale(.))) %>%                                      # z-score the total_km2 variable
      mutate(across(c(3), ~ if_else(is.na(.),0,.))) %>%                         # make all NA values 0
      na.omit(.) %>%
      ungroup(.) %>%
      mutate(q95 = quantile(ratio, 0.95),                                       # Quantify upper and lower quantile for ratio and remove them
             q05 = quantile(ratio, 0.05)) %>%
      filter(ratio <= q95) %>%
      filter(ratio >= q05) %>%
      select(-q95, -q05, -ratio) %>%
      group_by(hylak_id, centr_lat, centr_lon, pop_diff) %>%                    # Group by hylak_id and include location and pop_change
      summarise(across(c(1),  ~list(sens.slope(.) %>%                           # calculate sens slope from trend package
                                    tidy %>%
                                    select(p.value, conf.low, conf.high)))) %>%
      ungroup(.) %>%
      unnest(c(5), names_repair = "minimal") %>%
      mutate(sens.slope = (conf.low+conf.high)/2) %>%                           # Manually kludge the sens.slope (! Need to find a better approach)
      collect() %>%                                                             # collect() is used to speed up with the arrow process
      write.table(., file = paste0("./output/hylak_id_kendall_sens.csv"),       # write to file
                  append = T,
                  row.names = F,
                  col.names = !file.exists("./output/hylak_id_kendall_sens.csv")))
}

# Run each country.csv in parallel based on # of CPUs requested (I can successfully get 20 from kamiak)
no_cores <- detectCores()
cl <- makeCluster(no_cores, type="FORK")
registerDoParallel(cl)
foreach(i=country) %dopar% analysis_function(i)

# Calculate how long the entire run was
e <- Sys.time()
t=e-s
print(t)
