library(tidyverse)
library(tidycensus)
source("code/helpers.R")

# set the years of data we're using (in line with available ACS and PIT data)
first_year <- 2012
last_year <- 2019

# set the directory to output the final csv
output_dir <- "output_data"

# function that reads in the raw FMR spreadsheets and does some light processing
parse_fmr <- function(year) {
  # read in the spreadsheet and set all columns to text
  readxl::read_excel(paste0("input_data/hud_fmr/hud_fmr_county_", year, ".xlsx"), col_types = "text") %>%
    # set all column names to lowercase
    rename_with(str_to_lower, everything()) %>%
    # add a column with the year of the data
    mutate(year = year) %>%
    # only keep certain columns from the raw data
    select(year, starts_with("fips"), starts_with("fmr"), -ends_with("chg"), -matches("fips2000"), state, county, metro_code, areaname, metro)
}

# loop through the analysis years reading in the data, processing it more, and combining into one table
fmr_data_initial <- map_dfr(first_year:last_year, parse_fmr) %>%
  mutate(
    # sometimes the fips code is called fips and sometimes fips2010 so standardize it as fips10 by using the fips2010 values and if they're missing the fips values
    fips10 = if_else(is.na(fips2010), fips, fips2010),
    # create a 5-digit version of the fips code that has just state and county
    fips5 = str_sub(fips2010, 1, 5),
    # sometimes there's an underscore in the fmr columns so standardize in the column without the underscore
    fmr0 = if_else(is.na(fmr0), fmr_0, fmr0),
    fmr1 = if_else(is.na(fmr1), fmr_1, fmr1),
    fmr2 = if_else(is.na(fmr2), fmr_2, fmr2),
    fmr3 = if_else(is.na(fmr3), fmr_3, fmr3),
    fmr4 = if_else(is.na(fmr4), fmr_4, fmr4)
  ) %>%
  # just keep around the geo ids, year, and fmr's
  select(fips10, year, fmr0, fmr1, fmr2, fmr3, fmr4, fips5, state, county, metro_code, areaname, metro) %>%
  # convert the fmr columns to numeric
  mutate(across(starts_with("fmr"), as.numeric)) %>%
  # count how many records we have per county (some counties are subdivided into multiple fmr areas, we'll deal with that later)
  group_by(year, state, county) %>%
  mutate(count = n())

# pull out the simple counties that just have one record
fmr_data_one_county <- fmr_data_initial %>%
  filter(count == 1)

# get the states for counties that have more than one record
multi_county_states <- fmr_data_initial %>%
  filter(count > 1) %>%
  # pops the column out from the data fram and stores it as a vector
  pull(state) %>%
  # we just want the unique values (ie. states)
  unique()

# the acs variable code for counts of renting households changes after 2015 so create a vector with an entry for each year's variable code
variable_code_change_year <- 2015
renter_variable_codes <- c(replicate(variable_code_change_year - first_year, "DP04_0046"), replicate(last_year - (variable_code_change_year - 1), "DP04_0047"))

# loop over the analysis years, fetching the data and combining into one table
acs_num_renters <- map2_dfr(first_year:last_year, renter_variable_codes, ~ fetch_acs("county subdivision", variables = .y, year = .x, state = multi_county_states, key = Sys.getenv("CENSUS_API_KEY"), output = "wide", survey = "acs5")) %>% 
  # get the renting households estimates into one column
  mutate(renting_households = if_else(!is.na(DP04_0046E), DP04_0046E, DP04_0047E)) %>% 
  select(fips10 = fips, year, renting_households)

# calculate a weighted average FMR for the counties in more than one FMR region
# county subdivisions are unique to an FMR area so we can take the weighted average of FMRs in the county subdivisions
fmr_data_multi_county <- fmr_data_initial %>%
  filter(count > 1) %>%
  # merge on the ACS renter household counts
  left_join(acs_num_renters, by = c("fips10", "year")) %>%
  # set any NA renter household counts to 0 so they don't affect the weighted average, but also don't cause it to be NA
  mutate(renting_households = replace_na(renting_households, 0)) %>%
  # group by county and year
  group_by(fips5, year) %>%
  # calculate the weighted average for each FMR
  summarize(across(starts_with("fmr"), ~ weighted.mean(.x, renting_households, na.rm = TRUE)))

# put the single county and multi-county data back together
fmr_full_county <- fmr_data_one_county %>%
  bind_rows(fmr_data_multi_county)

# read in the county to CoC crosswalk
county_to_coc <- read_csv("output_data/county_to_coc_crosswalk.csv")

# calculate the weighted average FMR in each CoC (weights are county share of CoC population)
coc_fmr <- county_to_coc %>%
  # merge the county to CoC crosswalk with the county FMR data
  left_join(fmr_full_county, by = c("county_fips" = "fips5", "year")) %>%
  group_by(coc_number, year) %>%
  summarise(
    # calculate the weighted average for each FMR
    across(starts_with("fmr"), ~ weighted.mean(.x, pct_coc_pop_from_county, na.rm = TRUE), .names = "avg_{.col}"),
    # calculate the share of each CoC that was missing an FMR
    across(starts_with("fmr"), ~ sum(pct_coc_pop_from_county[is.na(.x)]), .names = "{.col}_pct_coc_na_rent")
  )

# set the path for the csv
coc_fmr_csv_path <- file.path(output_dir, "coc_fmr.csv")

# write the csv
write_csv(coc_fmr, coc_fmr_csv_path)
