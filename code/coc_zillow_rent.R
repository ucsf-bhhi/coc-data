library(tidyverse)
library(tidycensus)

zillow_data_start_year <- 2014
zillow_data_end_year <- 2020
zillow_file_name <- "zillow_rent_index_sa_zip_code_2014.01_2021.03.csv"

# read in the tract to CoC crosswalk (columns with a - in col_types are ignored)
tract_to_coc <- read_csv("input_data/geography/tract_to_coc_crosswalk.csv",
  col_types = "cc-c--i-"
) %>%
  # add a leading 0 where needed to fill out the tracts FIPS code
  mutate(tract_fips = str_pad(tract_fips, 11, "left", "0")) %>%
  # calculate the share of a CoC's population coming from each tract
  group_by(coc_number) %>%
  mutate(coc_pop_share_in_tract = total_population / sum(total_population, na.rm = TRUE))

# create a table shell with a row for each distinct tract in the crosswalk for each year in the zillow data
# we'll use this as the basis for tying together the tract to CoC relationship with the zip code to tract relationship
tracts_in_cocs <- tract_to_coc %>%
  # drop any rows that don't belong to a CoC (these are mostly table notes)
  filter(!is.na(coc_number)) %>%
  # just keep the tract column
  select(tract_fips) %>%
  # just keep one row for each tract
  distinct() %>%
  # create a row for each tract for each year of the zillow data
  expand(tract_fips, year = zillow_data_start_year:zillow_data_end_year)

# read in the zillow data (it's in wide form, we'll change that)
zillow_data <- read_csv(file.path("input_data/zillow_rent_index", zillow_file_name)) %>%
  # just keep the zip code column and the columns with the rent data (they all start with 20)
  select(zip = RegionName, starts_with("20")) %>%
  # reshape the data to long form
  pivot_longer(-zip, names_to = "year_month", values_to = "rent") %>%
  # split the year and month into separate columns
  separate(year_month, c("year", "month")) %>%
  # convert the year and month columns from text to integers
  mutate(across(c(year, month), as.integer))

# the zillow data are monthly, but we need annual data so we're just taking a simple average of the months
annual_zillow_data <- zillow_data %>%
  # take the average monthly rent in each zip code/year combo and ignore NA's while doing so
  group_by(zip, year) %>%
  summarize(annual_mean_rent = mean(rent, na.rm = TRUE))

# read in the tract to zip crosswalk so we can convert the zip code level rent data to tract level
tract_to_zip <- readxl::read_excel("input_data/geography/usps_tract_to_zip_crosswalk_2017.03.xlsx", col_types = c("text", "text", "numeric", "skip", "skip", "skip")) %>%
  rename_with(str_to_lower, everything())

# convert the zillow data to tract level
# start with the shell table that has a row for each tract/year that's in a CoC
tract_rent <- tracts_in_cocs %>%
  # merge on the zip code crosswalk to give us the share of each tract coming from its zip codes
  left_join(tract_to_zip, by = c("tract_fips" = "tract")) %>%
  # merge on the rent data
  left_join(annual_zillow_data, by = c("zip", "year")) %>%
  group_by(tract_fips, year) %>%
  summarise(
    # take the weighted average of the rent in the tract (weights are the share of residences in a tract coming from each zip code)
    tract_rent = weighted.mean(annual_mean_rent, res_ratio, na.rm = TRUE),
    # add up the share of the tract that we don't have rent data for
    tract_share_na_rent = sum(res_ratio[is.na(annual_mean_rent)])
  ) %>%
  # some tracts don't have full zip to tract data from the crosswalk which can mess up this share of tract with na rent calculation. if the tract rent value is NA, then the whole tract was missing rent so force the share to be 1
  mutate(tract_share_na_rent = if_else(is.na(tract_rent), 1, tract_share_na_rent))

# convert the tract level rent data to CoC level
# start with the tract to CoC crosswalk
coc_zillow_rent <- tract_to_coc %>%
  # merge on the tract level rent data
  left_join(tract_rent, by = "tract_fips") %>%
  group_by(coc_number, year) %>%
  summarise(
    # take the weighted average rent in each CoC (weights are share of the CoC's population coming from the tracts)
    coc_rent_zillow = weighted.mean(tract_rent, coc_pop_share_in_tract, na.rm = TRUE),
    # add up the share of the CoC (based on population) coming from tract's that don't have rent data
    coc_share_na_rent_zillow = sum(coc_pop_share_in_tract * tract_share_na_rent, na.rm = TRUE)
  )

# output the final CoC level dataset
write_csv(coc_zillow_rent, "output_data/coc_zillow_rent.csv")
