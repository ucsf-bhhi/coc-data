library(tidyverse)
library(tidycensus)
source("code/helpers.R")

first_year <- 2012
last_year <- 2019
county_to_coc_crosswalk <- "output_data/county_to_coc_crosswalk.csv"

if (!file.exists(county_to_coc_crosswalk)) {
  source("code/coc_population.R")
}
county_to_coc <- read_csv(county_to_coc_crosswalk)

# loop over the analysis years, fetching the data and combining into one table
acs_share_renters <- map_dfr(first_year:last_year, function(yr) {
  fetch_acs("county", variables = c(total_households = "B25002_001", renting_households = "B25002_003"), year = yr, key = Sys.getenv("CENSUS_API_KEY"), output = "wide", survey = "acs5")
}) %>%
  # calculate the share of households who are renting
  mutate(share_renters = renting_households / total_households) %>%
  select(fips, year, share_renters)

# convert the county renter shares to CoC renter shares
coc_renter_share <- county_to_coc %>%
  # merge the county renter data onto the crosswalk
  left_join(acs_share_renters, by = c("county_fips" = "fips", "year")) %>%
  group_by(coc_number, year) %>%
  # take the weighted average renter share weighting by share of the CoC population coming from each county
  summarise(avg_renter_share = weighted.mean(share_renters, pct_coc_pop_from_county, na.rm = TRUE))

# write out the data
output_file_name <- "output_data/coc_renter_share.csv"
write_csv(coc_renter_share, output_file_name)
