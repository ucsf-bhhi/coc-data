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

# the acs variable code for counts of renting households changes after 2015
variable_code_change_year <- 2015
vars_pre <- c("DP04_0044", "DP04_0046")
vars_post <- c("DP04_0045", "DP04_0047")

# loop over the analysis years, fetching the data and combining into one table
acs_share_renters <- map_dfr(first_year:last_year, function(yr) {
  # if the year in the loop is before 2015 use variable codes DP04_0044 & DP04_0046, else use DP04_0045 & DP04_0047
  vars <- if (yr < variable_code_change_year) vars_pre else vars_post
  fetch_acs("county", variables = vars, year = yr, key = Sys.getenv("CENSUS_API_KEY"), output = "wide", survey = "acs5")
}) %>%
  # get the renting households estimates into one column
  mutate(
    total_households = if_else(!is.na(DP04_0044E), DP04_0044E, DP04_0045E),
    renting_households = if_else(!is.na(DP04_0046E), DP04_0046E, DP04_0047E)
  ) %>%
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
