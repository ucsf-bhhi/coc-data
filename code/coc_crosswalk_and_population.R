library(tidyverse)
library(tidycensus)
source("code/helpers.R")

first_year <- 2012
last_year <- 2019

# grab the county population counts
county_population_data <- map_dfr(first_year:last_year, ~ fetch_acs("county", variables = c("S0101_C01_001", "S1701_C03_001"), year = .x, key = Sys.getenv("CENSUS_API_KEY"), output = "wide", survey = "acs5")) %>%
  rename(
    county_population = S0101_C01_001E,
    share_in_poverty = S1701_C03_001E
  ) %>%
  mutate(county_pop_in_poverty = county_population * (share_in_poverty / 100))

county_to_coc <- readxl::read_excel("input_data/geography/county_to_coc_crosswalk.xlsx") %>%
  # merge on the county population
  left_join(county_population_data, by = c("county_fips" = "fips")) %>%
  # pop in the CoC is the county population * % of the county pop in that CoC (from the crosswalk)
  mutate(
    county_pop_in_coc = county_population * (pct_cnty_pop_coc / 100),
    county_pov_pop_in_coc = county_pop_in_poverty * (pct_cnty_pov_coc / 100)
  ) %>%
  group_by(coc_number, year) %>%
  mutate(
    # sum the county populations in the CoC to get the CoC total population
    coc_pop = sum(county_pop_in_coc),
    coc_poverty_pop = sum(county_pov_pop_in_coc),
    # divide the CoC population from each county by the CoC total population to get the share of the CoC population from each county
    pct_coc_pop_from_county = county_pop_in_coc / coc_pop,
    pct_coc_poverty_pop_from_county = county_pop_in_poverty / coc_poverty_pop
  ) %>%
  select(year,
    county_fips,
    coc_number,
    coc_name,
    n_cocs_in_county,
    n_counties_in_coc,
    county_population,
    county_pop_in_poverty,
    county_pop_in_coc,
    county_pop_in_poverty,
    coc_pop,
    coc_poverty_pop,
    pct_coc_pop_from_county,
    pct_coc_poverty_pop_from_county,
    pct_county_pop_in_coc = pct_cnty_pop_coc,
    pct_county_poverty_pop_in_coc = pct_cnty_pov_coc
  )

write_csv(county_to_coc, "output_data/county_to_coc_crosswalk.csv")

coc_population <- county_to_coc %>%
  select(year, coc_number, coc_name, coc_pop, coc_poverty_pop) %>%
  distinct()

write_csv(coc_population, "output_data/coc_population.csv")
