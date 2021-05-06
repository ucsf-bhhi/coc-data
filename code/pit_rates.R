library(tidyverse)
library(tidycensus)

acs_start_year <- 2012
acs_end_year <- 2019

input_dir <- "input_data"
output_dir <- "output_data"


# read in the processed PIT data if the object doesn't already exist in the environment
if (!exists("pit_data")) {
  pit_data <- read_csv(file.path(output_dir, "pit_processed_2007_2019.csv"))
}

# just grab the overall homelessness count
pit_overall <- pit_data %>%
  filter(category == "Overall Homeless") %>%
  select(coc_number, coc_name, year, overall_homeless = count)

# read in the CoC to county crosswalk
coc_county_crosswalk <- readxl::read_excel(file.path(input_dir, "geography", "county_to_coc_crosswalk.xlsx"))

# fetch the ACS data by county for total population, total population for which poverty status is determined, total poverty population, share of population for which poverty status is determined that is in poverty
acs_data <- map_dfr(acs_start_year:acs_end_year, function(x) {
  # hit the census API for the data
  get_acs("county", variables = c("S0101_C01_001E", "S1701_C01_001E", "S1701_C02_001E", "S1701_C03_001E"), year = x, key = Sys.getenv("CENSUS_API_KEY"), output = "wide", survey = "acs5") %>%
    select(fips = GEOID, name = NAME, total_population = S0101_C01_001E, total_pop_with_pov_status = S1701_C01_001E, total_pop_in_poverty = S1701_C02_001E, share_in_poverty = S1701_C03_001E) %>%
    # add a year variable
    mutate(year = x)
})

# combine the data and calculate homelessness rates
combined_data <- coc_county_crosswalk %>%
  # join the ACS data onto the crosswalk by county
  left_join(acs_data, by = c("county_fips" = "fips")) %>%
  # calculate the total population and population in poverty for each CoC by County pair
  mutate(
    total_pop_in_coc = total_population * pct_cnty_pop_coc / 100,
    pov_pop_in_coc = total_pop_in_poverty * pct_cnty_pov_coc / 100
  ) %>%
  # group by CoC and year and then sum the populations across the counties in the CoC
  group_by(coc_number, year) %>%
  summarise(
    coc_total_population = sum(total_pop_in_coc),
    coc_poverty_population = sum(pov_pop_in_coc)
  ) %>%
  # join on the homelessness counts from the PIT data
  left_join(pit_overall, by = c("coc_number", "year")) %>%
  # calculate the homelessness rate in the total population and population in poverty
  mutate(
    homeless_rate_total_pop = overall_homeless / coc_total_population,
    homeless_rate_in_poverty = overall_homeless / coc_poverty_population,
    # also calculate the number of people experiencing homelessness per 1000 people in the total population and population in poverty
    homeless_per_1000_total_pop = homeless_rate_total_pop * 1000,
    homeless_per_1000_in_poverty = homeless_rate_in_poverty * 1000
  ) %>%
  # move around the variables to make the dataset easier to look at
  relocate(coc_name, .after = coc_number) %>%
  relocate(c(homeless_per_1000_total_pop, homeless_per_1000_in_poverty, homeless_rate_total_pop, homeless_rate_in_poverty), .after = year) %>%
  relocate(overall_homeless, .before = coc_total_population)

# set homelessness rates output file name
output_name <- paste0("pit_homelessness_rates_", acs_start_year, "_", acs_end_year, ".csv")
# set the output file path
output_path <- file.path(output_dir, output_name)

# write csv
write_csv(combined_data, output_path)
