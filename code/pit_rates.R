library(tidyverse)
source("code/helpers.R")

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
  select(coc_number, year, overall_homeless = count)

# if the augmented county to coc crosswalk doesn't exist, run the code to produce it
if (!file.exists("output_data/coc_population.csv")) {
  source("code/coc_population.R")
}
# read in the CoC population data
coc_population <- read_csv("output_data/coc_population.csv")

# combine the data and calculate homelessness rates
combined_data <- coc_population %>%
  # join on the homelessness counts from the PIT data
  left_join(pit_overall, by = c("coc_number", "year")) %>%
  # calculate the homelessness rate in the total population and population in poverty
  mutate(
    homeless_rate_total_pop = overall_homeless / coc_pop,
    homeless_rate_in_poverty = overall_homeless / coc_poverty_pop,
    # also calculate the number of people experiencing homelessness per 1000 people in the total population and population in poverty
    homeless_per_1000_total_pop = homeless_rate_total_pop * 1000,
    homeless_per_1000_in_poverty = homeless_rate_in_poverty * 1000
  ) %>%
  select(coc_number, coc_name, year, overall_homeless, homeless_rate_total_pop, homeless_rate_in_poverty, homeless_per_1000_total_pop, homeless_per_1000_in_poverty)

# set homelessness rates output file name
output_name <- paste0("pit_homelessness_rates_", acs_start_year, "_", acs_end_year, ".csv")
# set the output file path
output_path <- file.path(output_dir, output_name)

# write csv
write_csv(combined_data, output_path)
