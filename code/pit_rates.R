get_overall_pit_counts <- function(pit_data) {
  pit_data %>%
    filter(category == "Overall Homeless") %>%
    select(coc_number, year, overall_homeless = count)
}

make_pit_rates <- function(pit_data, coc_populations) {
  coc_populations %>%
    # join on the homelessness counts from the PIT data
    left_join(pit_data, by = c("coc_number", "year")) %>%
    # calculate the homelessness rate in the total population and population in poverty
    mutate(
      homeless_rate_total_pop = overall_homeless / coc_pop,
      homeless_rate_in_poverty = overall_homeless / coc_poverty_pop,
      # also calculate the number of people experiencing homelessness per 1000 people in the total population and population in poverty
      homeless_per_1000_total_pop = homeless_rate_total_pop * 1000,
      homeless_per_1000_in_poverty = homeless_rate_in_poverty * 1000
    ) %>%
    select(coc_number, coc_name, year, overall_homeless, homeless_rate_total_pop, homeless_rate_in_poverty, homeless_per_1000_total_pop, homeless_per_1000_in_poverty)
}

build_pit_rates <- function(pit_data, coc_populations) {
  get_overall_pit_counts(pit_data) %>%
    make_pit_rates(coc_populations)
}
