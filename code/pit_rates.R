#' Calculate homelessness rates by CoC
#'
#' @param pit_data A data frame with PiT counts in long form from
#'   [get_long_pit_data()].
#' @param coc_populations A data frame with CoC overall populations and
#'   populations in poverty from [build_coc_populations()].
#'
#' @return A data frame with homelessness counts and rates: 
#' * `coc_number`: CoC number (character) 
#' * `year`: Year (numeric) 
#' * `overall_homeless`: Count of all unhoused people in the CoC (numeric) 
#' * `homeless_rate_total_pop`: Share of total CoC population that is unhoused
#'      (numeric) 
#' * `homeless_rate_in_poverty`: Total unhoused count divided by population
#'      below poverty line (numeric)
#' * `homeless_per_1000_total_pop`: Number of unhoused people per 1,000 people
#'      in the overall population (numeric)
#' * `homeless_per_1000_in_poverty`: Number of unhoused people per 1,000 people
#'      below the poverty line (numeric)
build_pit_rates <- function(pit_data, coc_populations) {
  get_overall_pit_counts(pit_data) %>%
    make_pit_rates(coc_populations)
}

get_overall_pit_counts <- function(pit_data) {
  pit_data %>%
    filter(category == "Overall Homeless") %>%
    select(coc_number, year, overall_homeless = count)
}

make_pit_rates <- function(pit_data, coc_populations) {
  # start with the CoC population data
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
    select(coc_number, year, overall_homeless, homeless_rate_total_pop, homeless_rate_in_poverty, homeless_per_1000_total_pop, homeless_per_1000_in_poverty)
}
