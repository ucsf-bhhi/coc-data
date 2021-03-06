#' @describeIn build_coc_renter_shares Calculates the share of renters in each county
#'
#' @param year
#' 
#' @return 
build_county_renter_share = function(year) {
  # hit the census api for total household and renter household counts
  fetch_acs("county", variables = c(total_households = "B25003_001", renting_households = "B25003_003"), year = year, key = Sys.getenv("CENSUS_API_KEY"), output = "wide", survey = "acs5") %>%
    # calculate the share of households who are renting
    mutate(share_renters = renting_households / total_households) %>%
    select(county_fips = fips, year, share_renters)
}


#' Calculates the share of renters in each CoC
#'
#' @param renter_shares A data frame with county renter shares
#' @param crosswalk A data frame with a county to CoC crosswalk
#'
#' @return A data frame with CoC renter shares.
build_coc_renter_shares = function(renter_shares, crosswalk) {
  crosswalk %>%
  # merge the county renter data onto the crosswalk
  left_join(renter_shares, by = c("county_fips", "year")) %>%
  group_by(coc_number, year) %>%
  # take the weighted average renter share weighting by share of the CoC population coming from each county
  summarise(avg_renter_share = weighted.mean(share_renters, pct_coc_pop_from_county, na.rm = TRUE))
}

#' CoC-level shares of rent burdened households
#'
#' Builds shares of renter households who pay more than 30 or 50 percent of
#' their income in rent and the average median rent burden
#' (rent / household income) in the CoC.
#'
#' @param yr A numeric with the year of the data.
#' @param tract_crosswalk A data frame with a census tract to CoC crosswalk from
#'   [build_tract_crosswalk()].
#'
#' @return A data frame:
#' * `coc_number`: CoC number (character)
#' * `year`: Year (numeric)
#' * `share_rent_over_30_pct_inc`: Share of renter households paying more than
#'      30 percent of their income in rent (numeric)
#' * `share_rent_over_50_pct_inc`: Share of renter households paying more than
#'      50 percent of their income in rent (numeric)
#' * `median_rent_burden`: Median rent share of income in the CoC (numeric)
build_coc_rent_burden <- function(year, tract_crosswalk) {
  acs_variables <- c(
    "total" = "B25070_001",
    "count_30_35" = "B25070_007",
    "count_35_40" = "B25070_008",
    "count_40_50" = "B25070_009",
    "count_50_plus" = "B25070_010",
    "not_computed" = "B25070_011",
    "median_rent_burden" = "B25071_001"
  )
  
  fetch_acs_tracts(year, variables = acs_variables, output = "wide") %>%
    mutate(
      year = year,
      total_computed = total - not_computed,
      count_30_plus = count_30_35 + count_35_40 + count_40_50 + count_50_plus,
      median_rent_burden = median_rent_burden / 100,
    ) %>%
    select(year, tract_fips = fips, count_30_plus, count_50_plus, total_computed, median_rent_burden) %>% 
    make_coc_rent_burden(tract_crosswalk, year)
}

#' Construct CoC-level shares of rent burdened households
#'
#' Builds shares of renter households who pay more than 30 or 50 percent of
#' their income in rent and the average median rent burden
#' (rent / household income) in the CoC.
#'
#' @param tract_rent_data A data frame of tract level counts of rent burdened
#'   households created by [get_county_rent_burdened_count()].
#' @param tract_crosswalk A data frame with a tract to CoC crosswalk created
#'   by [build_tract_crosswalk()].
#'
#' @return A data frame:
#' * `year`: Year (numeric)
#' * `coc_number`: CoC number (character)
#' * `share_rent_over_30_pct_inc`: Share of renter households paying more than
#'      30 percent of their income in rent (numeric)
#' * `share_rent_over_50_pct_inc`: Share of renter households paying more than
#'      50 percent of their income in rent (numeric)
#' * `median_rent_burden`: Median rent share of income in the CoC (numeric)
#' @seealso [build_coc_rent_burden()] for the main function
#' @keywords internal
make_coc_rent_burden <- function(tract_rent_data, tract_crosswalk, yr) {
  tract_crosswalk %>%
    filter(year == yr, !is.na(coc_number)) %>% 
    left_join(tract_rent_data, by = c("year", "tract_fips")) %>%
    group_by(year, coc_number) %>%
    summarise(
      across(
        c(count_30_plus, count_50_plus, total_computed),
        sum, na.rm = TRUE
      ),
      median_rent_burden = weighted.mean(
        median_rent_burden,
        pct_coc_pop_from_tract,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    mutate(
      share_rent_over_30_pct_inc = count_30_plus / total_computed,
      share_rent_over_50_pct_inc = count_50_plus / total_computed
    ) %>%
    select(
      year,
      coc_number,
      share_rent_over_30_pct_inc,
      share_rent_over_50_pct_inc,
      median_rent_burden
    )
}

#' CoC housing vacancy rents
#'
#' Builds gross and rental vacancy rates.
#' 
#' # Definitions
#' Gross vacancy rate = all unoccupied housing units / all housing units
#' 
#' Rental vacancy rate = unoccupied rental units that are for rent /
#'   (occupied rental units + unoccupied rental units that are for rent +
#'   rented units that are unoccupied)
#'
#' @param year A numeric with the year of the data.
#' @param tract_crosswalk A data frame with a census tract to CoC crosswalk from
#'   [build_tract_crosswalk()].
#'
#' @return A data frame with the rental vacancy rates:
#' * `coc_number`: CoC number (character)
#' * `year`: Year (numeric)
#' * `gross_vacancy_rate`: Share of all housing units that are unoccupied
#'      (numeric)
#' * `rental_vacancy_rate`: Share of rental housing units not rented (numeric)
build_coc_vacancy_rates <- function(year, tract_crosswalk) {
  fetch_acs_tracts(
    year,
    variables = c(
      "total_housing_units" = "B25002_001",
      "vacant_housing_units" = "B25002_003",
      "occupied_rental_units" = "B25003_003",
      "for_rent" = "B25004_002",
      "rented_not_occupied" = "B25004_003"
    ),
    output = "wide"
  ) %>%
    make_coc_vacancy_rates(year, tract_crosswalk)
}

#' Constructs the CoC vacancy rates
#'
#' @param acs_data A data frame with tract vacancy data from
#'   [fetch_acs_vacancy_data()]
#' @inheritParams build_coc_vacancy_rates
#'
#' @keywords internal
#' @seealso [build_coc_vacancy_rates()]
make_coc_vacancy_rates <- function(acs_data, yr, tract_crosswalk) {
  tract_crosswalk %>%
    filter(year == yr, !is.na(coc_number)) %>%
    # grab the ACS data and join it to the crosswalk
    left_join(
      acs_data,
      by = c("tract_fips" = "fips", "year")
    ) %>%
    # add year to the grouping to make sure it's in the output
    group_by(coc_number, year) %>%
    summarise(
      across(
        c(
          total_housing_units,
          vacant_housing_units,
          occupied_rental_units,
          for_rent,
          rented_not_occupied
        ),
        sum, na.rm = TRUE
      ),
      gross_vacancy_rate = vacant_housing_units / total_housing_units,
      total_rental_units = occupied_rental_units + for_rent + rented_not_occupied,
      rental_vacancy_rate = for_rent / total_rental_units,
      .groups = "drop"
    ) %>% 
    select(coc_number, year, gross_vacancy_rate, rental_vacancy_rate)
}

#' Creates eviction data
#'
#' Builds counts and rates of eviction and eviction court case filing at the CoC level.
#' The rates are the number of evictions/court cases divided by the number of renting
#' households in the CoC.
#'
#' @param evictions A data frame with county-level eviction data.
#' @param county_crosswalk A data frame with the county to CoC crosswalk.
#'
#' @return A data frame:
#' * `coc_number`: CoC number
#' * `year`: Year
#' * `eviction_filings`: Number of eviction court cases filed
#' * `evictions`: Number of evictions
#' * `eviction_filing_rate`: Number of eviction court cases filed / renting households
#' * `eviction_rate`: Number of evictions / renting households
#' * `missing_evictions_rate`: Share of the CoC's renting households in counties with missing eviction data
build_coc_evictions <- function(evictions, county_crosswalk) {
  county_crosswalk %>%
    left_join(evictions, by = c("county_fips" = "GEOID", "year")) %>%
    rename(eviction_filings = eviction.filings) %>%
    group_by(coc_number, year) %>%
    summarise(
      missing_evictions_rate = sum(pct_coc_renting_hh_from_county[is.na(evictions)]),
      coc_renting_hh_with_evictions_data = coc_renting_hh * (1 - missing_evictions_rate),
      across(
        c(eviction_filings, evictions),
        ~ sum(.x * pct_county_renting_hh_in_coc, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    distinct() %>%
    mutate(
      across(
        c(eviction_filings, evictions),
        ~ .x / coc_renting_hh_with_evictions_data,
        # chop off the final s from eviction_filings and
        # evictions when naming the rate variables
        .names = "{str_sub({.col}, 1, -2)}_rate"
      )
    ) %>%
    select(-coc_renting_hh_with_evictions_data)
}
