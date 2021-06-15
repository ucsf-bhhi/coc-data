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

#' County-level counts of rent-burdened households
#'
#' Gets ACS data that has the counts of households who pay more than 30 or 50
#' percent of their income in rent.
#'
#' @param year A numeric with the year of ACS data to fetch.
#'
#' @return A data frame:
#' * `year`: Year (numeric)
#' * `county_fips`: County FIPS code (character)
#' * `count_30_plus`: Count of renting households paying more than 30% of their
#'      income in rent (numeric)
#' * `count_50_plus`: Count of renting households paying more than 50% of their
#'      income in rent (numeric)
#' * `total_computed`: Count of renting households that have a calculated rent
#'      share of income (numeric)
#' * `median_rent_burden`: Median rent share of income in the county (numeric)
#' @seealso [build_coc_rent_burdened_share()] for CoC-level rent burdened shares
get_county_rent_burdened_count <- function(year) {
  acs_variables <- c(
    "total" = "B25070_001",
    "count_30_35" = "B25070_007",
    "count_35_40" = "B25070_008",
    "count_40_50" = "B25070_009",
    "count_50_plus" = "B25070_010",
    "not_computed" = "B25070_011",
    "median_rent_burden" = "B25071_001"
  )
  fetch_acs("county", year = year, variables = acs_variables, output = "wide") %>%
    mutate(
      year = year,
      total_computed = total - not_computed,
      count_30_plus = count_30_35 + count_35_40 + count_40_50 + count_50_plus,
      median_rent_burden = median_rent_burden / 100,
    ) %>%
    select(
      year,
      county_fips = fips,
      count_30_plus,
      count_50_plus,
      total_computed,
      median_rent_burden
    )
}

#' CoC-level shares of rent burdened households
#'
#' Builds shares of renter households who pay more than 30 or 50 percent of
#' their income in rent.
#'
#' @param county_rent_data A data frame of county level counts of rent burdened
#'   households created by [get_county_rent_burdened_count()].
#' @param county_crosswalk A data frame with a county to CoC crosswalk created
#'   by [build_county_crosswalk()].
#'
#' @return A data frame:
#' * `year`: Year (numeric)
#' * `coc_number`: CoC number (character)
#' * `share_rent_over_30_pct_inc`: Share of renter households paying more than
#'      30 percent of their income in rent (numeric)
#' * `share_rent_over_50_pct_inc`: Share of renter households paying more than
#'      50 percent of their income in rent (numeric)
#' * `median_rent_burden`: Median rent share of income in the CoC (numeric)
#' @seealso [get_county_rent_burdened_count()] for fetching the ACS county-level
#'   rent burdened counts
build_coc_rent_burdened_share <- function(county_rent_data, county_crosswalk) {
  county_crosswalk %>%
    left_join(county_rent_data, by = c("year", "county_fips")) %>%
    group_by(year, coc_number) %>%
    summarise(
      across(
        c(count_30_plus, count_50_plus, total_computed),
        sum, na.rm = TRUE
      ),
      median_rent_burden = weighted.mean(
        median_rent_burden,
        pct_coc_pop_from_county,
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

build_coc_vacancy_rate <- function(yr, county_crosswalk) {
  fetch_acs_rental_vacancy_rate(yr) %>%
    make_coc_rental_vacancy_rate(yr, county_crosswalk)
}

fetch_acs_rental_vacancy_rate <- function(yr) {
  fetch_acs(
    "county",
    year = yr,
    variables = c("rental_vacancy_rate" = "DP04_0005"),
    output = "wide"
  ) %>%
    mutate(rental_vacancy_rate = rental_vacancy_rate / 100)
}

make_coc_rental_vacancy_rate <- function(acs_data, yr, county_crosswalk) {
  county_crosswalk %>%
    filter(year == yr) %>%
    # grab the ACS data and join it to the crosswalk
    left_join(
      acs_data,
      by = c("county_fips" = "fips", "year")
    ) %>%
    # add year to the grouping to make sure it's in the output
    group_by(coc_number, year) %>%
    summarise(
      rental_vacancy_rate = weighted.mean(
        rental_vacancy_rate, pct_coc_pop_from_county,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}
