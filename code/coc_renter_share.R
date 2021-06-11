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

get_county_rent_burdened_count = function(year) {
  acs_variables = c(
    "total" = "B25070_001",
    "count_30_35" = "B25070_007",
    "count_35_40" = "B25070_008",
    "count_40_50" = "B25070_009",
    "count_50_plus" = "B25070_010",
    "not_computed" = "B25070_011"
  )
  fetch_acs("county", year = year, variables = acs_variables, output = "wide") %>%
    mutate(
      year = year,
      total_computed = total - not_computed,
      count_30_plus = count_30_35 + count_35_40 + count_40_50 + count_50_plus,
      count_50_plus
    ) %>%
    select(
      year,
      county_fips = fips,
      count_30_plus,
      count_50_plus,
      total_computed
    )
}
    )
}

