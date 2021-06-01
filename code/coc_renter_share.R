build_county_renter_share = function(year) {
  # hit the census api for total household and renter household counts
  fetch_acs("county", variables = c(total_households = "B25003_001", renting_households = "B25003_003"), year = year, key = Sys.getenv("CENSUS_API_KEY"), output = "wide", survey = "acs5") %>%
    # calculate the share of households who are renting
    mutate(share_renters = renting_households / total_households) %>%
    select(county_fips = fips, year, share_renters)
}


build_coc_renter_shares = function(renter_shares, crosswalk) {
  crosswalk %>%
  # merge the county renter data onto the crosswalk
  left_join(renter_shares, by = c("county_fips", "year")) %>%
  group_by(coc_number, year) %>%
  # take the weighted average renter share weighting by share of the CoC population coming from each county
  summarise(avg_renter_share = weighted.mean(share_renters, pct_coc_pop_from_county, na.rm = TRUE))
}