test_year = 2019
test_county = "06001" # alameda county
test_coc = "CA-502" # alameda county coc (same as the county itself)
test_pct_coc_pop_from_county = 1

# https://data.census.gov/cedsci/table?g=0500000US06001&tid=ACSDT1Y2019.B25003
actual_renter_households = 268286
actual_total_households = 577177

actual_renter_share = actual_renter_households / actual_total_households

test_that("county renter shares work", {
  test_renter_share = build_county_renter_share(test_year) %>% 
    filter(county_fips == test_county) %>% 
    pull(share_renters)
  
  expect_equal(test_renter_share, actual_renter_share)
})

test_that("CoC renter shares work", {
  renter_shares = tribble(
    ~year, ~county_fips, ~share_renters,
    test_year, test_county, actual_renter_share
  )
  
  crosswalk = tribble(
    ~year, ~county_fips, ~coc_number, ~pct_coc_pop_from_county,
    test_year, test_county, test_coc, test_pct_coc_pop_from_county
  )
  
  test_renter_share = build_coc_renter_shares(renter_shares, crosswalk) %>% 
    filter(coc_number == test_coc) %>% 
    pull(avg_renter_share)
  
  expect_equal(test_renter_share, actual_renter_share)
})
