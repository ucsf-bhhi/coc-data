# load packages used by the tests and the functions being tested b/c this is a
# fake package which doesn't use package::function when calling external
# functions we need to load those packages first here so they are available when
# the functions are called during the test
library(dplyr, warn.conflicts = FALSE)
library(tidycensus, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)

test_that("county renter shares work", {
  test_year = 2019
  test_county = "06001" # alameda county
  
  # https://data.census.gov/cedsci/table?g=0500000US06001&tid=ACSDT1Y2019.B25003
  actual_renter_households = 268286
  actual_total_households = 577177
  
  actual_renter_share = actual_renter_households / actual_total_households
  
  # calculate the renter shares in the test year and then pull the renter share
  # value so it can be compared against the actual
  test_renter_share = build_county_renter_share(test_year) %>% 
    # filter just to the test county
    filter(county_fips == test_county) %>% 
    # pull out the renter share
    pull(share_renters) %>% 
    # turn off the tidycensus messages
    suppressMessages()
  
  # check that the calculated renter share equals what's in the ACS table
  expect_equal(test_renter_share, actual_renter_share)
})

test_that("CoC renter shares work", {
  # create some fake county renter share data
  renter_shares = tribble(
    ~year, ~county_fips, ~share_renters,
    2019, "99999", 0.5,
    2019, "99998", 0.75,
    2019, "99997", 0.25,
    2019, "99996", 0.5,
    2019, "99995", 0.5
  )
  
  # create a fake county to CoC crosswalk
  crosswalk = tribble(
    ~year, ~county_fips, ~coc_number, ~pct_coc_pop_from_county,
    2019, "99999", "AA-101", 1,
    2019, "99998", "AA-102", 0.5,
    2019, "99997", "AA-102", 0.5
  )
  
  # calculate the fake CoC renter shares
  test_renter_share = build_coc_renter_shares(renter_shares, crosswalk)
  
  # check that the fake renter shares are calculated as expected
  
  # instance where there is one county in the CoC
  test_one_county = test_renter_share %>% 
    filter(coc_number == "AA-101")
  
  expect_equal(test_one_county$avg_renter_share, 0.5)
  
  # instance where there is more than one county in the CoC
  test_multi_county = test_renter_share %>% 
    filter(coc_number == "AA-102")
  
  expect_equal(test_multi_county$avg_renter_share, 0.5)
})
