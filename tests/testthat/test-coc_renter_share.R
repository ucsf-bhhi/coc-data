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

test_that("fetch_acs_rent_burden is working for 2013", {
  expected = tibble(
    year = 2013,
    tract_fips = "06001403300",
    count_30_plus = 926,
    count_50_plus = 468,
    total_computed = 1577,
    median_rent_burden = 0.349 
  )
  
  fetch_acs_rent_burden("06", 2013) %>%
    filter(tract_fips == "06001403300") %>% 
    expect_equal(expected)
})

test_that("fetch_acs_rent_burden is working for 2019", {
  expected = tibble(
    year = 2019,
    tract_fips = "06001403300",
    count_30_plus = 716,
    count_50_plus = 357,
    total_computed = 1309,
    median_rent_burden = 0.320 
  )
  
  fetch_acs_rent_burden("06", 2019) %>%
    filter(tract_fips == "06001403300") %>% 
    expect_equal(expected)
})

test_that("coc share rent burdened is working correctly", {
  test_yr <- 2019
  
  # create some fake tract rent burdened counts
  test_acs <- tribble(
    ~year, ~tract_fips, ~count_30_plus, ~count_50_plus, ~total_computed, ~median_rent_burden,
    2019, "99999", 30, 15, 100, 0.3,
    2019, "99998", 20, 10, 60, 0.4,
    2019, "99997", 15, 7, 40, 0.2,
    2019, "99996", 30, 15, 100, 0.3,
    2019, "99995", 20, 10, 75, 0.3,
    2019, "99994", 40, 20, 125, 0.4
  )
  
  test_crosswalk <- tribble(
    ~year, ~tract_fips, ~coc_number, ~pct_coc_pop_from_tract,
    2019, "99999", "AA-101", 1,
    2019, "99998", "AA-102", 0.75,
    2019, "99997", "AA-102", 0.25,
    2019, "99996", "AA-103", 0.25,
    2019, "99995", "AA-103", 0.50,
    2019, "99994", "AA-103", 0.25
  )
  
  expected <- tribble(
    ~year, ~coc_number, ~share_rent_over_30_pct_inc, ~share_rent_over_50_pct_inc, ~median_rent_burden,
    2019, "AA-101", 0.3, 0.15, 0.3,
    2019, "AA-102", 0.35, 0.17, 0.35,
    2019, "AA-103", 0.3, 0.15, 0.325
  )
  
  expect_equal(
    make_coc_rent_burden(test_acs, test_crosswalk, test_yr),
    expected
  )
})

test_that("fetch_acs_rental_vacancy_data is working for 2013", {
  expected = tibble(
    fips = "06001403300",
    year = 2013,
    total_housing_units = 2365,
    vacant_housing_units = 225,
    occupied_rental_units = 1617,
    for_rent = 83,
    rented_not_occupied = 0
  )
  
  fetch_acs_vacancy_data("06", 2013) %>%
    filter(fips == "06001403300") %>% 
    expect_equal(expected)
})

test_that("fetch_acs_rental_vacancy_data is working for 2019", {
  expected = tibble(
    fips = "06001403300",
    year = 2019,
    total_housing_units = 2393,
    vacant_housing_units = 250,
    occupied_rental_units = 1440,
    for_rent = 91,
    rented_not_occupied = 0
  )
  
  fetch_acs_vacancy_data("06", 2019) %>%
    filter(fips == "06001403300") %>% 
    expect_equal(expected)
})

test_that("building coc vacancy rates works", {
  test_yr <- 2019

  test_acs <- tibble(
    fips = c("99999", "99998", "99997", "99996", "99995"),
    year = c(2019, 2019, 2019, 2019, 2019),
    total_housing_units = c(10000, 4000, 1000, 2000, NA),
    vacant_housing_units = c(1000, 50, 25, 30, NA),
    occupied_rental_units = c(3800, 2100, 1800, 980, NA),
    for_rent = c(100, 10, 40, 10, NA),
    rented_not_occupied = c(100, 5, 45, 10, NA)
  )

  test_crosswalk <- tribble(
    ~year, ~tract_fips, ~coc_number,
    2019, "99999", "AA-101",
    2019, "99998", "AA-102",
    2019, "99997", "AA-102",
    2019, "99996", "AA-103",
    2019, "99995", "AA-103"
  )

  expected <- tribble(
    ~coc_number, ~year, ~gross_vacancy_rate, ~rental_vacancy_rate,
    "AA-101", 2019, 0.1, 0.025,
    "AA-102", 2019, 0.015, 0.0125,
    "AA-103", 2019, 0.015, 0.01
  )

  expect_equal(
    make_coc_vacancy_rates(test_acs, test_yr, test_crosswalk),
    expected
  )
})

test_that("build_coc_evictions works properly", {
  evictions = tribble(
    ~GEOID, ~year, ~evictions, ~eviction.filings,
    "99999", 2019, 10, 15,
    "99998", 2019, 20, 25,
    "99997", 2019, 20, 30,
    "99996", 2019, 15, 20,
    "99995", 2019, 25, 30,
    "99994", 2019, NA, NA
  )
  
  acs_data = tribble(
    ~fips, ~year, ~renting_households,
    "99999", 2019, 250,
    "99998", 2019, 500,
    "99997", 2019, 250,
    "99996", 2019, 700,
    "99995", 2019, 200,
    "99994", 2019, 100
  )
  
  tract_crosswalk = tribble(
    ~tract_fips, ~year, ~coc_number,
    "99999", 2019, "AA-101",
    "99998", 2019, "AA-101",
    "99997", 2019, "AA-101",
    "99996", 2019, "AA-102",
    "99995", 2019, "AA-102",
    "99994", 2019, "AA-102"
  )
  
  expected = tribble(
    ~coc_number, ~year, ~eviction_filings, ~evictions, ~renting_households, ~eviction_filing_rate, ~eviction_rate,
    "AA-101", 2019, 70, 50, 1000, 0.07, 0.05,
    "AA-102", 2019, 50, 40, 1000, 0.05, 0.04
  )
  
  expect_equal(build_coc_evictions(evictions, acs_data, tract_crosswalk), expected)
})
