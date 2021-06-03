test_that("county_renter_shares data is valid", {
  county_renter_shares = targets::tar_read(county_renter_shares)
  
  # make sure the column names are as expected
  expect_named(county_renter_shares, c("county_fips", "year", "share_renters"), ignore.order = TRUE)
  
  # make sure we have no NA's in any of the columns
  expect_equal(count_na(county_renter_shares, county_fips), 0)
  expect_equal(count_na(county_renter_shares, year), 0)
  expect_equal(count_na(county_renter_shares, share_renters), 0)
  
  # make sure the county renter shares are all between 0 and 1
  expect_true(in_range(county_renter_shares, share_renters, 0, 1))
  
  # check the 2019 alameda county renter share against actual values from:
  # https://data.census.gov/cedsci/table?g=0500000US06001&tid=ACSDT1Y2019.B25003
  expect_equal(get_value(county_renter_shares, share_renters, county_fips == "06001" & year == 2019), 268286 / 577177)
})
