test_that("coc_renter_shares data is valid", {
  coc_renter_shares = targets::tar_read(coc_renter_shares)
  
  # make sure the column names are as expected
  expect_named(coc_renter_shares, c("coc_number", "year", "avg_renter_share"), ignore.order = TRUE)
  
  # make sure we have no NA's in any of the columns
  expect_equal(count_na(coc_renter_shares, coc_number), 0)
  expect_equal(count_na(coc_renter_shares, year), 0)
  expect_equal(count_na(coc_renter_shares, avg_renter_share), 0)
  
  # make sure the county renter shares are all between 0 and 1
  expect_true(in_range(coc_renter_shares, avg_renter_share, 0, 1))
  
  # check the 2019 alameda county CoC renter share against actual values from:
  # https://data.census.gov/cedsci/table?g=0500000US06001&tid=ACSDT1Y2019.B25003
  expect_equal(get_value(coc_renter_shares, avg_renter_share, coc_number == "CA-502" & year == 2019), 268286 / 577177)
})
