test_that("county_renter_shares data is valid", {
  tar_read(county_renter_shares) %>%
    expect_col_vals_not_null(everything()) %>%
    expect_col_vals_between(share_renters, 0, 1) %>%
    # check the 2019 alameda county renter share against actual values from:
    # https://data.census.gov/cedsci/table?g=0500000US06001&tid=ACSDT1Y2019.B25003
    expect_col_vals_equal(
      share_renters,
      0.46482,
      preconditions = . %>%
        filter(county_fips == "06001" & year == 2019) %>%
        mutate(share_renters = round(share_renters, 5))
    )
})
