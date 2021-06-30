test_that("coc_renter_shares data is valid", {
  tar_read(coc_renter_shares) %>%
    expect_col_vals_not_null(everything()) %>%
    expect_col_vals_between(avg_renter_share, 0, 1) %>%
    expect_rows_distinct(vars(coc_number, year)) %>%
    # check the 2019 alameda county CoC renter share against actual values from:
    # https://data.census.gov/cedsci/table?g=0500000US06001&tid=ACSDT1Y2019.B25003
    expect_col_vals_equal(
      avg_renter_share,
      0.4648,
      preconditions = . %>%
        filter(coc_number == "CA-502" & year == 2019) %>%
        mutate(avg_renter_share = round(avg_renter_share, 4))
    )
})
