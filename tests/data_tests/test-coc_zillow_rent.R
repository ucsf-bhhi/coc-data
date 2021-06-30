test_that("processed_zillow_data is valid", {
  tar_read(processed_zillow_data) %>%
    expect_col_vals_not_null(everything()) %>%
    expect_col_vals_between(year, 2014, 2021) %>%
    expect_col_vals_gt(annual_mean_rent, 0) %>%
    expect_col_vals_equal(
      annual_mean_rent,
      1690.41667,
      preconditions = . %>%
        filter(year == 2019, zip == "01752") %>%
        mutate(annual_mean_rent = round(annual_mean_rent, 5))
    ) %>%
    expect_col_vals_equal(
      annual_mean_rent,
      2578.3,
      preconditions = . %>%
        filter(year == 2014, zip == "94109") %>%
        mutate(annual_mean_rent = round(annual_mean_rent, 1))
    )
})

test_that("processed tract to zip is valid", {
  tar_read(tract_to_zip) %>%
    expect_col_vals_not_null(everything()) %>%
    expect_col_vals_between(year, 2014, 2019) %>%
    expect_col_vals_between(res_ratio, 0, 1) %>%
    expect_col_vals_equal(
      res_ratio,
      0.40948,
      preconditions = . %>%
        filter(year == 2014, tract_fips == "01001020200", zip == "36067") %>%
        mutate(res_ratio = round(res_ratio, 5))
    ) %>%
    expect_col_vals_equal(
      res_ratio,
      0.41153,
      preconditions = . %>%
        filter(year == 2015, tract_fips == "01001020200", zip == "36067") %>%
        mutate(res_ratio = round(res_ratio, 5))
    ) %>%
    expect_col_vals_equal(
      res_ratio,
      0.40911,
      preconditions = . %>%
        filter(year == 2016, tract_fips == "01001020200", zip == "36067") %>%
        mutate(res_ratio = round(res_ratio, 5))
    ) %>%
    expect_col_vals_equal(
      res_ratio,
      0.97176,
      preconditions = . %>%
        filter(year == 2017, tract_fips == "01001020200", zip == "36067") %>%
        mutate(res_ratio = round(res_ratio, 5))
    ) %>%
    expect_col_vals_equal(
      res_ratio,
      1,
      preconditions = . %>%
        filter(year == 2018, tract_fips == "01001020200", zip == "36067")
    ) %>%
    expect_col_vals_equal(
      res_ratio,
      1,
      preconditions = . %>%
        filter(year == 2019, tract_fips == "01001020200", zip == "36067")
    )
})

test_that("tract zillow rent is valid", {
  tar_read(tract_zillow_rent) %>%
    expect_col_vals_not_null(c(tract_fips, year, tract_share_na_rent)) %>%
    expect_col_vals_between(tract_share_na_rent, 0, 1.00001) %>%
    expect_col_vals_between(tract_annual_mean_rent, 0, 50000, na_pass = TRUE)
})

test_that("coc zillow rent is valid", {
  tar_read(coc_zillow_rent) %>%
    expect_col_vals_not_null(c(coc_number, year, coc_share_na_rent_zillow)) %>%
    expect_rows_distinct(vars(coc_number, year)) %>%
    expect_col_vals_between(coc_share_na_rent_zillow, 0, 1.00001) %>%
    expect_col_vals_between(coc_rent_zillow, 0, 10000, na_pass = TRUE)
})
