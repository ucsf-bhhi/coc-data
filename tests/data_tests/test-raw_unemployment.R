test_that("raw_unemployment is valid", {
  tar_read(raw_unemployment) %>%
    expect_col_vals_not_null(c(series, year, month)) %>%
    expect_col_vals_equal(
      value,
      3.5,
      preconditions = . %>%
        filter(year == 2019, month == "M01", series == "LAUCN060010000000003")
    )
})

test_that("coc_unemployment_rate is valid", {
  tar_read(coc_unemployment_rate) %>%
    expect_col_vals_not_null(everything()) %>%
    expect_col_vals_between(coc_unemployment_rate, 0, 1) %>%
    expect_rows_distinct(vars(coc_number, year)) %>%
    expect_col_vals_equal(
      coc_unemployment_rate,
      0.035,
      preconditions = . %>%
        filter(year == 2019, coc_number == "CA-502")
    ) %>%
    expect_col_vals_equal(
      coc_unemployment_rate,
      0.048,
      preconditions = . %>%
        filter(year == 2019, coc_number == "AL-501") %>%
        mutate(coc_unemployment_rate = round(coc_unemployment_rate, 3))
    )
})
