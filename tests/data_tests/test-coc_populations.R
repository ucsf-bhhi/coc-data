test_that("coc_populations is valid", {
  tar_read(coc_populations) %>%
    expect_col_vals_not_null(everything()) %>%
    expect_col_vals_between(coc_poverty_rate, 0, 1) %>%
    expect_rows_distinct(vars(coc_number, year))
})
