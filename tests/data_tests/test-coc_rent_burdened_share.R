test_that("coc_rent_burdened_share is valid", {
  tar_read(coc_rent_burden) %>%
    expect_col_vals_not_null(everything()) %>%
    expect_rows_distinct(vars(coc_number, year)) %>%
    expect_col_vals_between(
      c(
        share_rent_over_30_pct_inc,
        share_rent_over_50_pct_inc,
        median_rent_burden
      ),
      0, 1
    )
})
