test_that("coc_fmr is valid", {
  tar_read(coc_fmr) %>%
    expect_col_vals_not_null(everything()) %>%
    expect_col_vals_between(starts_with("avg_fmr"), 100, 10000, na_pass = TRUE) %>%
    expect_rows_distinct(vars(coc_number, year)) %>%
    expect_col_vals_equal(
      avg_fmr0,
      892,
      preconditions = ~ . %>% filter(coc_number == "CA-502", year == 2013)
    ) %>%
    expect_col_vals_equal(
      avg_fmr0,
      571.7391,
      preconditions = ~ . %>%
        filter(coc_number == "AL-501", year == 2013) %>%
        mutate(avg_fmr0 = round(avg_fmr0, 4))
    ) %>%
    expect_col_vals_equal(
      avg_fmr0,
      1409,
      preconditions = ~ . %>% filter(coc_number == "CA-502", year == 2019)
    ) %>%
    expect_col_vals_equal(
      avg_fmr0,
      703.4306,
      preconditions = ~ . %>%
        filter(coc_number == "AL-501", year == 2019) %>%
        mutate(avg_fmr0 = round(avg_fmr0, 4))
    )
})
