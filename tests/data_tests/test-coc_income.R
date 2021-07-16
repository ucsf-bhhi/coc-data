test_that("coc_income is valid", {
  tar_read(coc_income) %>% 
    expect_col_vals_not_null(everything()) %>% 
    expect_rows_distinct(c("coc_number", "year")) %>% 
    expect_col_vals_gte(c("household_income", "family_income", "individual_earnings"), 0) %>%
    expect_col_vals_equal(
      "household_income", 99406,
      preconditions = . %>% filter(coc_number == "CA-502", year == 2019)
    ) %>% 
    expect_col_vals_equal(
      "family_income", 119612,
      preconditions = . %>% filter(coc_number == "CA-502", year == 2019)
    ) %>% 
    expect_col_vals_equal(
      "individual_earnings", 50754,
      preconditions = . %>% filter(coc_number == "CA-502", year == 2019)
    ) %>% 
    expect_col_vals_equal(
      "household_income", 51175,
      preconditions = . %>% filter(coc_number == "AL-501", year == 2019)
    ) %>% 
    expect_col_vals_equal(
      "family_income", 65821,
      preconditions = . %>% filter(coc_number == "AL-501", year == 2019)
    ) %>% 
    expect_col_vals_equal(
      "individual_earnings", 31469,
      preconditions = . %>% filter(coc_number == "AL-501", year == 2019)
    )
})
