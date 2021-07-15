test_that("renting_households data is valid", {
  tar_read(renting_households) %>% 
    expect_col_vals_not_null(everything()) %>% 
    expect_col_vals_gte(renting_households, 0, na_pass = TRUE) %>% 
    expect_col_vals_equal(renting_households, 1456,
      preconditions = . %>% filter(fips == "06001403300" & year == 2011)
    ) %>% 
    expect_col_vals_equal(renting_households, 1440,
      preconditions = . %>% filter(fips == "06001403300" & year == 2019)
    )
})