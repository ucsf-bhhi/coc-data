test_that("county_crosswalk data is valid", {
  tar_read(county_crosswalk) %>%
    expect_col_vals_not_null(
      c(-pct_county_poverty_pop_in_coc, everything())
    ) %>%
    expect_col_vals_not_null(
      pct_county_poverty_pop_in_coc,
      preconditions = . %>% filter(county_pop_in_poverty > 0)
    ) %>%
    expect_rows_distinct(vars(county_fips, coc_number, year)) %>%
    expect_col_vals_between(starts_with("pct"), 0, 1, na_pass = TRUE) %>%
    # check that pct_coc_pop_from_county & pct_coc_poverty_pop_from_county sum
    # to 1 for every CoC
    expect_col_vals_between(
      c(pct_coc_pop_from_county, pct_coc_poverty_pop_from_county),
      0.99999, 1.00001,
      preconditions = . %>%
        group_by(coc_number, year) %>%
        summarise(
          across(
            c(pct_coc_pop_from_county, pct_coc_poverty_pop_from_county),
            sum
          ),
          .groups = "drop"
        )
    )
})
