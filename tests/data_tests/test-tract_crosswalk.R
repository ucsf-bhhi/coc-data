test_that("tract_crosswalk data is valid", {
  tar_read(tract_crosswalk) %>%
    expect_col_vals_not_null(
      everything(),
      preconditions = . %>% filter(!is.na(coc_number))
    ) %>%
    expect_col_vals_null(
      c(coc_pop, coc_poverty_pop, starts_with("pct")),
      preconditions = . %>% filter(is.na(coc_number))
    ) %>%
    expect_col_vals_not_null(
      c(-coc_pop, -coc_poverty_pop, -starts_with("pct"), everything()),
      preconditions = . %>% filter(is.na(coc_number))
    ) %>%
    expect_rows_distinct(vars(tract_fips, coc_number, year)) %>%
    expect_col_vals_between(starts_with("pct"), 0, 1, na_pass = TRUE) %>%
    # make sure the CoC pop from county shares add to 1 for each CoC
    expect_col_vals_between(
      c(pct_coc_pop_from_tract, pct_coc_poverty_pop_from_tract),
      0.99999, 1.00001,
      preconditions = . %>%
        filter(!is.na(coc_number)) %>%
        group_by(coc_number, year) %>%
        summarise(
          across(
            c(pct_coc_pop_from_tract, pct_coc_poverty_pop_from_tract),
            sum
          ),
          .groups = "drop"
        )
    )
})
