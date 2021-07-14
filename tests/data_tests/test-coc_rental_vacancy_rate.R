test_that("coc_rental_vacancy_rates is valid", {
  tar_read(coc_rental_vacancy_rates) %>%
    expect_col_vals_not_null(everything()) %>%
    expect_col_vals_between(ends_with("vacancy_rate"), 0, 1) %>%
    expect_rows_distinct(vars(coc_number, year)) %>%
    expect_col_vals_equal(
      rental_vacancy_rate, 0.043,
      preconditions = ~ . %>%
        filter(coc_number == "CA-502", year == 2013) %>%
        mutate(rental_vacancy_rate = round(rental_vacancy_rate, 3))
    ) %>%
    expect_col_vals_equal(
      gross_vacancy_rate, 0.068,
      preconditions = ~ . %>%
        filter(coc_number == "CA-502", year == 2013) %>%
        mutate(gross_vacancy_rate = round(gross_vacancy_rate, 3))
    ) %>%
    expect_col_vals_equal(
      rental_vacancy_rate, 0.029,
      preconditions = ~ . %>%
        filter(coc_number == "CA-502", year == 2019) %>%
        mutate(rental_vacancy_rate = round(rental_vacancy_rate, 3))
    ) %>%
    expect_col_vals_equal(
      gross_vacancy_rate, 0.051,
      preconditions = ~ . %>%
        filter(coc_number == "CA-502", year == 2019) %>%
        mutate(gross_vacancy_rate = round(gross_vacancy_rate, 3))
    )
})
