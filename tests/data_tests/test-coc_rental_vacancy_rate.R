test_that("coc_rental_vacancy_rates is valid", {
  tar_load(coc_rental_vacancy_rates)

  coc_rental_vacancy_rates %>%
    expect_no_na_df() %>%
    expect_between(rental_vacancy_rate, 0, 1) %>% 
    expect_between(gross_vacancy_rate, 0, 1)

  get_value(
    coc_rental_vacancy_rates, rental_vacancy_rate,
    coc_number == "CA-502", year == 2013
  ) %>%
    expect_equal(0.043, tolerance = 0.01)
  
  get_value(
    coc_rental_vacancy_rates, gross_vacancy_rate,
    coc_number == "CA-502", year == 2013
  ) %>%
    expect_equal(0.068, tolerance = 0.01)

  get_value(
    coc_rental_vacancy_rates, rental_vacancy_rate,
    coc_number == "CA-502", year == 2019
  ) %>%
    expect_equal(0.029, tolerance = 0.01)
  
  get_value(
    coc_rental_vacancy_rates, gross_vacancy_rate,
    coc_number == "CA-502", year == 2019
  ) %>%
    expect_equal(0.051, tolerance = 0.01)
})
