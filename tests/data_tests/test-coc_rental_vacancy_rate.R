test_that("coc_rental_vacancy_rate is valid", {
  tar_load(coc_rental_vacancy_rate)

  coc_rental_vacancy_rate %>%
    expect_no_na_df() %>%
    expect_between(rental_vacancy_rate, 0, 1)

  get_value(
    coc_rental_vacancy_rate, rental_vacancy_rate,
    coc_number == "CA-502", year == 2013
  ) %>%
    expect_equal(0.043)

  get_value(
    coc_rental_vacancy_rate, rental_vacancy_rate,
    coc_number == "CA-502", year == 2019
  ) %>%
    expect_equal(0.029)
})
