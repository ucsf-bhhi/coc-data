test_that("processed_zillow_data is valid", {
  tar_load(processed_zillow_data)

  processed_zillow_data %>%
    expect_no_na(zip) %>%
    expect_no_na(year) %>%
    expect_no_na(annual_mean_rent) %>%
    expect_between(year, 2014, 2021)

  expect_true(all(processed_zillow_data$annual_mean_rent > 0))

  # test a zip-year with complete data
  get_value(
    processed_zillow_data, annual_mean_rent,
    year == 2019, zip == "01752"
  ) %>%
    expect_equal(1690.41667)
  # test a zip-year with some missing data
  get_value(
    processed_zillow_data, annual_mean_rent,
    year == 2014, zip == "94109"
  ) %>%
    expect_equal(2578.3)
})

test_that("processed tract to zip is valid", {
  tar_load(tract_to_zip)

  tract_to_zip %>%
    expect_no_na(zip) %>%
    expect_no_na(tract_fips) %>%
    expect_no_na(year) %>%
    expect_no_na(res_ratio) %>%
    expect_between(year, 2014, 2019) %>%
    expect_between(res_ratio, 0, 1)


  get_value(
    tract_to_zip, res_ratio,
    year == 2014, tract_fips == "01001020200", zip == "36067"
  ) %>%
    expect_equal(0.40947697810)
  get_value(
    tract_to_zip, res_ratio,
    year == 2015, tract_fips == "01001020200", zip == "36067"
  ) %>%
    expect_equal(0.411528150)
  get_value(
    tract_to_zip, res_ratio,
    year == 2016, tract_fips == "01001020200", zip == "36067"
  ) %>%
    expect_equal(0.409111013)
  get_value(
    tract_to_zip, res_ratio,
    year == 2017, tract_fips == "01001020200", zip == "36067"
  ) %>%
    expect_equal(0.971757322)
  get_value(
    tract_to_zip, res_ratio,
    year == 2018, tract_fips == "01001020200", zip == "36067"
  ) %>%
    expect_equal(1)
  get_value(
    tract_to_zip, res_ratio,
    year == 2019, tract_fips == "01001020200", zip == "36067"
  ) %>%
    expect_equal(1)
})

test_that("tract zillow rent is valid", {
  tar_read(tract_zillow_rent) %>%
    expect_no_na(tract_fips) %>%
    expect_no_na(year) %>%
    expect_no_na(tract_share_na_rent) %>%
    expect_between(tract_share_na_rent, 0, 1) %>%
    expect_between(tract_annual_mean_rent, 0, 50000)
})

test_that("coc zillow rent is valid", {
  tar_read(coc_zillow_rent) %>%
    expect_no_na(coc_number) %>%
    expect_no_na(year) %>%
    expect_no_na(coc_share_na_rent_zillow) %>%
    expect_between(coc_share_na_rent_zillow, 0, 1) %>%
    expect_between(coc_rent_zillow, 0, 10000)
})
