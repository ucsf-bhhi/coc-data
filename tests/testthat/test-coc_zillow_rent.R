library(dplyr)
library(stringr)
library(fs)

test_that("tract to zip year parsing works", {
  expect_equal(
    get_tract_to_zip_year(
      "input_data/geography/usps_tract_to_zip/ZIP_TRACT_092014.xlsx"
    ),
    2014
  )
})

test_that("tract zillow index calculation is correct", {
  test_zillow_data = tribble(
    ~year,  ~zip, ~annual_mean_rent,
     2014, 94607,              1000,
     2014, 94608,              2000
  )
  test_tract_to_zip = tribble(
    ~year,  ~zip, ~tract_fips, ~res_ratio,
     2014, 94607,           1,          1,
     2014, 94607,           2,        0.5,
     2014, 94608,           2,        0.5,
     2014, 94608,           3,        0.5,
     2014, 94609,           3,        0.5,
     2014, 96410,           4,          1
  )
  expected_result = tribble(
    ~year, ~tract_fips, ~tract_annual_mean_rent, ~tract_share_na_rent,
    2014,           1,                    1000,                    0,
    2014,           2,                    1500,                    0,
    2014,           3,                    2000,                  0.5,
    2014,           4,                     NaN,                    1
  )
  
  expect_equal(
    build_tract_zillow_rent(test_zillow_data, test_tract_to_zip),
    expected_result
  )
})

test_that("coc zillow index calculation is correct", {
  test_tract_rent = tribble(
    ~year, ~tract_fips, ~tract_annual_mean_rent, ~tract_share_na_rent,
    2014,           1,                    1000,                    0,
    2014,           2,                    1500,                    0,
    2014,           3,                    2000,                  0.5,
    2014,           4,                     NaN,                    1,
    2014,           5,                     NaN,                    1,
    2014,           6,                     NaN,                    1
  )
  test_tract_crosswalk = tribble(
    ~year,  ~coc_number, ~tract_fips, ~pct_coc_pop_from_tract,
     2014,          "A",           1,                     0.5,
     2014,          "A",           2,                     0.5,
     2014,          "B",           3,                     0.5,
     2014,          "B",           4,                     0.5,
     2014,          "C",           5,                     0.5,
     2014,          "C",           6,                     0.5
  )
  expected_result = tribble(
    ~coc_number, ~year, ~coc_rent_zillow, ~coc_share_na_rent_zillow,
            "A",  2014,             1250,                         0,
            "B",  2014,             2000,                      0.75,
            "C",  2014,              NaN,                         1
  )
  
  expect_equal(
    build_coc_zillow_rent(test_tract_rent, test_tract_crosswalk),
    expected_result
  )
})

