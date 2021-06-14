test_that("county_rent_burdened_count is valid", {
  tar_load(county_rent_burdened_count)

  expect_equal(
    get_value(
      county_rent_burdened_count, count_30_plus,
      year == 2013, county_fips == "01001"
    ),
    2169
  )
  expect_equal(
    get_value(
      county_rent_burdened_count, count_50_plus,
      year == 2013, county_fips == "01001"
    ),
    1033
  )
  expect_equal(
    get_value(
      county_rent_burdened_count, total_computed,
      year == 2013, county_fips == "01001"
    ),
    4253
  )
  expect_equal(
    get_value(
      county_rent_burdened_count, count_30_plus,
      year == 2019, county_fips == "01001"
    ),
    2387
  )
  expect_equal(
    get_value(
      county_rent_burdened_count, count_50_plus,
      year == 2019, county_fips == "01001"
    ),
    1443
  )
  expect_equal(
    get_value(
      county_rent_burdened_count, total_computed,
      year == 2019, county_fips == "01001"
    ),
    4905
  )
})
