test_that("county_crosswalk data is valid", {
  county_crosswalk = targets::tar_read(county_crosswalk)

    # make sure the column names are as expected
  expect_named(county_crosswalk, c("county_fips", "coc_number", "year", "state_fips", "state_name", "coc_name", "coc_pop", "coc_poverty_pop", "county_pop_in_coc", "county_poverty_pop_in_coc", "county_pop", "county_pop_in_poverty", "pct_coc_pop_from_county", "pct_coc_poverty_pop_from_county", "pct_county_pop_in_coc", "pct_county_poverty_pop_in_coc"), ignore.order = TRUE)

    # make sure we have no NA's in the important columns
  expect_equal(count_na(county_crosswalk, county_fips), 0)
  expect_equal(count_na(county_crosswalk, coc_number), 0)
  expect_equal(count_na(county_crosswalk, year), 0)
  expect_equal(count_na(county_crosswalk, pct_coc_pop_from_county), 0)
  expect_equal(count_na(county_crosswalk, pct_coc_poverty_pop_from_county), 0)
  expect_equal(count_na(county_crosswalk, pct_county_pop_in_coc), 0)
  expect_equal(
    county_crosswalk %>% 
      filter(county_pop_in_poverty > 0) %>%
      count_na( pct_county_poverty_pop_in_coc),
    0
  )
  
  # make sure the county renter shares are all between 0 and 1
  expect_true(in_range(county_crosswalk, pct_coc_pop_from_county, 0, 1))
  expect_true(in_range(county_crosswalk, pct_coc_poverty_pop_from_county, 0, 1))
  expect_true(in_range(county_crosswalk, pct_county_pop_in_coc, 0, 1))
  expect_true(in_range(county_crosswalk, pct_county_poverty_pop_in_coc, 0, 1))
  
  # make sure the CoC pop from county shares add to 1 for each CoC
  coc_share_sum_check = county_crosswalk %>% 
    group_by(coc_number, year) %>% 
    summarise(across(c(pct_coc_pop_from_county, pct_coc_poverty_pop_from_county), sum)) %>% 
    ungroup()
  
  equal_to_1 = function(x) !is.na(x) && near(x, 1)

  expect_true(assert(coc_share_sum_check, equal_to_1, pct_coc_pop_from_county, success_fun = success_logical, error_fun = error_logical))
  expect_true(assert(coc_share_sum_check, equal_to_1, pct_coc_poverty_pop_from_county,  success_fun = success_logical, error_fun = error_logical))
})
