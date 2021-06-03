test_that("tract_crosswalk data is valid", {
  tract_crosswalk = targets::tar_read(tract_crosswalk)
  
  # make sure the column names are as expected
  expect_named(tract_crosswalk, c("tract_fips", "county_fips", "coc_number", "year", "state_fips", "state_name", "coc_name", "coc_pop", "coc_poverty_pop", "tract_pop",  "tract_pop_in_poverty", "pct_coc_pop_from_tract", "pct_coc_poverty_pop_from_tract"), ignore.order = TRUE)
  
  # make sure we have no NA's in the important columns
  expect_equal(count_na(tract_crosswalk, tract_fips), 0)
  expect_equal(count_na(tract_crosswalk, year), 0)
  expect_equal(count_na(tract_crosswalk, tract_pop), 0)
  expect_equal(
    tract_crosswalk %>%
      filter(!is.na(coc_number)) %>%
      count_na(coc_pop),
    0
  )
  expect_equal(
    tract_crosswalk %>%
      filter(!is.na(coc_number)) %>%
      count_na(pct_coc_pop_from_tract),
    0
  )
  expect_equal(
    tract_crosswalk %>%
      filter(!is.na(coc_number), tract_pop_in_poverty > 0) %>%
      count_na(pct_coc_poverty_pop_from_tract),
    0
  )

  # make sure the county renter shares are all between 0 and 1
  expect_true(in_range(tract_crosswalk, pct_coc_pop_from_tract, 0, 1))
  expect_true(in_range(tract_crosswalk, pct_coc_poverty_pop_from_tract, 0, 1))

  # make sure the CoC pop from county shares add to 1 for each CoC
  coc_share_sum_check = tract_crosswalk %>% 
    filter(!is.na(coc_number)) %>% 
    group_by(coc_number, year) %>% 
    summarise(across(c(pct_coc_pop_from_tract, pct_coc_poverty_pop_from_tract), sum, na.rm = TRUE)) %>% 
    ungroup()
  
  equal_to_1 = function(x) !is.na(x) && near(x, 1)
  
  expect_true(assert(coc_share_sum_check, equal_to_1, pct_coc_pop_from_tract, success_fun = success_logical, error_fun = error_logical))
  expect_true(assert(coc_share_sum_check, equal_to_1, pct_coc_poverty_pop_from_tract,  success_fun = success_logical, error_fun = error_logical))
})
