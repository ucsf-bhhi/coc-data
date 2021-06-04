test_that("coc_fmr is valid", {
  tar_load(coc_fmr)
  
  expect_equal(count_na(coc_fmr, coc_number), 0)
  expect_equal(count_na(coc_fmr, year), 0)
  expect_equal(count_na(coc_fmr, avg_fmr0), 0)
  expect_equal(count_na(coc_fmr, avg_fmr1), 0)
  expect_equal(count_na(coc_fmr, avg_fmr2), 0)
  expect_equal(count_na(coc_fmr, avg_fmr3), 0)
  expect_equal(count_na(coc_fmr, avg_fmr4), 0)
  
  expect_true(in_range(coc_fmr, avg_fmr0, 100, 10000))
  expect_true(in_range(coc_fmr, avg_fmr1, 100, 10000))
  expect_true(in_range(coc_fmr, avg_fmr2, 100, 10000))
  expect_true(in_range(coc_fmr, avg_fmr3, 100, 10000))
  expect_true(in_range(coc_fmr, avg_fmr4, 100, 10000))
  
  expect_equal(get_value(coc_fmr, avg_fmr0, coc_number == "CA-502", year == 2013), 892)
  expect_equal(get_value(coc_fmr, avg_fmr0, coc_number == "AL-501", year == 2013), 571.11523)
  
  expect_equal(get_value(coc_fmr, avg_fmr0, coc_number == "CA-502", year == 2019), 1409)
  expect_equal(get_value(coc_fmr, avg_fmr0, coc_number == "AL-501", year == 2019), 703.8183)
  
})