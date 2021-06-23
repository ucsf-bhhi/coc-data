test_that("raw_unemployment is valid", {
  tar_load(raw_unemployment)
  
  raw_unemployment %>% 
    expect_no_na(series) %>% 
    expect_no_na(year) %>% 
    expect_no_na(month)
  
  raw_unemployment %>% 
    get_value(
      value,
      year == 2019, month == "M01", series == "LAUCN060010000000003"
    ) %>% 
    expect_equal(3.5)
})

test_that("coc_unemployment_rate is valid", {
  tar_load(coc_unemployment_rate)
  
  coc_unemployment_rate %>% 
    expect_no_na_df() %>% 
    expect_between(coc_unemployment_rate, 0, 1)
  
  coc_unemployment_rate %>% 
    get_value(coc_unemployment_rate, year == 2019, coc_number == "CA-502") %>% 
    expect_equal(0.035)
  
  coc_unemployment_rate %>% 
    get_value(coc_unemployment_rate, year == 2019, coc_number == "AL-501") %>% 
    expect_equal(0.0476, tolerance = 0.001)
})
