test_that("coc_populations is valid", {
  tar_read(coc_populations) %>% 
    expect_no_na_df() %>% 
    expect_between(coc_poverty_rate, 0, 1)
})