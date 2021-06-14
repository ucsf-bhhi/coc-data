test_that("coc_rent_burdened_share is valid", {
  tar_read(coc_rent_burdened_share) %>%
    expect_no_na_df() %>%
    expect_between(share_rent_over_30_pct_inc, 0, 1) %>%
    expect_between(share_rent_over_50_pct_inc, 0, 1) %>% 
    expect_between(median_rent_burden, 0, 1)
})
