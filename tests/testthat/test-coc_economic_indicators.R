test_that("build_coc_public_program_use() works properly", {
  # create some fake tract rent burdened counts
  test_acs <- tibble(
    year = rep(2019, 5),
    fips = c("99999", "99998", "99997", "99996", "99995"),
    total_hh_snap = c(500, 400, 100, 700, 300),
    hh_with_snap = c(20, 15, 10, 30, 20),
    total_hh_pub_assist = c(500, 400, 100, 700, 300),
    hh_with_pub_assist = c(15, 20, 5, 20, 10),
    total_hh_snap_or_pub_assist = c(500, 400, 100, 700, 300),
    hh_with_snap_or_pub_assist = c(35, 35, 15, 50, 30),
    total_hh_ssi = c(500, 400, 100, 700, 300),
    hh_with_ssi = c(25, 20, 5, 30, 10),
    total_male_19_64 = c(600, 300, 100, 800, 200),
    male_19_64_with_medicaid = c(150, 100, 50, 200, 20),
    total_female_19_64 = c(550, 350, 100, 850, 150),
    female_19_64_with_medicaid = c(125, 75, 10, 225, 25)
  )

  test_crosswalk <- tribble(
    ~year, ~tract_fips, ~coc_number,
    2019, "99999", "AA-101",
    2019, "99998", "AA-101",
    2019, "99997", "AA-101",
    2019, "99996", "AA-102",
    2019, "99995", "AA-102"
  )

  expected <- tibble(
    coc_number = c("AA-101", "AA-102"),
    year = rep(2019, 2),
    share_hh_with_snap = c(0.045, 0.05),
    share_hh_with_pub_assist = c(0.04, 0.03),
    share_hh_with_snap_or_pub_assist = c(0.085, 0.08),
    share_hh_with_ssi = c(0.05, 0.04),
    share_with_medicaid = c(0.255, 0.235)
  )

  expect_equal(build_coc_public_program_use(test_acs, test_crosswalk), expected)
})
