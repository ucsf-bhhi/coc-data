test_that("fetching program use data works", {
  tar_load(tract_public_program_use)

  tract_public_program_use %>%
    expect_col_vals_not_null(names(.)) %>%
    expect_col_vals_gte(
      c(
        "total_hh_snap",
        "hh_with_snap",
        "total_hh_pub_assist",
        "hh_with_pub_assist",
        "total_hh_snap_or_pub_assist",
        "hh_with_snap_or_pub_assist",
        "total_hh_ssi",
        "hh_with_ssi",
        "total_male_19_64",
        "male_19_64_with_medicaid",
        "total_female_19_64",
        "female_19_64_with_medicaid"
      ),
      0
    )

  expected_2013 <- tibble(
    fips = "06001403300",
    year = 2013,
    total_hh_snap = 2140,
    hh_with_snap = 65,
    total_hh_pub_assist = 2140,
    hh_with_pub_assist = 62,
    total_hh_snap_or_pub_assist = 2140,
    hh_with_snap_or_pub_assist = 107,
    total_hh_ssi = 2140,
    hh_with_ssi = 130,
    total_male_19_64 = 1438,
    male_19_64_with_medicaid = 197,
    total_female_19_64 = 1591,
    female_19_64_with_medicaid = 207
  )

  expected_2019 <- tibble(
    fips = "06001403300",
    year = 2019,
    total_hh_snap = 2143,
    hh_with_snap = 148,
    total_hh_pub_assist = 2143,
    hh_with_pub_assist = 46,
    total_hh_snap_or_pub_assist = 2143,
    hh_with_snap_or_pub_assist = 194,
    total_hh_ssi = 2143,
    hh_with_ssi = 252,
    total_male_19_64 = 1408,
    male_19_64_with_medicaid = 246,
    total_female_19_64 = 1513,
    female_19_64_with_medicaid = 279
  )

  tract_public_program_use %>%
    filter(fips == "06001403300", year == 2013) %>%
    expect_equal(expected_2013)

  tract_public_program_use %>%
    filter(fips == "06001403300", year == 2019) %>%
    expect_equal(expected_2019)
})

test_that("coc_public_program_use is valid", {
  tar_read(coc_public_program_use) %>%
    expect_col_vals_not_null(names(.)) %>%
    expect_col_vals_between(
      c(
        "shr_hh_with_snap",
        "shr_hh_with_pub_assist",
        "shr_hh_with_snap_or_pub_assist",
        "shr_hh_with_ssi",
        "shr_with_medicaid"
      ),
      0, 1
    )
})
