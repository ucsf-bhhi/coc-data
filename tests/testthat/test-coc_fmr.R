library(dplyr, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(readxl, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(tidycensus, warn.conflicts = FALSE)

test_that("parsing a whole county 2012 fmr works", {
  expected = read_rds("testdata/expected_fmr_whole_county_2012.rds")
  
  expect_equal(process_fmr("testdata/fmr_whole_county_2012.xlsx"), expected)
})

test_that("parsing a whole county 2019 fmr works", {
  expected = read_rds("testdata/expected_fmr_whole_county_2019.rds")
  
  expect_equal(process_fmr("testdata/fmr_whole_county_2019.xlsx"), expected)
})

test_that("parsing a split county 2012 fmr works", {
  expected = read_rds("testdata/expected_fmr_split_county_2012.rds")
  
  expect_equal(process_fmr("testdata/fmr_split_county_2012.xlsx"), expected)
})

test_that("parsing a split county 2019 fmr works", {
  expected = read_rds("testdata/expected_fmr_split_county_2019.rds")
  
  expect_equal(process_fmr("testdata/fmr_split_county_2019.xlsx"), expected)
})

test_that("fetching subcounty acs data works", {
  expected_2012 = read_rds("testdata/expected_acs_split_county_2012.rds")
  expected_2019 = read_rds("testdata/expected_acs_split_county_2019.rds")
  
  test_processed_fmr = read_rds("testdata/parsed_fmr_split_county.rds")
  
  expect_equal(get_acs_county_sub(2012, test_processed_fmr), expected_2012)
  expect_equal(get_acs_county_sub(2019, test_processed_fmr), expected_2019)
})

test_that("processing a split county fmr works", {
  expected = read_rds("testdata/expected_processed_fmr_split_county.rds")
  
  test_parsed_fmr = read_rds("testdata/parsed_fmr_split_county.rds")
  test_acs = read_rds("testdata/acs_split_county.rds")
  
  expect_equal(process_split_county_fmr(test_parsed_fmr, test_acs), expected)
})
