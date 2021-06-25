library(targets, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(assertr, warn.conflicts = FALSE)
library(glue, warn.conflicts = FALSE)
library(rlang, warn.conflicts = FALSE)
library(crayon, warn.conflicts = FALSE)
library(pointblank, warn.conflicts = FALSE)

expect_between <- function(data, column, min, max, tolerance = tt_tol) {
  column_exists({{ data }}, {{ column }})
  
  is_between = in_range(data, {{ column }}, min, max, tolerance)
  
  column_name <- substitute(column)
  
  expect(
    is_between,
    glue_col("{red {column_name} has values outside {min} and {max}}")
  )
  
  invisible(data)
}

tt_tol <- testthat::testthat_tolerance()

in_range <- function(data, column, min, max, tolerance = tt_tol) {
  data %>%
    filter(!is.na({{ column }})) %>%
    pull({{ column }}) %>%
    in_between(min, max, tolerance) %>%
    all()
}

in_between <- function(x, min, max, tolerance = 0L) {
  x >= (min - tolerance) & x <= (max + tolerance)
}

expect_no_na <- function(data, column) {
  column_exists({{ data }}, {{ column }})
  
  na_count <- count_na(data, {{ column }})
  nas = if (na_count == 1) "NA" else "NAs"
  
  column_name = as_name(enquo(column))
  
  expect(
    na_count == 0,
    glue_col("{red `{column_name}` has {na_count} {nas}}")
  )
  
  invisible(data)
}

expect_no_na_df = function(data) {
  data_name = as_name(enquo(data))
  
  expect(
    !anyNA(data),
    glue_col("{red {data_name} has NAs.}")
  )
  
  invisible(data)
}

count_na <- function(data, column) {
  data %>%
    pull({{ column }}) %>%
    is.na() %>%
    sum()
}

column_exists <- function(data, column) {
  column_name <- as_name(enquo(column))
  data_name <- as_name(enquo(data))
  
  check = has_name(quo_squash(data), column_name)
  
  if (!check) {
     msg = glue_col("{red column '{column_name}' not present in '{data_name}'}")
     stop(msg, call. = FALSE)
  }
}

get_value <- function(data, column, ...) {
  column_exists({{ data }}, {{ column }})
  f <- quos(...)
  data %>%
    filter(!!!f) %>%
    pull({{ column }})
}

test_that("expect_between works properly", {
  test_data <- tribble(
    ~pass, ~too_low, ~too_high, ~both, ~tol_low, ~tol_high, ~tol_both,
    0, -1, 0, -1, -0.0000000001, 0, -0.0000000001,
    0.5, 0.5, 2, 2, 0.5, 1.0000000001, 1.0000000001
  )
  test_min <- 0
  test_max <- 1

  expect_success(expect_between(test_data, pass, test_min, test_max))
  expect_success(expect_between(test_data, tol_low, test_min, test_max))
  expect_success(expect_between(test_data, tol_high, test_min, test_max))
  expect_success(expect_between(test_data, tol_both, test_min, test_max))
  expect_failure(expect_between(test_data, too_low, test_min, test_max))
  expect_failure(expect_between(test_data, too_high, test_min, test_max))
  expect_failure(expect_between(test_data, both, test_min, test_max))
})

test_that("expect_success works properly", {
  test_data <- tribble(
    ~no_na, ~na, ~all_na,
    1, NA, NA,
    2, 1, NA
  )
  expect_success(expect_no_na(test_data, no_na))
  expect_failure(expect_no_na(test_data, na))
  expect_failure(expect_no_na(test_data, all_na))
})
