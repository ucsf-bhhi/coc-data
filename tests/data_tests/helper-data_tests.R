library(targets, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(assertr, warn.conflicts = FALSE)
library(glue, warn.conflicts = FALSE)
library(rlang, warn.conflicts = FALSE)
library(crayon, warn.conflicts = FALSE)
column_exists <- function(data, column) {
  column_name <- as_name(enquo(column))
  data_name <- as_name(enquo(data))
  
  check = rlang::has_name(quo_squash(data), column_name)
  
  if (!check) {
     msg = glue_col("{red column '{column_name}' not present in '{data_name}'}")
     stop(msg, call. = FALSE)
  }
}

count_na <- function(data, column) {
  data %>%
    pull({{ column }}) %>%
    is.na() %>%
    sum()
}

expect_no_na <- function(data, column) {
  column_exists({{ data }}, {{ column }})
  
  na_count <- count_na(data, {{ column }})

  column_name = as_name(enquo(column))
  
  expect(
    na_count == 0,
    glue_col("{red {column_name} has {na_count} NAs}")
  )

  invisible(data)
}

test_that("na's computed correctly", {
  test_data <- tribble(
    ~no_na, ~na, ~all_na,
    1, NA, NA,
    2, 1, NA
  )
  expect_success(expect_no_na(test_data, no_na))
  expect_failure(expect_no_na(test_data, na))
  expect_failure(expect_no_na(test_data, all_na))
})

in_between <- function(x, min, max, tolerance = 0L) {
  x >= (min - tolerance) & x <= (max + tolerance)
}

tt_tol <- testthat::testthat_tolerance()

in_range <- function(data, column, min, max, tolerance = tt_tol) {
  data %>%
    filter(!is.na({{ column }})) %>%
    pull({{ column }}) %>%
    in_between(min, max, tolerance) %>%
    all()
}

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

get_value <- function(data, column, ...) {
  column_exists({{ data }}, {{ column }})
  f <- quos(...)
  data %>%
    filter(!!!f) %>%
    pull({{ column }})
}
