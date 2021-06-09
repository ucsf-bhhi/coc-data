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

in_range <- function(data, column, min, max) {
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
  data %>%
    filter(!is.na({{ column }})) %>%
    pull({{ column }}) %>%
    between(min, max) %>%
    all()
}

get_value <- function(data, column, ...) {
  column_exists({{ data }}, {{ column }})
  f <- quos(...)
  data %>%
    filter(!!!f) %>%
    pull({{ column }})
}
