library(targets, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(assertr, warn.conflicts = FALSE)

count_na <- function(data, column) {
  data %>%
    pull({{ column }}) %>%
    is.na() %>%
    sum()
}

in_range <- function(data, column, min, max) {
  data %>%
    filter(!is.na({{ column }})) %>%
    pull({{ column }}) %>%
    between(min, max) %>%
    all()
}

get_value <- function(data, column, ...) {
  f <- quos(...)
  data %>%
    filter(!!!f) %>%
    pull({{ column }})
}
