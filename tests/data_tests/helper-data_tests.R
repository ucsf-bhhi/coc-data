library(targets, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

tar_config_set(file.path(usethis::proj_path(), "_targets"))

count_na = function(data, column) {
  data %>% 
    pull({{ column }}) %>% 
    is.na() %>% 
    sum()
}

in_range = function(data, column, min, max) {
  data %>% 
    pull({{ column }}) %>% 
    between(min, max) %>% 
    all()
}

get_value = function(data, column, ...) {
  f = quos(...)
  data %>%   
    filter(!!!f) %>% 
    pull({{ column }})
}
