fetch_acs = function(...) {
  # put the arguments in a list
  dots = list(...)
  
  # the year argument is optional in get_acs and defaults to 2019
  # if the call doesn't specify the year and thus uses the default we still want to have a year column
  # so check if the year argument was specified (it's null if it wasn't)
  if (is.null(dots[["year"]])) {
    # if it wasn't set our yr (which goes in the year column we create) to 2019
    yr = 2019
  } else {
    # if it was grab it, so we can put it into the year column 
    yr = dots[["year"]]
  } 
  
  # call get_acs
  get_acs(...) %>% 
    # we only want the fips code and estimates
    # rename the GEOID columns as fips
    select(fips = GEOID, 
           # we want to keep the variable & estimate columns when get_acs is run in tidy mode
           matches("variable"), 
           matches("estimate"), 
           # when get_acs is run in wide mode the estimate columns always have the last letter E, so we keep all columns ending in E
           ends_with("E"), 
           # moe and name end in E, but we don't want them, so explicitly drop them
           -matches("moe"), 
           -matches("NAME")) %>% 
    # add the year column to the data
    mutate(year = yr) %>%
    # put the year column after the fips column
    relocate(year, .after = fips)
}
