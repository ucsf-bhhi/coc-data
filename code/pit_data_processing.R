get_pit_years = function(filepath) {
  sheets = excel_sheets(filepath)
  sheets_filter = !is.na(suppressWarnings(as.numeric(sheets)))
  sort(sheets[sheets_filter])
}

# function that reads in a tab on the pit spreadsheet and does some light cleaning
parse_pit_year = function(filepath, year) {
  # read in the given year's tab
  read_excel(filepath, sheet = year) %>% 
    # add a year variable to the data
    mutate(year = as.numeric(year)) %>% 
    # move the year column after the CoC name column
    relocate(year, .after = `CoC Name`) %>% 
    # cut off the years from the variable names so they can be stacked across years
    rename_with(~ str_remove(.x, paste(",", year)),
                ends_with(year)) %>% 
    # make some of the variable names easier to reference (ie. lowercase and
    # underscores instead of spaces)
    rename_with(~ str_to_lower(str_replace(.x, " ", "_")), matches(c("CoC Number", "CoC Name", "CoC Category"))) %>% 
    # drop any rows that don't have a CoC name (ie. notes and empty rows on the spreadsheet)
    filter(!is.na(coc_name)) 
}

# reshape the pit data to long
get_long_pit_data = function(wide_pit_data) {
  wide_pit_data %>%
  # drop the coc_category since that is only in one year
  select(-coc_category) %>%
  # reshape long
  pivot_longer(-c(coc_number, coc_name, year), names_to = "category", values_to = "count")
}

# create a separate table containing the category for each CoC
get_coc_categories = function(wide_pit_data) {
  wide_pit_data %>%
  # category is only present in the last year of data so filter for that one
  filter(!is.na(coc_category)) %>%
  # just keep these variables around
  select(coc_number, coc_name, coc_category)
}



