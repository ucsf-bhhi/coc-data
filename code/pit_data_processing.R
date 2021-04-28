library(tidyverse)

# set the years for the pit data
first_year = 2007
last_year = 2019

# set the raw pit spreadsheet location and name
input_dir = "input_data"
output_dir = "output_data"
pit_spreadsheet_name = "2007-2019-Point-in-Time-Estimates-by-CoC.xlsx"

# function that reads in a tab on the pit spreadsheet and does some light cleaning
parse_pit_year = function(year) {
  # read in the given year's tab
  readxl::read_excel(file.path(input_dir, pit_spreadsheet_name), sheet = as.character(year)) %>% 
    # add a year variable to the data
    mutate(year = year) %>% 
    # move the year column after the CoC name column
    relocate(year, .after = `CoC Name`) %>% 
    # cut off the years from the variable names so they can be stacked across years
    rename_with(~ str_remove(.x, paste(",", year)),
                ends_with(as.character(year)))
}

# read in the data from the pit spreadsheet looping over the years
pit_data_raw = map_dfr(first_year:last_year, parse_pit_year) %>% 
  # make some of the variable names easier to reference
  rename(coc_number = `CoC Number`,
         coc_name = `CoC Name`,
         coc_category = `CoC Category`) %>% 
  # drop any rows that don't have a CoC name (ie. notes and empty rows on the spreadsheet)
  filter(!is.na(coc_name)) 

# reshape the pit data to long
pit_data = pit_data_raw  %>% 
  # drop the coc_category since that is only in one year
  select(-coc_category) %>% 
  # reshape long
  pivot_longer(-c(coc_number, coc_name, year), names_to = "category", values_to = "count")

# create a separate table containing the category for each CoC
coc_categories = pit_data_raw %>% 
  # category is only present in the last year of data so filter for that one
  filter(year == last_year) %>% 
  # just keep these variables around
  select(coc_number, coc_name, coc_category)

# set the name and location for the saved processed pit data
pit_data_output_name = paste0("pit_processed_", first_year, "_", last_year, ".csv")
pit_data_output_path = file.path(output_dir, pit_data_output_name)

# write out a csv with the processed pit data
write_csv(pit_data, pit_data_output_path)

# set the name and location for the saved CoC category data
coc_categories_output_name = paste0("coc_categories_", first_year, "_", last_year, ".csv")
coc_categories_output_path = file.path(output_dir, coc_categories_output_name)

# write out a csv with the CoC category data
write_csv(coc_categories, coc_categories_output_path)
