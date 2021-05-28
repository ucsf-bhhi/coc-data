#' Parse the years covered by the PIT counts
#'
#' Analyzes the PIT count excel file to pull out the years for which the file
#' has data. It expects a file where each year is in its own sheet and the name
#' of the sheet is just the year.
#'
#' @param filepath Path to the PIT count excel file
#'
#' @return A character vector with the years in the file.
get_pit_years <- function(filepath) {
  # get the names of the sheets in the file
  sheets <- excel_sheets(filepath)
  # TRUE if the tab name is just a number, FALSE otherwise
  sheets_filter <- !is.na(suppressWarnings(as.numeric(sheets)))
  # subset to just the sheet names that are numbers (TRUE in the filter above)
  # and sort
  sort(sheets[sheets_filter])
}


#' Read and clean raw PIT count data
#'
#' Reads in a tab on the pit spreadsheet and does some light cleaning by adding
#' a year column and making friendlier column names.
#'
#' @param filepath Path to the PIT count excel file
#' @param year Year of the data to parse
#'
#' @return A data frame with the processed PIT counts in wide form.
parse_pit_year <- function(filepath, year) {
  # read in the given year's tab
  read_excel(filepath, sheet = year) %>%
    # add a year variable to the data
    mutate(year = as.numeric(year)) %>%
    # move the year column after the CoC name column
    relocate(year, .after = `CoC Name`) %>%
    # cut off the years from the variable names so they can be stacked across years
    rename_with(
      ~ str_remove(.x, paste(",", year)),
      ends_with(year)
    ) %>%
    # make some of the variable names easier to reference (ie. lowercase and
    # underscores instead of spaces)
    rename_with(~ str_to_lower(str_replace(.x, " ", "_")), matches(c("CoC Number", "CoC Name", "CoC Category"))) %>%
    # drop any rows that don't have a CoC name (ie. notes and empty rows on the spreadsheet)
    filter(!is.na(coc_name))
}


#' Reshapes the PIT count data to long form
#'
#' Puts the PIT counts in long form with the count categories in the `category`
#' column and the count values in the `count` column.
#'
#' @param wide_pit_data A data frame with the PIT counts in wide form
#'
#' @return A data frame with the processed PIT counts in long form.
get_long_pit_data <- function(wide_pit_data) {
  wide_pit_data %>%
    # drop the coc_category since that is only in one year
    select(-coc_category) %>%
    # reshape long
    pivot_longer(-c(coc_number, coc_name, year), names_to = "category", values_to = "count")
}


#' Creates a table of urban/rural CoC categories
#'
#' @param wide_pit_data A data frame with the PIT counts in wide form.
#'
#' @return A data frame with the urban/rural status for each CoC.
get_coc_categories <- function(wide_pit_data) {
  wide_pit_data %>%
    # category is only present in the last year of data so filter for that one
    filter(!is.na(coc_category)) %>%
    # just keep these variables around
    select(coc_number, coc_name, coc_category)
}
