#' Reads and processes raw Zillow rent index data
#'
#' Reads in the raw Zillow data, cleans up variable names and types, reshapes it
#' to long form, and calculates an annual average from the monthly data.
#'
#' @param file_path A character string with the path to the raw data.
#'
#' @return A data frame with the annual average rent by zip code.
#' * `zip`: zip code (character)
#' * `year`: year (integer)
#' * `annual_mean_rent`: average annual Zillow rent index (numeric)
#' @seealso [build_coc_zillow_rent()] for creating the CoC level Zillow rent
#'    index, [build_tract_zillow_rent()] for creating the census tract level
#'    Zillow rent index
process_zillow_data <- function(file_path) {
  read_csv(file_path) %>%
    # keep the zip code and the columns with the rent data (which start with 20)
    select(zip = RegionName, starts_with("20")) %>%
    pivot_longer(-zip, names_to = "year_month", values_to = "rent") %>%
    separate(year_month, c("year", "month")) %>%
    mutate(across(c(year, month), as.integer)) %>%
    # the zillow data are monthly, but we need annual data so we're just taking
    # a simple average of the months
    group_by(zip, year) %>%
    summarize(annual_mean_rent = mean(rent, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(annual_mean_rent))
}

#' Parses year from tract to zip crosswalk
#' 
#' @inheritParams process_tract_to_zip
#' 
#' @return An integer value with the year of the crosswalk.
#' @seealso [process_tract_to_zip()]
#' @keywords internal 
get_tract_to_zip_year <- function(file_path) {
  file_path %>%
    # extract just the file name from the path
    path_file() %>%
    # strip off the extension
    path_ext_remove() %>%
    # the year is the last 4 characters
    str_sub(-4, -1) %>%
    as.integer()
}

#' Reads and processes tract to zip code crosswalk
#'
#' Reads in the USPS census tract to zip code crosswalk, cleans up the variable
#' names and types, adds a year variable, and removes entries with no
#' residential units.
#'
#' @param file_path A character vector with the path to the crosswalk.
#'
#' @return A data frame with the processed crosswalk.
#' * `zip`: zip code (character)
#' * `tract_fips`: census tract FIPS code (character)
#' * `year`: year (integer)
#' * `res_ratio`: share of census tract's residential addresses in given zip
#'      code (numeric)
process_tract_to_zip <- function(file_path) {
  read_excel(file_path, col_types = "text") %>%
    rename_with(str_to_lower, everything()) %>%
    select(zip, tract_fips = tract, res_ratio) %>%
    mutate(
      year = get_tract_to_zip_year(file_path),
      res_ratio = as.numeric(res_ratio)
    ) %>%
    filter(res_ratio > 0)
}

#' Creates a census tract level Zillow rent index
#'
#' The census tract level index is created by taking a weighted average of the
#' original zip code level Zillow rent index for the zip codes in each census
#' tract. The weights are the share of the census tract's residences that are in
#' each zip code. That data comes from the USPS census tract to zip code
#' crosswalk.
#'
#' @param rent_data A data frame with processed zip code level Zillow rent index
#'   data created by [process_zillow_data()]
#' @param tract_to_zip A data frame with a processed USPS census tract to zip
#'   code crosswalk created by [process_tract_to_zip()].
#'
#' @return A data frame with the census tract level Zillow rent index.
#' * `tract_fips`: census tract FIPS code (character)
#' * `year`: year (integer)
#' * `tract_annual_mean_rent`: annual average Zillow rent index, NA for tracts
#'      with no zip codes covered by the Zillow rent index (numeric)
#' * `tract_share_na_rent`: share of the tract's residence in zip codes not
#'      covered by the Zillow rent index (numeric)
#' @seealso [process_zillow_data()] for processing the raw Zillow rent index,
#'    [process_tract_to_zip()] for processing the census tract to zip code
#'    crosswalk, [build_coc_zillow_rent()] for creating the CoC level Zillow 
#'    rent index
build_tract_zillow_rent <- function(rent_data, tract_to_zip) {
  tract_to_zip %>%
    left_join(rent_data, by = c("year", "zip")) %>%
    group_by(year, tract_fips) %>%
    # take the weighted average of the rent in the tract (weights are the share
    # of residences in a tract coming from each zip code)
    summarise(
      tract_annual_mean_rent = weighted.mean(
        annual_mean_rent,
        res_ratio,
        na.rm = TRUE
      ),
      # add up the share of the tract for which we don't have rent data
      tract_share_na_rent = sum(res_ratio[is.na(annual_mean_rent)]),
      .groups = "drop"
    )
}

#' Creates a CoC level Zillow rent index
#'
#' The CoC level index is created by taking a weighted average of the census
#' tract level Zillow rent index for the tracts in each CoC. The weights are the
#' share of the CoC's population that are in each tract.
#'
#' @param tract_rent A data frame with processed census tract level Zillow rent
#'   index data created by [build_tract_zillow_rent()].
#' @param tract_to_coc A data frame with a census tract to CoC crosswalk created
#'   by [build_tract_crosswalk()].
#'
#' @return A data frame with the CoC level Zillow rent index.
#' * `coc_number`: CoC number (character)
#' * `year`: year (integer)
#' * `coc_rent_zillow`: annual average Zillow rent index, NA for CoC
#'      with no zip codes covered by the Zillow rent index (numeric)
#' * `coc_share_na_rent`: share of the CoC's population in zip codes not
#'      covered by the Zillow rent index (numeric)
#' @seealso [process_zillow_data()] for processing the raw Zillow rent index,
#'    [build_coc_zillow_rent()] for creating the CoC level Zillow rent index,
#'    [build_tract_crosswalk()] for creating the census tract to CoC crosswalk
build_coc_zillow_rent <- function(tract_rent, tract_to_coc) {
  tract_to_coc %>%
    left_join(tract_rent, by = c("year", "tract_fips")) %>%
    group_by(coc_number, year) %>%
    summarise(
      # take the weighted average rent in each CoC (weights are share of the
      # CoC's population coming from the tracts)
      coc_rent_zillow = weighted.mean(
        tract_annual_mean_rent,
        pct_coc_pop_from_tract,
        na.rm = TRUE
      ),
      # add up the share of the CoC (based on population) coming from tract's
      # that don't have rent data
      coc_share_na_rent_zillow = sum(
        pct_coc_pop_from_tract * tract_share_na_rent,
        na.rm = TRUE
      )
    )
}
