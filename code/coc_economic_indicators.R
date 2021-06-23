#' Fetch raw BLS unemployment rates
#'
#' Downloads flat text file with BLS unemployment data. Parses just unemployment
#' rate data from the download.
#'
#' @param url A string with the url of the flat text file.
#' @param min_year A number with the first year of data to keep.
#'
#' @return A data frame:
#' * `series`: BLS series ID 
#'     (see https://download.bls.gov/pub/time.series/la/la.txt)
#' * `year`: Year
#' * `month`: Month of the data (annual averages are "M13")
#' * `value`: Unemployment rate
#' * `footnote`: BLS footnote code
#'     (see https://download.bls.gov/pub/time.series/la/la.footnote)
#' @seealso [build_coc_unemployment()] for building CoC-level unemployment rates
fetch_unemployment = function(url, min_year = 2010) {
  read_tsv(
    url,
    col_names = c("series", "year", "month", "value", "footnote"),
    col_types = "cicnc",
    skip = 1,
    na = "-"
  ) %>% 
    # we only want the unemployment rate series. the last two digits of the
    # series id gives the measure and 03 is the unemployment rate.
    filter(str_sub(series, -2, -1) == "03", year >= min_year)
}

#' CoC-level unemployment rates
#'
#' Builds CoC level unemployment rates by taking a weighted average of county
#' unemployment rates from the [BLS Local Area Unemployment Statistics
#' (LAUS)](https://www.bls.gov/lau/). The weights are the share of the CoC's
#' population coming from each county.
#'
#' @param unemployment_data A data frame with raw unemployment data from
#'   [fetch_unemployment()].
#' @param county_crosswalk A data frame with a county to CoC crosswalk from
#'   [build_county_crosswalk()].
#'
#' @return A data frame:
#' * `coc_number`: CoC number
#' * `year`: Year
#' * `unemployment_rate`: CoC unemployment rate
#' @seealso [fetch_unemployment()] for downloading the BLS data
build_coc_unemployment = function(unemployment_data, county_crosswalk) {
  process_unemployment(unemployment_data) %>% 
    make_coc_unemployment(county_crosswalk)
}

#' Clean unemployment rates
#'
#' Grabs the chosen month's or annual average unemployment rate and parses the
#' county FIPS code.
#'
#' @inheritParams build_coc_unemployment
#' @param use_month A character value with the month to use or the annual
#'   average. For annual average use "M13". Defaults to January ("M01").
#'
#' @return A data frame:
#' * `county_fips`: County FIPS code
#' * `year`: Year
#' * `unemployment_rate`: Unemployment rate
#' 
#' @seealso [build_coc_unemployment()] for the main function
#' @keywords internal
process_unemployment = function(unemployment_data, use_month = "M01") {
  unemployment_data %>% 
    filter(month == use_month) %>% 
    mutate(
      county_fips = str_sub(series, 6, 10),
      unemployment_rate = value / 100) %>%
    select(county_fips, year, unemployment_rate)
}

#' Construct the CoC unemployment rate
#'
#' @inheritParams build_coc_unemployment 
#'
#' @return A data frame:
#' * `coc_number`: CoC number
#' * `year`: Year
#' * `unemployment_rate`: CoC unemployment rate
#'  
#' @seealso [build_coc_unemployment()] for the main function
#' @keywords internal
make_coc_unemployment = function(unemployment_data, county_crosswalk) {
  county_crosswalk %>% 
    left_join(unemployment_data, by = c("county_fips", "year")) %>% 
    group_by(coc_number, year) %>% 
    summarise(
      coc_unemployment_rate = weighted.mean(
        unemployment_rate, 
        pct_coc_pop_from_county,
        na.rm = TRUE
      ),
      # sum the share of the CoC population from counties missing an
      # unemployment rate
      share_na_coc_unemployment_rate = sum(
        pct_coc_pop_from_county[is.na(unemployment_rate)]
      ),
      .groups = "drop"
    )
}