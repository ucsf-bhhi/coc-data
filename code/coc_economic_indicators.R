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

#' Fetch ACS public program use data
#'
#' Fetches Census Tract level enrollment in SNAP, public assistance, SSI, and
#' Medicaid from the Census API. Medicaid data is for age 19-64.
#'
#' @param year A numeric with the year of the data to fetch.
#'
#' @return A data frame:
#' * `fips`: Census Tract FIPS code
#' * `year`: Year
#' * `total_hh_snap`: Total number of households from the SNAP enrollment table
#' * `hh_with_snap`: Number of households receiving SNAP benefits
#' * `total_hh_pub_assist`: Total number of households from the public
#'     assistance enrollment table
#' * `hh_with_pub_assist`: Number of households receiving public assistance
#'     benefits
#' * `total_hh_ssi`: Total number of households from the SSI enrollment table
#' * `hh_with_ssi`: Number of households receiving SSI benefits
#' * `total_male_19_64`: Total number of males age 19-64
#' * `male_19_64_with_medicaid`: Number of males age 19-64 enrolled in Medicaid
#' * `total_female_19_64`: Total number of females age 19-64
#' * `female_19_64_with_medicaid`: Number of females age 19-64 enrolled in
#'     Medicaid
#' @seealso [build_coc_public_program_use()] for CoC public program utilization
#'   rates
fetch_public_program_use <- function(year) {
  acs_variables <- c(
    "total_hh_snap" = "B22001_001",
    "hh_with_snap" = "B22001_002",
    "total_hh_pub_assist" = "B19057_001",
    "hh_with_pub_assist" = "B19057_002",
    "total_hh_snap_or_pub_assist" = "B19058_001",
    "hh_with_snap_or_pub_assist" = "B19058_002",
    "total_hh_ssi" = "B19056_001",
    "hh_with_ssi" = "B19056_002",
    "total_male_19_64" = "C27007_006",
    "male_19_64_with_medicaid" = "C27007_007",
    "total_female_19_64" = "C27007_016",
    "female_19_64_with_medicaid" = "C27007_017"
  )

  states <- tidycensus::fips_codes %>%
    distinct(state_code) %>%
    filter(state_code < 60) %>%
    pull()

  map_dfr(
    states,
    ~ fetch_acs("tract", state = .x, year = year, output = "wide",
                variables = acs_variables)
  )
}

#' CoC public program utilization rates
#'
#' Builds CoC public program utilization rates by summing total enrollment in
#' the Census Tracts within each CoC and dividing by the relevant total
#' population in the CoC.
#'
#' @param acs_data A data frame with tract-level enrollment counts from
#'   [fetch_public_program_use()].
#' @param tract_crosswalk A data frame with a tract to CoC crosswalk from
#'   [build_tract_crosswalk()].
#'
#' @return A data frame: 
#' * `coc_number`: CoC number 
#' * `year`: Year 
#' * `share_hh_with_snap`: Share of households receiving SNAP benefits
#' * `share_hh_with_pub_assist`: Share of households receiving public assistance
#'     benefits 
#' * `share_hh_with_ssi`: Share of households receiving SSI benefits
#' * `share_with_medicaid`: Share of individuals age 19-64 enrolled in
#'     Medicaid
build_coc_public_program_use <- function(acs_data, tract_crosswalk) {
  tract_crosswalk %>%
    left_join(acs_data, by = c("tract_fips" = "fips", "year")) %>%
    group_by(coc_number, year) %>%
    summarise(
      across(
        c(
          "total_hh_snap",
          "hh_with_snap",
          "total_hh_pub_assist",
          "hh_with_pub_assist",
          "total_hh_snap_or_pub_assist",
          "hh_with_snap_or_pub_assist",
          "total_hh_ssi",
          "hh_with_ssi",
          "total_male_19_64",
          "male_19_64_with_medicaid",
          "total_female_19_64",
          "female_19_64_with_medicaid"
        ),
        sum, na.rm = TRUE
      ),
      .groups = "keep"
    ) %>%
    transmute(
      share_hh_with_snap = hh_with_snap / total_hh_snap,
      share_hh_with_pub_assist = hh_with_pub_assist / total_hh_pub_assist,
      share_hh_with_snap_or_pub_assist =
        hh_with_snap_or_pub_assist / total_hh_snap_or_pub_assist,
      share_hh_with_ssi = hh_with_ssi / total_hh_ssi,
      total_19_64_with_medicaid =
        male_19_64_with_medicaid + female_19_64_with_medicaid,
      total_19_64 = total_male_19_64 + total_female_19_64,
      share_with_medicaid = total_19_64_with_medicaid / total_19_64
    ) %>%
    select(-total_19_64_with_medicaid, -total_19_64) %>%
    ungroup()
}
