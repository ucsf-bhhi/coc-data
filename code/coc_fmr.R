#' Read and process raw FMR data
#'
#' Reads in the raw FMR spreadsheets, standardizes the FIPS code and FMR
#' variables, removes unneeded variables, and counts the number of records per
#' county. 
#' 
#' Counting the records per county is necessary because unlike the rest
#' of the U.S, which uses groups of counties for FMR areas, New England states
#' create FMR areas with county subdivisions, so a county can be in more than
#' one FMR. This flags those records so we can deal with them further down the
#' line.
#'
#' @param raw_fmr_path A character string with the file path of the raw FMR
#'   spreadsheet
#'
#' @return A data frame with the processed FMR data.
process_fmr <- function(raw_fmr_path) {
  read_excel(raw_fmr_path, col_types = "text") %>%
    rename_with(str_to_lower, everything()) %>%
    # add a column with the year of the data which we get from parsing the filepath
    mutate(year = parse_number(raw_fmr_path)) %>%
    select(year, starts_with("fips"), starts_with("fmr"), -ends_with("chg"), -matches("fips2000"), state, county, metro_code, areaname, metro) %>% 
    # sometimes the FIPS code is called fips and sometimes fips2010 so
    # standardize it as fips10
    rename_with(~ "fips10", starts_with("fips")) %>%  
    #create a 5-digit version of the fips code that has just state and county
    mutate(fips5 = str_sub(fips10, 1, 5)) %>%
    # sometimes there's an underscore in the fmr columns so standardize without
    # the underscore
    rename_with(~ str_remove(.x, "_"), starts_with("fmr")) %>% 
    mutate(across(starts_with("fmr"), as.numeric)) %>% 
    select(fips10, year, fmr0, fmr1, fmr2, fmr3, fmr4, fips5, state, county, metro_code, areaname, metro) %>%
    # count how many records we have per county (some counties are subdivided
    # into multiple fmr areas, we'll deal with that later)
    group_by(year, state, county) %>%
    mutate(count = n()) %>% 
    ungroup()
}

#' @describeIn get_acs_county_sub Finds the states with sub-county FMRs
#'
#' @param processed_fmr 
#'
#' @return
get_states_with_split_counties = function(processed_fmr) {
  processed_fmr %>%
    filter(count > 1) %>%
    # pops the column out from the data frame and stores it as a vector
    pull(state) %>%
    # we just want the unique values (ie. states)
    unique()
}

#' Fetches ACS renter household counts
#'
#' Finds the states that have sub-county FMR areas and fetches renter household
#' counts in those states at the county subdivision level.
#'
#' @param year Year of the ACS data
#' @param processed_fmr Data frame with processed FMRs
#'
#' @return A data frame with renter household counts.
get_acs_county_sub = function(year, processed_fmr) {
  states = get_states_with_split_counties(processed_fmr)
  
  fetch_acs("county subdivision", variables = c(renting_households = "B25003_003"), year = year, state = states, key = Sys.getenv("CENSUS_API_KEY"), output = "wide", survey = "acs5") %>% 
    rename(fips10 = fips) 
}

#' @describeIn build_coc_fmr Convert sub-county FMRs to county FMRs 
#' 
#' Calculates a weighted average FMR for the counties that are in more than one
#' FMR region. County subdivisions are unique to an FMR area so we take the
#' weighted average of FMRs in the county subdivisions to get the county level
#' FMR. 
#' 
#' @param processed_fmr Data frame with processed FMRs
#' @param acs Data frame with the county subdivision renter household counts
#' 
#' @return
process_split_county_fmr = function(processed_fmr, acs) {
  processed_fmr %>% 
    filter(count > 1) %>% 
    left_join(acs, by = c("fips10", "year"))  %>%
    # there are some fips10 codes that don't have ACS data
    # set those renter household counts to 0 so weighted.mean doesn't return
    # na (na.rm doesn't apply to the weights in weighted.mean for some reason)
    mutate(renting_households = replace_na(renting_households, 0)) %>%
    group_by(fips5, year) %>%
    # calculate the weighted average for each FMR
    summarize(across(starts_with("fmr"), ~ weighted.mean(.x, renting_households, na.rm = TRUE)),
              .groups = "drop") %>% 
    ungroup()
}

#' Builds CoC level FMRs
#'
#' Calculates a CoC level FMR by taking a weighted average of the county FMRs in
#' the CoC. The weights are the share of the CoC population from each county.
#'
#' @param processed_fmr Data frame with processed FMRs
#' @param acs Data frame with the county subdivision renter household counts
#' @param crosswalk Data frame with the county to CoC crosswalk
#'
#' @return A data frame with CoC level FMRs.
build_coc_fmr = function(processed_fmr, acs, crosswalk) {
  process_split_county_fmr(processed_fmr, acs) %>% 
    bind_rows(filter(processed_fmr, count == 1)) %>% 
    right_join(crosswalk, by = c("fips5" = "county_fips", "year")) %>% 
    group_by(coc_number, year) %>%
    summarise(
      # calculate the weighted average for each FMR
      # (weights are county share of CoC population)
      across(starts_with("fmr"), ~ weighted.mean(.x, pct_coc_pop_from_county, na.rm = TRUE), .names = "avg_{.col}"),
      # calculate the share of each CoC that was missing an FMR
      across(starts_with("fmr"), ~ sum(pct_coc_pop_from_county[is.na(.x)]), .names = "{.col}_pct_coc_na_rent"),
      .groups = "drop"
    )
}

