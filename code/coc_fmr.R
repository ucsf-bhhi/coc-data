# function that reads in the raw FMR spreadsheets and does some light processing
process_fmr <- function(raw_fmr_path) {
  # read in the spreadsheet and set all columns to text
  read_excel(raw_fmr_path, col_types = "text") %>%
    # set all column names to lowercase
    rename_with(str_to_lower, everything()) %>%
    # add a column with the year of the data which we get from parsing the filepath
    mutate(year = parse_number(raw_fmr_path)) %>%
    # only keep certain columns from the raw data
    select(year, starts_with("fips"), starts_with("fmr"), -ends_with("chg"), -matches("fips2000"), state, county, metro_code, areaname, metro) %>% 
    # sometimes the FIPS code is called fips and sometimes fips2010 so
    # standardize it as fips10
    rename_with(~ "fips10", starts_with("fips")) %>%  
    #create a 5-digit version of the fips code that has just state and county
    mutate(fips5 = str_sub(fips10, 1, 5)) %>%
    # sometimes there's an underscore in the fmr columns so standardize without
    # the underscore
    rename_with(~ str_remove(.x, "_"), starts_with("fmr")) %>% 
    # convert the fmr columns to numeric
    mutate(across(starts_with("fmr"), as.numeric)) %>% 
    # just keep around the geo ids, year, and fmr's
    select(fips10, year, fmr0, fmr1, fmr2, fmr3, fmr4, fips5, state, county, metro_code, areaname, metro) %>%
    # count how many records we have per county (some counties are subdivided
    # into multiple fmr areas, we'll deal with that later)
    group_by(year, state, county) %>%
    mutate(count = n()) %>% 
    ungroup()
}

get_states_with_split_counties = function(processed_fmr) {
  processed_fmr %>%
    filter(count > 1) %>%
    # pops the column out from the data frame and stores it as a vector
    pull(state) %>%
    # we just want the unique values (ie. states)
    unique()
}

get_acs_county_sub = function(year, processed_fmr) {
  states = get_states_with_split_counties(processed_fmr)
  
  fetch_acs("county subdivision", variables = c(renting_households = "B25003_003"), year = year, state = states, key = Sys.getenv("CENSUS_API_KEY"), output = "wide", survey = "acs5") %>% 
    rename(fips10 = fips) 
}

# calculate a weighted average FMR for the counties in more than one FMR region
# county subdivisions are unique to an FMR area so we can take the weighted average of FMRs in the county subdivisions
process_split_county_fmr = function(processed_fmr, acs) {
  processed_fmr %>% 
    filter(count > 1) %>% 
    # merge on the ACS renter household counts
    left_join(acs, by = c("fips10", "year"))  %>%
    # there are some fips10 codes that don't have ACS data
    # set those renter household counts to 0 so weighted.mean doesn't return
    # na (na.rm doesn't apply to the weights in weighted.mean for some reason)
    mutate(renting_households = replace_na(renting_households, 0)) %>%
    # group by county and year
    group_by(fips5, year) %>%
    # calculate the weighted average for each FMR
    summarize(across(starts_with("fmr"), ~ weighted.mean(.x, renting_households, na.rm = TRUE)),
              .groups = "drop") %>% 
    ungroup()
}

build_coc_fmr = function(processed_fmr, acs, crosswalk) {
  process_split_county_fmr(processed_fmr, acs) %>% 
    bind_rows(filter(processed_fmr, count == 1)) %>% 
    # merge on the crosswalk keeping just the year/county combos in the crosswalk
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

