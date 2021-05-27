#' Gets ACS data from the Census API
#'
#' Requests ACS population data for all tracts in the 50 states + DC as well as
#' spatial data for the tracts. It also converts the spatial data to the given
#' CRS.
#'
#' @param year Year of the ACS data and tract boundaries to fetch
#' @param crs The EPSG code for the tract spatial data CRS
#'
#' @return An sf object with tract population data and boundaries
fetch_tract_data <- function(year, crs) {
  # start with the FIPS code table from the tidycensus package
  state_fips <- tidycensus::fips_codes %>%
    # just keep the distinct states (the table has sub-state geographies so
    # there are multiple entries for a state)
    distinct(state_code, state_name) %>%
    # filter out the territories (50 states & DC all have FIPS codes < 60)
    filter(as.integer(state_code) < 60)
  
  # iterate over the list of states fetching the ACS data
  map_dfr(state_fips$state_code, ~
  # hit the census API for tract population and boundaries
  fetch_acs("tract", year = year, variables = c(tract_pop = "B01003_001", tract_pop_in_poverty = "B17001_002"), key = Sys.getenv("CENSUS_API_KEY"), state = .x, survey = "acs5", geometry = TRUE, output = "wide")) %>%
    # clarify the tract FIPS code column name
    rename(tract_fips = fips) %>%
    # change the CRS for the tract boundaries
    st_transform(crs)
}

#' Trim tracts outside of CoC boundaries
#'
#' Not all Census tracts are in CoC boundaries, this separates out the tracts
#' that are not in a CoC.
#'
#' @param tract_geodata An sf object with the tract data & boundaries
#' @param dissolved_cocs An sf object with the internal boundaries dissolved
#'   since we only care at this point if a tract is in *any* CoC
#'
#' @return A list with two sf objects: one with the tracts that are in a CoC and
#'   one with the tracts that aren't in a CoC
clip_tracts <- function(tract_geodata, dissolved_cocs) {
  # grab the year of the tract data
  tract_year = unique(tract_geodata$year)
  # grab the year of the CoC data
  coc_year = unique(dissolved_cocs$year)
  # throw an error if the tract and CoC data are not from the same year
  stopifnot("tracts and dissolved coc shapefile not from same year" = tract_year == coc_year)
  
  # trim to just the tracts that intersect with a CoC
  tracts_in_cocs <- ms_clip(tract_geodata, dissolved_cocs)
  # get the tracts not in a CoC by starting with all the tracts
  tracts_not_in_cocs <- tract_geodata %>%
    # then filter to just the ones not in the tracts_in_coc table
    filter(!(tract_fips %in% pluck(tracts_in_cocs$tract_fips))) %>%
    # remove the boundary data from these tracts. they aren't in a CoC so we
    # don't need it anymore
    st_drop_geometry()
  
  # return a list with both tables
  return(list("tracts_in_cocs" = tracts_in_cocs, "tracts_not_in_cocs" = tracts_not_in_cocs))
}

#' Match Census Tracts to CoCs
#'
#' Matches Census Tracts to CoCs based on the CoC the center of the Census Tract
#' lies within.
#'
#' @param tract_points sf object with the center points of the Census Tracts
#' @param coc_boundaries sf object with the CoC boundaries
#'
#' @return A data frame with the tract, tract data, and its corresponding CoC
match_tract_to_coc <- function(tract_points, coc_boundaries) {
  # match up the tract centers and CoCs
  st_intersection(tract_points, coc_boundaries) %>%
    # strip off the geodata now that we're done with it
    st_drop_geometry()
}

#' Combine tables of tracts in and not in CoCs
#'
#' Takes the matched table of tracts in CoCs, joins it with the table of tracts
#' not in CoCs and cleans up the variables.
#'
#' @param tracts_in_cocs A data frame with tracts matched to CoCs
#' @param tracts_not_in_cocs A data frame with tracts not in CoCs
#'
#' @return A clean data frame of all tracts, their tract population data, and
#'   the corresponding CoC if there is one.
recombine_tracts <- function(tracts_in_cocs, tracts_not_in_cocs) {
  # start with the tracts that are in CoCs
  tracts_in_cocs %>%
    # combine with the tracts that aren't in CoCs
    bind_rows(tracts_not_in_cocs) %>%
    mutate(
      # add a column for the tract's county FIPS code (the first 5 characters of
      # the tract FIPS code)
      county_fips = str_sub(tract_fips, 1, 5),
      # add a column for the tract's state FIPS code (the first 2 characters of
      # the tract FIPS code)
      state_fips = str_sub(tract_fips, 1, 2)
    ) %>%
    # just keep these variables and give friendlier names for the coc_number and
    # coc_name columns
    select(year, tract_fips, state_fips, county_fips, coc_number = COCNUM, coc_name = COCNAME, tract_pop, tract_pop_in_poverty)
}

map_tracts_to_cocs = function(clipped_tracts, coc_boundaries) {
  clipped_tracts %>% 
    pluck("tracts_in_cocs") %>% 
    st_point_on_surface() %>% 
    match_tract_to_coc(coc_boundaries) %>% 
    recombine_tracts(clipped_tracts$tracts_not_in_cocs)
}

build_tract_crosswalk <- function(recombined_tracts) {
  recombined_tracts %>%
    left_join(
      distinct(tidycensus::fips_codes, state_code, state_name),
      by = c("state_fips" = "state_code")
    ) %>%
    group_by(coc_number, year) %>%
    mutate(
      coc_pop = sum(tract_pop),
      coc_poverty_pop = sum(tract_pop_in_poverty),
      pct_coc_pop_from_tract = tract_pop / coc_pop,
      pct_coc_poverty_pop_from_tract = tract_pop_in_poverty / coc_poverty_pop,
      across(c(coc_pop, coc_poverty_pop, pct_coc_pop_from_tract, pct_coc_poverty_pop_from_tract), ~ if_else(is.na(coc_number), NA_real_, .x))
    ) %>%
    relocate(state_name, .after = state_fips)
}

build_county_crosswalk <- function(tract_crosswalk) {
  tract_crosswalk %>%
    group_by(county_fips, coc_number, year) %>%
    summarise(across(c(state_fips, state_name, coc_name, coc_pop, coc_poverty_pop), first),
      county_pop_in_coc = sum(tract_pop),
      county_poverty_pop_in_coc = sum(tract_pop_in_poverty)
    ) %>%
    group_by(county_fips) %>%
    mutate(
      county_pop = sum(county_pop_in_coc),
      county_pop_in_poverty = sum(county_poverty_pop_in_coc),
      pct_coc_pop_from_county = county_pop_in_coc / coc_pop,
      pct_coc_poverty_pop_from_county = county_poverty_pop_in_coc / coc_poverty_pop,
      pct_county_pop_in_coc = county_pop_in_coc / county_pop,
      pct_county_poverty_pop_in_coc = county_poverty_pop_in_coc / county_pop_in_poverty
    )
}

write_crosswalk <- function(crosswalk, type, output_directory) {
  filename <- paste(type, "coc_crosswalk", sep = "_")
  crosswalk_path <- path(output_directory, filename, ext = "csv")
  write_csv(crosswalk, crosswalk_path)
  
  return(crosswalk_path)
}
