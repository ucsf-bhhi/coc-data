#' Gets ACS data from the Census API
#'
#' Requests ACS population data for all tracts in the 50 states + DC as well as
#' spatial data for the tracts. It also converts the spatial data to the given
#' CRS.
#'
#' @param year Year of the ACS data and tract boundaries to fetch
#' @param crs The EPSG code for the tract spatial data CRS
#'
#' @return A spatial data frame:
#' * `tract_fips`: Census Tract FIPS code
#' * `year`: Year
#' * `tract_pop`: Census Tract population
#' * `tract_poverty_pop`: Census Tract population below poverty line
#' * `tract_renting_hh`: Number of renter households in Census Tract
#' * `geometry`: Census Tract border spatial data
#' 
#' @seealso [match_tracts_to_cocs()], [build_tract_crosswalk()], [build_county_crosswalk()]
fetch_tract_data <- function(year, crs) {
  fetch_acs_tracts(
    year,
    variables = c(
      tract_pop = "B01003_001",
      tract_poverty_pop = "B17001_002",
      tract_renting_hh = "B25003_003"
    ),
    geometry = TRUE,
    output = "wide"
  ) %>%
    # clarify the tract FIPS code column name
    rename(tract_fips = fips) %>%
    # change the CRS for the tract boundaries
    st_transform(crs) %>%
    # all we need is the center point of the tract
    st_point_on_surface()
}

#' Match Census Tracts to CoCs
#'
#' Matches Census Tracts to CoCs based on the CoC the center of the Census Tract
#' lies within.
#'
#' @param tract_points sf object with the center points of the Census Tracts
#' @param coc_boundaries sf object with the CoC boundaries
#' 
#' @return A data frame:
#' * `tract_fips`: Census Tract FIPS code
#' * `coc_number`: CoC number
#' * `coc_name`: CoC name
#' * `year`: Year
#' * `tract_pop`: Census Tract population
#' * `tract_poverty_pop`: Census Tract population below poverty line
#' * `tract_renting_hh`: Number of renter households in the Census Tract
#' 
#' @seealso [fetch_tract_data()], [build_tract_crosswalk()], [build_county_crosswalk()]
match_tracts_to_cocs <- function(tract_points, coc_boundaries) {
  # match up the tract centers and CoCs
  st_intersection(tract_points, coc_boundaries) %>%
    # strip off the spatial now that we're done with it
    st_drop_geometry() %>%
    # just need the CoC info
    select(tract_fips, coc_number, coc_name) %>%
    # tracts that aren't in a CoC get dropped above so add them back in
    right_join(st_drop_geometry(tract_points))
}

#' Creates the tract to CoC crosswalk
#'
#' Calculates the CoC population and share of the CoC coming from each tract.
#' Both for the total population and the population under the poverty line.
#'
#' @param recombined_tracts A data frame of tracts, their populations, and the
#'   corresponding CoC
#'
#' @return A data frame:
#' * `tract_fips`: Census Tract FIPS code
#' * `coc_number`: CoC number
#' * `coc_name`: CoC name
#' * `year`: Year
#' * `tract_pop`: Census Tract population
#' * `tract_poverty_pop`: Census Tract population below poverty line
#' * `tract_renting_hh`: Number of renter households in the Census Tract
#' * `coc_pop`: CoC population
#' * `coc_poverty_pop`: CoC population below poverty line
#' * `coc_renting_hh`: Number of renter households in the CoC
#' * `pct_coc_pop_from_tract`: Share of the CoC population from the Census Tract
#' * `pct_coc_renting_hh_from_tract`: Share of the CoC renter households from the Census Tract
#' 
#' @seealso [fetch_tract_data()], [match_tracts_to_cocs()], [build_county_crosswalk()]
build_tract_crosswalk <- function(recombined_tracts) {
  # start with the tract/CoC table
  recombined_tracts %>%
    # calculate yearly CoC level stats by summing over the tracts in each CoC
    group_by(coc_number, year) %>%
    mutate(
      coc_pop = sum(tract_pop),
      coc_poverty_pop = sum(tract_poverty_pop, na.rm = TRUE),
      coc_renting_hh = sum(tract_renting_hh),
      pct_coc_pop_from_tract = tract_pop / coc_pop,
      pct_coc_renting_hh_from_tract = tract_renting_hh / coc_renting_hh,
      # make sure these variables are NA for tracts that aren't in a CoC (where
      # coc_number is NA)
      across(
        c(coc_pop, coc_poverty_pop, coc_renting_hh, pct_coc_pop_from_tract, pct_coc_renting_hh_from_tract),
        ~ if_else(is.na(coc_number), NA_real_, .x)
      )
    ) %>%
    # remove grouping for when it's used later
    ungroup()
}

#' Creates the county to CoC crosswalk
#'
#' Calculates the CoC population, share of the CoC coming from each tract, and share of each CoC in each county.
#' Both for the total population and the population under the poverty line.
#'
#' @param tract_crosswalk A tract to CoC crosswalk
#'
#' @return A data frame:
#' * `county_fips`: County FIPS code
#' * `coc_number`: CoC number
#' * `coc_name`: CoC name
#' * `year`: Year
#' * `coc_pop`: CoC population
#' * `coc_renting_hh`: Number of renter households in the CoC
#' * `county_pop_in_coc`: Number of county residents in the CoC
#' * `county_renting_hh_in_coc`: Number of county renting households in the CoC
#' * `county_pop`: County population
#' * `county_renting_hh`: Number of renter households in the county
#' * `pct_coc_pop_from_county`: Share of the CoC population from the county
#' * `pct_coc_renting_hh_from_county`: Share of the CoC renter households from the county
#' * `pct_county_pop_in_coc`: Share of the county population in the CoC
#' * `pct_county_renting_hh_in_coc`:Share of the county renter households in the CoC
#' @seealso [fetch_tract_data()], [match_tracts_to_cocs()], [build_tract_crosswalk()]
build_county_crosswalk <- function(tract_crosswalk) {
  # start with the tract crosswalk
  tract_crosswalk %>%
    # county fips is the first five digits of the tract fips
    mutate(county_fips = str_sub(tract_fips, 1, 5)) %>%
    # sum the county population in the CoC by grouping the tracts by county,
    # coc, and year
    group_by(county_fips, coc_number, year) %>%
    # grab the first value of these label columns that are the same for every
    # tract in a given CoC
    summarise(across(c(coc_name, coc_pop, coc_renting_hh), first),
      county_pop_in_coc = sum(tract_pop),
      county_renting_hh_in_coc = sum(tract_renting_hh)
    ) %>%
    # now, grouping by county sum the county population, % of CoC from the
    # county, and % of the county in each of its CoCs
    group_by(county_fips, year) %>%
    mutate(
      # sum the total county population by adding up its population in each CoC
      # it's part of
      county_pop = sum(county_pop_in_coc),
      county_renting_hh = sum(county_renting_hh_in_coc),
      pct_coc_pop_from_county = county_pop_in_coc / coc_pop,
      pct_coc_renting_hh_from_county = county_renting_hh_in_coc / coc_renting_hh,
      pct_county_pop_in_coc = county_pop_in_coc / county_pop,
      pct_county_renting_hh_in_coc = county_renting_hh_in_coc / county_renting_hh
    ) %>%
    # get rid of the counties and pieces of counties which are not in CoCs
    filter(!is.na(coc_number)) %>%
    # remove grouping for when it's used later
    ungroup()
}

#' CoC populations and poverty rates
#' 
#' Provides CoC level total population, population in poverty, and poverty rate.
#'
#' @param tract_crosswalk A data frame with a tract to CoC crosswalk produced by [build_tract_crosswalk()].
#'
#' @return A data frame.
#' * `year`: Year (numeric)
#' * `coc_number`: CoC number (character)
#' * `coc_pop`: CoC population (numeric)
#' * `coc_poverty_pop`: CoC population below poverty line (numeric)
#' * `coc_poverty_rate`: Share of CoC population below poverty line (numeric)
#' @seealso [build_tract_crosswalk()] for the source data
build_coc_populations = function(tract_crosswalk) {
  tract_crosswalk %>% 
    # remove tracts that are not in CoCs
    filter(!is.na(coc_number)) %>% 
    distinct(
      year,
      coc_number,
      coc_pop,
      coc_poverty_pop
    ) %>% 
    mutate(coc_poverty_rate = coc_poverty_pop / coc_pop)
}
