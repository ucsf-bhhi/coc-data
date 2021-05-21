library(sf)
library(tidyverse)
library(rmapshaper)
library(tidycensus)
source("code/helpers.R")

load_coc_shapefile <- function(year, prefix, shapefile_directory, layer = NULL) {
  specific_dir <- path(shapefile_directory, prefix)
  shapefile_path <- dir_ls(specific_dir, regexp = as.character(year))

  if (is.null(layer)) {
    st_read(shapefile_path)
  } else {
    st_read(shapefile_path)
  }
}

fetch_tract_data <- function(year, crs) {
  state_fips <- tidycensus::fips_codes %>%
    distinct(state_code, state_name) %>%
    filter(as.integer(state_code) < 60)

  map_dfr(state_fips$state_code, ~
  fetch_acs("tract", year = year, variables = c(tract_pop = "B01003_001", tract_pop_in_poverty = "B17001_002"), key = Sys.getenv("CENSUS_API_KEY"), state = .x, survey = "acs5", geometry = TRUE, output = "wide")) %>%
    rename(tract_fips = fips) %>%
    st_transform(crs)
}

clip_tracts <- function(tract_geodata, dissolved_cocs) {
  tracts_in_cocs <- ms_clip(tract_geodata, dissolved_cocs)
  tracts_not_in_cocs <- tract_geodata %>%
    filter(!(tract_fips %in% pluck(tracts_in_cocs$tract_fips))) %>%
    st_drop_geometry()
  return(list("tracts_in_cocs" = tracts_in_cocs, "tracts_not_in_cocs" = tracts_not_in_cocs))
}

match_tract_to_coc <- function(tract_points, coc_boundaries) {
  st_intersection(tract_points, coc_boundaries) %>%
    st_drop_geometry()
}

recombine_tracts <- function(tracts_in_cocs, tracts_not_in_cocs) {
  tracts_in_cocs %>%
    bind_rows(tracts_not_in_cocs) %>%
    mutate(
      county_fips = str_sub(tract_fips, 1, 5),
      state_fips = str_sub(tract_fips, 1, 2)
    ) %>%
    select(tract_fips, state_fips, county_fips, coc_number = COCNUM, coc_name = COCNAME, tract_pop, tract_pop_in_poverty)
}

build_tract_crosswalk <- function(recombined_tracts) {
  recombined_tracts %>%
    left_join(
      distinct(tidycensus::fips_codes, state_code, state_name),
      by = c("state_fips" = "state_code")
    ) %>%
    group_by(coc_number) %>%
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
    group_by(county_fips, coc_number) %>%
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

write_crosswalk <- function(crosswalk, year, type, output_directory) {
  filename <- paste(type, "coc_crosswalk", year, sep = "_")
  crosswalk_path <- path(output_directory, filename, ext = "csv")
  write_csv(crosswalk, crosswalk_path)
}

create_crosswalks <- function(year, shapefile_directory = "output_data/coc_shapefiles", output_directory = "output_data", crs = 2163) {
  coc_shapefile <- load_coc_shapefile(year, prefix = "simplified", shapefile_directory)
  coc_dissolved <- load_coc_shapefile(year, prefix = "dissolved_simplified", shapefile_directory)

  tracts <- fetch_tract_data(year, crs)

  clipped_tracts <- clip_tracts(tracts, coc_dissolved)

  tract_points_on_surface <- st_point_on_surface(clipped_tracts$tracts_in_cocs)

  tract_cocs <- match_tract_to_coc(tract_points_on_surface, coc_shapefile)

  recombined_tracts <- recombine_tracts(tract_cocs, clipped_tracts$tracts_not_in_cocs)

  tract_crosswalk <- build_tract_crosswalk(recombined_tracts)

  county_crosswalk <- build_county_crosswalk(tract_crosswalk)

  write_crosswalk(tract_crosswalk, year, "tract", output_directory)
  write_crosswalk(county_crosswalk, year, "county", output_directory)
}

walk(2013:2019, create_crosswalks)
