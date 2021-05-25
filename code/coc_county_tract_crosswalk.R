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

write_crosswalk <- function(crosswalk, years, type, output_directory) {
  first_year = min(years)
  last_year = max(years)
  filename <- paste(type, "coc_crosswalk", first_year, last_year, sep = "_")
  crosswalk_path <- path(output_directory, filename, ext = "csv")
  write_csv(crosswalk, crosswalk_path)
  
  return(crosswalk_path)
}

# create_crosswalks <- function(year, shapefile_directory = "output_data/coc_shapefiles", output_directory = "output_data", crs = 2163) {

# write_crosswalk(tract_crosswalk, year, "tract", output_directory)
# write_crosswalk(county_crosswalk, year, "county", output_directory)


