library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)
source("code/helpers.R")
source("code/process_coc_shapefiles.R")
source("code/coc_county_tract_crosswalk.R")
tar_option_set(packages = c("tidyverse", "sf", "rmapshaper", "tidycensus", "fs"))

list(
  tar_files_input(
    raw_coc_shapefiles,
    fs::dir_ls("input_data/geography/coc_shapefiles", recurse = 1, regex = build_regex()),
    format = "file"
  ),
  tar_target(
    shapefile_years,
    parse_number(raw_coc_shapefiles)
  ),
  tar_target(
    original_coc_shapefiles,
    read_raw_coc_shapefile(raw_coc_shapefiles),
    pattern = map(raw_coc_shapefiles),
    iteration = "list"
  ),
  tar_target(
    simplified_coc_shapefiles, 
    simplify_shapefile(original_coc_shapefiles),
    pattern = map(original_coc_shapefiles),
    iteration = "list"
  ),
  tar_target(
    dissolved_coc_shapefiles, 
    ms_dissolve(simplified_coc_shapefiles, copy_fields = "year"),
    pattern = map(simplified_coc_shapefiles),
    iteration = "list"
  ),
  tar_target(
    tracts, 
    fetch_tract_data(shapefile_years, crs = 2163),
    pattern = map(shapefile_years),
    iteration = "list"
  ),
  tar_target(
    clipped_tracts, 
    clip_tracts(tracts, dissolved_coc_shapefiles),
    pattern = map(tracts, dissolved_coc_shapefiles),
    iteration = "list"
  ),
  tar_target(
    tract_cocs, 
    map_tracts_to_cocs(clipped_tracts, simplified_coc_shapefiles),
    pattern = map(clipped_tracts, simplified_coc_shapefiles),
    iteration = "list"
  ),
  tar_target(
    tract_crosswalk, 
    build_tract_crosswalk(tract_cocs),
    pattern = map(tract_cocs)
  ),
  tar_target(
    county_crosswalk, 
    build_county_crosswalk(tract_crosswalk),
    pattern = map(tract_crosswalk)
  ),
  tar_target(
    save_tract_crosswalk, 
    write_crosswalk(tract_crosswalk, "tract", output_directory = "output_data/"),
    format = "file"
  ),
  tar_target(
    save_county_crosswalk,
    write_crosswalk(county_crosswalk, "county", output_directory = "output_data/"),
    format = "file"
  )
)
