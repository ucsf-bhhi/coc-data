library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)
source("code/helpers.R")
source("code/coc_county_tract_crosswalk.R")
tar_option_set(packages = c("tidyverse", "sf", "rmapshaper", "tidycensus", "fs"))

list(
  tar_target(shapefile_year, 2013:2019),
  tar_target(
    coc_shapefile,
    load_coc_shapefile(shapefile_year, "simplified", "output_data/coc_shapefiles"),
    pattern = map(shapefile_year),
    iteration = "list"
  ),
  tar_target(
    coc_dissolved, 
    load_coc_shapefile(shapefile_year, "dissolved_simplified", "output_data/coc_shapefiles"),
    pattern = map(shapefile_year),
    iteration = "list"
  ),
  tar_target(
    tracts, 
    fetch_tract_data(shapefile_year, crs = 2163),
    pattern = map(shapefile_year),
    iteration = "list"
  ),
  tar_target(
    clipped_tracts, 
    clip_tracts(tracts, coc_dissolved),
    pattern = map(tracts, coc_dissolved),
    iteration = "list"
  ),
  tar_target(
    tract_cocs, 
    map_tracts_to_cocs(clipped_tracts, coc_shapefile),
    pattern = map(clipped_tracts, coc_shapefile),
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
    write_crosswalk(tract_crosswalk, shapefile_year, "tract", output_directory = "output_data/"),
    format = "file"
  ),
  tar_target(save_county_crosswalk, write_crosswalk(county_crosswalk, shapefile_year, "county", output_directory = "output_data/"), format = "file")
)
