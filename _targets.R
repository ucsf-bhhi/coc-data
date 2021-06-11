library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)
library(fs)

# load functions
source("code/helpers.R")
source("code/process_coc_shapefiles.R")
source("code/coc_county_tract_crosswalk.R")
source("code/pit_data_processing.R")
source("code/pit_rates.R")
source("code/coc_renter_share.R")
source("code/coc_fmr.R")
source("code/coc_zillow_rent.R")

# load packages used by functions
tar_option_set(
  packages = c(
    "tidyverse",
    "sf",
    "rmapshaper",
    "tidycensus",
    "fs",
    "readxl",
    "haven"
  )
)

output_formats <- list(
  functions = c("write_csv", "write_dta", "write_sas", "write_rds"),
  extensions = c("csv", "dta", "sas7bdat", "rds")
)

list(
  #### Input Data ####
  tar_files_input(
    raw_coc_shapefiles,
    dir_ls(
      "input_data/geography/coc_shapefiles",
      recurse = 1,
      regex = build_regex()
    ),
    format = "file"
  ),
  tar_files_input(
    raw_pit_counts,
    "input_data/pit_counts/pit_counts_2007_2019.xlsx",
    format = "file"
  ),
  tar_files_input(
    raw_fmr,
    dir_ls("input_data/hud_fmr"),
    format = "file"
  ),
  tar_files_input(
    raw_zillow_data,
    "input_data/zillow_rent_index/zillow_rent_index_sa_zip_code_2014.01_2021.03.csv",
    format = "file"
  ),
  tar_files_input(
    raw_tract_to_zip,
    dir_ls("input_data/geography/usps_tract_to_zip"),
    format = "file"
  ),
  #### Tract/County to CoC Crosswalk Creation ####
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
    coc_populations,
    build_coc_populations(tract_crosswalk)
  ),
  #### PIT Data ####
  tar_target(
    pit_years,
    get_pit_years(raw_pit_counts)
  ),
  tar_target(
    wide_pit_data,
    parse_pit_year(raw_pit_counts, pit_years),
    pattern = map(pit_years)
  ),
  tar_target(
    long_pit_data,
    get_long_pit_data(wide_pit_data)
  ),
  tar_target(
    coc_categories,
    get_coc_categories(wide_pit_data)
  ),
  tar_target(
    pit_rates,
    build_pit_rates(long_pit_data, coc_populations)
  ),
  #### Renter Shares ####
  tar_target(
    county_renter_shares,
    build_county_renter_share(shapefile_years),
    pattern = map(shapefile_years)
  ),
  tar_target(
    coc_renter_shares,
    build_coc_renter_shares(county_renter_shares, county_crosswalk)
  ),
  #### FMRs ####
  tar_target(
    processed_fmr,
    process_fmr(raw_fmr),
    pattern = map(raw_fmr)
  ),
  tar_target(
    acs_county_subdivision,
    get_acs_county_sub(shapefile_years, processed_fmr),
    pattern = map(shapefile_years)
  ),
  tar_target(
    coc_fmr,
    build_coc_fmr(processed_fmr, acs_county_subdivision, county_crosswalk)
  ),
  #### Zillow Rent Index ####
  tar_target(
    processed_zillow_data,
    process_zillow_data(raw_zillow_data)
  ),
  tar_target(
    tract_to_zip,
    process_tract_to_zip(raw_tract_to_zip),
    pattern = map(raw_tract_to_zip)
  ),
  tar_target(
    tract_zillow_rent,
    build_tract_zillow_rent(processed_zillow_data, tract_to_zip)
  ),
  tar_target(
    coc_zillow_rent,
    build_coc_zillow_rent(tract_zillow_rent, tract_crosswalk)
  ),
  #### Combined Dataset ####
  tar_target(
    combined_dataset,
    coc_categories %>% 
      full_join(pit_rates, by = "coc_number") %>% 
      full_join(coc_populations, by = c("coc_number", "year")) %>% 
      full_join(coc_renter_shares, by = c("coc_number", "year")) %>% 
      full_join(coc_fmr, by = c("coc_number", "year")) %>% 
      full_join(coc_zillow_rent, by = c("coc_number", "year"))
  ),
  #### Output Dataset Files ####
  tar_map(
    values = output_formats,
    tar_file(
      output_file,
      write_dataset(combined_dataset, functions, extensions)
    ),
    names = extensions
  )
)

