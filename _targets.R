library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)
library(fs)

# load functions
source("R/helpers.R")
source("R/process_coc_shapefiles.R")
source("R/coc_county_tract_crosswalk.R")
source("R/pit_data_processing.R")
source("R/pit_rates.R")
source("R/coc_renter_share.R")
source("R/coc_fmr.R")
source("R/coc_zillow_rent.R")
source("R/coc_economic_indicators.R")

# load packages used by functions
tar_option_set(
  packages = c(
    "tidyverse",
    "sf",
    "tidycensus",
    "fs",
    "readxl",
    "haven",
    "rmapshaper",
    "maptools",
    "labelled"
  )
)

output_formats <- list(
  functions = c("write_csv", "write_dta", "write_sas", "write_rds"),
  extensions = c("csv", "dta", "sas7bdat", "rds")
)

list(
  #### Input Data ####
  tar_target(years, 2011:2019),
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
  tar_files_input(
    raw_evictions_file,
    "input_data/eviction_lab/all-counties.csv",
    format = "file"
  ),
  #### Tract/County to CoC Crosswalk Creation ####
  tar_target(
    coc_shapefiles,
    get_shapefiles(years, raw_coc_shapefiles, crs = 2163),
    pattern = map(years),
    iteration = "list"
  ),
  tar_target(
    coc_list,
    build_coc_list(coc_shapefiles),
    pattern = map(coc_shapefiles)
  ),
  tar_target(
    display_maps,
    build_display_map(coc_shapefiles),
    pattern = map(coc_shapefiles),
    iteration = "list"
  ),
  tar_target(
    coc_display_maps_file,
    save_maps(display_maps),
    format = "file"
  ),
  tar_target(
    tracts, 
    fetch_tract_data(years, crs = 2163),
    pattern = map(years),
    iteration = "list"
  ),
  tar_target(
    tract_cocs, 
    match_tracts_to_cocs(tracts, coc_shapefiles),
    pattern = map(tracts, coc_shapefiles)
  ),
  tar_target(
    tract_crosswalk, 
    build_tract_crosswalk(tract_cocs)
  ),
  tar_target(
    county_crosswalk, 
    build_county_crosswalk(tract_crosswalk)
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
    build_county_renter_share(years),
    pattern = map(years)
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
    get_acs_county_sub(years, processed_fmr),
    pattern = map(years)
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
  #### Share Rent Burdened ####
  tar_target(
    coc_rent_burden,
    build_coc_rent_burden(years, tract_crosswalk),
    pattern = map(years)
  ),
  #### Rental Vacancy Rate ####
  tar_target(
    coc_rental_vacancy_rates,
    build_coc_vacancy_rates(years, tract_crosswalk),
    pattern = map(years)
  ),
  #### Evictions ####
  tar_target(
    eviction_data,
    read_csv(raw_evictions_file)
  ),
  tar_target(
    renting_households,
    fetch_acs("county", year = years, output = "wide",
              variables = c(renting_households = "B25003_003")
    ),
    pattern = map(years)
  ),
  tar_target(
    coc_evictions,
    build_coc_evictions(eviction_data, county_crosswalk)
  ),
  #### Unemployment Rate ####
  tar_files_input(
    unemployment_url,
    "https://download.bls.gov/pub/time.series/la/la.data.64.County",
    format = "url"
  ),
  tar_target(
    raw_unemployment,
    fetch_unemployment(unemployment_url)
  ),
  tar_target(
    coc_unemployment_rate,
    build_coc_unemployment(raw_unemployment, county_crosswalk)
  ),
  #### Public Program Use ###
  tar_target(
    tract_public_program_use,
    fetch_public_program_use(years),
    pattern = map(years)
  ),
  tar_target(
    coc_public_program_use,
    build_coc_public_program_use(tract_public_program_use, tract_crosswalk)
  ),
  #### Income ####
  tar_target(
    coc_income,
    build_coc_income(
      years, county_crosswalk,
      income_variables = c(
        "household_income" = "S1901_C01_012",
        "family_income" = "S1901_C02_012",
        "individual_earnings" = "S2001_C01_002"
      )
    ),
    pattern = map(years)
  ),
  #### Combined Dataset ####
  tar_target(
    combined_dataset,
    coc_list %>% 
      full_join(coc_categories, by = "coc_number") %>% 
      full_join(pit_rates, by = c("coc_number", "year")) %>% 
      full_join(coc_populations, by = c("coc_number", "year")) %>% 
      full_join(coc_renter_shares, by = c("coc_number", "year")) %>% 
      full_join(coc_fmr, by = c("coc_number", "year")) %>% 
      full_join(coc_zillow_rent, by = c("coc_number", "year")) %>% 
      full_join(coc_rent_burden, by = c("coc_number", "year")) %>% 
      full_join(coc_rental_vacancy_rates, by = c("coc_number", "year")) %>%  
      full_join(coc_unemployment_rate, by = c("coc_number", "year")) %>%  
      full_join(coc_public_program_use, by = c("coc_number", "year")) %>% 
      full_join(coc_evictions, by = c("coc_number", "year")) %>% 
      full_join(coc_income, by = c("coc_number", "year")) %>% 
      label_dataset()
  ),
  #### Output Dataset Files ####
  tar_map(
    values = output_formats,
    tar_file(
      output_file,
      write_dataset(combined_dataset, functions, extensions)
    ),
    names = extensions
  ),
  #### Basic Summary Stats ####
  tar_target(summary_stats, build_summary_stats(combined_dataset))
)
