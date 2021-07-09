#' Build file extension regular expression
#'
#' Creates a regular expression used to filter shapefile in input_data
#'
#' @param file_extensions A character vector of shapefile file extensions to
#'   include in the regular expression.
#'
#' @return A character vector containing the regular expression.
#' @keywords internal
build_regex <- function(file_extensions = c("gdb", "shp")) {
  paste0("[.](", paste(file_extensions, collapse = "|"), ")$")
}

#' Fetches pre-2013 CoC shapefiles
#'
#' Before 2013, HUD released individual shapefiles for each CoC instead of one
#' national file. This downloads the individual shapefiles and combines them.
#'
#' @param year A numeric with the year of the shapefiles.
#'
#' @return A spatial data frame.
get_pre_2013_shapefiles <- function(year) {
  # augment the built-in vector of state names & abbreviations with DC
  state_name <- c(state.name, "District of Columbia") 
  state_abb <- c(state.abb, "DC")
  
  # iterate over the states and download the shapefiles
  map2_dfr(state_abb, state_name, download_shapefile, year)
}

#' Download a state's CoC shapefiles
#'
#' Downloads the CoC shapefiles for a state in the given year.
#'
#' @param state_abb A character vector with two letter state abbreviations.
#' @param state_name A character vector with state names.
#' @param year A numeric with the year of the shapefiles.
#' @param td A directory where the shapefiles will be unzipped. Defaults to the
#'   session temporary directory.
#'
#' @return A spatial data frame.
#' @seealso [get_pre_2013_coc_shapefiles()] for the main function
#' @keywords internal
download_shapefile <- function(state_abb, state_name, year, td = tempdir()) {
  # paste together the url for the zip file with the state's shapefiles
  url <- paste0("https://files.hudexchange.info/reports/published/CoC_GIS_State_Shapefile_", state_abb, "_", year, ".zip")
  
  # download the zipfile to a tempfile
  curl::curl_download(url, file_temp()) %>%
    # extract the contents of the zipfile into the temp directory
    unzip(exdir = td)
  
  # swap spaces for underscores in the state name
  state_name <- str_replace_all(state_name, " ", "_")
  
  # find the directories with the shapefiles in them
  shapefile_dirs <- dir_ls(
    # look for paths in the temp directory with the state's name
    path(td, state_name),
    # we don't want the metadata directory, so filter on that 
    glob = "*metadata*",
    # and take the paths that don't have metadata in them
    invert = TRUE
  )
  
  # iterate over the directories and read in the shapefile and tack on the year
  map_dfr(shapefile_dirs, function(file) {
    st_read(file, quiet = TRUE) %>%
      mutate(year = year) %>% 
      select(coc_number = COCNUM, coc_name = COCNAME, year, geometry)
  })
}

#' Read in original CoC shapefile
#'
#' Loads an original CoC shapefile from HUD as a spatial data frame and adds a
#' year column. Returns an error if it cannot parse a 4-digit year from the file
#' path.
#' @param shapefile_path Path to the shapefile or other spatial data
#'   file/database
#'
#' @return The shapefile in a spatial data frame
#' @seealso [simplify_shapefile()] for simplifying the shapefile to reduce its
#'   complexity
read_raw_coc_shapefile <- function(shapefile_path) {
  # try to parse a year from the shapefile path
  shapefile_year <- parse_number(shapefile_path)
  # throw an error if we don't have a 4 digit number for the year
  stopifnot("coc shapefile year not 4 digits" = nchar(as.character(shapefile_year)) == 4)
  # read in the shapefile
  st_read(shapefile_path) %>%
    # add a column with the year
    mutate(year = shapefile_year)
}


#' Simplify shapefile and set projected CRS
#'
#' Simplifies a shapefile using [rmapshaper::ms_simplify()] and also sets a
#' projected CRS using [sf::st_transform()] since [rmapshaper::ms_simplify()]
#' prefers a projected CRS.
#'
#' @param shapefile An sf object with a raw CoC shapefile from
#'   [read_raw_coc_shapefile()]
#' @param projected_crs The EPSG code for the projected CRS
#' @param simplify_keep_pct The share of the shapefile to keep when simplifying
#'
#' @return The simplified sf object now in the projected CRS.
#' @seealso [read_raw_coc_shapefile()] for reading in the raw shapefile
#' @seealso [match_tract_to_coc()] for using the shapefile to match CoCs to tracts
simplify_shapefile <- function(shapefile, projected_crs = 2163, simplify_keep_pct = 0.1) {
  # transform to the projected CRS
  st_transform(shapefile, crs = projected_crs) %>%
    # simplify the spatial object
    ms_simplify(keep = simplify_keep_pct)
}
