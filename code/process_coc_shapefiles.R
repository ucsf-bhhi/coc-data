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
