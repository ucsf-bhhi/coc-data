#' Fetches ACS table data from the Census Bureau API
#'
#' A wrapper around [tidycensus::get_acs()] which fetches data from the Census
#' Bureau API, adds a column with the year of the data, and removes the unneeded
#' margin of error columns.
#'
#' @param ... Parameters passed to [tidycensus::get_acs()].
#' @param quiet Boolean to suppress status messages.
#'
#' @return A data frame with the requested ACS data. 
#' * `year`: Year (numeric) 
#' * `fips`: Geographic unit FIPS code (character) 
#' * Additional columns with requested data
#' @seealso [tidycensus::get_acs()]
fetch_acs = function(..., quiet = TRUE) {
  f = function(...) {
    # put the arguments in a list
    dots = list(...)
    
    # the year argument is optional in get_acs and defaults to 2019
    # if the call doesn't specify the year and thus uses the default we still want to have a year column
    # so check if the year argument was specified (it's null if it wasn't)
    if (is.null(dots[["year"]])) {
      # if it wasn't set our yr (which goes in the year column we create) to 2019
      yr = 2019
    } else {
      # if it was grab it, so we can put it into the year column 
      yr = dots[["year"]]
    } 
    
    # call get_acs
    get_acs(...) %>% 
      # we only want the fips code and estimates
      # rename the GEOID columns as fips
      select(fips = GEOID, 
             # we want to keep the variable & estimate columns when get_acs is run in tidy mode
             matches("variable"), 
             matches("estimate"), 
             # when get_acs is run in wide mode the estimate columns always have the last letter E, so we keep all columns ending in E
             ends_with("E", ignore.case = FALSE), 
             # name end in E, but we don't want it, so explicitly drop it
             -matches("NAME")) %>% 
      rename_with(~ str_remove(.x, "E"), ends_with("E", ignore.case = FALSE)) %>% 
      # add the year column to the data
      mutate(year = yr) %>%
      # put the year column after the fips column
      relocate(year, .after = fips)
  }
  if (quiet) {
    f(...) %>% suppressMessages()
  }
  else {
    f(...)
  }
}

#' Write combined dataset to disk
#'
#' Flexibly writes the combined data set to disk. Since the actual call to the
#' function that will write the dataset is built dynamically, it can support
#' many filetypes as long as they are of the form `output_function(data,
#' file_path`.
#' @param data A data frame with the combined dataset.
#' @param output_function A character string with the function that will
#'   actually write the file.
#' @param extension A character string with the extension for the output file.
#' @param output_dir A character string with the path to the output directory.
#' @param file_name A character string with the file name.
#'
#' @return Invisibly returns a character string with the output file's path.
write_dataset <- function(data, output_function, extension,
                          output_dir = "output_data",
                          file_name = "coc_data") {
  # make sure output directory exists
  dir_create(output_dir)
  file_path <- path(output_dir, file_name, ext = extension)
  # build the actual call to a function that will write the output file
  output_call <- call(output_function, data, file_path)
  # run the function
  eval(output_call)
  # invisibly return the file path so targets can monitor it
  invisible(return(file_path))
}

#' @export
build_summary_stats = function(data) {
  data %>%
    pivot_longer(-c(coc_number, coc_name, coc_category, year), names_to = "Variable", values_to = "values") %>% 
    group_by(Variable) %>% 
    summarise(
      N = n(),
      across(
        values,
        list(
          `Share missing` = ~ sum(is.na(.x)) / N,
          `Mean` = ~ mean(.x, na.rm = TRUE),
          `Median` = ~ median(.x, na.rm = TRUE),
          `Min` = ~ min(.x, na.rm = TRUE),
          `Max` = ~ max(.x, na.rm = TRUE),
          `10th` = ~ quantile(.x, 0.1, na.rm = TRUE),
          `25th` = ~ quantile(.x, 0.25, na.rm = TRUE),
          `75th` = ~ quantile(.x, 0.75, na.rm = TRUE),
          `90th` = ~ quantile(.x, 0.9, na.rm = TRUE),
          `99th` = ~ quantile(.x, 0.99, na.rm = TRUE)
        ),
        .names = "{.fn}"
      )
    ) %>% 
    mutate(
      across(c(where(is.numeric), -N), format_values),
      N = scales::comma(N, accuracy = 1)
    )
  
}

format_values = function(x) {
  case_when(
    abs(x) <= 1 ~ scales::comma(x, accuracy = 0.01, trim = FALSE),
    abs(x) > 1  ~ scales::comma(x, accuracy = 1, trim = FALSE)
  )
}

get_state_fips <- function(fips_filter = 60) {
  tidycensus::fips_codes %>%
    distinct(state_code) %>%
    filter(as.numeric(state_code) < fips_filter) %>%
    pull(state_code)
}

fetch_acs_tracts = function(year, variables, states = get_state_fips(), ...) {
  map_dfr(
     states,
     function(x) fetch_acs("tract", state = x, year = year, variables = variables, ...)
  )
}

#' Moves Alaska and Hawaii for compact 50 state map
#'
#' Moves, scales, and rotates Alaska and Hawaii to make a compact 50 state map for
#' data vizualization.
#'
#' @param shapefile A shapefile with CoC boundaries.
#' @param smooth A numeric parameter for shapefile simplification.
#' @param rotate_ak Rotation parameter for Alaksa, in degrees.
#' @param scale_ak Scale factor for Alaska.
#' @param shift_ak A vector with the horizonal and vertical shift for Alaska.
#' @param rotate_hi Rotation parameter for Hawaii, in degrees
#' @param scale_hi Scale factor for Hawaii.
#' @param shift_hi A vector with the horizonal and vertical shift for Hawaii
#'
#' @return A spatial data frame with adjusted representations of Alaska and Hawaii. 
build_display_map = function(
  shapefile, smooth = 0.005,
  rotate_ak = -27, scale_ak = 0.4, shift_ak = c(-500000, -3250000), 
  rotate_hi = -25, scale_hi = 1,   shift_hi = c(5000000, -1400000)
) {
  shapefile %>% 
    rename("Shape" = contains("geometry")) %>% 
    mutate(st = str_sub(coc_number, 1, 2)) %>% 
    filter(!(st %in% c("AS", "GU", "MP", "PR", "VI"))) %>% 
    ms_simplify(keep = smooth, keep_shapes = TRUE) %>% 
    move_state("AK", rotate_ak, scale_ak, shift_ak) %>% 
    move_state("HI", rotate_hi, scale_hi, shift_hi) %>% 
    select(-st)
}

#' Move, scale, rotate a state on a map
#'
#' Helps move states like Alaska and Hawaii so the map-based visualizations can be 
#' more compact. This function simplifies changing the position of the state 
#' (shifting), changing the size of the state (scaling), and rotating the state.
#'
#' @param map A spatial data frame with the map data.
#' @param state A string with the abbreviation of the state to adjust (ie. "AK").
#' @param rotation A numeric with rotation adjustment given in degrees.
#' @param scale A numeric with the scale factor for the state.
#' @param shift A numeric vector with the horizontal and vertical adjustments for
#'  the state's position.
#'
#' @return The same spatial data frame but with the adjusted state.
#' 
#' @keywords internal
move_state = function(map, state, rotation = 0, scale = 1, shift = c(0,0)) {
  new_state = map %>% 
    filter(st == state) %>% 
    as_Spatial() %>% 
    elide(rotate = rotation) %>% 
    st_as_sf() %>% 
    rename(Shape = geometry) %>% 
    mutate(
      Shape = Shape * scale,
      Shape = Shape + shift
    )
  
  map %>% 
    filter(st != state) %>% 
    bind_rows(new_state)
}

save_maps = function(maps, output_dir = "maps") {
  # make sure output directory exists, create it if it doesn't
  dir_create(output_dir)
  # build the path to the output file
  output_file = path(output_dir, "coc_display_maps.rds")
  # save the maps object
  write_rds(maps, output_file)
  # invisibly return the file path so targets can track it
  invisible(output_file)
}
