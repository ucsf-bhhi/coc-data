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
  file_path <- path(output_dir, file_name, ext = extension)
  # build the actual call to a function that will write the output file
  output_call <- call(output_function, data, file_path)
  # run the function
  eval(output_call)
  # invisibly return the file path so targets can monitor it
  invisible(return(file_path))
}
