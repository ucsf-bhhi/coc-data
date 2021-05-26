build_regex = function(file_extensions = c("gdb", "shp")) {
  paste0("[.](", paste(file_extensions, collapse = "|"), ")$")
}

read_raw_coc_shapefile = function(shapefile_path) {
  shapefile_year = parse_number(shapefile_path)
  stopifnot("coc shapefile year not 4 digits" = nchar(as.character(shapefile_year)) == 4)
  st_read(shapefile_path) %>% 
    mutate(year = shapefile_year)
}

simplify_shapefile = function(shapefile, projected_crs = 2163, simplify_keep_pct = 0.1) {
  st_transform(shapefile, crs = projected_crs) %>% 
    ms_simplify(keep = simplify_keep_pct)
}


