library(sf)
library(rmapshaper)
library(tidyverse)
library(fs)

shapefile_directory = "input_data/geography/coc_shapefiles"
file_extensions = c("gdb", "shp")
projected_crs = 2163
simplify_keep_pct = 0.1
shapefile_out_directory = "output_data/coc_shapefiles"
shapefile_out_format = "geojson"

reg_ex = paste0("[.](", paste(file_extensions, collapse = "|"), ")$")
shapefile_paths = dir_ls(shapefile_directory, recurse = 1, regexp = reg_ex)

original_shapefiles = map(shapefile_paths, st_read)
simplified_shapefiles = map(original_shapefiles, st_transform, projected_crs) %>% 
  map(ms_simplify, keep = simplify_keep_pct)
dissolved_simplified_shapefiles = map(simplified_shapefiles, ms_dissolve)

build_file_path = function(orig_path, prefix, output_format, shapefile_out_directory) {
  raw_year = path_file(orig_path) %>% parse_number() %>% as.character()
  year = if (nchar(raw_year) == 2) paste0("20", raw_year) else raw_year
  filename = paste("coc", prefix, year, sep = "_")
  return(path(shapefile_out_directory, prefix, filename, ext = output_format))
}

simplified_shapefile_paths = map(shapefile_paths, build_file_path, prefix = "simplified", shapefile_out_format, shapefile_out_directory)
dir_create(path_dir(simplified_shapefile_paths[[1]]))
walk2(simplified_shapefiles, simplified_shapefile_paths, st_write, delete_dsn = TRUE)

dissolved_simplified_shapefile_paths = map(shapefile_paths, build_file_path, "dissolved_simplified", shapefile_out_format, shapefile_out_directory)
dir_create(path_dir(dissolved_simplified_shapefile_paths[[1]]))
walk2(dissolved_simplified_shapefiles, dissolved_simplified_shapefile_paths, st_write, delete_dsn = TRUE)

