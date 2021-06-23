fetch_unemployment = function(url) {
  read_tsv(
    url,
    col_names = c("series", "year", "month", "value", "footnote"),
    col_types = "cicnc",
    skip = 1,
    na = "-"
  ) %>% 
    filter(str_sub(series, -2, -1) == "03", year >= 2010)
}

