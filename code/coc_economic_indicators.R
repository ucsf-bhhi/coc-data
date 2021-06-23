fetch_unemployment = function(url, min_year = 2010) {
  read_tsv(
    url,
    col_names = c("series", "year", "month", "value", "footnote"),
    col_types = "cicnc",
    skip = 1,
    na = "-"
  ) %>% 
    filter(str_sub(series, -2, -1) == "03", year >= min_year)
}

build_coc_unemployment = function(unemployment_data, county_crosswalk) {
  process_unemployment(unemployment_data) %>% 
    make_coc_unemployment(county_crosswalk)
}

process_unemployment = function(unemployment_data, use_month = "M01") {
  unemployment_data %>% 
    filter(month == use_month) %>% 
    mutate(
      county_fips = str_sub(series, 6, 10),
      unemployment_rate = value / 100) %>%
    select(county_fips, year, unemployment_rate)
}

make_coc_unemployment = function(unemployment_data, county_crosswalk) {
  county_crosswalk %>% 
    left_join(unemployment_data, by = c("county_fips", "year")) %>% 
    group_by(coc_number, year) %>% 
    summarise(
      coc_unemployment_rate = weighted.mean(
        unemployment_rate, 
        pct_coc_pop_from_county,
        na.rm = TRUE
      ),
      share_na_coc_unemployment_rate = sum(
        pct_coc_pop_from_county[is.na(unemployment_rate)]
      ),
      .groups = "drop"
    )
}