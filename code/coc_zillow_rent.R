library(tidyverse)
library(tidycensus)

tract_to_coc <- read_csv("input_data/geography/tract_to_coc_crosswalk.csv",
                         col_types = "cc-c--i-"
) %>%
  mutate(tract_fips = str_pad(tract_fips, 11, "left", "0")) %>%
  group_by(coc_number) %>%
  mutate(coc_pop_share_in_tract = total_population / sum(total_population, na.rm = TRUE))

tracts_in_cocs <- tract_to_coc %>%
  filter(!is.na(coc_number)) %>%
  select(tract_fips) %>%
  distinct() %>%
  expand(tract_fips, year = 2014:2020)

zillow_data <- read_csv("input_data/zillow_rent_index/zillow_rent_index_sa_zip_code_2014.01_2021.03.csv") %>%
  select(zip = RegionName, starts_with("20")) %>%
  pivot_longer(-zip, names_to = "year_month", values_to = "rent") %>%
  separate(year_month, c("year", "month")) %>%
  mutate(across(c(year, month), as.integer))

annual_zillow_data <- zillow_data %>%
  group_by(zip, year) %>%
  summarize(annual_mean_rent = mean(rent, na.rm = TRUE))


acs_data <- map_dfr(2012:2019, function(x) {
  # hit the census API for the data
  get_acs("county", variables = "S0101_C01_001E", year = x, key = Sys.getenv("CENSUS_API_KEY"), output = "wide", survey = "acs5") %>%
    select(fips = GEOID, name = NAME, total_population = S0101_C01_001E) %>%
    # add a year variable
    mutate(year = x)
})

tract_to_zip <- readxl::read_excel("input_data/geography/usps_tract_to_zip_crosswalk_2017.03.xlsx", col_types = c("text", "text", "numeric", "skip", "skip", "skip")) %>%
  rename_with(str_to_lower, everything())

tract_rent <- tracts_in_cocs %>%
  left_join(tract_to_zip, by = c("tract_fips" = "tract")) %>%
  left_join(annual_zillow_data, by = c("zip", "year")) %>%
  group_by(tract_fips, year) %>%
  summarise(
    tract_rent = weighted.mean(annual_mean_rent, res_ratio, na.rm = TRUE),
    tract_share_na_rent = sum(res_ratio[is.na(annual_mean_rent)])
  )

coc_zillow_rent <- tract_to_coc %>%
  left_join(tract_rent, by = "tract_fips") %>%
  group_by(coc_number, year) %>%
  summarise(
    coc_rent_zillow = weighted.mean(tract_rent, coc_pop_share_in_tract, na.rm = TRUE),
    coc_share_na_rent_zillow = sum(coc_pop_share_in_tract * tract_share_na_rent, na.rm = TRUE)
  )

write_csv(coc_zillow_rent, "output_data/coc_zillow_rent.csv")
