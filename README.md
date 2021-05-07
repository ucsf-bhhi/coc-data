# coc-data

Code to process the raw Point In Time (PIT) counts of people experiencing homelessness and to calculate homelessness rates in each Continuum of Care (CoC).

To run the code which creates homelessness rates, make sure you have the environmental variable `CENSUS_API_KEY` set to your API key for U.S. Census Bureau data. You can get a free API key here: <https://api.census.gov/data/key_signup.html>.

### File Creation

| Output File                            | Created By                   |
|----------------------------------------|------------------------------|
| `pit_processed_2007_2019.csv`          | `code/pit_data_processing.R` |
| `coc_categories_2007_2019.csv`         | `code/pit_data_processing.R` |
| `pit_homelessness_rates_2012_2019.csv` | `code/pit_rates.R`           |
| `coc_zillow_rent.csv`                  | `code/coc_zillow_rent.R`     |

### Input Data Sources

| File                                                                  | Source                                                                                                                                                           |
|-----------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `geography/county_to_coc_crosswalk.xlsx`                              | [HUD CoC Geography Crosswalk, county_coc_match.csv](https://github.com/tomhbyrne/HUD-CoC-Geography-Crosswalk/blob/master/output/county_coc_match.csv)            |
| `geography/tract_to_coc_crosswalk.csv`                                | [HUD CoC Geography Crosswalk, tract_coc_match.csv](https://github.com/tomhbyrne/HUD-CoC-Geography-Crosswalk/blob/master/output/tract_coc_match.csv)              |
| `geography/usps_tract_to_zip_crosswalk_yyyy_mm.xlsx`                  | [HUD USPS ZIP Code Crosswalk Files](https://www.huduser.gov/portal/datasets/usps_crosswalk.html)                                                                 |
| `hud_fmr/hud_fmr_county_yyyy.xlsx`                                    | [HUD Fair Market Rents, County Level Data](https://www.huduser.gov/portal/datasets/fmr.html)                                                                     |
| `pit_counts/pit_counts_2007_2019.xlsx`                                | [HUD 2007-2019 PIT Counts by CoC](https://www.huduser.gov/portal/sites/default/files/xls/2007-2019-Point-in-Time-Estimates-by-CoC.xlsx)                          |
| `zillow_rent_index/zillow_rent_index_sa_zip_code_2014.01_2021.03.csv` | [Zillow Observed Rent Index (Smoothed, Seasonally Adjusted): All Homes Plus Multifamily Time Series (Zip Code Geography)](https://www.zillow.com/research/data/) |
