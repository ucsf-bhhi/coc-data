# coc-data

Builds a dataset that combines Point in Time counts with housing market and other economic data converted to be at the Continuum of Care (CoC) level.

The project makes extensive use of the [**targets** R package](https://docs.ropensci.org/targets/).

To run the code, make sure you have the environmental variable `CENSUS_API_KEY` set to your API key for U.S. Census Bureau data. You can get a free API key here: <https://api.census.gov/data/key_signup.html>.

The strategy to link census tracts and counties to CoCs is based on [Byrne, et al. \(2016\)](https://github.com/tomhbyrne/HUD-CoC-Geography-Crosswalk).

A data dictionary is avaiable in [data-dictionary.md](data-dictionary.md).

### Input Data Sources

| Data                                   | File Location                                     | Source                                                                                                                                                           |
| -------------------------------------- | ------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| CoC Shapefiles                         | `input_data/geography/input_data/coc_shapefiles`  | [HUD CoC GIS Geodatabases](https://www.hudexchange.info/programs/coc/gis-tools/)                                                                                 |
| Census Tract Shapefiles                | **tigris** R package via **tidycensus** R package | [**tidycensus** package](https://walker-data.com/tidycensus/), [**tigris** package](https://github.com/walkerke/tigris)                                          |
| American Community Survey (ACS) Tables | **tidycensus** R package                          | [**tidycensus** package](https://walker-data.com/tidycensus/)                                                                                                    |
| Census Tract to ZIP Code Crosswalks    | `input_data/geography/usps_tract_to_zip/`         | [HUD-USPS ZIP Code Crosswalk Files](https://www.huduser.gov/portal/datasets/usps_crosswalk.html)                                                                 |
| HUD Fair Market Rents (FMR)            | `input_data/hud_fmr/`                             | [HUD Fair Market Rents, County Level Data](https://www.huduser.gov/portal/datasets/fmr.html)                                                                     |
| HUD Point in Time (PIT) Counts         | `input_data/pit_counts/`                          | [HUD 2007-2019 PIT Counts by CoC](https://www.huduser.gov/portal/sites/default/files/xls/2007-2019-Point-in-Time-Estimates-by-CoC.xlsx)                          |
| Zillow Rent Index                      | `input_data/zillow_rent_index/`                   | [Zillow Observed Rent Index (Smoothed, Seasonally Adjusted): All Homes Plus Multifamily Time Series (Zip Code Geography)](https://www.zillow.com/research/data/) |
