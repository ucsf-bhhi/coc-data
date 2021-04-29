# coc-data

Code to process the raw Point In Time (PIT) counts of people experiencing homelessness and to calculate homelessness rates in each Continuum of Care (CoC).

To run the code which creates homelessness rates, make sure you have the environmental variable `CENSUS_API_KEY` set to your API key for U.S. Census Bureau data. You can get a free API key here: <https://api.census.gov/data/key_signup.html>.

### File Creation

| Output File                            | Created By                   |
|----------------------------------------|------------------------------|
| `pit_processed_2007_2019.csv`          | `code/pit_data_processing.R` |
| `coc_categories_2007_2019.csv`         | `code/pit_data_processing.R` |
| `pit_homelessness_rates_2012_2019.csv` | `code/pit_rates.R`           |
