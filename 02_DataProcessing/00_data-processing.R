# load libraries --------------------------------------------------------------
libs <- c("dplyr", "tidyr", "purrr", "rlist", "stringr")
lapply(libs, library, character.only = T)
source('02_DataProcessing/helperfuns.R')

# read data files and column lists --------------------------------------------
datafile  <- '00_RawData/HQI_HOSP.xlsx'
hospchars <- '00_RawData/hospchars.csv'

locations <- '01_AnalyticData/locations.csv'
cols      <- '01_AnalyticData/varnames.yaml' %>% yaml_process()
tableVars <- '01_AnalyticData/table_vars.yaml' %>% yaml_process()

performance_cols <- grep('_comparison$', cols, value = TRUE)

# process data ----------------------------------------------------------------
stars <- readxl::read_excel(datafile) %>%
  set_names(gsub('\\s+', '_', tolower(names(.)))) %>%
  select(one_of(cols)) %>% 
  left_join(readr::read_csv(locations, col_types = 'ccc'), by = 'provider_id') %>%
  left_join(readr::read_csv(hospchars, col_types = 'ccinnniiii'), by = 'provider_id') %>%
  mutate(search = sprintf( "%s %s %s %s USA", address, city, state, zip_code)) %>%
  dmap_at(performance_cols, stringr::str_replace, 'Above', 'Better than') %>%
  dmap_at(performance_cols, stringr::str_replace, 'Below', 'Worse than')
  
# geocode missing hospitals ---------------------------------------------------
stars %>% 
  filter(is.na(lat)) %>%
  select(matches('search')) %>% 
  as.data.frame() %>%
  ggmap::mutate_geocode(search) %>%
  write.table('01_AnalyticData/geocoded_data.csv', row.names = F, sep = ',')

geocoded_locations <- '01_AnalyticData/geocoded_data.csv'

# merge geocoded data back onto dataframe ------------------------------------- 
stars <- stars %>% 
  left_join(readr::read_csv(geocoded_locations, col_types = 'ccc'), by = 'search') %>%
  mutate(lon         = ifelse(is.na(lon.x), as.numeric(lon.y), as.numeric(lon.x)),
         lat         = ifelse(is.na(lat.x), as.numeric(lat.y), as.numeric(lat.x)),
         missing_geo = is.na(lon)) %>%
  select(-matches("\\.[a-z]")) %>% 
  filter(!is.na(readmission_national_comparison))

# save file to disk -----------------------------------------------------------
saveRDS(stars %>% filter(!is.na(lon)), '03_Dashboard/analyticfile.RDS')

# produce and export univariate summary stats ---------------------------------
stars %>% 
  select(one_of(tableVars)) %>% 
  lapply(table, useNA = 'ifany') %>%
  lapply(as.data.frame) %>% 
  list.map(sprintf("%s: %i", Var1, Freq)) %>%
  writeYml("01_AnalyticData/summarystats_univariate.yaml")

# export list of hospitals with missing geographic information -----------------
stars %>% 
  select(one_of(c('provider_id', 'hospital_name', 'address', 'city', 'state', 'zip_code', 'lat', 'lon'))) %>%
  filter(is.na(lon)) %>% 
  write.table('missing_hospital.csv', row.names = F, sep = ',')