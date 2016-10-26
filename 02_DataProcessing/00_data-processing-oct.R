# Read packages ------------------------------------------------------------------------------
source('helperfuns.R')
library(purrr)
library(stringr)
library(tidyr)
library(dplyr)
library(RJSONIO)

# Clean and process data ---------------------------------------------------------------------
process_data <- . %>%
                set_names(gsub(' ', '_', tolower(names(.)))) %>%
                dmap_at(col_string(., cols), str_replace, 'Above', 'Better than') %>%
                dmap_at(col_string(., cols), str_replace, 'Below', 'Worse than') %>%
                dmap_at('location', str_replace, 'POINT \\(', '') %>%
                dmap_at('location', str_replace, '\\)', '') %>%
                separate(location, c('lon', 'lat'), ' ') %>%
                dmap_at(c('lon', 'lat'), as.numeric) %>%
                select(-matches('footnote')) %>%
                select(-matches(':@computed')) %>%
                filter(!is.na(lon)) %>%
                dmap_at('hospital_overall_rating', str_replace_na, 'Not Available')
  
# Download data ------------------------------------------------------------------------------
file <- 'https://data.medicare.gov/resource/rbry-mqwu.csv'
cols <- 'national_comparison$' 

data <- readr::read_csv(file) %>%
        process_data()

# Save to disk -------------------------------------------------------------------------------
saveRDS(data, '1_Processed_Data/analyticfile_oct.RDS')
