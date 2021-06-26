## This script is for processing surface fuels data for the NPS Fire Effects in Washington State,
## from the plot level summary provided by Karen (NOT the raw data).
## This is fairly minimal preprocessing, really just formatting column names and getting my 'plot_visit' column.
## Input data were downloaded from NPS data store: https://irma.nps.gov/DataStore/Reference/Profile/2271695.

## Script written by Don Radcliffe, PhD Student at University of Washington, dradclif@uw.edu.
## Collaborators Karen Kopper, National Park Service, and Brian Harvey, UW.
## Started 2021 May 01
## See Github Repo https://github.com/don-radcliffe/nps_fx.

require(here)
require(dplyr)
require(stringr)
require(tidyr)
require(reshape2)

conflict_prefer('select', 'dplyr')
conflict_prefer('filter', 'dplyr')

import_dir_sfs <- here::here('data', 'data_raw')
import_dir_sfs_tidy <- here::here('data', 'data_tidy')
export_dir_sfs <- here::here('data', 'data_tidy')

surface_fuels_raw_noca <- read.csv(file.path(import_dir_sfs, 'surface_fuels_report_raw_noca.csv'), stringsAsFactors = TRUE)
surface_fuels_raw_laro <- read.csv(file.path(import_dir_sfs, 'surface_fuels_report_raw_laro.csv'), stringsAsFactors = TRUE)
plot_visit_data <- read.csv(file.path(import_dir_sfs_tidy, 'plot_visit_data.csv'), stringsAsFactors = TRUE)

## To filter out the sterling valley plots, easiest thing to do for time being
sterling_valley <- data.frame (plot_visit = c('lr01_2011_-1_-1', 'lr01_2013_03_01', 'lr01_2014_03_02', 'lr01_2017_03_05', 
                     'lr02_2011_-1_-1', 'lr02_2013_03_00', 'lr02_2013_03_01', 'lr02_2014_03_02',
                     'lr02_2017_03_05', 'lr03_2011_-1_-1', 'lr03_2013_03_00', 'lr03_2013_03_01',
                     'lr03_2014_03_02', 'lr03_2017_03_05'))


## To get the year for my plot_visit column.
year <- plot_visit_data %>%
  select(plot, monitoring_status, year)

## Combine laro and noca.
surface_fuels_noca <- surface_fuels_raw_noca

surface_fuels_laro <- surface_fuels_raw_laro %>%
  ## macroplot got an annoying i.. name
  rename(Macroplot = 1)

surface_fuels_combined <- surface_fuels_noca %>%
  bind_rows(surface_fuels_laro)

## Values are in kg/m^2 except for second set of duff and litter measurements
surface_fuels <- surface_fuels_combined %>%
  rename(c('macroplot' = Macroplot, 'monitoring_status' = Monitoring.Status,
           'one_hour' = X1.hr, 'ten_hour' = X10.hr, 'hundred_hour' = X100.hr, 'fwd' = X1.100.hr, 
           'thousand_sound' = X1000.hr.sound, 'thousand_rotten' = X1000.hr.rotten, 
           'one_to_thousand' = X1.1000.hr, 
           'duff' = Duff, 'litter' = Litter, 'total' = Total)) %>%
  ## This gets rid of the depth in centimeter values for duff and litter,
  ## the ones we have above are the biomass values. 
  select(c(-Duff.1, -Litt, -Total.1)) %>%
  ## Now let's get my plot_visit column.
  ## We need to join with combination of plot and monitoring status to get year.
  ## First need to make my minor edits to monitoring status.
  mutate(monitoring_status = tolower(monitoring_status)) %>%
  mutate(monitoring_status = str_replace_all(monitoring_status, c('00pre' = '-1pre-1',
                                                                  '00pro01' = '-2pre-2',
                                                                  'post' = 'post00'))) %>%
  ## Create years_post and treatment_code columns from the monitoring status coding.
  mutate(years_post = str_sub(monitoring_status, start = -2), .after = monitoring_status) %>%
  mutate(treatment_code = str_sub(monitoring_status, end = 2), .after = monitoring_status) %>%
  ## Also need to get the plot name formatted.
  mutate(plot = str_replace_all(macroplot, c('FPSME2D08-' = 'nc',
                                             'FPIPO1D10-' = 'lr',
                                             'FPIPO2D10-'= 'lr'))) %>%
  ## Now we can join with year.
  full_join(year, by = c('plot', 'monitoring_status')) %>%
  ## We had one observation in the year dataframe (derived from trees) that wasn't in fuels.
  filter(complete.cases(macroplot)) %>%
  unite(plot_visit, c('plot', 'year', 'treatment_code', 'years_post'), sep = '_', remove = TRUE) %>%
  ## Take out irrelevant columns.
  select(c(-macroplot, -monitoring_status)) %>%
  ## Multiplying by ten to convert from kg/m to Mg/ha, which we use for other datasets.
  mutate_at(c('one_hour', 'ten_hour', 'hundred_hour', 'fwd',
              'thousand_sound', 'thousand_rotten', 'one_to_thousand', 
              'duff', 'litter', 'total'), function(x)(x*10)) %>%
  ## Get rid of plots that could be sterling valley
  anti_join(sterling_valley, by = 'plot_visit') %>%
  ## There are a bunch of NA's in the year column for lr01-03, bet those could be sterling valley
  mutate(nacount = str_count(plot_visit, 'NA')) %>%
  filter(nacount == 0) %>%
  select(-nacount)

## Export if desired.
#write.csv(surface_fuels, file.path(export_dir_sfs, 'surface_fuels_report_tidy.csv'), row.names = FALSE)
#View(surface_fuels)
