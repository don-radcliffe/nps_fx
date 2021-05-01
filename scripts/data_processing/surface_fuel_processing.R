## This script is for processing and tidying surface fuel data for the NPS Fire Effects in Washington State.
## Input data were downloaded from NPS data store: https://irma.nps.gov/DataStore/Reference/Profile/2271695.

## Script written by Don Radcliffe, PhD Student at University of Washington, dradclif@uw.edu.
## Collaborators Karen Kopper, National Park Service, and Brian Harvey, UW.
## Started 2021 May 01.
## See Github Repo https://github.com/don-radcliffe/nps_fx.

require(here)
require(dplyr)
require(stringr)
require(tidyr)
require(reshape2)

conflict_prefer('select', 'dplyr')
conflict_prefer('filter', 'dplyr')

import_dir_sf <- here::here('data', 'data_raw')
export_dir_sf <- here::here('data', 'data_tidy')

fwd_raw <- read.csv(file.path(import_dir_sf, 'fwd_raw.csv'), stringsAsFactors = TRUE)
cwd_raw <- read.csv(file.path(import_dir_sf, 'cwd_raw.csv'), stringsAsFactors = TRUE)
litter_duff_raw <- read.csv(file.path(import_dir_sf, 'litter_duff_raw.csv'), stringsAsFactors = TRUE)

fwd <- fwd_raw %>%
  select(c(Date, Monitoring.Status, MacroPlot.Name, MacroPlot.Purpose, Transect, Slope, OneHr, TenHr, HunHr)) %>%
  ## Use mutate to rename columns to get rid of spaces and uppercase in column names,
  ## with tolower() to get rid of uppercase in values,
  ## kills two birds with one stone
  mutate(date = tolower(Date)) %>%
  mutate(monitoring_status = tolower(Monitoring.Status)) %>%
  mutate(plot = tolower(MacroPlot.Name)) %>%
  mutate(area = tolower(MacroPlot.Purpose)) %>%
  mutate(transect = tolower(Transect)) %>%
  mutate(slope = tolower(Slope)) %>%
  mutate(one_hour = tolower(OneHr)) %>%
  mutate(ten_hour = tolower(TenHr)) %>%
  mutate(hundred_hour = tolower(HunHr)) %>%
  ## Remove the originally-named columns.
  select(c(date:hundred_hour)) %>%
  ## Plot has an annoying string of the same ten characters in front of each meaningful identifier.
  ## I'm replacing it with 'nc' to denote 'North Cascades'.
  mutate(plot = str_replace_all(plot, 'fpsme2d08-', 'nc')) %>%
  ## There are some blank rows at the top of each plot that need ridding,
  ## where the metadata on transect length and number reside in their own row, always giving this info:
  ## number transects: 4, length one hour: six, length ten hour: six, length hundred hour: 12.
  filter(complete.cases(transect)) %>%
  ## Below I run string splits on the monitoring_status column to isolate treatment code and year,
  ## by cutting off the first two and last two values in the string.
  ## A few codes cause problems: so I change them here so it gives me what I want later:
  ## 00pre, immediate pre-treatment, to -1pre-1,
  ## 00pro01, older pre-treatment, to -2post-2,
  ## XXpost, immediate post treatment with XX being treatment code, to XXpost00.
  mutate(monitoring_status = str_replace_all(monitoring_status, c('00pre' = '-1pre-1',
                                                                  '00pro01' = '-2pre-2',
                                                                  'post' = 'post00'))) %>%
  ## The substr() the monitoring status column to get treatment code and year.
  ## The .after command places it right after monitoring status code.
  mutate(years_post = str_sub(monitoring_status, start = -2), .after = monitoring_status) %>%
  mutate(treatment_code = str_sub(monitoring_status, end = 2), .after = monitoring_status) %>%
  ## There are some spaces in the area column that create duplicate factors.
  mutate(area = str_replace_all(area, 
                                c('boulder ' = 'boulder', 'mcgregor ' = 'mcgregor', 'weaver ' = 'weaver'))) %>%
  ## Now lets fix that date column.
  ## First split the date from the time; the NA argument gets rid of the time, which isn't useful.
  separate(date, c('date', NA), sep = ' ') %>%
  ## Format to a date.
  mutate(date = as.Date(date, '%m/%d/%Y')) %>%
  ## Format to iso 8601 for easy sorting.
  mutate(date = format(date, '%Y/%m/%d')) %>%
  ## Create a year column.
  mutate(year = format(as.Date(date, format='%Y'), '%Y'), .after = date) %>%
  ## Now we need a unique identifier for each plot visit so we can summarize data.
  unite(plot_visit, c('plot', 'year', 'treatment_code', 'years_post'), sep = '_', remove = FALSE) %>%
  ## And lastly, convert vector types for columns we'll work with below
  mutate(plot_visit = as.factor(plot_visit)) %>%
  mutate(slope = as.numeric(slope)) %>%
  ## Now that we have plot visit column, cut down to the essentials. 
  ## we'll bring in the supporting data with a table join.
  select(plot_visit, transect, slope, one_hour, ten_hour, hundred_hour) %>%
  arrange(plot_visit, transect) %>%
  ## Process it to biomass.
  ## First tidy the three fuel classes:
  pivot_longer(cols = c(one_hour, ten_hour, hundred_hour), names_to = 'fuel_type', values_to = 'values_raw') %>%
  mutate(slope_correction = round(sqrt(1+((slope/100)*(slope/100))), digits = 2)) %>%
  mutate(values_raw = as.numeric(values_raw))


## Stopped here for purposes of DNR report, decided to use summarized data instead.
%>%
  mutate(
    value_processed = case_when(
      fuel_type == 'one_hour' ~ (0.09533*((value_raw*slope_correction)/(6.56168))*2.2417),
      fuel_type == 'ten_hour' ~ (1.825*((value_raw*slope_correction)/(9.84252))*2.2417),
      fuel_type == 'hundred_hour' ~ (14.52*((value_raw*slope_correction)/(16.4042))*2.2417)
    )) %>%
head(fwd)  

