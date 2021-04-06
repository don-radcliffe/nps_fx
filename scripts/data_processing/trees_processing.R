## This script is for processing and tidying trees data for the NPS Fire Effects in Washington State
## Input data straight from FFI database

## Script written by Don Radcliffe, PhD Student at University of Washington, dradclif@uw.edu
## Collaborators Karen Kopper, National Park Service, and Brian Harvey, UW
## Started 2021 April 05
## See Github Repo https://github.com/don-radcliffe/nps_fx

library(dplyr)
library(stringr)
library(tidyr)
library(here)

here('data')
trees_raw <- read.csv('data_raw/trees_raw.csv')

## Start processing the tree data
trees <- trees_raw %>%
  ## Use mutate to rename columns to get rid of spaces and uppercase in column names
  ## with tolower() to get rid of uppercase in values
  mutate(date = tolower(Date)) %>%
  mutate(monitoring_status = tolower(Monitoring.Status)) %>%
  mutate(macroplot = tolower(MacroPlot.Name)) %>%
  mutate(macroplot_utm_zone = tolower(MacroPlot.UTM.Zone)) %>% 
  mutate(macroplot_utm_y = tolower(MacroPlot.UTM.Y)) %>%
  mutate(macroplot_utm_x = tolower(MacroPlot.UTM.X)) %>%
  mutate(macroplot_purpose = tolower(MacroPlot.Purpose)) %>%
  mutate(species = tolower(Species.Symbol)) %>%
  mutate(qtr = tolower(QTR)) %>%
  mutate(tag = tolower(TagNo)) %>%
  mutate(status = tolower(Status)) %>%
  mutate(dbh = tolower(DBH)) %>%
  mutate(ht = tolower(Ht)) %>%
  mutate(crown = tolower(CrwnCl)) %>%
  mutate(dam_cd_1 = tolower(DamCd1)) %>%
  mutate(dam_sev_1 = tolower(DamSev1)) %>%
  mutate(dam_cd_2 = tolower(DamCd2)) %>%
  mutate(dam_sev_2 = tolower(DamSev2)) %>%
  mutate(dam_cd_3 = tolower(DamCd3)) %>%
  mutate(dam_sev_3 = tolower(DamSev3)) %>%
  mutate(dam_cd_4 = tolower(DamCd4)) %>%
  mutate(dam_sev_4 = tolower(DamSev4)) %>%
  mutate(dam_cd_5 = tolower(DamCd5)) %>%
  mutate(dam_sev_5 = tolower(DamSev5)) %>%
  mutate(comment = tolower(Comment)) %>%
  mutate(uv1 = tolower(UV1)) %>%
  mutate(uv2 = tolower(UV2)) %>%
  mutate(uv3 = tolower(UV3)) %>%
  mutate(visited = tolower(Visited)) %>%
  ## need to remove the original columns; for some reason transmute() wouldn't work
  select(c(date:visited)) %>%
  ## below I run string splits on the monitoring_status column to isolate treatment code and year
  ## by cutting off the first two and last two values in the string
  ## a few codes cause problems: so I change them here so it gives me what I want later
  ## 00pre, immediate pre-treatment, to -1pre-1
  ## 00pro01, older pre-treatment, to -2post-2
  ## XXpost, immediate post treatment with XX being treatment code, to XXpost00
  mutate(monitoring_status = str_replace_all(monitoring_status, c('00pre' = '-1pre-1',
                                                                  '00pro01' = '-2pre-2',
                                                                  'post' = 'post00'))) %>%
  ## substr() the monitoring status column to get treatment code and year
  ## .after command places it right after monitoring status code
  mutate(years_post = str_sub(monitoring_status, start = -2), .after = monitoring_status) %>%
  mutate(treatment_code = str_sub(monitoring_status, end = 2), .after = monitoring_status) %>%
  ## now copy the the treatment code column
  ## so we can turn the codes into treatment names while keeping the treatment code column as a check
  mutate(treatment = treatment_code, .after = years_post) %>%
  ## now rename them, treatments in chronological order
  mutate(treatment = str_replace_all(treatment, c('-2' = 'pretreatment_early',
                                                  '-1' = 'pretreatment_current',
                                                  '01' = 'burn',
                                                  '02' = 'thin',
                                                  '03' = 'pileburn',
                                                  '04' = 'thin_burn',
                                                  '05' = 'thin_burn_burn',
                                                  '06' = 'thin_burn_burn_burn',
                                                  '07' = 'thin_thin_burn',
                                                  '08' = 'thin_thin_burn_burn',
                                                  '09' = 'thin_thin_burn_burn_burn',
                                                  '20' = 'pileburn_thin'))) %>%
  ## let's also give numeric columns for numbers of the different treatments
  ## there's probably a more elegant way to do this
  ## burns
  mutate(burns = treatment, .after = treatment) %>%
  mutate(burns = str_replace_all(burns, c('pretreatment_early' = '0',
                                          'pretreatment_current' = '0',
                                          'burn' = '1',
                                          'thin' = '0',
                                          'pileburn' = '0',
                                          'thin_burn' = '1',
                                          'thin_burn_burn' = '2',
                                          'thin_burn_burn_burn' = '3',
                                          'thin_thin_burn' = '1',
                                          'thin_thin_burn_burn' = '2',
                                          'thin_thin_burn_burn_burn' = '3',
                                          'pileburn_thin' = '0'))) %>%
  ## thins
  mutate(thins = treatment, .after = burns) %>%
  mutate(thins = str_replace_all(thins, c('pretreatment_early' = '0',
                                          'pretreatment_current' = '0',
                                          'burn' = '0',
                                          'thin' = '1',
                                          'pileburn' = '0',
                                          'thin_burn' = '1',
                                          'thin_burn_burn' = '1',
                                          'thin_burn_burn_burn' = '1',
                                          'thin_thin_burn' = '2',
                                          'thin_thin_burn_burn' = '2',
                                          'thin_thin_burn_burn_burn' = '2',
                                          'pileburn_thin' = '0'))) %>%
  ## pileburns
  mutate(pileburns = treatment, .after = thins) %>%
  mutate(pileburns = str_replace_all(pileburns, c('pretreatment_early' = '0',
                                          'pretreatment_current' = '0',
                                          'burn' = '0',
                                          'thin' = '0',
                                          'pileburn' = '1',
                                          'thin_burn' = '0',
                                          'thin_burn_burn' = '0',
                                          'thin_burn_burn_burn' = '0',
                                          'thin_thin_burn' = '0',
                                          'thin_thin_burn_burn' = '0',
                                          'thin_thin_burn_burn_burn' = '0',
                                          'pileburn_thin' = '1'))) %>%
  ## now we need a unique identifier for plot by treatment by sample period so we can summarize data by plot
  unite(macroplot_unique, c('macroplot', 'treatment_code', 'years_post'), sep = '_', remove = FALSE) %>%
  ## now lets fix that date column
  ## split the date from the time
  ## NA argument gets rid of the time, which isn't useful
  separate(date, c('date', NA), sep = ' ') %>%
  ## format to a date
  mutate(date = as.Date(date, '%m/%d/%Y')) %>%
  ## format to iso 8601 for sorting
  mutate(date = format(date, '%Y/%m/%d'))

View(trees)

