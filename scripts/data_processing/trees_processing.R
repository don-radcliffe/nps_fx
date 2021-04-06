## This script is for processing and tidying trees data for the NPS Fire Effects in Washington State
## Input data straight from FFI database

## Script written by Don Radcliffe, PhD Student at University of Washington, dradclif@uw.edu
## Collaborators Karen Kopper, National Park Service, and Brian Harvey, UW
## Started 2021 April 05
## See Github Repo https://github.com/don-radcliffe/nps_fx

library(dplyr)
library(tidyr)
library(here)

## Here skips the 'data' folder, wtf?? Try it if you're running on a different machine
#here('data/')

setwd('C:/ProgramR/nps_fx/data')
trees_raw <- read.csv('data_raw/trees_raw.csv')
#str(trees_raw)

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
  ## to remove the original columns; for some reason transmute() wouldn't work
  select(c(date:visited))

View(trees)           
  
  
  
  

