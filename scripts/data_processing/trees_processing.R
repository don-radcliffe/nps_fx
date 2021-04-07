## This script is for processing and tidying trees data for the NPS Fire Effects in Washington State.
## Input data were downloaded from NPS data store: https://irma.nps.gov/DataStore/Reference/Profile/2271695.

## Script written by Don Radcliffe, PhD Student at University of Washington, dradclif@uw.edu.
## Collaborators Karen Kopper, National Park Service, and Brian Harvey, UW.
## Started 2021 April 05.
## See Github Repo https://github.com/don-radcliffe/nps_fx.

library(here)
library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)

## The here() function for automatically finding the working directory is only working sporadically working for me.
## try the command below if you're working on a different machine
here('data')

## The 'Sorry Jenny Bryan' Option
#setwd('C:/ProgramR/nps_fx/data')
#trees_raw <- read.csv('data_raw/trees_raw.csv')

##### Preprocessing ######

## This first long chain of pipes makes the data more R and Don friendly.
trees <- trees_raw %>%
  ## Use mutate to rename columns to get rid of spaces and uppercase in column names,
  ## with tolower() to get rid of uppercase in values.
  mutate(date = tolower(Date)) %>%
  mutate(monitoring_status = tolower(Monitoring.Status)) %>%
  mutate(plot = tolower(MacroPlot.Name)) %>%
  mutate(plot_utm_zone = tolower(MacroPlot.UTM.Zone)) %>% 
  mutate(plot_utm_y = tolower(MacroPlot.UTM.Y)) %>%
  mutate(plot_utm_x = tolower(MacroPlot.UTM.X)) %>%
  mutate(plot_purpose = tolower(MacroPlot.Purpose)) %>%
  mutate(species = tolower(Species.Symbol)) %>%
  mutate(quarter = tolower(QTR)) %>%
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
  ## Remove the originally-named columns.
  select(c(date:visited)) %>%
  ## Plot has an annoying string of the same ten characters in front of each meaningful identifier.
  ## I'm replacing it with 'nc' to denote 'North Cascades'.
  mutate(plot = str_replace_all(plot, 'fpsme2d08-', 'nc')) %>%
  ## There are some blank rows at the top of each plot that need ridding.
  ## Quarter seems to have NAs each time this is the case, but not otherwise.
  filter(complete.cases(quarter)) %>%
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
  ## Now copy the the treatment code column,
  ## so we can turn the codes into treatment names while keeping the treatment code column as a check.
  mutate(treatment = treatment_code, .after = years_post) %>%
  ## Now rename them, treatments in chronological order.
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
  ## Let's also give numeric columns for numbers of the different treatments.
  ## There's probably a more elegant way to do this.
  ## You have to invert the order of the str_replace_all() commands to not premature replacement.
  ## Burns:
  mutate(burns = treatment, .after = treatment) %>%
  mutate(burns = str_replace_all(burns, c('thin_thin_burn_burn_burn' = '3',
                                          'thin_thin_burn_burn' = '2',
                                          'thin_thin_burn' = '1',
                                          'thin_burn_burn_burn' = '3',
                                          'thin_burn_burn' = '2',
                                          'pileburn_thin' = '0',
                                          'thin_burn' = '1',
                                          'pileburn' = '0',
                                          'burn' = '1',
                                          'thin' = '0',
                                          'pretreatment_early' = '0',
                                          'pretreatment_current' = '0'))) %>%
  ## Thins:
  mutate(thins = treatment, .after = burns) %>%
  mutate(thins = str_replace_all(thins, c('thin_thin_burn_burn_burn' = '2',
                                          'thin_thin_burn_burn' = '2',
                                          'thin_thin_burn' = '2',
                                          'thin_burn_burn_burn' = '1',
                                          'thin_burn_burn' = '1',
                                          'pileburn_thin' = '1',
                                          'thin_burn' = '1',
                                          'pileburn' = '0',
                                          'burn' = '0',
                                          'thin' = '1',
                                          'pretreatment_early' = '0',
                                          'pretreatment_current' = '0'))) %>%
  ## Pileburns:
  mutate(pileburns = treatment, .after = thins) %>%
  mutate(pileburns = str_replace_all(pileburns, c('thin_thin_burn_burn_burn' = '0',
                                                  'thin_thin_burn_burn' = '0',
                                                  'thin_thin_burn' = '0',
                                                  'thin_burn_burn_burn' = '0',
                                                  'thin_burn_burn' = '0',
                                                  'pileburn_thin' = '1',
                                                  'thin_burn' = '0',
                                                  'pileburn' = '1',
                                                  'burn' = '0',
                                                  'thin' = '0',
                                                  'pretreatment_early' = '0',
                                                  'pretreatment_current' = '0'))) %>%
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
  mutate(species = as.factor(species)) %>%
  mutate(dbh = as.numeric(dbh)) %>%
  mutate(status = factor(status, levels = c('l','d')))

##### Plot and Plot-Visit Dataframes ######

## Here I'm making the key for plot level information,
## so we don't have to hold onto a bunch of superfluous stuff when aggregating,
## i.e. so that we can use it like a relational database.
plot_data <- trees %>%
  ## Select relevent columns.
  select(c('plot', 'plot_utm_zone', 'plot_utm_x', 'plot_utm_y', 'plot_purpose')) %>%
  ## Rid duplicate rows and sort.
  unique() %>%
  arrange(plot)
## Save to 'data_tidy' folder if desired;
#write.csv(plot_data, 'data_tidy/plot_data.csv', row.names = FALSE)


## We need to do the same thing for plot-visit level information
plot_visit_data <- trees %>%
  ## Select relavant columns.
  ## Purposefully leaving out 'monitoring_status,' redundant. 
  select(c('plot_visit', 'plot', 'year', 'date', 
           'treatment_code', 'years_post', 'treatment', 
           'burns', 'thins', 'pileburns')) %>%
  ## Rid duplicate rows and sort.
  unique() %>%
  arrange(plot_visit)
## Save to 'data_tidy' folder if desired
#write.csv(plot_visit_data, 'data_tidy/plot_visit_data.csv', row.names = FALSE)


##### Stand Structure #####

## This section is for creating plot-visit level stand structure metrics.

## Basal area per plot-visit.
basal_area_species <- trees %>%
  ## Select relevant columns.
  select(c(plot_visit, species, dbh, status)) %>%
  ## Make a column for basal area (m²/ha) per individual tree.
  ## Plots are 20x50m, 1000m², 1/10 ha. 
  ## And dbh is in cm.
  ## So the formula is pi*radius²[(((dbh/2)^2)*pi], convert cm² to meters² [*0.0001], then scale to hectares [*10].
  mutate(basal_area = (((dbh/2)^2)*pi*0.0001*10)) %>%
  ## Summarize basal area by plot_visit + species + status.
  dcast(plot_visit + species + status ~ ., value.var = 'basal_area', sum) %>%
  rename('basal_area' = '.') %>%
  ## Fill out all the species in each plot.
  pivot_wider(names_from = species, values_from = basal_area) %>%
  pivot_longer(c(pipo:abgr), names_to = 'species', values_to = 'basal_area') %>%
  ## Make zeros from the NAs that result.
  replace(is.na(basal_area), 0)
