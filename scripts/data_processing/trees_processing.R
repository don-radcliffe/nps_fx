## This script is for processing and tidying trees data for the NPS Fire Effects in Washington State.
## Input data were downloaded from NPS data store: https://irma.nps.gov/DataStore/Reference/Profile/2271695.

## Script written by Don Radcliffe, PhD Student at University of Washington, dradclif@uw.edu.
## Collaborators Karen Kopper, National Park Service, and Brian Harvey, UW.
## Started 2021 April 05.
## See Github Repo https://github.com/don-radcliffe/nps_fx.

## Questions: heights of NA and zero?  Mostly dead trees?
## Dbh of NA and zero?

require(here)
require(dplyr)
require(stringr)
require(tidyr)
require(reshape2)

conflict_prefer('select', 'dplyr')
conflict_prefer('filter', 'dplyr')

import_dir_nps <- here::here('data')
export_dir_nps <- here::here('data', 'data_tidy')

trees_raw <- read.csv(file.path(import_dir_nps, 'data_raw/trees_raw.csv'), stringsAsFactors = TRUE)

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
#write.csv(plot_data, file.path(export_dir_nps, 'plot_data.csv'), row.names = FALSE)


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
#write.csv(plot_visit_data, file.path(export_dir_nps, 'plot_visit_data.csv'), row.names = FALSE)


##### Stand Structure #####

## This section is for creating plot-visit level stand structure metrics.

##### Basal Area #####

## Basal area per plot-visit, by species and live-dead status.
basal_area_spp <- trees %>%
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
  replace(is.na(.), 0) %>%
  mutate(basal_area = round(basal_area, digits = 1))

## Add them up to get a species blind dataframe
basal_area <- basal_area_species %>%
  aggregate(basal_area ~ plot_visit + status, data = ., FUN = sum) %>%
  mutate(basal_area = round(basal_area, digits = 1))


###### Density #######
density_spp <- trees %>%
  select(c(plot_visit, species, status)) %>%
  ## Add a column of tens to make for easy density calculation,
  ## each plot is 1/10 of a hectare so each tree sampled represents 10 trees per hectare.
  mutate(density = 10) %>%
  ## Aggregate that column.
  aggregate(density ~ plot_visit + species + status, data = ., FUN = sum) %>%
  ## Spread and gather to fill out species for each plot.
  pivot_wider(id_cols = c(plot_visit, status), names_from = species, values_from = density) %>%
  pivot_longer(cols = abam:samy, names_to = 'species', values_to = 'density') %>%
  mutate(density = replace_na(density, 0)) %>%
  arrange(plot_visit, species, status)

density <- density_spp %>%
  ## Just add up the density values for each species within a plot visit.
  aggregate(density ~ plot_visit + status, data = ., FUN = sum)


###### QMD ######

## Not sure how to handle the zeros in dbh. 

qmd_spp <- trees %>%
  select(c(plot_visit, species, status, dbh)) %>%
  ## Square dbh for the first part of the calculation.
  mutate(dbh2 = dbh*dbh) %>% 
  ## Average.
  aggregate(dbh2 ~ plot_visit + species + status, data = ., FUN = mean) %>%
  ## Finish the qmd calc with square root.
  mutate(qmd = round(sqrt(dbh2), digits = 1)) %>%
  ## Spread and gather for zeros where a species is not in a plot.
  pivot_wider(id_cols = c(plot_visit, status), names_from = species, values_from = qmd) %>%
  ## Only one ABAM on record and it had a dbh of NA, so it shows up in density but not here or basal area.
  pivot_longer(cols = abgr:samy, names_to = 'species', values_to = 'qmd') %>%
  mutate(qmd = replace_na(qmd, 0)) %>%
  arrange(plot_visit, species, status)

## Need to to it all over again for qmd no species
qmd <- trees %>%
  select(c(plot_visit, status, dbh)) %>%
  ## Square dbh for the first part of the calculation.
  mutate(dbh2 = dbh*dbh) %>% 
  ## Average over plot visit and status.
  aggregate(dbh2 ~ plot_visit + status, data = ., FUN = mean) %>%
  ## Square root
  mutate(qmd = round(sqrt(dbh2), digits = 1)) %>%
  select(plot_visit, status, qmd)


###### Tidy Tree Dataframes ######

tidy_tree_spp <- basal_area_spp %>%
  full_join(density_spp, by = c('plot_visit', 'status', 'species')) %>%
  full_join(qmd_spp, by = c('plot_visit', 'status', 'species')) %>%
  arrange(plot_visit, status, species) %>%
  replace(is.na(.), 0)
#write.csv(tidy_tree_spp, file.path(export_dir_nps, 'trees_spp_tidy.csv'), row.names = FALSE)

tidy_tree <- basal_area %>%
  full_join(density, by = c('plot_visit', 'status')) %>%
  full_join(qmd, by = c('plot_visit', 'status')) %>%
  arrange(plot_visit, status) %>%
  replace(is.na(.), 0)
#write.csv(tidy_tree, file.path(export_dir_nps, 'trees_tidy.csv'), row.names = FALSE)
