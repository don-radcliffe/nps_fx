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

trees_raw_noca <- read.csv(file.path(import_dir_nps, 'data_raw/trees_raw_noca.csv'), stringsAsFactors = TRUE)
trees_raw_laro <- read.csv(file.path(import_dir_nps, 'data_raw/trees_raw_laro.csv'), stringsAsFactors = TRUE)


##### Preprocessing ######

## Need to make a column to differentiate North Cascades and Lake Roosevelt data,
## and then combine the two csvs.
trees_laro <- trees_raw_laro
  rename(Date = 1)

trees_noca <- trees_raw_noca

trees_combined <- bind_rows(trees_laro, trees_noca)

## This first long chain of pipes makes the data more R and Don friendly.
trees1 <- trees_combined %>%
  ## Use mutate to rename columns to get rid of spaces and uppercase in column names,
  ## with tolower() to get rid of uppercase in values.
  mutate(date = tolower(Date)) %>%
  mutate(monitoring_status = tolower(Monitoring.Status)) %>%
  mutate(plot = tolower(MacroPlot.Name)) %>%
  mutate(plot_utm_zone = tolower(MacroPlot.UTM.Zone)) %>% 
  mutate(plot_utm_y = tolower(MacroPlot.UTM.Y)) %>%
  mutate(plot_utm_x = tolower(MacroPlot.UTM.X)) %>%
  mutate(area = tolower(MacroPlot.Purpose)) %>%
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
  ## I'm replacing it with 'nc' to denote 'North Cascades' or 'Lake Roosevelt'.
  ## Note there's possibly 2 pipo forest zones at Lake Roosevelt.
  mutate(plot = str_replace_all(plot, 'fpsme2d08-', 'nc')) %>%
  mutate(plot = str_replace_all(plot, 'fpipo1d10-', 'lr')) %>%
  mutate(plot = str_replace_all(plot, 'fpipo2d10-', 'lr')) %>%
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
  ## Get region (north cascades or lake roosevelt) from the prefix for the plots.
  mutate(region = str_sub(plot, start = 1, end = 2), .after = plot) %>%
  mutate(region = str_replace_all(region, c('lr' = 'lake roosevelt', 'nc' = 'north cascades'))) %>%
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
                                                  ## 10 is seeded pileburn, ignoring difference here.
                                                  '10' = 'pileburn',
                                                  ## 12 'most recently a pileburn', may be some broadcast burns here.
                                                  '12' = 'thin_thin_pileburn_pileburn',
                                                  ## 13 two burns, most recent is broadcast, also a seeding
                                                  '13' = 'thin_burn_burn',
                                                  ## 14 is seeded, ignoring that here.
                                                  '14' = 'thin_pileburn',
                                                  '15' = 'pileburn_pileburn',
                                                  ## 19 includes seeding, ignoring that here.
                                                  '19' = 'pileburn_pileburn',
                                                  '20' = 'thin_pileburn'))) %>%
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
                                          'thin_thin_pileburn_pileburn' = '0',
                                          'pileburn_pileburn' = '0',
                                          'thin_pileburn' = '0',
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
                                          'thin_thin_pileburn_pileburn' = '2',
                                          'pileburn_pileburn' = '0',
                                          'thin_pileburn' = '1',
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
                                                  'thin_thin_pileburn_pileburn' = '2',
                                                  'pileburn_pileburn' = '2',
                                                  'thin_pileburn' = '1',
                                                  'thin_burn' = '0',
                                                  'pileburn' = '1',
                                                  'burn' = '0',
                                                  'thin' = '0',
                                                  'pretreatment_early' = '0',
                                                  'pretreatment_current' = '0'))) %>%
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
  ## Evans and Sterling Valley Areas both got lr01 - lr03, need to fix here.
  ## Did this manually in my plot visit manual data: June 25, 2021
  mutate(plot = case_when(
    plot == 'lr01' & area == 'sterling valley' ~ 'lr91',
    plot == 'lr02' & area == 'sterling valley' ~ 'lr92',
    plot == 'lr03' & area == 'sterling valley' ~ 'lr93',
    TRUE ~ plot)) %>%
  ## Now we need a unique identifier for each plot visit so we can summarize data.
  unite(plot_visit, c('plot', 'year', 'treatment_code', 'years_post'), sep = '_', remove = FALSE) %>%
  ## Convert vector types for columns we'll work with below
  mutate(plot_visit = as.factor(plot_visit)) %>%
  mutate(species = as.factor(species)) %>%
  mutate(dbh = as.numeric(dbh)) %>%
  ## order live and dead.
  mutate(status = factor(status, levels = c('l','d'))) %>%
  ## Gifford clover at LARO has a bunch of rows with species label 'bare' with NAs for data, 
  ## removing those here.
  filter(species != 'bare') %>%
  ## There's also a row with a blank in species, changing to 'unkn',
  ## str_replace_all won't deal with blanks.
  mutate(species = sub(x = species, "^$", "unkn")) %>%
  ## One tree has two 'nas' for status, but was alive both before and after those NAs.  Fixing:
  mutate(status = replace_na(status, 'l')) %>%
  ## Get rid of spaces that area causing duplicate labels for kettle falls and ricky point areas
  mutate(area = str_replace_all(area, c('ricky north ' = 'ricky north', 'kettle falls ' = 'kettle falls',
                                        'evans ' = 'evans', 'gifford clover ' = 'gifford clover')))
  

## Some plot-visits have a lot of NAs in the dbh column, 
## mostly concentrated around the immediate post-treatment read but not always.
## They create a lot of false zeros in the stand structural dataframe,
## so I'm getting rid of them here. 
dbh_na <- trees1 %>% 
  group_by(plot_visit) %>% 
  summarize(number_of_na = sum(is.na(dbh))) %>%
#hist(dbh_na$number_of_na, breaks = 70).
## The histogram suggests there's a break in NAs after 4 (per plot).
## We can adjust this after talking with Karen.
 filter(number_of_na <5)

## The right join filters out plot visits with a lot of nas in dbh.
trees <- trees1 %>%
   right_join(dbh_na, by = 'plot_visit')
## And check it worked:
hist(trees$number_of_na)

##### Plot and Plot-Visit Dataframes ######

## Here I'm making the key for plot level information,
## so we don't have to hold onto a bunch of superfluous stuff when aggregating,
## i.e. so that we can use it like a relational database.
## We're using trees1 for this dataframe because it doesn't have the plots with too many dbh NAs filtered out.
plot_data <- trees1 %>%
  ## Select relevent columns.
  select(c('plot', 'plot_utm_zone', 'plot_utm_x', 'plot_utm_y', 'region', 'area')) %>%
  ## Rid duplicate rows and sort.
  unique() %>%
  arrange(plot)
## Save to 'data_tidy' folder if desired;
#write.csv(plot_data, file.path(export_dir_nps, 'plot_data.csv'), row.names = FALSE)


## We need to do the same thing for plot-visit level information
plot_visit_data <- trees1 %>%
  ## Select relavant columns.
  ## Purposefully leaving out 'monitoring_status,' redundant. 
  select(c('plot_visit', 'plot', 'region', 'area', 'year', 'date', 
           'treatment_code', 'years_post', 'treatment', 
           'burns', 'thins', 'pileburns', 'monitoring_status')) %>%
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
  pivot_wider(names_from = species, values_from = basal_area)%>%
  pivot_longer(c(pipo:abgr), names_to = 'species', values_to = 'basal_area') %>%
  ## Make zeros from the NAs that result.
  replace(is.na(.), 0) %>%
  mutate(basal_area = round(basal_area, digits = 1))

## Add them up to get a species blind dataframe
basal_area <- basal_area_spp %>%
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
  pivot_longer(cols = abgr:samy, names_to = 'species', values_to = 'density') %>%
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

