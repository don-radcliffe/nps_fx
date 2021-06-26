## This script is for formatting the data for the DNR report.
## Going to use plot_visit_manual to assign treatments and years from now on; 
## There were many ambiguities or errors in the coding system that necessitates manual checking. 

require(here)
require(dplyr)
require(stringr)
require(tidyr)
require(reshape2)

import_dir_n <- here::here('data', 'data_tidy')
export_dir_n <- here::here('data', 'data_tidy')

surface_fuels_raw <- read.csv(file.path(import_dir_n, 'surface_fuels_report_tidy.csv'), stringsAsFactors = TRUE)
trees_raw <- read.csv(file.path(import_dir_n, 'trees_tidy.csv'), stringsAsFactors = TRUE)
plot_visits_raw <- read.csv(file.path(import_dir_n, 'plot_visit_manual.csv'), stringsAsFactors = TRUE)

plot_visits <- plot_visit_raw %>%
  ## Taking out plots with logical inconsistencies or which I couldn't be sure of. 
  filter(cut == 0) %>%
  select(plot_visit, plot, year, years_manual, treatment_manual, region, area) %>%
  rename(c('treatment' = treatment_manual, 'years_post' = years_manual)) %>%
  filter(treatment == 'pretreatment_current' | treatment == 'burn' | treatment == 'thin' |
         treatment == 'thin_burn' | treatment == 'thin_burn' | treatment == 'burn_thin' |
         treatment == 'burn_thin_burn')

trees <- trees_raw %>% 
  filter(status =='l') %>%
  select(-status) %>%
  filter(basal_area < 200) %>%
  filter(qmd < 61) %>%
  pivot_longer(cols = c(basal_area, density, qmd), names_to = 'variable', values_to = 'value')

surface_fuels <- surface_fuels_raw %>% 
  mutate(cwd = thousand_sound + thousand_rotten) %>%
  mutate(litterduff = litter + duff) %>%
  select(plot_visit, fwd, cwd, litterduff) %>%
  filter(fwd < 30) %>%
  filter(cwd < 100) %>%
  filter(litterduff < 110) %>%
  pivot_longer(cols = c(fwd, cwd, litterduff), names_to = 'variable', values_to = 'value')

both <- bind_rows(trees, surface_fuels)

## To know which plots to assign as controls for thinburn
thinburn_plots <- plot_visits %>%
  filter(treatment == 'thin_burn') %>%
  select(plot) %>%
  unique()

year_15_plots <- plot_visits %>%
  filter(years_post == 15) %>%
  inner_join(thinburn_plots, by = 'plot') %>%
  select(plot) %>%
  unique()

nps <- plot_visits %>%
  inner_join(both, by = 'plot_visit') %>%
  ## burnthinburn seems functionally similar to thin burn
  #mutate(treatment = str_replace_all(treatment, c('burn_thin_burn' = 'thin_burn'))) %>%
  ## This will be better data for our question if we take burn and thin out, 
  ## leaving whatever pretreatment plots left as control for thinburn. 
  inner_join(thinburn_plots, by = 'plot') %>%
  ## We get some weird effects from not all plots going 15 years, fixing that:
  inner_join(year_15_plots, by = 'plot') %>%
  mutate(treatment = str_replace_all(treatment, c('pretreatment_current' = 'thin_burn'))) %>%
  filter(treatment == 'thin_burn')


require(ggplot2)

gg_fwd <- nps %>%
  filter(variable == 'fwd') %>%
  ggplot(aes(x = years_post, y = value, color = region, group = plot)) +
  geom_point() +  
  geom_line() +
  theme_bw()
gg_fwd

###### Aggregate for DNR #######

nps_agg_by_year <- nps %>%
  ## Let's get rid of low sample size years
  filter(years_post == -1 | years_post == 0 | years_post == 1 | years_post == 2 | 
         years_post == 5 | years_post == 10 | years_post == 15) %>%
  aggregate(value ~ variable + treatment + years_post + region, data = ., FUN = mean)

gg_litterduff <- nps_agg_by_year %>%
  filter(variable == 'litterduff') %>%
  ggplot(aes(x = years_post, y = value, color = region)) +
  geom_point(size = 3) +
  geom_line() +
  theme_bw() 
gg_litterduff

