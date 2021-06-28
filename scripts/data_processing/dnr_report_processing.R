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

plot_visits <- plot_visits_raw %>%
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
thinburn_plots_all <- plot_visits %>%
  filter(treatment == 'thin_burn' | treatment == 'burn_thin_burn') %>%
  select(plot) %>%
  unique()

thinburn_plots_only <- plot_visits %>%
  filter(treatment == 'thin_burn') %>%
  select(plot) %>%
  unique()

burnthinburn_plots_only <- plot_visits %>%
  filter(treatment == 'burn_thin_burn') %>%
  select(plot) %>%
  unique()

year_15_plots <- plot_visits %>%
  filter(years_post == 15) %>%
  #inner_join(thinburn_plots, by = 'plot') %>%
  select(plot) %>%
  unique()

nps1 <- plot_visits %>%
  inner_join(both, by = 'plot_visit')

nps_tb <- nps1 %>%
  inner_join(thinburn_plots_only, by = 'plot') %>%
  mutate(treatment = str_replace_all(treatment, 'pretreatment_current', 'thin_burn'))

nps_btb <- nps1 %>%
  inner_join(burnthinburn_plots_only, by = 'plot') %>%
  mutate(treatment = str_replace_all(treatment, 'pretreatment_current', 'burn_thin_burn'))
  
nps <- bind_rows(nps_tb, nps_btb) %>%
  ## burnthinburn seems functionally similar to thin burn
  #mutate(treatment = str_replace_all(treatment, c('burn_thin_burn' = 'thin_burn'))) %>%
  ## This will be better data for our question if we take burn and thin out, 
  ## leaving whatever pretreatment plots left as control for thinburn. 
  inner_join(thinburn_plots, by = 'plot') %>%
  ## We get some weird effects from not all plots going 15 years, fixing that:
  filter(treatment == 'thin_burn' | treatment == 'burn_thin_burn') %>%
  mutate(treatment = str_replace_all(treatment, c('burn_thin_burn' = 'Burn thin burn',
                                                  'thin_burn' = 'Thin burn'))) %>%
  mutate(treatment = factor(treatment, levels = c('Thin burn', 'Burn thin burn'))) %>%
  mutate(forest_type = str_replace_all(region, c('lake roosevelt' = 'Ponderosa pine',
                                                 'north cascades' = 'Mixed conifer'))) %>%
  mutate(forest_type = factor(forest_type, levels = c('Ponderosa pine', 'Mixed conifer')))
 
 

require(ggplot2)

gg_fwd <- nps %>%
  filter(variable == 'fwd') %>%
  ggplot(aes(x = years_post, y = value, color = forest_type, pch = treatment, group = plot)) +
  geom_point() +  
  geom_line() +
  theme_bw()
gg_fwd

###### Aggregate for DNR #######

nps_agg_by_year <- nps %>%
  ## Let's get rid of low sample size years
  filter(years_post == -1 | years_post == 0 | years_post == 1 | years_post == 2 | 
         years_post == 5 | years_post == 10 | years_post == 15) %>%
  aggregate(value ~ variable + treatment + years_post + forest_type, data = ., FUN = mean)

gg_fwd <- nps_agg_by_year %>%
  filter(variable == 'fwd') %>%
  ggplot(aes(x = years_post, y = value, color = treatment, pch = forest_type)) +
  geom_point(size = 3) +
  geom_line() +
  ggtitle('Twig dynamics in NPS Fire Effects Monitoring sites, Washington') +
  xlab('Years after treatment') +
  ylab('Megagrams per hectare') +
  scale_y_continuous(limits = c(0,10),
                     breaks = c(0,2,4,6,8,10)) +
  scale_color_discrete(name = 'Treatment') +
  scale_shape_discrete(name = 'Forest type') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(),
             panel.background = element_blank())
gg_fwd

#ggsave(gg_fwd, device = 'png', filename = 'fwd.png', width = 8, height = 4, units = 'in')

nps_for_dnr <- nps_agg_by_year %>%
  mutate(units = case_when(
    variable == 'fwd' | variable == 'cwd' | variable == 'litterduff' ~ 'Mg/ha',
    variable == 'basal_area' ~ 'm2/ha', 
    variable == 'density' ~ 'trees/ha', 
    variable == 'qmd' ~ 'cm')) %>%
  mutate(study = 'Radcliffe3') %>%
  mutate(forest_type = case_when(
    region == 'lake roosevelt' ~ 'Ponderosa Pine',
    region == 'north cascades' ~ 'Mixed Conifer',
  )) %>%
  mutate(region = 'Washington') %>%
  mutate(burn_season = 'mixed') %>%
  mutate(thin_type = 'commercial') %>%
  mutate(other = NA) %>%
  mutate(other2 = NA) %>%
  mutate(value = round(value, digits = 2)) %>%
  select(value, variable, units, years_post, treatment, study, forest_type,
         region, burn_season, thin_type, other, other2)

## Export if desired
#write.csv(nps_for_dnr, file.path(export_dir_n, 'nps_for_dnr.csv'), row.names = FALSE)
