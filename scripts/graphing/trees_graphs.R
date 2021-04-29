## This script is for graphing stand structure variables, in order to get a better sense of the study site.
## Don Radcliffe, dradclif@uw.edu,
## Started 27 April 2021.

require(here)
require(dplyr)
require(ggplot2)
require(ggthemes)

import_dir_tg <- here::here('data', 'data_tidy')
export_dir_tg <- here::here('plots', 'trees')

trees_raw <- read.csv(file.path(import_dir_tg, 'trees_tidy.csv'), stringsAsFactors = TRUE)
plot_data_raw <- read.csv(file.path(import_dir_tg, 'plot_visit_data.csv'), stringsAsFactors = TRUE)

## Theme for graphing.
simpletheme <- theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.line = element_line(size = 0.2, color = 'black')) +
  theme(legend.key=element_blank(), legend.background=element_blank())

## Common color scheme for areas
area_colors <- c('weaver' = 'gray0', 'boulder' = 'gray8', 'orchard rainbow' = 'gray16', 'company creek' = 'gray24',
                'lower mcgregor' = 'gray32', 'mcgregor' = 'gray40', 'upper mcgregor' = 'gray48', 'bullion' = 'gray56')

## And for treatments.
treatment_colors <- c('pretreatment_current' = 'dodgerblue3', 
                      'pileburn' = 'purple3', 'burn' = 'red3', 'thin' = 'gold1',
                      'thin_burn' = 'gold3', 'thin_burn_burn' = 'tan4',
                      'thin_thin_burn' = 'gray40', 'thin_thin_burn_burn' = 'gray20', 'thin_thin_burn_burn_burn' = 'gray0')

trees <- full_join(trees_raw, plot_data_raw, by = 'plot_visit') %>%
  ## -2 means early pretreatment read, only want to use most recent pretreatment read
  filter(years_post != -2) %>%
  ## There are some weird zeros in the data that 
  filter(status == 'l') %>%
  ## Arrange treatments in some kind of order.
  mutate(treatment = factor(treatment, levels = c('pretreatment_current', 'pileburn', 
                               'burn', 'thin',
                               'thin_burn', 'thin_burn_burn', 
                               'thin_thin_burn', 'thin_thin_burn_burn','thin_thin_burn_burn_burn'))) %>%
  ## Arrange areas by proximity to Stehekin, 
  ## don't see Bullion on my map, maybe it replaced a problematic older name that's farthest away.
  mutate(area = factor(area, levels = c('weaver', 'boulder', 'orchard rainbow', 'company creek',
                                        'lower mcgregor', 'mcgregor', 'upper mcgregor', 'bullion')))

basal_area <- ggplot(trees, aes(x = years_post, y = basal_area, group = plot)) +
  geom_point(alpha = 0.5, size = 1.4) +
  geom_line(size = 1.1, aes(color = area)) +
  facet_grid(thins~burns, labeller = label_both) +
  ## Note my x scale cut off ten data points at the 20 years mark,
  ## mostly one thin one burn,
  ## to get better spread on most the data.
  scale_x_continuous(limits=c(-2,16)) + 
  scale_y_continuous(limits=c(0,75)) + 
  scale_color_manual(values = area_colors) +
  theme_bw() +
  ggtitle('Basal area in repeat treatment, North Cascades') +
  xlab('years after treatment combination') +
  ylab('basal area (meters squared per hectare)')
basal_area

density <- ggplot(trees, aes(x = years_post, y = density, group = plot)) +
  geom_point(alpha = 0.3, size = 1.8) +
  geom_line(size = 1.1, aes(color = area)) +
  facet_grid(thins~burns, labeller = label_both) +
  ## Note my x scale cut off ten data points at the 20 years mark,
  ## mostly one thin one burn,
  ## to get better spread on most the data.
  scale_x_continuous(limits = c(-2, 16)) +
  scale_y_continuous(limits = c(0, 700)) +
  scale_color_manual(values = area_colors) +
  theme_bw() +
  ggtitle('Density in repeat treatment, North Cascades') +
  xlab('years after treatment combination') +
  ylab('density (trees per hectare)')
density

qmd <- ggplot(trees, aes(x = years_post, y = qmd, group = plot)) +
  geom_point(alpha = 0.3, size = 1.8) +
  geom_line(size = 1.1, aes(color = area)) +
  facet_grid(thins~burns, labeller = label_both) +
  ## Note my x scale cut off ten data points at the 20 years mark,
  ## mostly one thin one burn,
  ## to get better spread on most the data.
  theme_bw() +
  scale_x_continuous(limits = c(-2, 16)) +
  ## Cutting out some of the more extreme y values.
  scale_y_continuous(limits = c(0,65)) +
  scale_color_manual(values = area_colors) +
  ggtitle('Quadratic mean diameter in repeat treatment, North Cascades') +
  xlab('years after treatment combination') +
  ylab('quadratic mean diameter (centimeters)')
qmd

## Let's try a more longitutinal graph. 
basal_area_one <- ggplot(trees, aes(x = year, y = basal_area, group = plot, color = treatment)) +
  geom_line(size = 1.1, aes(color = treatment)) + 
  scale_color_manual(values = treatment_colors) +
  simpletheme +
  ggtitle('Longitudinal basal area changes, Lake Roosevelt') +
  ylab('basal area (meters squared per hectare)')
basal_area_one  

density_one <- ggplot(trees, aes(x = year, y = density, group = plot, color = treatment)) +
  geom_line(size = 1.1, aes(color = treatment)) + 
  scale_color_manual(values = treatment_colors) +
  simpletheme +
  ggtitle('Longitudinal density changes, Lake Roosevelt') +
  ylab('density (trees per hectare)')
density_one

qmd_one <- ggplot(trees, aes(x = year, y = qmd, group = plot, color = treatment)) +
  geom_line(size = 1.1, aes(color = treatment)) + 
  scale_color_manual(values = treatment_colors) +
  simpletheme +
  ggtitle('Longitudinal qmd changes, Lake Roosevelt') +
  ylab('quadratic mean diameter (centimeters)')
qmd_one
