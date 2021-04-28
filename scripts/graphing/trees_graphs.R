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

simpletheme <- theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

trees <- full_join(trees_raw, plot_data_raw, by = 'plot_visit') %>%
  ## -2 means early pretreatment read, only want to use most recent pretreatment read
  filter(years_post != -2) %>%
  filter(status == 'l')
head(trees)

basal_area <- ggplot(trees, aes(x = years_post, y = basal_area)) +
  geom_point(alpha = 0.3, size = 1.8) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  facet_grid(thins~burns, labeller = label_both) +
  ## Note my x scale cut off ten data points at the 20 years mark,
  ## mostly one thin one burn,
  ## to get better spread on most the data.
  scale_x_continuous(limits=c(-2,16)) + 
  scale_y_continuous(limits=c(0,75)) + 
  theme_bw() +
  ggtitle('Basal area in repeat treatment, North Cascades') +
  xlab('years after treatment combination') +
  ylab('basal area (meters squared per hectare)')
basal_area

density <- ggplot(trees, aes(x = years_post, y = density)) +
  geom_point(alpha = 0.3, size = 1.8) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  facet_grid(thins~burns, labeller = label_both) +
  ## Note my x scale cut off ten data points at the 20 years mark,
  ## mostly one thin one burn,
  ## to get better spread on most the data.
  scale_x_continuous(limits = c(-2, 16)) +
  scale_y_continuous(limits = c(0, 700)) +
  theme_bw() +
  ggtitle('Density in repeat treatment, North Cascades') +
  xlab('years after treatment combination') +
  ylab('density (trees per hectare)')
density

qmd <- ggplot(trees, aes(x = years_post, y = qmd)) +
  geom_point(alpha = 0.3, size = 1.8) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  facet_grid(thins~burns, labeller = label_both) +
  ## Note my x scale cut off ten data points at the 20 years mark,
  ## mostly one thin one burn,
  ## to get better spread on most the data.
  theme_bw() +
  scale_x_continuous(limits = c(-2, 16)) +
  ## Cutting out some of the more extreme y values.
  scale_y_continuous(limits = c(0,65)) +
  ggtitle('Quadratic mean diameter in repeat treatment, North Cascades') +
  xlab('years after treatment combination') +
  ylab('quadratic mean diameter (centimeters)')
qmd

basal_area_one <- ggplot(trees, aes(x = years_post, y = density, fill = as.factor(thins), lty = as.factor(burns))) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black')
basal_area_one  
