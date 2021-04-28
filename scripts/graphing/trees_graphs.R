## This script is for graphing stand structure variables, in order to get a better sense of the study site.
## Don Radcliffe, dradclif@uw.edu,
## Started 27 April 2021.

require(here)
require(dplyr)
require(ggplot2)

import_dir_tg <- here::here('data', 'data_tidy')
export_dir_tg <- here::here('plots', 'trees')

trees <- read.csv(file.path(import_dir_tg, 'trees_tidy.csv'), stringsAsFactors = TRUE)
