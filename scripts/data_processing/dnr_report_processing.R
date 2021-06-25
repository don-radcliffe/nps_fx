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

surface_fuels_raw <- read.csv(file.path(import_dir_n, 'surface_fuels_report_tidy.csv'))
trees_raw <- read.csv(file.path(import_dir_n, 'trees_tidy.csv'))
plot_visit_raw <- read.csv(file.path(import_dir_n, 'plot_visit_manual.csv'))
