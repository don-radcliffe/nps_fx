## This script is for processing and tidying trees data for the NPS Fire Effects in Washington State
## Written by Don Radcliffe, PhD Student at University of Washington, dradclif@uw.edu
## See Github Repo https://github.com/don-radcliffe/nps_fx

library(dplyr)
library(tidyr)
library(here)

here('data_raw')

trees_raw <- read.csv('/data_raw/trees_raw.csv')
