## This script is for processing and tidying surface fuel data for the NPS Fire Effects in Washington State.
## Input data were downloaded from NPS data store: https://irma.nps.gov/DataStore/Reference/Profile/2271695.

## Script written by Don Radcliffe, PhD Student at University of Washington, dradclif@uw.edu.
## Collaborators Karen Kopper, National Park Service, and Brian Harvey, UW.
## Started 2021 May 01.
## See Github Repo https://github.com/don-radcliffe/nps_fx.

require(here)
require(dplyr)
require(stringr)
require(tidyr)
require(reshape2)

conflict_prefer('select', 'dplyr')
conflict_prefer('filter', 'dplyr')

import_dir_fwd <- here::here('data', 'data_raw')
export_dir_fwd <- here::here('data', 'data_tidy')

fwd_raw <- read.csv(file.path(import_dir_nps, 'fwd_raw.csv'), stringsAsFactors = TRUE)
