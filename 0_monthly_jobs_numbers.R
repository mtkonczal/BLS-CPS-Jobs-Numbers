# This script does some data analysis on jobs.
# Written by: Mike Konczal
# Last Updated: 3-12-2022

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")
library(tidyverse)
library(ggtext)
library(scales)
library(lubridate)

##### SET UP SOME THINGS #####
source(file = "1_a_load_bls_cps_jobs_data.R")
source(file = "1_b_load_bls_ces_jobs_data.R")

source(file = "2_1_epop_change.R")
source(file = "2_2_lfp_change.R")
source(file = "2_3_cbo_projections.R")
source(file = "2_4_low_U_job_growth.R")
source(file = "2_5_2018_comparsion.R")
source(file = "2_6_wages_compression.R")