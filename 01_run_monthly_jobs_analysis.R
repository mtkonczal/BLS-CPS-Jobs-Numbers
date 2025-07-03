library(tidyverse)
library(ggtext)
library(blsR)
library(govMacroTools)
source("scripts/03_graphic_scripts.R")


source("02_api_calls_1st_unrate_jobs.R")
source("02a_api_calls_1st_young_unrate.R")
source("03_api_calls_unrate_by_type_AHE.R")
source("04_api_calls_flows_4_types.R")


# Remote work API call still.
# source("08_remote_work.R")



ces_data <- getBLSFiles("ces", "rortybomb@gmail.com")
ces_data$data_type_code_org <- ces_data$data_type_code
ces_data$data_type_code <- as.numeric(ces_data$data_type_code)

# Two calls that use ces.
source("05_cyclical_industries.R")
make_jobs_chart(ces_data)


cps_jobs_data <- getBLSFiles("cps", "rortybomb@gmail.com")
# Two calls that use CPS.
source("06_unemployment_durations.R")
source("07_immigration.R")