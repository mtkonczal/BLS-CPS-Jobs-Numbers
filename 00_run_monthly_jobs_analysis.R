library(tidyverse)
library(ggtext)
library(blsR)
library(tidyusmacro)
source("scripts/graphic_scripts.R")


# https://www.bls.gov/web/empsit/cesnaicsrev.htm
ces_revisions <- read_csv("data/bls_ces_monthly_revisions.csv")
format(
  ces_revisions %>%
    filter(!is.na(sa_1st)) %>%
    filter(date == max(date)) %>%
    pull(date),
  '%B, %Y'
)


source("01_revisions_estimate.R")
source("02_unrate_jobs.R")
source("03_initial_tweet.R")

cat(paragraph, "\n")

source("04_health_care.R")
source("05_goods_loglines.R")
source("07_young_unrate.R")
source("08_immigration_unrate.R")
source("09_unrate_by_type_ahe.R")
source("10_flows_4_types.R")
source("11_unemployment_durations.R")
source("12_where_unrate_increased.R")

# Remote work API call still.
# source("08_remote_work.R")

ces_data <- getBLSFiles("ces", "rortybomb@gmail.com")
ces_data$data_type_code_org <- ces_data$data_type_code
ces_data$data_type_code <- as.numeric(ces_data$data_type_code)

# Two calls that use ces.
source("13_cyclical_industries.R")
make_jobs_chart(ces_data)
