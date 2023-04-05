# This script looks at employment growth both:
# - in periods of low unemployment, and
# - since 1980
# It creates two graphics in the graphics folder.
# It requires CES (for employment) and CPS (for unemployment rate) numbers.
# Written by: Mike Konczal, Roosevelt Institute
# Last Updated: 7/6/2021

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")
library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(lubridate)

##### SET UP SOME THINGS #####
#source(file = "1_a_load_bls_cps_jobs_data.R")
#source(file = "1_b_load_bls_ces_jobs_data.R")
#source(file = "1_c_load_bls_ces_wages_data.R")
#load("data/cps_jobs_data.RData")
#load("data/ces_data.RData")



flows_series_ids <- c("LNS17000000",
"LNS17100000",
"LNS17200000",
"LNS17400000",
"LNS17500000",
"LNS17600000",
"LNS17800000",
"LNS17900000",
"LNS18000000")

a <- cps_jobs_data %>% filter(date == max(date), seasonal == "S", sexs_code == "0")

cps_jobs_data %>% filter(series_id %in% flows_series_ids) %>%
  mutate(series_title = str_remove_all(series_title,"\\(Seas\\) Labor Force Flows")) %>%
  ggplot(aes(date, value)) + geom_line() + theme_classic() + facet_wrap(~series_title, scales = "free")



ggsave("graphics/2018_services_wage_growth.png", width = 9.5, height=5.34, dpi="retina")