# This script looks at employment growth both:
# - in periods of low unemployment, and
# - since 1980
# It creates two graphics in the graphics folder.
# It requires CES (for employment) and CPS (for unemployment rate) numbers.
# Written by: Mike Konczal, Roosevelt Institute
# Last Updated: 7/6/2021

library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(lubridate)
library(broom)

source("scripts/01_load_cps_jobs.R")
cps_lfst_code <- GET("https://download.bls.gov/pub/time.series/ln/ln.indy", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()
cps_jobs_data <- inner_join(cps_jobs_data, cps_lfst_code, by = "indy_code")

source("scripts/02_load_ces_jobs.R")


remote_codes <- c("LNU0201B67E",
"LNU0201B680",
"LNU0201B684",
"LNU0201B688",
"LNU0201B68A",
"LNU0201B686",
"LNU0201B68C",
"LNU0201B68E",
"LNU0201B690",
"LNU0201B692",
"LNU0201B694",
"LNU0201B682",
"LNU0201B696",
"LNU0201B698",
"LNU0201B69A",
"LNU0201B69C",
"LNU0201B69E",
"LNU0201B6A0",
"LNU0201B6A2",
"LNU0201B6A4",
"LNU0201B6A6",
"LNU0201B6A8",
"LNU0201B6AC",
"LNU0201B6AA",
"LNU0201B6AE",
"LNU0201B6B0",
"LNU0201B6B2",
"LNU0201B6B4",
"LNU0201B6B6",
"LNU0201B6B8",
"LNU0201B6BA",
"LNU0201B6BC")


cps_jobs_data %>% filter(series_id %in% remote_codes)



df <- unique(cps_jobs_data$series_title)

cps_jobs_data %>%
  filter(series_id == "LNU0201B67C") %>%
  ggplot(aes(date, value)) + geom_line()