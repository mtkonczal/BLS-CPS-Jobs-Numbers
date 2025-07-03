# This
# This is a master file that runs multiple graphic creation function calls.
# Written by: Mike Konczal, Roosevelt Institute
# Last Updated: 1/5/2024

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")
library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(lubridate)
library(tidytext)
library(seasonal)


df <- cps_jobs_data %>% filter(date == "2023-01-01")


cps_sex_codes <- GET("https://download.bls.gov/pub/time.series/ln/ln.sexs", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()
cps_jobs_data <- inner_join(cps_jobs_data, cps_sex_codes, by = "sexs_code")

men <- cps_jobs_data %>% filter(sexs_text == "Men")

unique(men$ages_text)

a <- men %>% filter(date == "2020-08-01" & ages_text == "25 to 34 years")

cps_jobs_data %>% filter(series_id == "LNS12300164") %>%
  ggplot(aes(date, value)) + geom_line() + theme_classic()
# Point one: employment to population ratios decline slightly for 16-24 year old men, from 70 in 1948 to 60 in 2000, after 2000 they fall dramatically.
# for 20-24 80% to 70%, then big drop.
# For 25 to 34 year olds, it's in the 90s from 1950s through 1980, then high 80s from 1980 to 2000. After each recession they generally don't return to previous levels, and after the 2000 and 2008 recessions
# they fall off a cliff. This recession notably returned faster and quickly.


View(as_tibble(unique(ces_data$series_title)))

ces_data %>% filter(data_type_text == "AVERAGE HOURLY EARNINGS OF PRODUCTION AND NONSUPERVISORY EMPLOYEES")
LNS11027659
LNS12327659
cps_jobs_data %>% filter(series_id %in% c("LNS12327660","LNS12327659","LNS12327661","LNS12327662","LNS12327663")) %>%
  ggplot(aes(date, value, color=series_title)) + geom_line() + theme_classic()