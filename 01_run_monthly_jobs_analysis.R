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
library(tidytext)

source("scripts/01_load_cps_jobs.R")
source("scripts/02_load_ces_jobs.R")
source("scripts/03_graphic_scripts.R")


total_title <- "Job Growth Remains Strongfffff"
total_jobs_graphic(ces_data, total_title)
ggsave("graphics/total_jobs.png", dpi = "retina", width = 12, height = 6.75, units = "in")


detailed_compare_2019(ces_data)
ggsave("graphics/compare_2019_detail.png",  width = 15, height=15, dpi="retina")

double_jobs_chart(ces_data, "Hello there")

black_white_comparison(cps_jobs_data)

by_type_title <- "This things"
unemployment_rate_by_type(cps_jobs_data, by_type_title)

duration_title <- "Unemployment duration slowly adjusting"
draw_u_duration(cps_jobs_data, duration_title)


three_six_wages_title <- "Hello"
three_six_wages(ces_data, three_six_wages_title)


make_jobs_chart(ces_data)