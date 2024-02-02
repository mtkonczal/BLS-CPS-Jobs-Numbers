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

source("scripts/01_load_cps_jobs.R")
source("scripts/02_load_ces_jobs.R")
source("scripts/03_graphic_scripts.R")


total_title <- "Relax, it's a Soft Landing: Job Growth Remains Steady"
total_jobs_graphic(ces_data, total_title)
ggsave("graphics/total_jobs.png", dpi = "retina", width = 12, height = 6.75, units = "in")


detailed_compare_2019(ces_data)
ggsave("graphics/compare_2019_detail.png",  width = 15, height=15, dpi="retina")

double_jobs_chart(ces_data, "Hello there")
ggsave("graphics/double_jobs.png", dpi = "retina", width = 12, height = 8, units = "in")

black_white_comparison(cps_jobs_data)
ggsave("graphics/black_white_comparsion.png", dpi = "retina", width = 12, height = 8, units = "in")

by_type_title <- "This things"
unemployment_rate_by_type(cps_jobs_data, by_type_title)
ggsave("graphics/u_by_type.png", dpi = "retina", width = 12, height = 8, units = "in")

duration_title <- "Unemployment duration slowly adjusting"
draw_u_duration(cps_jobs_data, duration_title)
ggsave("graphics/duration.png", dpi = "retina", width = 12, height = 8, units = "in")

three_six_wages_title <- "Wage Growth, While Volatile, in Line With Lower Inflation"
three_six_wages(ces_data, three_six_wages_title)
ggsave("graphics/wages_3_6.png", dpi = "retina", width = 12, height = 8, units = "in")

make_jobs_chart(ces_data)

source("2_3_CBO_projections.R")

source("annual_wrapup.R")