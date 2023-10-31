# A script that runs all the other scripts that
# read in the monthly BLS jobs numbers, and
# then do some analysis and create several graphics.
#
# Written by: Mike Konczal
# Last Updated: 10-6-22

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")
library(hrbrthemes)
library(tidyverse)
library(ggtext)
library(scales)
library(lubridate)
library(httr)
library(data.table)
library(magrittr)


theme_lass <-   theme_modern_rc() + theme(legend.position = "none", legend.title = element_blank(),
                                          panel.grid.major.y = element_line(size=0.5),
                                          panel.grid.minor.y = element_blank(),
                                          plot.title.position = "plot",
                                          axis.title.x = element_blank(),
                                          axis.title.y = element_blank(),
                                          plot.title = element_text(size = 25, face="bold"),
                                          plot.subtitle = element_text(size=15, color="white"),
                                          plot.caption = element_text(size=10, face="italic"),
                                          legend.text = element_text(size=12),
                                          axis.text.y = element_text(size=12, face="bold"),
                                          axis.text.x = element_text(size=12, face="bold"),
                                          strip.text = element_text(face = "bold", color="white", hjust = 0.5, size = 10),
                                          panel.grid.major.x = element_blank(),
                                          panel.grid.minor.x = element_blank(),
                                          strip.background = element_blank()) +
  theme(text = element_text(family = "Larsseit"),
        plot.title = element_text(family = "Larsseit"),
        plot.subtitle = element_text(family = "Larsseit"),
        plot.caption = element_text(family="Larsseit"),
        strip.text = element_text(family="Larsseit"))


##### The following two scripts read the data ####
source(file = "1_a_load_bls_cps_jobs_data.R")
source(file = "1_b_load_bls_ces_jobs_data.R")
# They pull from the following two websites, check to see if they're updated:
# https://download.bls.gov/pub/time.series/ce/
# https://download.bls.gov/pub/time.series/ln/


title_longer_graphic <- "This is the wage graphic"
title_longer_graphic <- "Wages Are Decelerating to 2018-2019 Values"

total_jobs_title <- "Jobs title"
total_jobs_title <- "Job Growth Remains Strong"

six_month_jobs_occupations_title <- "Occupations last month versus previous six months"
six_month_jobs_occupations_title <- "Job Growth is Slowing in Cyclical While Still Strong in Services"

#### These are the current graphics we produce immediately ####
source(file = "2_1_epop_change.R")
source(file = "2_2_jobs_in_context.R")
source(file = "2_3_unemployment_durations.R")
source(file = "2_4_wage_comparsion.R")
source(file = "2_1_monthly_data_graphics.R")
source(file = "2_3_CBO_projections.R")
