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


#### These are the current graphics we produce immediately ####
source(file = "2_1_epop_change.R")
source(file = "2_2_lfp_change.R")
source(file = "2_3_cbo_projections.R")
source(file = "2_4_low_U_job_growth.R")
source(file = "2_5_2018_comparsion.R")
source(file = "2_6_wages_compression.R")