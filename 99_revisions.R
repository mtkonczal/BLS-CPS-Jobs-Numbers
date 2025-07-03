library(tidyverse)
library(ggtext)
library(blsR)
library(govMacroTools)
library(scales)
library(zoo)
library(tidyverse)
library(govMacroTools)
library(lubridate)
library(viridis)
library(janitor)
source("scripts/03_graphic_scripts.R")


##### TITLES #####
mrevision <- read_csv("data/ces_monthly_revisions.csv") %>%
  arrange(date) %>%
  clean_names() %>%
  mutate(total_change = as.numeric(x1_month_revision) + as.numeric(x2_month_revision))