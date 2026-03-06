library(tidyverse)
library(ggtext)
library(blsR)
library(scales)
library(zoo)
library(tidyverse)
library(lubridate)
library(viridis)
source("scripts/graphic_scripts.R")


##### TITLES #####
title4_unrate_by_type <- "Job Losers and New Entrants Drive Unemployment Increase"
title5_ahe <- "Average Hourly Earnings Remain Volatile"


bls_set_key(Sys.getenv("BLS_KEY"))


unrate_types_raw <- get_n_series_table(
  c(
    "LNS13023705",
    "LNS11000000",
    "LNS13023557",
    "LNS13023569",
    "LNS13023653",
    "LNS13025699",
    "CES0500000003"
  ),
  api_key = bls_get_key(),
  start_year = 2019,
  end_year = year(Sys.Date()),
  tidy = TRUE
)

unrate_types_raw <- unrate_types_raw %>%
  mutate(
    across(
      -c(year, month),
      as.numeric
    )
  )

unrate_types <- unrate_types_raw %>%
  mutate(date = as.Date(paste0(year, "/", month, "/", 1))) %>%
  select(-year, -month) %>%
  pivot_longer(
    LNS13023705:CES0500000003,
    names_to = "series_id",
    values_to = "value"
  )


unemployment_rate_by_type(
  unrate_types,
  graphic_title = title4_unrate_by_type,
  start_date = "2023-01-01",
  axis_months = 6
)
ggsave(
  "graphics/09a_u_by_type.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

three_six_wages_title <- title5_ahe
three_six_wages(unrate_types, three_six_wages_title)
ggsave(
  "graphics/09b_wages_3_6.png",
  dpi = "retina",
  width = 12,
  height = 8,
  units = "in"
)
