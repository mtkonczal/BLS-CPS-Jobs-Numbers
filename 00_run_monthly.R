library(tidyverse)
library(ggtext)
library(blsR)
library(tidyusmacro)
source("scripts/graphic_scripts.R")

load_revisions_data <- function(path = "data/bls_ces_monthly_revisions.csv") {
  read_csv(path) %>%
    mutate(
      non_na_count = rowSums(
        !is.na(select(., starts_with("sa_"), starts_with("nsa_")))
      )
    ) %>%
    arrange(year, month_num, non_na_count) %>%
    group_by(year, month_num) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(-non_na_count)
}

load_headline_jobs_data <- function() {
  get_n_series_table(
    c(
      "LNS13000000",
      "LNS11000000",
      "CES0500000021",
      "CES0000000001",
      "CES0000000010",
      "CES9091000001"
    ),
    api_key = bls_get_key(),
    start_year = 2011,
    end_year = 2026,
    tidy = TRUE
  ) %>%
    mutate(
      LNS11000000 = as.numeric(LNS11000000),
      LNS13000000 = as.numeric(LNS13000000),
      unrate = LNS13000000 / LNS11000000,
      date = as.Date(paste0(year, "/", month, "/", 1)),
      diffusion = CES0500000021 / 100,
      ces = CES0000000001 - lag(CES0000000001, 1),
      federal = CES9091000001,
      nonfederal = CES0000000001 - federal,
      federal = CES9091000001 - lag(CES9091000001, 1),
      nonfederal = nonfederal - lag(nonfederal, 1)
    )
}

cat("Refreshing CES revisions cache...\n")
py_status <- system("python3 99_download_jobs_revisions.py")
if (py_status != 0) {
  warning(
    "Python download script failed (exit ",
    py_status,
    "). Using cached CSV."
  )
}

bls_set_key(Sys.getenv("BLS_KEY"))

revisions_df <- load_revisions_data()
unrate <- load_headline_jobs_data()

source("01_initial_tweet.R")
source("02_unrate_jobs.R")
source("03_lfp_epop.R")

source("04_health_care_gender.R")
source("05_goods_loglines.R")
source("07_young_unrate.R")
source("08_immigration_unrate.R")
source("09_unrate_by_type_ahe.R")
source("10_flows_4_types.R")
source("11_unemployment_durations.R")
source("12_where_unrate_increased.R")

ces_data <- getBLSFiles("ces", "rortybomb@gmail.com")
ces_data$data_type_code_org <- ces_data$data_type_code
ces_data$data_type_code <- as.numeric(ces_data$data_type_code)

# Two calls that use ces.
source("13_cyclical_industries.R")
make_jobs_chart(ces_data)
#source("98_revisions_estimate.R")
