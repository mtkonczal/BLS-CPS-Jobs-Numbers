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

start_year = 2017

ces_data %>%
  filter(seasonal == "S", data_type_code == 1, display_level <= 2, year >= start_year, year <= 2019) %>%
  group_by(series_id) %>%
  mutate(index_projection = row_number()) %>%
  # get log-linear regression variables
  do(tidy(lm(log(value) ~ index_projection, data = .))) %>%
  select(series_id, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  # keep piping!
  inner_join(ces_data, by="series_id") %>%
  filter(year >= start_year) %>%
  group_by(series_id) %>%
  mutate(index = row_number(),
         projection = exp(index_projection*index + `(Intercept)`)) %>%
  ungroup() %>%
  ggplot() + geom_line(aes(date, value)) + geom_line(aes(date, projection, linetype="dashed")) +
  facet_wrap(~industry_name, scales = "free")
  


titles <- cps_jobs_data %>%
  distinct(series_title, .keep_all = TRUE) %>%
  select(series_title, series_id)

View(titles)


cps_jobs_data %>%
  filter(
         series_id %in% c("LNU01300066","LNU01300067","LNU01300068")) %>%
  ggplot(aes(date, value)) + geom_line() +
  facet_wrap(~series_title) +
  theme_classic()


cps_jobs_data %>%
  filter(
    series_id %in% c("LNU01300066","LNU01300067","LNU01300068"),
    year(date) %in% c(2019, 2024),
    month(date) <= 8) %>%
  group_by(series_title, year) %>%
  reframe(mean(value))

cps_jobs_data %>%
  filter(
    series_id %in% c("LNU01300066"),
    month(date) <= 8) %>%
  group_by(series_title, year) %>%
  reframe(mean(value))

cps_jobs_data %>%
  filter(series_id %in% c("LNU01300066"))
  arrange(date) %>%
  mutate(
    moving_avg = map_dbl(date, function(current_date) {
      # Define the start of the 12-month window
      window_start <- current_date %m-% months(11)
      # Filter values within the window and calculate the mean
      mean(value[date >= window_start & date <= current_date], na.rm = TRUE)
    })
  )


  
  cps_lfst_code <- GET("https://download.bls.gov/pub/time.series/ln/ln.sexs", user_agent("rortybomb@gmail.com")) %>%
    content(as = "text") %>%
    fread()
  cps_jobs_data <- inner_join(cps_jobs_data, cps_lfst_code, by = "sexs_code")
  
  cps_lfst_code <- GET("https://download.bls.gov/pub/time.series/ln/ln.race", user_agent("rortybomb@gmail.com")) %>%
    content(as = "text") %>%
    fread()
  cps_jobs_data <- inner_join(cps_jobs_data, cps_lfst_code, by = "race_code")
  
  cps_lfst_code <- GET("https://download.bls.gov/pub/time.series/ln/ln.indy", user_agent("rortybomb@gmail.com")) %>%
    content(as = "text") %>%
    fread()
  cps_jobs_data <- inner_join(cps_jobs_data, cps_lfst_code, by = "indy_code")
  
    
  cps_jobs_data %>%
    filter(series_id %in% c("LNU02000066","LNU02000067","LNU02000068","LNU00000066","LNU00000067","LNU00000068")) %>%
    group_by(date, sexs_text) %>%
    reframe(emp_rate = value[lfst_code == 20] / value[lfst_code == 0]) %>%
    ungroup() %>%
    pivot_wider(names_from = sexs_text, values_from = emp_rate) %>%
    write_csv("black_epop_25_54.csv")
  
  
cps_jobs_data %>%
  filter(series_id %in% c("LNU02000066","LNU02000067","LNU02000068","LNU00000066","LNU00000067","LNU00000068")) %>%
  group_by(date, sexs_text) %>%
  reframe(emp_rate = value[lfst_code == 20] / value[lfst_code == 0]) %>%
  ungroup() %>%
  ggplot(aes(date, emp_rate)) + geom_line() +
  facet_wrap(~sexs_text) +
  theme_classic() +
  labs(title="Black Employment-to-Population Ratio for 25-54 Year Olds")
  

a <- cps_jobs_data %>%
  filter(series_id %in% c("LNU02000066","LNU02000067","LNU02000068","LNU00000066","LNU00000067","LNU00000068")) %>%
  group_by(date, sexs_code) %>%
  reframe(emp_rate = value[lfst_code == 20] / value[lfst_code == 0]) %>%
  ungroup() %>%
  filter(month(date) <= 8) %>%
  mutate(year = year(date)) %>%
  group_by(year, sexs_code) %>%
  reframe(avg = mean(emp_rate)) %>%
  ungroup() %>%
  group_by(sexs_code) %>%
  mutate(diff = avg - lag(avg))



black_cps <- cps_jobs_data %>%
  filter(race_code == 3)

black_cps <- black_cps %>%
  filter(series_id == "LNU02000006") %>%
  select(date, black_employ_NSA = value) %>%
  right_join(black_cps, by="date")


black_cps %>%
  filter((date == "2024-08-01" | date == "2019-08-01"),
         periodicity_code == "M",
         lfst_code == 20,
         sexs_code == 0,
         ages_code == 0,
         seasonal == "U",
         tdat_code == 0,
         indy_text != "All Industries") %>%
  mutate(percent_employment = value/black_employ_NSA) %>%
  select(series_title, year, percent_employment) %>%
  pivot_wider(names_from = year, values_from = percent_employment)

black_cps_titles <- black_cps %>%
  distinct(series_title)

black_cps %>% filter(occupation_code == 8,
                          periodicity_code == "M",
                          indy_code != 0,
                          date == "2024-08-01") %>%
  reframe(sumss = sum(value))



# Not working
black_cps %>% filter(occupation_code == 8,
                     periodicity_code == "M",
     indy_code != 0,
     year %in% c(2019,2023)) %>%
  group_by(year) %>%
  reframe(total = sum(value)) %>%
  ungroup() %>%
  group_by(year, series_title) %>%
  reframe(percent_in = sum(value)/sum(total)) %>%
  pivot_wider(names_from = year, values_from = percent_in) %>%
  write_csv("percent_by_industry.csv")

