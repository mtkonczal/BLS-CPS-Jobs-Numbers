# Mike Konczal

setwd("/Users/mkonczal/Documents/GitHub/BLS-CES-Wages/")
library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(lubridate)
library(janitor)

#### LOAD ECI DATA ########

eci_wages_start <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.data.1.AllData") %>%
  clean_names()
eci_wages_start$value <- as.numeric(eci_wages_start$value)
# Do Date


eci_wages_series <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.series") %>%
  clean_names()
eci_wages_series$series_id <- str_trim(eci_wages_series$series_id)
eci_industry_series <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.industry") %>%
  clean_names()
eci_owner_series <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.owner") %>%
  clean_names()
eci_subcell <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.subcell") %>%
  clean_names()
eci_occupation_series <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.occupation") %>%
  clean_names()
eci_period <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.periodicity") %>%
  clean_names()
eci_estimate <- read_delim(file = "https://download.bls.gov/pub/time.series/ci/ci.estimate") %>%
  clean_names()

eci_wages_next <- eci_wages_start %>%
  inner_join(eci_wages_series, by="series_id") %>%
  inner_join(eci_industry_series, by="industry_code") %>%
  inner_join(eci_occupation_series, by="occupation_code") %>%
  inner_join(eci_period, by="periodicity_code") %>%
  inner_join(eci_estimate, by="estimate_code") %>%
  inner_join(eci_owner_series, by="owner_code") %>%
  inner_join(eci_subcell, by="subcell_code")

eci_wages_next <- eci_wages_next %>%
  mutate(month = case_when(
    period == "Q01" ~ 3,
    period == "Q02" ~ 6,
    period == "Q03" ~ 9,
    period == "Q04" ~ 12))
eci_wages_next$date <- paste(eci_wages_next$month, "01", eci_wages_next$year, sep="/")
eci_wages_next$date <- as.Date(eci_wages_next$date, "%m/%d/%Y")

eci <- eci_wages_next
##### INITIAL ANALYSIS ####

# Test graphics
eci_a <- eci %>% filter(date == max(date)) %>%
  filter(estimate_code == "02", periodicity_code %in% c("Q","X"), owner_code == 2,
         occupation_code == "000000", seasonal == "S")

eci_b <- eci %>% filter(date == max(date)) %>%
  filter(estimate_code == "02", periodicity_code %in% c("Q","X"), owner_code == 2, subcell_code == "00",
         occupation_code == "000000", seasonal == "U", industry_code %in% c("G00000", "S00000"))

# Test graphics
eci %>% filter(year >= 2015) %>%
  filter(estimate_code == "02", periodicity_code == "Q", owner_code == 2,
         industry_code == "000000", seasonal == "S") %>%
  ggplot(aes(date, value, color=occupation_text)) + geom_line() + theme_classic() + theme(legend.position='bottom')

### First real graphic
eci %>%
  filter(estimate_code == "02", periodicity_code == "Q", owner_code == 2,
         occupation_code == "000000", seasonal == "S", industry_code %in% c("G00000", "S00000")) %>%
  ggplot(aes(date, value, color=industry_title)) + geom_line() + theme_classic() + theme(legend.position='bottom') +
  labs(x="", y="", title="Wages and salaries for Private industry workers, 3-month percent change")

### First with excluding “incentive paid occupations”

tester <- eci %>% filter(date == max(date))
comp_filter <- c("CIU202G000000710Q", "CIU202G000000710A", "CIU202G000000710I","CIU202S000000710A","CIU202S000000710I","CIU202S000000710Q", "CIU2020000000710Q")

eci %>% filter(series_id %in% comp_filter) %>% filter(periodicity_code == "Q") %>%
  ggplot(aes(date, value, color=industry_title)) + geom_line() + theme_classic() + theme(legend.position='bottom') +
  labs(x="", y="", title="Wages and salaries for Private industry workers, excluding incentive paid occupations, 3-month percent change")

a <- eci %>% filter(series_id == "CIU202G000000710Q") %>% filter(year > 2014, year < 2019) %>%
  select(date, series_title, value)
a %>% mutate(month = month(date)) %>% group_by(month) %>% summarize(avg = mean(value))

a <- eci %>% filter(series_id == "CIU2020000000710Q") %>%
  select(date, series_title, value)

eci %>% filter(series_id == "CIU2020000000710Q") %>% filter(periodicity_code == "Q") %>%
  ggplot(aes(date, value, color=industry_title)) + geom_line() + theme_classic() + theme(legend.position='bottom') +
  labs(x="", y="", title="Wages and salaries for Private industry workers, excluding incentive paid occupations, 3-month percent change")


Q2 <- eci %>% filter(series_id == "CIU2020000000710Q") %>% filter(period %in% c("Q02")) %>% select(Q2 = value, year)
Q3 <- eci %>% filter(series_id == "CIU2020000000710Q") %>% filter(period %in% c("Q03")) %>% select(Q3 = value, year, series_title) %>%
  left_join(Q2, by="year") %>% mutate(diff = Q3-Q2)

# Here
Q3 %>% filter(year != 2009, year != 2008, year < 2020) %>% summarize(avg = mean(diff))
# Outside of recession years like 2008-2009, and 2020-2021, in general there's a -0.0667 dropoff in the ECI for Q2 to Q3.



### Blergh, pass:
#### First real graphic - faceted real and nominal, not working yet (mutliple values in date times?)
eci %>%
  filter(estimate_code == "02", periodicity_code %in% c("Q","X"), owner_code == 2, subcell_code == "00",
         occupation_code == "000000", seasonal == "U", industry_code %in% c("G00000", "S00000")) %>%
  ggplot(aes(date, value, color=industry_title)) + geom_line() + facet_wrap(~ periodicity_text, nrow = 1) +
  theme_classic() + theme(legend.position='bottom')