###############################################################
# Code to read in jobs data from BLS CES website and begin analysis.
# This file reads in and store all the Establishment Survey monthly jobs data.
#
# Mike Konczal
# Last updated 3/31/22

setwd("/Users/mkonczal/Documents/GitHub/BLS-CES-Wages/")
library(janitor)
library(tidyverse)
library(ggtext)

#### CES EMPLOYMENT ########
ces_data <- read_delim(file = "data/ce.data.0.AllCESSeries")


ces_data <- ces_data %>%
  clean_names()
# Right now R doesn't handle dates before 1970 straightforward, so as a workaround,
# and since we don't need them, I'm just deleting them. Will fix in future version.
ces_data$series_id <- str_trim(ces_data$series_id)
ces_data$value <- as.numeric(ces_data$value)
ces_data$date <- paste(substr(ces_data$period, 2,3), "01", ces_data$year, sep="/")
ces_data$date <- as.Date(ces_data$date, "%m/%d/%Y")

ces_series <- read_delim(file = "data/ce.series")
ces_series <- ces_series %>% 
  clean_names()
ces_series$series_id <- str_trim(ces_series$series_id)

ces_data_type <- read_delim(file = "data/ce.datatype")
ces_super_sector <- read_delim(file = "data/ce.supersector")
ces_industry <- read_delim(file = "data/ce.industry")
ces_series <- inner_join(ces_series, ces_data_type, by = "data_type_code")
ces_series <- inner_join(ces_series, ces_super_sector, by = "supersector_code")
ces_series <- inner_join(ces_series, ces_industry, by = "industry_code")

ces_data <- inner_join(ces_data, ces_series, by = c("series_id"))
#ces_data <- select(ces_data, -c("footnote_codes.x", "footnote_codes.y", "begin_year", "begin_period", "end_year", "end_period"))

rm(ces_series, ces_industry, ces_super_sector, ces_data_type)
#save(ces_data, file = "data/ces_data.RData")
######################################################################



ces <- ces_data %>% filter(seasonal == "S")

unique(ces$data_type_text)

ces %>% filter(data_type_text == "ALL EMPLOYEES, THOUSANDS", date == max(date)) %>% group_by(display_level) %>% summarize(number = sum(value))

a <- ces %>% filter(display_level == 5, date == max(date))
a <- ces %>% filter(data_type_code == "03") %>% filter(date == max(date))

a <- ces %>% filter(data_type_code %in% c("03")) %>% group_by(industry_name) %>%
  mutate(wage_change = (value/lag(value,1))^12-1,
         previous_three_wages = (lag(value,1)/lag(value,4))^4-1) %>%
  ungroup() %>% filter(date == max(date)) %>%
  select(industry_name, wage_change, previous_three_wages) %>%
  mutate(wage_change = wage_change*100, previous_three_wages = previous_three_wages*100)

b <- ces %>% filter(data_type_code %in% c("01")) %>% group_by(industry_name) %>% mutate(employment_change = (value/lag(value,1))^12-1) %>% ungroup() %>% filter(date == max(date)) %>%
  select(industry_name, employment_change) %>% mutate(employment_change = employment_change*100) %>% inner_join(a, by="industry_name") %>% mutate(fewer_jobs = employment_change < 0)

ggplot(b, aes(previous_three_wages, wage_change)) + geom_point() + theme_classic() + geom_smooth(method="lm") + facet_wrap(~fewer_jobs)


c <- lm(wage_change ~ previous_three_wages, data=b)
summary(c)

c <- lm(wage_change ~ previous_three_wages + fewer_jobs, data=b)
summary(c)


View(a)