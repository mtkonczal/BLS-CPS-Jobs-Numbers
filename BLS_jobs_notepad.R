setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")

library(tidyverse)
library(ggtext)

#source(file = "1_b_load_bls_ces_jobs_data.R")

ces_data %>% filter(date > "2019-01-01") %>%
  ggplot(aes(date, value)) + geom_line() + facet_wrap( ~ supersector_name)

table(ces_data$seasonal)

a$value


View(cps_jobs_data %>% filter(series_title=="(Seas) Unemployment Rate", periodicity_code == "M") %>% select(date, value) %>%
  arrange(value) %>% filter(value <= value[date==max(date)]))

U <- cps_jobs_data %>% filter(series_title=="(Seas) Unemployment Level",  periodicity_code == "M") %>% select(date, UL = value)
a <-cps_jobs_data %>% filter(series_title=="(Seas) Civilian Labor Force Level",  periodicity_code == "M") %>% left_join(U, by="date") %>%
  mutate(urate_full = UL/value) %>% select(date, urate_full) %>%
  arrange(urate_full)

b <-cps_jobs_data %>% filter(series_title=="(Seas) Civilian Labor Force Level",  periodicity_code == "M") %>% left_join(U, by="date") %>%
  mutate(urate_full = UL/value) %>% select(date, urate_full) %>% filter(urate_full <= urate_full[date==max(date)]) %>%
  arrange(urate_full) %>% mutate(year = year(date))

unique(b$year)

cpi_data %>% filter(item_name == "All items less food, shelter, energy, and used cars and trucks", seasonal == "S") %>%
  mutate(m1a = value/lag(value,1)-1, m1a = (1+m1a)^4-1) %>%
  ggplot(aes(date,m1a)) + geom_line()