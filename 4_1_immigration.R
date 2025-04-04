# This
# This is a master file that runs multiple graphic creation function calls.
# Written by: Mike Konczal, Roosevelt Institute
# Last Updated: 1/5/2024

library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(lubridate)
library(tidytext)
library(seasonal)


prime_pop_foreign <- c("LNU00073399", "LNU00073400", "LNU00073401")
prime_emp_foreign <- c("LNU02073399", "LNU02073400", "LNU02073401")


prime_pop_native <- c("LNU00073417", "LNU00073418", "LNU00073419")
prime_emp_native <- c("LNU02073417", "LNU02073418", "LNU02073419")

df4 <- cps_jobs_data %>%
  filter(series_id %in% c(prime_pop_native, prime_emp_native, prime_pop_foreign, prime_emp_foreign)) %>%
  group_by(date, born_code) %>%
  summarize(prime_emp = sum(value[lfst_text == "Employed"]),
            prime_pop = sum(value[lfst_text == "Civilian noninstitutional population"]),
            prime_emp_pop = prime_emp/prime_pop)


df3 <- cps_jobs_data %>%
  filter(series_id %in% c(prime_pop_native, prime_emp_native)) %>%
  group_by(date) %>%
  summarize(prime_emp = sum(value[lfst_text == "Employed"]),
            prime_pop = sum(value[lfst_text == "Civilian noninstitutional population"]),
            prime_emp_pop = prime_emp/prime_pop)

df3_seasonal <- ts(df3$prime_emp_pop, start = c(min(year(df3$date))), frequency = 12)
m <- seas(df3_seasonal)
df3_seasonal_SA <- final(m)
df3_seasonal_SA <- data.frame(value = as.matrix(df3_seasonal_SA), date = as.Date(zoo::yearmon(time(df3_seasonal_SA)))) %>% rename(seasonal_adjusted_epop = value)

df3 %>%
  left_join(df3_seasonal_SA, by="date") %>%
  select(date, prime_emp_pop, seasonal_adjusted_epop) %>%
  pivot_longer(cols = -date, names_to = "type", values_to = "values") %>%
  ggplot(aes(date, values, color=type)) + geom_line() + theme_classic() +
  scale_y_continuous(label=percent) +
  labs(title="Native-Born, 25-54, CPS, Manual seasonal-adjustment, added by sub-category, Mike Konczal") +
  theme(legend.position = "top")


cps_jobs_data %>%
  filter(series_id %in% c("LNU04073395","LNU04073413")) %>%
  ggplot(aes(date, value, color=series_title)) + geom_line()


cps_jobs_data %>%
  filter(date >= "2019-01-01") %>%
  filter(series_id %in% c("LNU02073395", "LNU02073413")) %>%
  ggplot(aes(date, value, color=series_title)) + geom_line()



cps_jobs_data %>%
  filter(date >= "2018-01-01", month(date) == 2) %>%
  filter(series_id %in% c("LNU02073395", "LNU02073413")) %>%
  select(value, series_title, year)

df3 %>% mutate(year = year(date), month = month(date)) %>% filter(month %in% c(1,2,3)) %>%
  group_by(year) %>%
  summarize(mean(prime_emp)) %>%
