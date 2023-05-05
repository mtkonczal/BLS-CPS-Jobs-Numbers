# This script looks at employment growth both:
# - in periods of low unemployment, and
# - since 1980
# It creates two graphics in the graphics folder.
# It requires CES (for employment) and CPS (for unemployment rate) numbers.
# Written by: Mike Konczal, Roosevelt Institute
# Last Updated: 7/6/2021

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")
library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(lubridate)

##### SET UP SOME THINGS #####
#source(file = "1_a_load_bls_cps_jobs_data.R")
#source(file = "1_b_load_bls_ces_jobs_data.R")
##### Graphic 2

###### BETTER THREE SIX ####

AHE <- ces_data %>% filter(series_title == "Average hourly earnings of all employees, total private, seasonally adjusted") %>%
  select(date, value) %>%
  mutate(ThreeMonth = (value/lag(value,3))^4-1) %>%
  mutate(SixMonth = (value/lag(value,6))^2-1) %>%
  mutate(YoY = (value/lag(value,12))-1) %>%
  select(-value, YoY) %>%
  pivot_longer(ThreeMonth:SixMonth, names_to = "time_length", values_to = "change") %>%
  mutate(time_length = str_replace_all(time_length,"SixMonth", "6-Month Change")) %>%
  mutate(time_length = str_replace_all(time_length,"ThreeMonth", "3-Month Change")) %>%
  mutate(last_value = ifelse(date==max(date),change,NA))


one_month_change <- ces_data %>% filter(series_title == "Average hourly earnings of all employees, total private, seasonally adjusted") %>%
  select(date, value) %>%
  mutate(one_month = value/lag(value,1)) %>%
  mutate(one_month = one_month^12-1) %>%
  select(date, one_month)

MI_dates <- ces_data %>% filter(date > "2010-12-01")
MI_dates <- unique(MI_dates$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates = MI_dates[seq(1, length(MI_dates), 12)]

date_start = "2018-01-01"
date_end = "2020-01-01"
date_period <- interval(date_start, date_end)
date_period = date_period %/% months(1)

pre_AHE <- ces_data %>% filter(series_title == "Average hourly earnings of all employees, total private, seasonally adjusted", (date == date_start | date == date_end)) %>%
  mutate(change = value/lag(value,1)) %>% filter(!is.na(change)) %>% mutate(change = change^(12/date_period) - 1) %>% select(change)
pre_AHE <- as.numeric(pre_AHE)

AHE %>% filter(date > "2020-12-01") %>%
  left_join(one_month_change, by="date") %>%
  mutate(one_month = if_else(one_month == lag(one_month,1),as.double(NA),one_month)) %>%
  ggplot(aes(date, change, color=time_length, label=label_percent()(round(last_value,3)))) + geom_line(size=3) +
  geom_col(aes(date,one_month), alpha=0.25, size=0, show.legend = FALSE) +
  labs(x="", y="",
       title=title_longer_graphic,
       subtitle = paste("Annualized, monthly average hourly earnings of all employees, total private.\nBars are 1-month annualized. Dotted line represents 2018-2019 value of ", round(100*pre_AHE,1), "%.", sep=""), #, round(pre_core,3)*100, "%.", sep=""),
       caption = "Dotted line is annualized, BLS, Author's calculations. Mike Konczal, Roosevelt Institute.") +
  theme_lass +
  geom_hline(yintercept = pre_AHE, linetype="dashed", color="#A4CCCC") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  theme(legend.position = c(0.85,0.80), legend.text = element_text(size=15)) +
  scale_color_manual(values=c("#2D779C", "#A4CCCC")) +
  geom_text(show.legend=FALSE, nudge_x = 44, size = 5.5)

ggsave("graphics/three_six_wages.png", dpi="retina", width = 8, height=10, units = "in")


#### Many Wages ####

ces_data %>% filter(seasonal == "S", data_type_code == "03", display_level == 3) %>%
  group_by(industry_name) %>% summarize(date = date, three_month_change = value/lag(value,3)-1) %>%
  ungroup() %>% filter(date >= "2021-01-01") %>%
  ggplot(aes(date,three_month_change)) + geom_line() + facet_wrap(~industry_name) + theme_classic()