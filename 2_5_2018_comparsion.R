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

pre_core <- ces_data %>% filter(series_title == "Average hourly earnings of all employees, total private, seasonally adjusted", (date == date_start | date == date_end)) %>%
  mutate(change = value/lag(value,1)) %>% filter(!is.na(change)) %>% mutate(change = change^(12/date_period) - 1) %>% select(change)
pre_core <- as.numeric(pre_core)

AHE %>% filter(date > "2020-12-01") %>%
  left_join(one_month_change, by="date") %>%
  mutate(one_month = if_else(one_month == lag(one_month,1),as.double(NA),one_month)) %>%
  ggplot(aes(date, change, color=time_length, label=label_percent()(round(last_value,3)))) + geom_line(size=3) +
  geom_col(aes(date,one_month), alpha=0.25, size=0, show.legend = FALSE) +
  labs(x="", y="",
       title="title_longer_graphic",
       subtitle = paste("Annualized, monthly average hourly earnings of all employees, total private.\nBars are 1-month annualized. Dotted line represented 2018-2019 value of TK", sep=""), #, round(pre_core,3)*100, "%.", sep=""),
       caption = "Dotted line is annualized, BLS, Author's calculations. Mike Konczal, Roosevelt Institute.") +
  theme_lass +
  geom_hline(yintercept = pre_core, linetype="dashed", color="#A4CCCC") +
  scale_fill_brewer(palette="Paired") +
  theme(panel.grid.major.y = element_line(size=0.5)) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  theme(legend.position = c(0.85,0.80), legend.text = element_text(size=15)) +
  scale_color_manual(values=c("#2D779C", "#A4CCCC")) +
  geom_text(show.legend=FALSE, nudge_x = 44, size = 5.5)

ggsave("graphics/three_six_wages.png", dpi="retina", width = 8, height=10, units = "in")



AHE <- ces_data %>% filter(series_title == "Average hourly earnings of all employees, total private, seasonally adjusted") %>%
    select(date, series_title, year, wages = value) %>%
    mutate(wage_growth = wages - lag(wages,1), wage_growth = wage_growth/lag(wages,1)) %>%
    mutate(rolling_jg = wage_growth + lag(wage_growth,1) + lag(wage_growth,2),
           rolling_jg = rolling_jg/3)
  
AHE1 <- AHE %>% filter(year == 2019 | year == 2018) %>%
    summarize(mean_jg = mean(wage_growth))
AHE1 <- as.numeric(AHE1)
  
ST_AHE_Now <- AHE %>% filter(date == max(date)) %>% select(rolling_jg) %>% mutate(rolling_jg = (1+rolling_jg)^12-1)
ST_AHE_Now <- percent(as.numeric(ST_AHE_Now), accuracy = 0.1)
ST_AHE_2018 <- (1+AHE1)^12-1
ST_AHE_2018 <- percent(ST_AHE_2018, accuracy=0.1)

a <- AHE %>% filter(date >= "2021-01-01") %>% mutate(wage_growth = (1+wage_growth)^12-1) %>%
  mutate(rolling_jg = (rolling_jg+1)^12-1)

AHE %>% filter(date >= "2021-01-01") %>% mutate(wage_growth = (1+wage_growth)^12-1) %>%
  mutate(rolling_jg = (rolling_jg+1)^12-1) %>%
  ggplot(aes(date, wage_growth)) + geom_line(size=1.2, color="#03a0ff") + theme_classic() +
  geom_hline(yintercept = (1+AHE1)^12-1, color="#FF00FF", size=0.9, linetype="dashed") +
  geom_line(aes(date, rolling_jg), color="#00FFFF", size=1, linetype="dashed") +
  annotate(
    "text", label = "Actual Values",
    x=as.Date("2021-04-15"), y = 0.08, size = 7, colour = "#03a0ff") +
  annotate(
    "text", label = "Average\n2018-19 Value",
    x=as.Date("2022-10-15"), y = 0.025, size = 7, colour = "#FF00FF") +
  annotate(
    "text", label = "3-Month\nAverage",
    x=as.Date("2022-4-25"), y = .065, size = 7, colour = "#00FFFF") +
  labs(x="", y="",
       title="Through the Noise, Wage Growth Is Returning to 2018-2019 Levels",
       subtitle = paste("Annualized, monthly average hourly earnings of all employees, total private. Rate over the last three months was", ST_AHE_Now, "versus", ST_AHE_2018, "in 2018-2019"),
       caption = "BLS, CES, seasonally adjusted, author's calculations. Mike Konczal, Roosevelt Institute") +
  scale_x_date(date_labels = "%b %y") +
  scale_y_continuous(labels = scales::percent) +
  theme_lass + theme(plot.title = element_text(size=30),
                     axis.text.y = element_text(size=18),
                     axis.text.x = element_text(size=18))

ggsave("graphics/2018_wage.png", width = 14.25, height=8.01, dpi="retina")


##### Graphic 3: Nonsupervisory
# Average hourly earnings of production and nonsupervisory employees, total private, seasonally adjusted
AHE <- ces_data %>% filter(series_id == "CES0500000008") %>%
  select(date, series_title, year, wages = value) %>%
  mutate(wage_growth = wages - lag(wages,1), wage_growth = wage_growth/lag(wages,1)) %>%
  mutate(rolling_jg = wage_growth + lag(wage_growth,1) + lag(wage_growth,2),
         rolling_jg = rolling_jg/3)

AHE1 <- AHE %>% filter(year == 2019 | year == 2018) %>%
  summarize(mean_jg = mean(wage_growth))
AHE1 <- as.numeric(AHE1)

ST_AHE_Now <- AHE %>% filter(date == max(date)) %>% select(rolling_jg) %>% mutate(rolling_jg = (1+rolling_jg)^12-1)
ST_AHE_Now <- percent(as.numeric(ST_AHE_Now), accuracy = 0.1)
ST_AHE_2018 <- (1+AHE1)^12-1
ST_AHE_2018 <- percent(ST_AHE_2018, accuracy=0.1)

a <- AHE %>% filter(date >= "2021-01-01") %>% mutate(wage_growth = (1+wage_growth)^12-1) %>%
  mutate(rolling_jg = (rolling_jg+1)^12-1)

AHE %>% filter(date >= "2021-01-01") %>% mutate(wage_growth = (1+wage_growth)^12-1) %>%
  mutate(rolling_jg = (rolling_jg+1)^12-1) %>%
  ggplot(aes(date, wage_growth)) + geom_line(size=1.2, color="#03a0ff") + theme_classic() +
  geom_hline(yintercept = (1+AHE1)^12-1, color="#FF00FF", size=0.9, linetype="dashed") +
  geom_line(aes(date, rolling_jg), color="#00FFFF", size=1, linetype="dashed") +
  annotate(
    "text", label = "Actual Values",
    x=as.Date("2021-06-15"), y = 0.08, size = 8, colour = "#03a0ff") +
  annotate(
    "text", label = "Average\n2018-19 Value",
    x=as.Date("2022-06-15"), y = (1+AHE1-0.00053)^12-1, size = 8, colour = "#FF00FF") +
  annotate(
    "text", label = "3-Month\nAverage",
    x=as.Date("2021-09-25"), y = 0.05, size = 8, colour = "#00FFFF") +
  labs(x="", y="",
       title="Through the Noise, Wage Growth Still Returning to 2018-2019 Levels",
       subtitle = paste("Average hourly earnings of production and nonsupervisory employees, total private.\nRate over the last three months was", ST_AHE_Now, "versus", ST_AHE_2018, "in 2018-2019"),
       caption = "BLS, CES, seasonally adjusted, author's calculations. Mike Konczal, Roosevelt Institute") +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_y_continuous(labels = scales::percent) +
  theme_lass + theme(plot.title = element_text(size=30),
                     axis.text.y = element_text(size=18),
                     axis.text.x = element_text(size=18))

ggsave("graphics/2018_wage_NS.png", width = 14.25, height=8.01, dpi="retina")




##### Graphic 4: Leisure
# Average hourly earnings of production and nonsupervisory employees, total private, seasonally adjusted
AHE <- ces_data %>% filter(series_title == "Average hourly earnings of all employees, accommodation and food services, seasonally adjusted") %>%
  select(date, series_title, year, wages = value) %>%
  mutate(wage_growth = wages - lag(wages,1), wage_growth = wage_growth/lag(wages,1)) %>%
  mutate(rolling_jg = wage_growth + lag(wage_growth,1) + lag(wage_growth,2),
         rolling_jg = rolling_jg/3)

AHE1 <- AHE %>% filter(year == 2019 | year == 2018) %>%
  summarize(mean_jg = mean(wage_growth))
AHE1 <- as.numeric(AHE1)

ST_AHE_Now <- AHE %>% filter(date == max(date)) %>% select(rolling_jg) %>% mutate(rolling_jg = (1+rolling_jg)^12-1)
ST_AHE_Now <- percent(as.numeric(ST_AHE_Now), accuracy = 0.1)
ST_AHE_2018 <- (1+AHE1)^12-1
ST_AHE_2018 <- percent(ST_AHE_2018, accuracy=0.1)

a <- AHE %>% filter(date >= "2021-01-01") %>% mutate(wage_growth = (1+wage_growth)^12-1) %>%
  mutate(rolling_jg = (rolling_jg+1)^12-1)

AHE %>% filter(date >= "2021-01-01") %>% mutate(wage_growth = (1+wage_growth)^12-1) %>%
  mutate(rolling_jg = (rolling_jg+1)^12-1) %>%
  ggplot(aes(date, wage_growth)) + geom_line(size=1.2, color="#03a0ff") + theme_classic() +
  geom_hline(yintercept = (1+AHE1)^12-1, color="#FF00FF", size=0.9, linetype="dashed") +
  geom_line(aes(date, rolling_jg), color="#00FFFF", size=1, linetype="dashed") +
  annotate(
    "text", label = "Actual Values",
    x=as.Date("2021-06-15"), y = 0.08, size = 8, colour = "#03a0ff") +
  annotate(
    "text", label = "Average\n2018-19 Value",
    x=as.Date("2022-06-15"), y = 0.12, size = 8, colour = "#FF00FF") +
  annotate(
    "text", label = "3-Month Average",
    x=as.Date("2022-03-25"), y = 0.2, size = 8, colour = "#00FFFF") +
  labs(x="", y="",
       title="Average hourly earnings, all employees, accommodation and food services",
       subtitle = paste("Monthly change, annualized. Rate over the last three months was", ST_AHE_Now, "versus", ST_AHE_2018, "in 2018-2019"),,
       caption = "BLS, CES, seasonally adjusted, author's calculations. Mike Konczal, Roosevelt Institute") +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_y_continuous(labels = scales::percent) +
  theme_lass + theme(plot.title = element_text(size=20),
                     axis.text.y = element_text(size=18),
                     axis.text.x = element_text(size=18))

ggsave("graphics/leisure_hospitality1.png", width = 14.25, height=8.01, dpi="retina")




##### Graphic 4: Comparison
# Average hourly earnings of production and nonsupervisory employees, total private, seasonally adjusted
ces_data %>% filter(series_title %in%
                             c("Average hourly earnings of all employees, accommodation and food services, seasonally adjusted",
                               "Average hourly earnings of all employees, total private, seasonally adjusted")) %>%
  group_by(series_title) %>%
  mutate(Threemonth = value/lag(value,3)-1) %>%
  ungroup() %>%
  filter(date >= "2021-01-01") %>%
  ggplot(aes(date, Threemonth, color=series_title)) + geom_line(size=1.2) +
  labs(x="", y="",
       title="Average hourly earnings all employees: accommodation and food services (orange) vs total private (blue)",
       subtitle = "Monthly change, not annualized",
       caption = "BLS, CES, seasonally adjusted, author's calculations. Mike Konczal, Roosevelt Institute") +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_y_continuous(labels = scales::percent) +
  theme_lass + theme(plot.title = element_text(size=20),
                     axis.text.y = element_text(size=18),
                     axis.text.x = element_text(size=18))

ggsave("graphics/leisure_hospitality_wages.png", width = 14.25, height=8.01, dpi="retina")