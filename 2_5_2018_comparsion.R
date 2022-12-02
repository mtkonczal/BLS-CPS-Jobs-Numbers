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


#GRAPHIC 1
JG <- ces_data %>% filter(series_title=="All employees, thousands, total nonfarm, seasonally adjusted") %>%
  select(date, series_title, year, employment = value) %>%
  mutate(job_growth = employment - lag(employment,1)) %>%
  mutate(rolling_jg = job_growth + lag(job_growth,1) + lag(job_growth,2),
         rolling_jg = rolling_jg/3)

JG1 <- JG %>% filter(year == 2019 | year == 2018) %>%
  summarize(mean_jg = mean(job_growth))
JG1 <- as.numeric(JG1)

JG %>% filter(year == 2022) %>%
  ggplot(aes(date, job_growth)) + geom_line(size=1.2, color="#03a0ff") + theme_classic() +
  geom_hline(yintercept = JG1, color="#FF00FF", size=0.9, linetype="dashed") +
  geom_line(aes(date, rolling_jg), color="#00FFFF", size=1, linetype="dashed") +
  annotate(
    "text", label = "Average 2018-2019 Value",
    x=as.Date("2022-07-01"), y = JG1+40, size = 8, color = "#FF00FF") +
  annotate(
    "text", label = "Actual Values",
    x=as.Date("2022-02-01"), y = 750, size = 8, color = "#03a0ff") +
  ylim(0,800) +
  annotate(
    "text", label = "3-Month Average",
    x=as.Date("2022-04-01"), y = 560, size = 8, color = "#00FFFF") +
  labs(x="", y="",
       title="Monthly Job Growth is Returning to 2018-2019 Levels",
       subtitle = "Monthly job growth in thousands. Given lags in monetary tightening, real worry is overshooting these previous healthy levels.",
       caption = "BLS, CES, seasonally-adjusted, author's calculations. Mike Konczal, Roosevelt Institute") +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 months") +
  theme_lass + theme(plot.title = element_text(size=30),
                     axis.text.y = element_text(size=18),
                     axis.text.x = element_text(size=18))

ggsave("graphics/2018_jobs.png", width = 14.25, height=8.01, dpi="retina")

##### Graphic 2
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
    x=as.Date("2021-04-15"), y = (1+0.00563)^12-1, size = 8, colour = "#03a0ff") +
  annotate(
    "text", label = "Average\n2018-19 Value",
    x=as.Date("2022-06-15"), y = (1+AHE1-0.00053)^12-1, size = 8, colour = "#FF00FF") +
  annotate(
    "text", label = "3-Month\nAverage",
    x=as.Date("2021-09-25"), y = (1+0.00355)^12-1, size = 8, colour = "#00FFFF") +
  labs(x="", y="",
       title="Through the Noise, Wage Growth Still Returning to 2018-2019 Levels",
       subtitle = paste("Annualized, monthly average hourly earnings of all employees, total private. Rate over the last three months was", ST_AHE_Now, "versus", ST_AHE_2018, "in 2018-2019"),
       caption = "BLS, CES, seasonally adjusted, author's calculations. Mike Konczal, Roosevelt Institute") +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
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