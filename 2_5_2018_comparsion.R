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
  group_by(year) %>% summarize(mean_jg = mean(job_growth)) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = mean_jg)

JG %>% filter(year == 2022) %>%
  ggplot(aes(date, job_growth)) + geom_line(size=1.2, color="#0072B2") + theme_classic() +
  geom_hline(yintercept = JG1$`2018`, color="#D55E00", size=0.9, linetype="dashed") +
  geom_hline(yintercept = JG1$`2019`, color="#E69F00", size=0.9, linetype="dashed") +
  geom_line(aes(date, rolling_jg), color="#56B4E9", size=1, linetype="dashed") +
  annotate(
    "text", label = "Average 2018 Value",
    x=as.Date("2022-07-01"), y = JG1$`2018`+30, size = 5, colour = "#D55E00") +
  annotate(
    "text", label = "Actual Values",
    x=as.Date("2022-02-01"), y = 750, size = 5, colour = "#0072B2") +
  annotate(
    "text", label = "Average 2019 Value",
    x=as.Date("2022-07-01"), y = JG1$`2019`-30, size = 5, colour = "#E69F00") +
  ylim(0,800) +
  annotate(
    "text", label = "3-Month Average",
    x=as.Date("2022-04-01"), y = 550, size = 5, colour = "#56B4E9") +
  ylim(0,800) +
  labs(x="", y="",
       title="Monthly Job Growth is Beginning to Return to 2018-2019 Levels",
       subtitle = "Given lags in monetary tightening, real worry is overshooting these previous healthy levels",
       caption = "Monthly Job Growth in Thousands, CES, Author's calculations. Mike Konczal, Roosevelt Institute") +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 months") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20, face="bold"),
        plot.subtitle = element_text(size=15),
        plot.caption = element_text(size=10),
        plot.title.position = "plot",
        axis.text=element_text(size=18))


ggsave("graphics/2018_jobs.png", width = 9.5, height=5.34, dpi="retina")

##### Graphic 2
AHE <- ces_data %>% filter(series_title == "Average hourly earnings of all employees, total private, seasonally adjusted") %>%
    select(date, series_title, year, wages = value) %>%
    mutate(wage_growth = wages - lag(wages,1), wage_growth = wage_growth/lag(wages,1)) %>%
    mutate(rolling_jg = wage_growth + lag(wage_growth,1) + lag(wage_growth,2),
           rolling_jg = rolling_jg/3)
  
  AHE1 <- AHE %>% filter(year == 2019 | year == 2018) %>%
    group_by(year) %>% summarize(mean_jg = mean(wage_growth)) %>%
    ungroup() %>%
    pivot_wider(names_from = year, values_from = mean_jg)
  
  AHE %>% filter(date >= "2021-01-01") %>%
    ggplot(aes(date, wage_growth)) + geom_line(size=1.2, color="#0072B2") + theme_classic() +
    geom_hline(yintercept = AHE1$`2018`, color="#D55E00", size=0.9, linetype="dashed") +
    geom_hline(yintercept = AHE1$`2019`, color="#E69F00", size=0.9, linetype="dashed") +
    geom_line(aes(date, rolling_jg), color="#56B4E9", size=1, linetype="dashed") +
    annotate(
      "text", label = "Actual Values",
      x=as.Date("2021-04-15"), y = 0.00553, size = 5, colour = "#0072B2") +
    annotate(
      "text", label = "Average 2018 Value",
      x=as.Date("2022-05-15"), y = AHE1$`2018`+0.00023, size = 5, colour = "#D55E00") +
    annotate(
      "text", label = "Average 2019 Value",
      x=as.Date("2022-05-15"), y = AHE1$`2019`-0.00023, size = 5, colour = "#E69F00") +
    annotate(
      "text", label = "3-Month\nAverage",
      x=as.Date("2021-06-15"), y = 0.00355, size = 5, colour = "#56B4E9") +
    labs(x="", y="",
         title="Average Hourly Wage Growth is Returning to 2018-2019 Levels",
         subtitle = "Given lags in monetary tightening, real worry is overshooting these previous healthy levels",
         caption = "Monthly average hourly earnings of all employees, total private, seasonally adjusted; CES, Author's calculations. Mike Konczal, Roosevelt Institute") +
    scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
    scale_y_continuous(labels = scales::percent) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 20, face="bold"),
          plot.subtitle = element_text(size=15),
          plot.caption = element_text(size=10),
          plot.title.position = "plot",
          axis.text=element_text(size=18))
    
ggsave("graphics/2018_wage.png", width = 9.5, height=5.34, dpi="retina")