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
#source(file = "1_c_load_bls_ces_wages_data.R")
load("data/cps_jobs_data.RData")
load("data/ces_data.RData")
load("data/ces_wages_data.RData")

a <- ces_wages_data %>% filter(period != "M13", seasonal == "S") %>%
  filter(display_level == 2, data_type_code == "03")

ggplot(a, aes(date, value)) + geom_line() + theme_classic() + facet_wrap(~industry_name)

b <- ces_wages_data %>% filter(period != "M13", seasonal == "S") %>%
  filter(display_level == 2, data_type_code == "01") %>%
  select(date, jobs = value, industry_name) %>%
  inner_join(a, by = c("date", "industry_name")) %>%
  select(date, jobs, industry_name, wages = value)


b %>% mutate(year = year(date)) %>% filter(year > 2020) %>%
  group_by(industry_name) %>%
  mutate(l_jobs = (jobs - lag(jobs,12))/lag(jobs,12)) %>%
  mutate(l_wages = (wages - lag(wages,12))/lag(wages,12)) %>%
  ungroup() %>%
  filter(date==max(date)) %>%
  ggplot(aes(l_jobs, l_wages, color=industry_name, label=industry_name)) + geom_point() + theme_classic() +
  geom_label_repel() + theme(legend.position='none')



#COMPARISON 1 GRAPHIC - ANNUAL VERSUS 219
comparison <- b %>% mutate(year = year(date)) %>%
  group_by(industry_name) %>%
  mutate(l_jobs = (jobs - lag(jobs,12))/lag(jobs,12)) %>%
  mutate(l_wages = (wages - lag(wages,12))/lag(wages,12)) %>%
  ungroup() %>%
  filter(date==max(date) | date == "2020-01-01")

comparison <- comparison %>% mutate(display_valueJ = l_jobs*(date == max(date)))
comparison$display_valueJ <- na_if(comparison$display_valueJ, 0)
comparison <- comparison %>% mutate(display_valueW = l_wages*(date == max(date)))
comparison$display_valueW <- na_if(comparison$display_valueW, 0)
maxdate <- max(comparison$date)
maxdate <- as.character(format(maxdate, format="%B %Y"))

ggplot(comparison, aes(l_jobs, l_wages, color=industry_name)) + geom_line(size=1.2) +
  geom_point(size=5) +
  geom_point(aes(display_valueJ,display_valueW),size=10) + theme_classic() +
  theme(legend.position='bottom') +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  labs(title="2019 Versus the Last 12 Months: Job and Wage Growth Both Increased Across Most Industries",
       subtitle= paste("The larger circle represents the 12-month change ending ", maxdate, ". The smaller circle is the annual value for 2019. Consistent with a demand curve?", sep=""),
       caption=paste("CES values for employment and average hourly wages. Seasonally adjusted. Author's Calculations. Mike Konczal, Roosevelt Institute"),
       x="Employment Growth", y="Average Hourly Earnings, All Employees") +
  theme(plot.title = element_text(size = 25),
        plot.caption = element_text(size=12),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=20, margin=ggplot2::margin(9,0,15,0),lineheight=1.05)) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=15, color="#222222"), panel.background = element_blank())

ggsave("graphics/labor_curve.png", width = 19, height=10.68, dpi="retina")

#COMPARISON 2 GRAPHIC - ANNUAL VERSUS 219
comparison <- b %>% mutate(year = year(date)) %>%
  group_by(industry_name) %>%
  mutate(l_jobs = (jobs - lag(jobs,6))/lag(jobs,6)) %>%
  mutate(l_wages = (wages - lag(wages,6))/lag(wages,6)) %>%
  ungroup() %>%
  filter(date==max(date) | date == "2022-01-01")

comparison <- comparison %>% mutate(display_valueJ = l_jobs*(date == max(date)))
comparison$display_valueJ <- na_if(comparison$display_valueJ, 0)
comparison <- comparison %>% mutate(display_valueW = l_wages*(date == max(date)))
comparison$display_valueW <- na_if(comparison$display_valueW, 0)
maxdate <- max(comparison$date)
maxdate <- as.character(format(maxdate, format="%B %Y"))

ggplot(comparison, aes(l_jobs, l_wages, color=industry_name)) + geom_line(size=1.2) +
  geom_point(size=5) +
  geom_point(aes(display_valueJ,display_valueW),size=10) + theme_classic() +
  theme(legend.position='bottom') +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  labs(title="Compared to 2021, Job and Wage Growth Have Slowed Across Many Industries in the Past Six Months",
       subtitle= paste("The circle represents the 6-month change ending ", maxdate, ". The line starts with the value for the second half of 2021. Not annualized", sep=""),
       caption=paste("CES Employment, Author's Calculations. Mike Konczal, Roosevelt Institute"),
       x="Employment Growth", y="Average Hourly Earnings, All Employees") +
  theme(plot.title = element_text(size = 25),
        plot.caption = element_text(size=12),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=20, margin=ggplot2::margin(9,0,15,0),lineheight=1.05)) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=15, color="#222222"), panel.background = element_blank())

ggsave("graphics/labor_curve2.png", width = 19, height=10.68, dpi="retina")
