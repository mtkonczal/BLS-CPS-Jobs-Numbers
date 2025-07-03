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
library(tidytext)
library(hrbrthemes)
library(viridis)

##### SET UP SOME THINGS #####
#source(file = "1_a_load_bls_cps_jobs_data.R")
#source(file = "1_b_load_bls_ces_jobs_data.R")

graphic_1_title = "Unemployment Duration Still Decreasing, Remains Below 2019 Levels"
graphic_2_title = "Fewer Entrants, More Job Losers, Among Unemployed"


#### Graphic 1: Duration Length of Unemployment ####
g_dates <- cps_jobs_data %>% filter(date >= "2017-01-01")
g_dates <- unique(g_dates$date)
g_dates <- sort(g_dates, decreasing = TRUE)
g_dates = g_dates[seq(1, length(g_dates), 12)]

u_duration_series <- c("(Seas) Median Weeks Unemployed", "(Seas) Average Weeks Unemployed")

cps_jobs_data %>% filter(series_title %in% u_duration_series, periodicity_code == "M") %>%
  group_by(series_title) %>%
  mutate(pre_value = mean(value[year(date)==2019])) %>%
  mutate(last_value = if_else(date == max(date), value, as.numeric(NA))) %>%
  ungroup() %>%
  mutate(pre_value = if_else(year(date)>=2019,pre_value,as.numeric(NA))) %>%
  filter(date >= "2017-01-01") %>%
  mutate(series_title = str_remove(series_title,"\\(Seas\\)")) %>%
  ggplot(aes(date,value,color=series_title,label=last_value)) + 
  geom_line(size=2) + theme_lass +
  geom_line(aes(date,pre_value,color=series_title), linetype="dashed") +
  theme(legend.position = c(0.3,0.9)) +
  scale_x_date(date_labels = "%B\n%Y", breaks=g_dates) +
  labs(title=graphic_1_title,
       subtitle="Average and median weeks of unemployment length, dotted line is average 2019 value.",
       caption="BLS, CPS, Seasonally-Adjusted, Mike Konczal, Roosevelt Institute") +
  scale_color_manual(values=c("#2D779C", "#97BC56")) +
  geom_text(show.legend=FALSE, nudge_x = 60, size = 5.5)

ggsave("graphics/durations.png",  width = 12, height=9, dpi="retina")


#### Graphic 2: Unemployment Type by Percent ####
g_dates <- cps_jobs_data %>% filter(date >= "2021-01-01")
g_dates <- unique(g_dates$date)
g_dates <- sort(g_dates, decreasing = TRUE)
g_dates = g_dates[seq(1, length(g_dates), 6)]

u_reasons_series <- c("LNS13023706", "LNS13023654", "LNS13026511")
new_reentrants <- c("LNS13023558", "LNS13023570")

new_reentrants_data <- cps_jobs_data %>% filter(series_id %in% new_reentrants, periodicity_code == "M") %>%
  arrange(date) %>% group_by(date) %>% summarize(value = sum(value)) %>% ungroup() %>%
  mutate(series_title = "New entrants and reentrants") %>% select(series_title, date, value)
  

cps_jobs_data %>% filter(series_id %in% u_reasons_series, periodicity_code == "M") %>%
  select(series_title, date, value) %>%
  rbind(new_reentrants_data) %>%
  mutate(value = value/100) %>%
  mutate(series_title = str_remove(series_title,"as a Percent of Total Unemployed")) %>%
  mutate(series_title = str_remove(series_title,"\\(Seas\\)")) %>%
  group_by(series_title) %>%
  mutate(pre_value = mean(value[year(date)==2019])) %>%
  mutate(last_value = if_else(date == max(date), value, as.numeric(NA))) %>%
  ungroup() %>%
  mutate(pre_value = if_else(year(date)>=2019,pre_value,as.numeric(NA))) %>%
  filter(date >= "2021-01-01") %>%
  ggplot(aes(date,value, color=series_title,label=label_percent()(round(last_value,2)))) +
  geom_line(size=2) + theme_lass + facet_wrap(~series_title, scales = "free") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=g_dates) +
  labs(title=graphic_2_title,
       subtitle="Percent of unemployed, by category of unemployment. Dotted line is average 2019 value.",
       caption="BLS, CPS, Seasonally-Adjusted, Mike Konczal, Roosevelt Institute") +
  scale_color_manual(values=c("#6EA4BF","#2D779C", "#97BC56","#E2E47E")) +
  geom_line(aes(date,pre_value,color=series_title), linetype="dashed") +
  geom_text(show.legend=FALSE, nudge_x = 60, size = 4)

ggsave("graphics/u_by_percent.png",  width = 10, height=10, dpi="retina")

#### Graphic 3: Unemployment Type by Unemployment Rate ####

job_leavers <- cps_jobs_data %>% filter(series_id %in% c("LNS13023705","LNS11000000")) %>%
  group_by(date) %>%
  summarize(better_percent = value[series_id == "LNS13023705"]/value[series_id == "LNS11000000"]) %>%
  filter(!is.na(better_percent)) %>%
  mutate(series_title = "Job Leavers Unemployment Rate")

entrants <- cps_jobs_data %>% filter(series_id %in% c("LNS13023557","LNS13023569","LNS11000000")) %>%
  group_by(date) %>%
  summarize(better_percent = (value[series_id == "LNS13023557"]+value[series_id == "LNS13023569"])/value[series_id == "LNS11000000"]) %>%
  filter(!is.na(better_percent)) %>%
  mutate(series_title = "New Entrants and Reentrants")

on_temporary_layoff <- cps_jobs_data %>% filter(series_id %in% c("LNS13023653","LNS11000000")) %>%
  group_by(date) %>%
  summarize(better_percent = value[series_id == "LNS13023653"]/value[series_id == "LNS11000000"]) %>%
  filter(!is.na(better_percent)) %>%
  mutate(series_title = "Job Losers on Temporary Layoff")

not_on_temporary_layoff <- cps_jobs_data %>% filter(series_id %in% c("LNS13025699","LNS11000000")) %>%
  group_by(date) %>%
  summarize(better_percent = value[series_id == "LNS13025699"]/value[series_id == "LNS11000000"]) %>%
  filter(!is.na(better_percent)) %>%
  mutate(series_title = "Job Losers Not on Temporary Layoff")

urate_mine <- rbind(job_leavers,entrants,on_temporary_layoff,not_on_temporary_layoff) %>% group_by(date) %>% summarize(this_u_rate_works_maybe = sum(better_percent)) %>% ungroup()
year_delay <- max(not_on_temporary_layoff$date) %m-% months(12)

rbind(job_leavers,entrants,on_temporary_layoff,not_on_temporary_layoff) %>%
  rename(value = better_percent) %>%
  group_by(series_title) %>%
  mutate(pre_value = mean(value[year(date)==2019])) %>%
  mutate(last_value = if_else(date == max(date), value, as.numeric(NA))) %>%
  ungroup() %>%
  mutate(pre_value = if_else(year(date)>=2019,pre_value,as.numeric(NA))) %>%
  filter(date >= year_delay) %>%
  ggplot(aes(date,value, color=series_title,label=label_percent()(round(last_value,3)))) +
  geom_line(size=2) + theme_lass + facet_wrap(~series_title, scales = "free") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=g_dates) +
  labs(title=graphic_2_title,
       subtitle="Unemployment rate contribution, by category of unemployment. Dotted line is average 2019 value.",
       caption="BLS, CPS, Seasonally-Adjusted, Mike Konczal, Roosevelt Institute") +
  scale_color_manual(values=c("#6EA4BF","#2D779C", "#97BC56","#E2E47E")) +
  geom_line(aes(date,pre_value,color=series_title), linetype="dashed") +
  geom_text(show.legend=FALSE, nudge_x = 25, size = 4)

ggsave("graphics/u_by_urate.png",  width = 10, height=10, dpi="retina")