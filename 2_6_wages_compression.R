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
#source(file = "1_c_load_bls_ces_wages_data.R")
#load("data/cps_jobs_data.RData")
#load("data/ces_data.RData")

hours_worked_categories <- c("Average weekly hours of all employees, goods-producing, seasonally adjusted",
"Average weekly hours of all employees, private service-providing, seasonally adjusted")

wages_index_categories <- c("Indexes of aggregate weekly payrolls of all employees, 2007=100, goods-producing, seasonally adjusted",
"Indexes of aggregate weekly payrolls of all employees, 2007=100, private service-providing, seasonally adjusted")

wages_weekly_categories <- c("Average weekly earnings of all employees, goods-producing, seasonally adjusted",
  "Average weekly earnings of all employees, private service-providing, seasonally adjusted")

wages_hourly_categories <- c("Average hourly earnings of all employees, goods-producing, seasonally adjusted",
                             "Average hourly earnings of all employees, private service-providing, seasonally adjusted")


#####
g_v_s_wages <- ces_data %>% filter(series_title %in% wages_hourly_categories | series_title %in% hours_worked_categories) %>%
  select(date, series_title, value) %>%
  mutate(sector = ifelse(str_detect(series_title, "service"), "Services", "Goods")) %>%
  mutate(type = ifelse(str_detect(series_title, "weekly"), "Average weekly hours of all employees",
                       "Average hourly earnings of all employees"))

g_v_s_wages %>% ggplot(aes(date, value, color=sector)) + geom_line(size=2) + theme_classic() +
  facet_wrap(~type, scales = 'free') +
  theme(legend.position='bottom', legend.title = element_blank()) +
  scale_color_manual(values = c("#01579B", "#bc5090")) +
  labs(title="Goods and Services Hourly Wage Difference Closing, Even As Weekly Earnings Stays the Same",
       subtitle="",
       caption="CES, Author's Calculations. Mike Konczal, Roosevelt Institute",
       x="", y="") +
  theme(strip.text = element_text(color = "black", hjust = 0.5, size = 16),
        plot.title = element_text(size = 25), plot.subtitle = element_text(size=20),
        plot.caption = element_text(size=22, margin=margin(19,0,11,0), lineheight=1.05),
        plot.title.position = "plot") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()) +
  theme(axis.text=element_text(size=20), legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(), legend.text = element_text(size=22, color="#222222"), panel.background = element_blank())

ggsave("graphics/g_v_s_wages.png", width = 19, height=10.68, dpi="retina")

####


#####
g_v_s_wages %>% filter(type == "Average hourly earnings of all employees") %>%
  group_by(sector) %>%
  mutate(diff = value - lag(value,1), diff = diff/lag(value,1)) %>%
  ungroup() %>%
  ggplot(aes(date, diff)) + geom_line(size=2) + theme_classic() +
  facet_wrap(~sector, scales = 'free') +
  theme(legend.position='bottom', legend.title = element_blank()) +
  scale_color_manual(values = c("#01579B", "#bc5090")) +
  labs(title="Goods and Services Hourly Wage Difference Closing, Even As Weekly Earnings Stays the Same",
       subtitle="",
       caption="CES, Author's Calculations. Mike Konczal, Roosevelt Institute",
       x="", y="") +
  theme(strip.text = element_text(color = "black", hjust = 0.5, size = 16),
        plot.title = element_text(size = 25), plot.subtitle = element_text(size=20),
        plot.caption = element_text(size=22, margin=margin(19,0,11,0), lineheight=1.05),
        plot.title.position = "plot") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()) +
  theme(axis.text=element_text(size=20), legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(), legend.text = element_text(size=22, color="#222222"), panel.background = element_blank())
