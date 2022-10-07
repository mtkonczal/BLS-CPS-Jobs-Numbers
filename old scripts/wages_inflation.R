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
source(file = "1_a_load_bls_cps_jobs_data.R")
source(file = "1_b_load_bls_ces_jobs_data.R")
#load("data/cps_jobs_data.RData")
#load("data/ces_data.RData")
#load("/Users/mkonczal/Documents/GitHub/BLS-CPI-Inflation-Analysis/data/cpi_data.RData")


full_categories <- as_tibble(unique(ces_wages_data$series_title))

hours_worked_categories <- c("Average weekly hours of all employees, goods-producing, seasonally adjusted",
"Average weekly hours of all employees, private service-providing, seasonally adjusted")

wages_index_categories <- c("Indexes of aggregate weekly payrolls of all employees, 2007=100, goods-producing, seasonally adjusted",
"Indexes of aggregate weekly payrolls of all employees, 2007=100, private service-providing, seasonally adjusted")

wages_weekly_categories <- c("Average weekly earnings of all employees, goods-producing, seasonally adjusted",
  "Average weekly earnings of all employees, private service-providing, seasonally adjusted")

wages_hourly_categories <- c("Average hourly earnings of all employees, goods-producing, seasonally adjusted",
                             "Average hourly earnings of all employees, private service-providing, seasonally adjusted")

item_basket_topline <- c("Commodities less food and energy commodities", "Services less energy services")

wages_index_categories <- c("Indexes of aggregate weekly payrolls of all employees, 2007=100, goods-producing, seasonally adjusted",
                            "Indexes of aggregate weekly payrolls of all employees, 2007=100, private service-providing, seasonally adjusted")

wages_index <- ces_wages_data %>% filter(series_title %in% wages_hourly_categories) %>% select(date, item_name = series_title, value) %>%
  mutate(type = "Average Hourly Earnings All Employees, Payrolls (BLS-CES)")

start_date <- mdy("9/01/2020")
item_basket_topline <- c("Commodities less food and energy commodities", "Services less energy services")
inflation_index <- cpi_data %>% filter(item_name %in% item_basket_topline) %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  filter(area_code == "0000") %>%
  select(date, item_name, value) %>%
  mutate(type = "CPI Core Inflation")

combined <- rbind(wages_index, inflation_index) %>%
  mutate(sector = ifelse(str_detect(item_name, "service"), "Services", "Goods")) %>%
  filter(!is.na(date), date >= start_date) %>%
  group_by(item_name) %>%
  mutate(start_value = value[date==start_date]) %>%
  mutate(normalized_value = value/start_value) %>%
  mutate(percent_change = normalized_value/lag(normalized_value,3)-1)

ggplot(combined, aes(date, percent_change, color=type)) + geom_line(size=1.2) + facet_wrap(~sector) +
  theme_classic() +
  theme(legend.position='bottom') +
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months") +
  scale_color_manual(values = c("#01579B", "darkred")) +
  labs(title="Inflation and Wages Story is Complicated by Looking at Goods and Services",
       subtitle="Average hourly earnings of all employees vs. CPI core inflation, goods vs. services, three-month percent change",
       caption="Author's calculations. Mike Konczal, Roosevelt Institute",
       x="", y="") +
  theme(strip.text = element_text(face = "bold", color = "black", hjust = 0.5, size = 21),
        plot.title = element_text(size = 25, face="bold"), plot.subtitle = element_text(size=20, margin=margin(9,0,15,0),lineheight=1.05),
        plot.caption = element_text(size=14, margin=margin(19,0,11,0), lineheight=1.05),
        plot.title.position = "plot") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()) +
  theme(axis.text=element_text(size=11), legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(), legend.text = element_text(size=15, color="#222222"), panel.background = element_blank())

ggsave("graphics/g_v_s_inflation.png", width = 19, height=10.68, dpi="retina")

start_date <- mdy("1/01/2000")

inflation_index <- cpi_data %>% filter(item_name %in% item_basket_topline) %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  filter(area_code == "0000") %>%
  select(date, item_name, value) %>%
  mutate(type = "CPI Core Inflation")

combined2 <- rbind(wages_index, inflation_index) %>%
  mutate(sector = ifelse(str_detect(item_name, "service"), "Services", "Goods")) %>%
  filter(!is.na(date), date >= start_date) %>%
  group_by(item_name) %>%
  mutate(start_value = value[date==start_date]) %>%
  mutate(normalized_value = value/start_value) %>%
  mutate(percent_change = normalized_value/lag(normalized_value,3)-1) %>%
  mutate(percent_change2 = ifelse((date > "2019-12-01" & date < "2021-01-01"), NA, (1+percent_change)^4-1))

ggplot(combined2, aes(date, percent_change2, color=type)) + geom_line(size=1.2) + facet_wrap(~sector) +
  theme_classic() +
  theme(legend.position='bottom') +
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months") +
  scale_color_manual(values = c("#01579B", "darkred")) +
  labs(title="Inflation and Wages Story is Complicated by Looking at Goods and Services",
       subtitle="Average hourly earnings of all employees vs. CPI core inflation, goods vs. services, three-month percent change",
       caption="Author's calculations. Mike Konczal, Roosevelt Institute",
       x="", y="") +
  theme(strip.text = element_text(face = "bold", color = "black", hjust = 0.5, size = 21),
        plot.title = element_text(size = 25, face="bold"), plot.subtitle = element_text(size=20, margin=margin(9,0,15,0),lineheight=1.05),
        plot.caption = element_text(size=14, margin=margin(19,0,11,0), lineheight=1.05),
        plot.title.position = "plot") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()) +
  theme(axis.text=element_text(size=11), legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(), legend.text = element_text(size=15, color="#222222"), panel.background = element_blank())

ggsave("graphics/g_v_s_inflation_longer.png", width = 19, height=10.68, dpi="retina")

