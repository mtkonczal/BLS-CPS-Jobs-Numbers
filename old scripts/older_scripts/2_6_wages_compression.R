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



GS_diff <- g_v_s_wages %>% filter(type == "Average hourly earnings of all employees") %>%
  group_by(sector) %>%
  mutate(diff = value - lag(value,1), diff = diff/lag(value,1), diff = (1+diff)^12-1) %>%
  mutate(rolling_diff = diff + lag(diff,1) + lag(diff,2),
         rolling_diff = rolling_diff/3)

GS_diff1 <- GS_diff %>% mutate(year = year(date)) %>% filter(year == 2019 | year == 2018) %>%
  group_by(sector) %>% summarize(mean_jg = mean(diff)) %>%
  ungroup() %>%
  pivot_wider(names_from = sector, values_from = mean_jg)


#####
GS_diff %>% filter(type == "Average hourly earnings of all employees", sector == "Services") %>%
  filter(date >= "2021-04-01") %>%
  ggplot(aes(date, diff)) + geom_line(size=1.2, color="#0072B2") + theme_classic() +
  geom_hline(yintercept = GS_diff1$Services, color="#D55E00", size=0.9, linetype="dashed") +
  geom_line(aes(date, rolling_diff), color="#56B4E9", size=1, linetype="dashed") +
  annotate(
    "text", label = "Average 2018-2019 Value",
    x=as.Date("2022-07-01"), y = GS_diff1$Services-.003004128, size = 5, colour = "#D55E00") +
  annotate(
    "text", label = "Actual Values",
    x=as.Date("2021-07-01"), y = 1.00625^12-1, size = 5, colour = "#0072B2") +
  annotate(
    "text", label = "3-Month Average",
    x=as.Date("2022-01-01"), y = 1.00575^12-1, size = 5, colour = "#56B4E9") +
  labs(x="", y="",
       title="Services Wages are Beginning to Return to 2018-2019 Levels",
       subtitle = "Average hourly earnings, all employees, private service-providing, monthly change annualized",
       caption = "Seasonally adjusted, CES, Author's calculations. Mike Konczal, Roosevelt Institute") +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_y_continuous(labels = scales::percent) +
  theme_lass + theme(plot.title = element_text(size=30),
                     axis.text.y = element_text(size=18),
                     axis.text.x = element_text(size=18))

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

ggsave("graphics/2018_services_wage_growth.png", width = 9.5, height=5.34, dpi="retina")