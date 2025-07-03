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

##### SET UP SOME THINGS #####
#source(file = "1_a_load_bls_cps_jobs_data.R")
#source(file = "1_b_load_bls_ces_jobs_data.R")

ces_dates <- ces_data %>% filter(!is.na(date)) %>% filter(date >= "2021-01-01")
ces_dates <- unique(ces_dates$date)
ces_dates <- sort(ces_dates, decreasing = TRUE)
ces_dates = ces_dates[seq(1, length(ces_dates), 3)]

# Double Graphic
months_difference <- 6

one_month <- ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == 3) %>% group_by(industry_name) %>%
  summarize(before = value[date == max(date) %m-% months(1)],
            after = value[date == max(date)]) %>%
  mutate(difference = after - before, months_category = "Last Month")


double_jobs_graphic <- ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == 3) %>% group_by(industry_name) %>%
  summarize(before = value[date == max(date) %m-% months(months_difference)],
            after = value[date == max(date)]) %>%
  mutate(difference = after - before, difference = difference/months_difference) %>% arrange(desc(difference)) %>%
  mutate(months_category = "Last Six Months (Monthly Average)") %>%
  rbind(one_month) %>%
  mutate(difference_label = round(difference)) %>%
  mutate(positive = difference_label >= 0)

double_jobs_graphic %>%
  mutate(industry_name = reorder_within(industry_name, difference, months_category)) %>%
  
  ggplot(aes(industry_name, difference, label=difference_label, fill=positive)) +
  geom_col(size=0) +
  scale_x_reordered() +
  facet_wrap( ~ months_category, scales = "free_y", ncol = 1) + coord_flip() +
  theme_lass +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  theme(panel.grid.major.y = element_line(size=0)) +
  theme(plot.title.position = "plot") +
  labs(y = NULL,
       x = NULL,
       title = "six_month_jobs_occupations_title",
       subtitle = "Job growth last month versus average of the last six months, by occupations, employment survey, seasonally adjusted",
       caption ="BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = c(0.75,0.5)) +
  geom_text(aes(y = difference_label + 4 * sign(difference_label), label = difference_label), color="white", size=6, ) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "none") +
  theme(strip.text.x = element_text(size = 15))


ggsave("graphics/sectors_employment_differences_bars.png",  width = 12, height=8, dpi="retina")


# Helper Graphic
double_jobs_graphic %>%
  group_by(industry_name) %>%
  summarize(slowdown = difference[months_category == "Last Month"] - difference[months_category == "Last Six Months (Monthly Average)"]) %>%
  ggplot(aes(reorder(industry_name, slowdown), slowdown)) + geom_col() + theme_classic() + coord_flip() +
  labs(title="Not for circulation", subtitle="A positive value means last month was higher than previous six months by that amount.",
       x="",y="") +
  theme(plot.title.position = "plot")

ggsave("graphics/sectors_employment_differences_summary.png",  width = 12, height=6.75, dpi="retina")


latest <- ces_data %>% filter(seasonal == "S") %>% filter(date == max(date), data_type_code == 3)

# We start here

total_private <- ces_data %>% filter(series_id %in% c("CES0500000001","CES0500000003","CES0500000002")) %>% select(date, total_private = value, data_type_code)

# This line gives us all total private, these add up to the total private value
total_industries <- ces_data %>% filter(seasonal == "S", (display_level == 2 & data_type_code %in% c(1,3,2) & supersector_name != "Government")) %>% filter(date >= "2023-01-01")

#Confirm job number

tester <- total_industries %>% group_by(date) %>% mutate(total_private_jobs = sum(value[data_type_code==1])) %>% ungroup() %>% select(date, value, data_type_code, supersector_name, total_private_jobs) %>%
  pivot_wider(names_from = data_type_code, values_from = value) %>% rename(jobs = `1`, hours = `3`) %>%
  mutate(marginal_wages = jobs*hours/total_private_jobs) %>% group_by(date) %>%
  summarize(calculated_AHE = sum(marginal_wages))

total_private %>% filter(data_type_code == 3) %>% inner_join(tester, by="date")

#ces_data %>% filter(series_id == "CES0500000002", date >="2023-01-01") %>% select(series_title, date, value)
total_industries <- ces_data %>% filter(seasonal == "S", (display_level == 2 & data_type_code %in% c(1,3,2) & supersector_name != "Government")) %>%
  group_by(date) %>% mutate(total_private_jobs = sum(value[data_type_code==1])) %>% ungroup() %>%
  select(date, value, data_type_code, supersector_name, total_private_jobs) %>%
  pivot_wider(names_from = data_type_code, values_from = value) %>% rename(jobs = `1`, hours = `2`, wages = `3`) %>% filter(!is.na(hours)) %>%
  arrange(date) %>% group_by(supersector_name) %>%
  mutate(diff_jobs = jobs - lag(jobs,1), diff_jobs_percent = jobs/lag(jobs,1)-1) %>%
  mutate(diff_wages = wages - lag(wages,1), diff_wages_percent = wages/lag(wages,1)-1) %>%
  ungroup()




####### NOT SURE

ces_data %>% filter(seasonal == "S", (display_level == 2 & data_type_code %in% c(1,3) & supersector_name != "Government")) %>%
  group_by(date, data_type_code,supersector_name) %>%
  mutate(difference = value/lag(value,1)-1, difference_label = difference) %>%
  mutate(positive = difference_label >= 0) %>%
  filter(date == max(date)) %>%
  mutate(industry_name = reorder_within(industry_name, value, data_type_text)) %>%
  ggplot(aes(industry_name, value, label=difference_label, fill=positive)) +
  geom_col(size=0) +
  scale_x_reordered() +
  facet_wrap( ~data_type_text, scales = "free", ncol = 1) + coord_flip() +
  theme_lass +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  theme(panel.grid.major.y = element_line(size=0)) +
  theme(plot.title.position = "plot") +
  labs(y = NULL,
       x = NULL,
       title = "six_month_jobs_occupations_title",
       subtitle = "Job growth last month versus average of the last six months, by occupations, employment survey, seasonally adjusted",
       caption ="BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = c(0.75,0.5)) +
  geom_text(aes(y = difference_label + 4 * sign(difference_label), label = difference_label), color="white", size=6, ) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "none") +
  theme(strip.text.x = element_text(size = 15))


ggsave("graphics/sectors_employment_differences_bars.png",  width = 12, height=8, dpi="retina")




jobs <- ces_data %>% filter(seasonal == "S", (display_level == 2 & data_type_code %in% c(1) & supersector_name != "Government")) %>%
  group_by(supersector_name) %>%
  mutate(difference = value/lag(value,1, order_by = date)-1) %>% filter(date == max(date)) %>% ungroup() %>% select(date,data_type_text, supersector_name, value, difference, industry_name)

ces_data %>% filter(seasonal == "S", (display_level == 2 & data_type_code %in% c(3) & supersector_name != "Government")) %>%
  group_by(supersector_name) %>%
  mutate(difference = value/lag(value,1, order_by = date) - 1) %>% filter(date == max(date)) %>% ungroup() %>% select(date,data_type_text, supersector_name, value, difference, industry_name) %>%
  rbind(jobs) %>% mutate(difference_label = difference) %>%
  mutate(positive = difference_label >= 0) %>%
  filter(date == max(date)) %>%
  mutate(industry_name = reorder_within(industry_name, value, data_type_text)) %>%
  ggplot(aes(industry_name, difference, label=difference_label, fill=positive)) +
  geom_col(size=0) +
  scale_x_reordered() +
  facet_wrap( ~data_type_text, scales = "free", ncol = 1) + coord_flip() +
  theme_lass +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  theme(panel.grid.major.y = element_line(size=0)) +
  theme(plot.title.position = "plot") +
  labs(y = NULL,
       x = NULL,
       title = "six_month_jobs_occupations_title",
       subtitle = "Job growth last month versus average of the last six months, by occupations, employment survey, seasonally adjusted",
       caption ="BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = c(0.75,0.5)) +
#  geom_text(aes(y = difference_label + 4 * sign(difference_label), label = difference_label), color="white", size=6, ) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "none") +
  theme(strip.text.x = element_text(size = 15))



ces_data %>% filter(seasonal == "S", (display_level == 2 & data_type_code %in% c(3) & supersector_name != "Government")) %>%
  group_by(supersector_name) %>%
  mutate(difference = value/lag(value,1, order_by = date) - 1) %>% filter(date == max(date)) %>% ungroup() %>% select(date,data_type_text, supersector_name, value, difference, industry_name) %>%
  mutate(difference_label = difference) %>%
  mutate(positive = difference_label >= 0) %>%
  filter(date == max(date)) %>%
  mutate(industry_name = reorder_within(industry_name, value, data_type_text)) %>%
  ggplot(aes(industry_name, difference, label=difference_label, fill=positive)) +
  geom_col(size=0) +
  scale_x_reordered() +
  facet_wrap( ~data_type_text, scales = "free", ncol = 1) + coord_flip() +
  theme_lass +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  theme(panel.grid.major.y = element_line(size=0)) +
  theme(plot.title.position = "plot") +
  labs(y = NULL,
       x = NULL,
       title = "six_month_jobs_occupations_title",
       subtitle = "Job growth last month versus average of the last six months, by occupations, employment survey, seasonally adjusted",
       caption ="BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = c(0.75,0.5)) +
  #  geom_text(aes(y = difference_label + 4 * sign(difference_label), label = difference_label), color="white", size=6, ) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "none") +
  theme(strip.text.x = element_text(size = 15))




months_difference <- 1

wages <- ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == 3) %>% group_by(industry_name) %>%
  summarize(before = value[date == max(date) %m-% months(months_difference)],
            after = value[date == max(date)]) %>%
  ungroup() %>%
  mutate(difference = after-before) %>%
  mutate(positive = difference >= 0)

wages %>%
  arrange(difference) %>%
  mutate(industry_name=factor(industry_name, levels=industry_name)) %>%
  ggplot(aes(industry_name, difference, fill=positive)) +
  geom_col(size=0) +
  scale_x_reordered() +
  coord_flip() +
  theme_lass +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  theme(panel.grid.major.y = element_line(size=0)) +
  theme(plot.title.position = "plot") +
  labs(y = NULL,
       x = NULL,
       title = "Wage Growth Slowed in Highest Wage Level Industries",
       subtitle = "1-month absolute difference in average hourly earning, dollars, employment survey, seasonally adjusted",
       caption ="BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = c(0.75,0.5)) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "none") +
  theme(strip.text.x = element_text(size = 15))

ggsave("graphics/one_month_wages.png",  width = 12, height=8, dpi="retina")



