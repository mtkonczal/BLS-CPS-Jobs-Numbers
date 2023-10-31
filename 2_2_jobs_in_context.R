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

ces_dates <- unique(ces_data$date)
ces_dates <- sort(ces_dates, decreasing = TRUE)
ces_dates = ces_dates[seq(1, length(ces_dates), 3)]


##### IMAGE 1: TOTAL JOBS #####
jobs_growth <- ces_data %>% filter(series_id == "CES0000000001") %>% arrange(date) %>%
  mutate(jobs_difference = value - lag(value,1)) %>%
  mutate(jobs_difference_label = if_else(date >= max(date, na.rm=TRUE) %m-% months(6), jobs_difference, as.numeric(NA))) %>%
  mutate(before_trend = (value[date == "2020-01-01"]-value[date == "2018-01-01"] ) /24) %>%
  filter(date >= "2021-01-01")

avg_job_growth <- mean(jobs_growth$before_trend)

jobs_growth %>%
  ggplot(aes(date, jobs_difference, label=jobs_difference_label)) + geom_bar(stat="identity") +
  geom_text(nudge_y = 30) + theme_lass + geom_line(aes(date, before_trend), color="red", linetype="dashed", size=1.2) +
  labs(y = NULL,
       x = NULL,
       title = total_jobs_title,
       subtitle = paste("Total job growth, employment survey, seasonally-adjusted. Dotted line reflects 2018-19 average growth of ", avg_job_growth, " thousand.", sep=""),
       caption ="BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal") +
  scale_x_date(date_labels = "%b\n%Y", breaks=ces_dates)

ggsave("graphics/total_jobs.png", dpi="retina", width = 12, height=6.75, units = "in")

# Double Graphic


# Double Graphic
months_difference <- 6

one_month <- ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == 1) %>% group_by(industry_name) %>%
  summarize(before = value[date == max(date) %m-% months(1)],
            after = value[date == max(date)]) %>%
  mutate(difference = after - before, months_category = "Last 1 Month")

one_month_total <- sum(one_month$difference)
one_month$months_category <- paste("Last 1 Month, ", one_month_total, " thousand jobs", sep="")

double_jobs_graphic <- ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == 1) %>% group_by(industry_name) %>%
  summarize(before = value[date == max(date) %m-% months(months_difference)],
            after = value[date == max(date)]) %>%
  mutate(difference = after - before, difference = difference/months_difference) %>% arrange(desc(difference))

six_months_total <- sum(double_jobs_graphic$difference)
double_jobs_graphic$months_category <- paste("Last 6 Months, Average Monthly, ", round(six_months_total), " thousand jobs", sep="")

double_jobs_graphic <- double_jobs_graphic %>% rbind(one_month) %>%
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
       title = six_month_jobs_occupations_title,
       subtitle = "Job growth last month versus average of the last three months, by occupations, employment survey, seasonally adjusted",
       caption ="BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = c(0.75,0.5)) +
  geom_text(aes(y = difference_label + 4 * sign(difference_label), label = difference_label), color="white", size=6, ) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "none") +
  theme(strip.text.x = element_text(size = 15))


ggsave("graphics/3_month_sectors_employment_differences_bars.png",  width = 12, height=12, dpi="retina")




#### Compared to 2019 ####
compare_2019 <- ces_data %>% filter(seasonal == "S", (display_level == 5), data_type_code == 1) %>% group_by(industry_name) %>%
  summarize(before = value[date == "2019-12-01"],
            after = value[date == max(date)]) %>%
  mutate(difference = after - before,
         months_category = "Since End of 2019",
         difference_label = round(difference),
         positive = difference_label >= 0)

top_10 <- compare_2019 %>%
  mutate(pos = difference > 0) %>%
  group_by(pos) %>%
  mutate(level = abs(difference)) %>%
  arrange(level) %>%
  top_n(10, level) %>%
  select(industry_name)
  

compare_2019 %>%
  filter(industry_name %in% top_10$industry_name) %>%
  mutate(industry_name = str_replace_all(industry_name, "and other general merchandise", "and merchandise")) %>%
  mutate(industry_name = reorder_within(industry_name, difference, months_category)) %>%
  ggplot(aes(industry_name, difference, label=difference_label, fill=positive)) +
  geom_col(size=0) +
  scale_x_reordered() +
  coord_flip() +
  theme_lass +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  theme(panel.grid.major.y = element_line(size=0)) +
  theme(plot.title.position = "plot") +
  labs(y = NULL,
       x = NULL,
       title = "Top 10 Job Growers/Losers Since 2019 of 241 Level 5 Industries, in Thousands",
       subtitle = "Total Jobs growth last month versus Dec 2019, 5th-level detail category by occupations, employment survey, seasonally adjusted",
       caption ="BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = c(0.75,0.5)) +
  geom_text(aes(y = difference_label + 40 * sign(difference_label), label = difference_label), color="white", size=6, ) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(size = 18))


ggsave("graphics/compare_2019_detail.png",  width = 15, height=15, dpi="retina")



#### Compared to 2019 ####
compare_2019 <- ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == 1) %>% group_by(industry_name) %>%
  summarize(before = value[date == "2019-12-01"],
            after = value[date == max(date)]) %>%
  mutate(difference = after - before,
         months_category = "Since End of 2019",
         difference_label = round(difference),
         positive = difference_label >= 0)

compare_2019 %>%
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
       title = "Top 10 Job Growers/Losers Since 2019 of 241 Level 5 Industries",
       subtitle = "Total Jobs growth last month versus Dec 2019, 5th-level detail category by occupations, employment survey, seasonally adjusted",
       caption ="BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = c(0.75,0.5)) +
  geom_text(aes(y = difference_label + 75 * sign(difference_label), label = difference_label), color="white", size=6, ) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 15))


ggsave("graphics/compare_2019_overall.png",  width = 12, height=12, dpi="retina")





compare_2019 <- ces_data %>% filter(seasonal == "S", (display_level == 5), data_type_code == 1) %>% group_by(industry_name) %>%
  summarize(before = value[date == "2019-12-01"],
            after = value[date == max(date)],
            ss_name = supersector_name) %>%
  mutate(difference = after - before,
         months_category = "Since End of 2019",
         difference_label = round(difference),
         positive = difference_label >= 0)

