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

### Sectoral differences #####
ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == 1) %>% group_by(industry_name) %>%
  summarize(before = value[date == "2019-12-01"] + value[date == "2020-01-01"] + value[date == "2020-01-01"],
            after = value[date == max(date)] + value[date == max(date) %m-% months(1)] + value[date == max(date) %m-% months(2)]) %>%
  mutate(before = before/3, after = after/3, difference = after - before) %>% arrange(desc(difference)) %>%
  mutate(industry_name = reorder_within(industry_name, difference, industry_name))  %>%
  mutate(difference_label = round(difference)) %>%
  mutate(positive = difference_label >= 0) %>%
  
  ggplot(aes(industry_name, difference, label=difference_label, color=positive)) +
  geom_point(size=10.5) +
  geom_segment(aes(x=industry_name, 
                   xend=industry_name, 
                   y=0, 
                   yend=difference)) +
  coord_flip() +
  scale_x_reordered() +
  theme_lass +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  theme(panel.grid.major.y = element_line(size=0)) +
  theme(plot.title.position = "plot") +
  labs(y = NULL,
       x = NULL,
       title = "Testing",
       subtitle = "More Testing NO TREND.",
       caption ="BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = c(0.75,0.5)) +
  geom_text(color="white") +
  scale_y_continuous(position = "right") +
  theme(legend.position = "none")

ggsave("graphics/sectors_employment_differences_pre_pandemic.png",  width = 12, height=6.75, dpi="retina")



#Sectoral - Last Three Months
months_difference <- 3
ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == 1) %>% group_by(industry_name) %>%
  summarize(before = value[date == max(date) %m-% months(months_difference)],
            after = value[date == max(date)]) %>%
  mutate(difference = after - before) %>% arrange(desc(difference)) %>%
  mutate(industry_name = reorder_within(industry_name, difference, industry_name))  %>%
  mutate(difference_label = round(difference)) %>%
  mutate(positive = difference_label >= 0) %>%
  
  ggplot(aes(industry_name, difference, label=difference_label, color=positive)) +
  geom_point(size=10.5) +
  geom_segment(aes(x=industry_name, 
                   xend=industry_name, 
                   y=0, 
                   yend=difference)) +
  coord_flip() +
  scale_x_reordered() +
  theme_lass +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  theme(panel.grid.major.y = element_line(size=0)) +
  theme(plot.title.position = "plot") +
  labs(y = NULL,
       x = NULL,
       title = "Last Three Months",
       subtitle = "More Testing NO TREND.",
       caption ="BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = c(0.75,0.5)) +
  geom_text(color="white") +
  scale_y_continuous(position = "right") +
  theme(legend.position = "none")

ggsave("graphics/sectors_employment_last_three_month.png",  width = 12, height=6.75, dpi="retina")

# Double Graphic
months_difference <- 6

one_month <- ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == 1) %>% group_by(industry_name) %>%
  summarize(before = value[date == max(date) %m-% months(1)],
            after = value[date == max(date)]) %>%
  mutate(difference = after - before, months_category = "Last Month")


double_jobs_graphic <- ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == 1) %>% group_by(industry_name) %>%
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
       title = six_month_jobs_occupations_title,
       subtitle = "Job growth last month versus average of the last six months, by occupations, employment survey, seasonally adjusted",
       caption ="BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = c(0.75,0.5)) +
  geom_text(aes(y = difference_label + 4 * sign(difference_label), label = difference_label), color="white", size=6, ) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "none") +
  theme(strip.text.x = element_text(size = 15))


ggsave("graphics/6_month_sectors_employment_differences_bars.png",  width = 12, height=8, dpi="retina")



# Double Graphic
months_difference <- 6

one_month <- ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == 1) %>% group_by(industry_name) %>%
  summarize(before = value[date == max(date) %m-% months(1)],
            after = value[date == max(date)]) %>%
  mutate(difference = after - before, months_category = "Last 1 Month")

three_months <- ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == 1) %>% group_by(industry_name) %>%
  summarize(before = value[date == max(date) %m-% months(3)],
            after = value[date == max(date)]) %>%
  mutate(difference = after - before, difference = difference/3) %>% arrange(desc(difference)) %>%
  mutate(months_category = "Last 3 Month (Monthly Average)")

one_month_total <- sum(one_month$difference)
one_month$months_category <- paste("Last 1 Month, ", one_month_total, " thousand jobs", sep="")

three_months_total <- sum(three_months$difference)
three_months$months_category <- paste("Last 3 Months, Average Monthly, ", round(three_months_total), " thousand jobs", sep="")


double_jobs_graphic <- ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == 1) %>% group_by(industry_name) %>%
  summarize(before = value[date == max(date) %m-% months(months_difference)],
            after = value[date == max(date)]) %>%
  mutate(difference = after - before, difference = difference/months_difference) %>% arrange(desc(difference))

six_months_total <- sum(double_jobs_graphic$difference)
double_jobs_graphic$months_category <- paste("Last 6 Months, Average Monthly, ", round(six_months_total), " thousand jobs", sep="")

double_jobs_graphic <- double_jobs_graphic %>% rbind(one_month, three_months) %>%
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



# Helper Graphic
double_jobs_graphic %>%
  group_by(industry_name) %>%
  summarize(slowdown = difference[months_category == "Last Month"] - difference[months_category == "Last Six Months (Monthly Average)"]) %>%
  ggplot(aes(reorder(industry_name, slowdown), slowdown)) + geom_col() + theme_classic() + coord_flip() +
  labs(title="Not for circulation", subtitle="A positive value means last month was higher than previous six months by that amount.",
       x="",y="") +
  theme(plot.title.position = "plot")

ggsave("graphics/sectors_employment_differences_summary.png",  width = 12, height=6.75, dpi="retina")
