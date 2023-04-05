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

## Test image:
# Sectoral differences

## ADD LOG LINEAR TRENDLINE!

# Sectoral differences
ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == "01") %>% group_by(industry_name) %>%
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
ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == "01") %>% group_by(industry_name) %>%
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

one_month <- ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == "01") %>% group_by(industry_name) %>%
  summarize(before = value[date == max(date) %m-% months(1)],
            after = value[date == max(date)]) %>%
  mutate(difference = after - before, months_category = "Last Month")


double_jobs_graphic <- ces_data %>% filter(seasonal == "S", (display_level == 2), data_type_code == "01") %>% group_by(industry_name) %>%
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
       title = "Last Month Versus Last Six Months TKTKTK",
       subtitle = "Need to TEST TK.",
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
