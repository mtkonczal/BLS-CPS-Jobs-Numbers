# This
# This is a master file that runs multiple graphic creation function calls.
# Written by: Mike Konczal, Roosevelt Institute
# Last Updated: 1/5/2024

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")
library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(lubridate)
library(tidytext)

source("scripts/01_load_cps_jobs.R")
source("scripts/02_load_ces_jobs.R")
source("scripts/03_graphic_scripts.R")


total_title <- "Relax, it's a Soft Landing: Job Growth Remains Steady"
total_jobs_graphic(ces_data, total_title)
ggsave("graphics/total_jobs.png", dpi = "retina", width = 12, height = 6.75, units = "in")


detailed_compare_2019(ces_data)
ggsave("graphics/compare_2019_detail.png",  width = 15, height=15, dpi="retina")

double_jobs_chart(ces_data, "Hello there")
ggsave("graphics/double_jobs.png", dpi = "retina", width = 12, height = 8, units = "in")

black_white_comparison(cps_jobs_data)
ggsave("graphics/black_white_comparsion.png", dpi = "retina", width = 12, height = 8, units = "in")

by_type_title <- "This things"
unemployment_rate_by_type(cps_jobs_data, by_type_title)
ggsave("graphics/u_by_type.png", dpi = "retina", width = 12, height = 8, units = "in")

duration_title <- "Unemployment duration slowly adjusting"
draw_u_duration(cps_jobs_data, duration_title)
ggsave("graphics/duration.png", dpi = "retina", width = 12, height = 8, units = "in")

three_six_wages_title <- "Wage Growth, While Volatile, in Line With Lower Inflation"
three_six_wages(ces_data, three_six_wages_title)
ggsave("graphics/wages_3_6.png", dpi = "retina", width = 12, height = 8, units = "in")

make_jobs_chart(ces_data)

source("2_3_CBO_projections.R")

<<<<<<< Updated upstream
source("annual_wrapup.R")
=======



##### Notepad ####

df <- ces_data %>% filter(data_type_code == 1, seasonal == "S", display_level == 4) %>%
  group_by(industry_code) %>%
  mutate(change3 = value - lag(value,3),
         change3P = value/lag(value, 3) - 1) %>%
  ungroup() %>%
  filter(!is.na(change3)) %>%
  group_by(date) %>%
  summarize(Q25 = quantile(change3, 0.10),
            Q75 = quantile(change3, 0.90),
            median = median(change3)) %>%
  ungroup()

df %>%
  filter(year(date) >= 2015) %>%
  mutate(Q25 = if_else(year(date) %in% c(2020,2021), NA, Q25),
         Q75 = if_else(year(date) %in% c(2020,2021), NA, Q75),
         median = if_else(year(date) %in% c(2020,2021), NA, median)) %>%
  ggplot(aes(x = date)) + geom_ribbon(aes(ymin=Q25, ymax=Q75), fill="skyblue") +
  geom_line(aes(y = median), color="black") +
  theme_classic(base_size = 18) +
  labs(title = "25th, median, and 75th quantiles for 3-month total employment change in 84 distinct (display-level 4) subindustries from BLS CES.",
       caption = "Mike Konczal, Roosevelt Institute.", y="3-month change") +
  theme(plot.title.position = "plot")


ces_data %>%
  filter(data_type_code == 1, seasonal == "S", display_level == 5) %>%
  group_by(industry_code) %>%
  mutate(neg = value - lag(value, 1) < 0) %>%
  filter(!is.na(neg)) %>%
  ungroup() %>%
  filter(year(date) >= 2000, date <= max(date) %m-% months(1)) %>%
  group_by(date) %>%
  summarize(percent_neg = sum(neg) / n(),
            emp_neg = sum(value[neg==TRUE]/sum(value))) %>%
  ggplot(aes(date, percent_neg)) +
  geom_line(size=1.2) +
  labs(
    subtitle = "Percent of 241 Level-5 subindustries with negative month-over-month job growth.",
    caption = "Mike Konczal, Roosevelt Institute."
  ) +
  theme_classic(base_size = 22) +
  scale_y_continuous(label = percent) +
  theme(plot.title.position = "plot")



#dl_0_2_2023 <- ces_data %>% filter(data_type_code == 1, seasonal == "S", display_level <= 2)
#saveRDS(df, "data/2023_ces_data.rds")
dl_0_2_2023 <- readRDS("data/2023_ces_data.rds") %>% mutate(type = "original 2023 data")
#ces_data %>% filter(data_type_code == 1, seasonal == "S", display_level <= 2) %>%
dl_factor <- dl_0_2_2023 %>% select(industry_code, industry_name, display_level) %>% distinct(industry_code, .keep_all = TRUE) %>% pull(industry_name)

dl_0_2_2023 %>% mutate(value = value - year*100) %>%
  mutate(type = "revised 2023 data") %>%
  rbind(dl_0_2_2023) %>%
  group_by(type, industry_name) %>%
  mutate(diff = value - lag(value, 12)) %>%
  filter(date == "2023-12-01") %>%
  arrange(display_level, industry_code) %>%
  ungroup() %>%
  mutate(chart_type = case_when(
    industry_name %in% c("Total nonfarm", "Goods-producing", "Private service-providing", "Total private") ~ "Total",
    industry_name %in% c("Mining and logging", "Construction", "Manufacturing") ~ "Goods",
    TRUE ~ "Services"
  )) %>%
  mutate(chart_type = factor(chart_type, levels = c("Total", "Goods", "Services"))) %>%
  filter(industry_name != "Service-providing") %>%
  mutate(industry_nameF = fct_rev(factor(industry_name, levels=dl_factor))) %>%
  ggplot(aes(x = industry_nameF, y = diff, fill=type)) +
  geom_bar(position="dodge", stat="identity", linewidth=0) +
  coord_flip() +
  theme_lass +
  geom_text(aes(y = diff + 200 * sign(diff), label = diff, group = type), color="white", size=6, position = position_dodge(width = 0.9)) +
  labs(title = "Comparison means very little.", caption="Mike Konczal, Roosevelt Institute.") +
  theme(legend.position = "bottom")
>>>>>>> Stashed changes
