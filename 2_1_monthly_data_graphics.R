# A script that runs all the other scripts that
# read in the monthly BLS jobs numbers, and
# then do some analysis and create several graphics.
#
# Written by: Mike Konczal
# Last Updated: 10-6-22

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")
library(hrbrthemes)
library(tidyverse)
library(ggtext)
library(scales)
library(lubridate)
library(httr)
library(data.table)
library(magrittr)


theme_lass <-   theme_modern_rc() + theme(legend.position = "none", legend.title = element_blank(),
                                          panel.grid.major.y = element_line(size=0.5),
                                          panel.grid.minor.y = element_blank(),
                                          plot.title.position = "plot",
                                          axis.title.x = element_blank(),
                                          axis.title.y = element_blank(),
                                          plot.title = element_text(size = 25, face="bold"),
                                          plot.subtitle = element_text(size=15, color="white"),
                                          plot.caption = element_text(size=10, face="italic"),
                                          legend.text = element_text(size=12),
                                          axis.text.y = element_text(size=12, face="bold"),
                                          axis.text.x = element_text(size=12, face="bold"),
                                          strip.text = element_text(face = "bold", color="white", hjust = 0.5, size = 10),
                                          panel.grid.major.x = element_blank(),
                                          panel.grid.minor.x = element_blank(),
                                          strip.background = element_blank()) +
  theme(text = element_text(family = "Larsseit"),
        plot.title = element_text(family = "Larsseit"),
        plot.subtitle = element_text(family = "Larsseit"),
        plot.caption = element_text(family="Larsseit"),
        strip.text = element_text(family="Larsseit"))

#### Set Title ####
graphic_1_total_jobs <- "Strong Number, But Total Job Growth Revised Down in Recent Months"



#### The following two scripts read the data ####

ces_dates <- ces_data %>% filter(!is.na(date)) %>% filter(date >= "2021-01-01")
ces_dates <- unique(ces_dates$date)
ces_dates <- sort(ces_dates, decreasing = TRUE)
ces_dates = ces_dates[seq(1, length(ces_dates), 3)]

#### Graphic 1 - Monthly Jobs, to Start ####


jobs_growth <- ces_data %>% filter(series_id == "CES0000000001") %>% arrange(date) %>%
  mutate(jobs_difference = value - lag(value,1)) %>%
  mutate(jobs_difference_label = if_else(date > max(date, na.rm=TRUE) %m-% months(12), jobs_difference, as.numeric(NA))) %>%
  mutate(before_trend = (value[date == "2020-01-01"]-value[date == "2018-01-01"] ) /24) %>%
  filter(date >= "2021-01-01")

avg_job_growth <- mean(jobs_growth$before_trend)

jobs_growth %>%
  ggplot(aes(date, jobs_difference, label=jobs_difference_label)) + geom_bar(stat="identity") +
  geom_text(nudge_y = 30) + theme_lass + geom_line(aes(date, before_trend), color="red", linetype="dashed", size=1.2) +
  labs(y = NULL,
       x = NULL,
       title = graphic_1_total_jobs,
       subtitle = paste("Total job growth, employment survey, seasonally-adjusted. Dotted line reflects 2018-19 average growth of ", avg_job_growth, " thousand.", sep=""),
       caption ="BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal") +
  scale_x_date(date_labels = "%b\n%Y", breaks=ces_dates)

ggsave("graphics/total_jobs.png", dpi="retina", width = 12, height=6.75, units = "in")



b_w_breaks <- cps_jobs_data %>% filter(series_id == "LNS14000006")
b_w_breaks <- unique(b_w_breaks$date)
b_w_breaks <- sort(b_w_breaks, decreasing = TRUE)
b_w_breaks = b_w_breaks[seq(1, length(b_w_breaks), 36)]

#### Graphic Black-White Unemployment ####
cps_jobs_data %>% filter(series_id %in% c("LNS14000006","LNS14000003")) %>%
  group_by(date) %>% summarize(black_u = value[series_id== "LNS14000006"],
                               relative_diff = value[series_id == "LNS14000006"]/value[series_id == "LNS14000003"],
                               absolute_diff = value[series_id == "LNS14000006"] - value[series_id == "LNS14000003"]) %>%
  pivot_longer(black_u:absolute_diff, names_to = "type", values_to = "values") %>%
  ungroup() %>%
  group_by(type) %>%
  mutate(dotted_line = values[date == max(date)]) %>%
  mutate(type = str_replace_all(type, "black_u","Black Unemployment"),
         type = str_replace_all(type, "absolute_diff","Black Unemployment Minus White Unemployment"),
         type = str_replace_all(type, "relative_diff","Black Unemployment Divided By White Unemployment"),
         ) %>%
  ungroup() %>%
  mutate(type_F = factor(type, levels = c("Black Unemployment","Black Unemployment Minus White Unemployment", "Black Unemployment Divided By White Unemployment"))) %>%
  mutate(values = values/100, dotted_line = dotted_line/100) %>%
  filter(year(date)>2001) %>%
  ggplot(aes(date, values, color=type_F)) + geom_line() +
  geom_line(aes(date,dotted_line, color=type_F), linetype="dashed") +
  theme_lass +
  theme(legend.position = c(0.25,0.85)) +
  labs(title="Black-White Unemployment Gaps Among Lowest Levels Across Measures", caption="Dotted Line is Last Value. BLS, CPS, Seasonally-Adujusted. Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%B\n%Y", breaks = b_w_breaks)

ggsave("graphics/black_white_U.png", dpi="retina", width = 12, height=6.75, units = "in")