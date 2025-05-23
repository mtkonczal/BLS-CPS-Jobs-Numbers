# This script does some data analysis on jobs.
# Written by: Mike Konczal
# Last Updated: 3-12-2022

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")
library(tidyverse)
library(ggtext)
library(ggrepel)
library(viridis)

cbo_data <- read_csv("data/cbo_projections_new.csv")
cbo_data$period <- substr(cbo_data$date, nchar(cbo_data$date) - 1, nchar(cbo_data$date))
cbo_data$year <- substr(cbo_data$date, 1, 4)

cbo_data <- cbo_data %>%
  mutate(month = case_when(
    period == "Q1" ~ 3,
    period == "Q2" ~ 6,
    period == "Q3" ~ 9,
    period == "Q4" ~ 12)) %>%
  mutate(value = if_else(type=="Employment Level (CES)", value*1000, value))

cbo_data$date <- paste(cbo_data$month, "01", cbo_data$year, sep="/")
cbo_data$date <- as.Date(cbo_data$date, "%m/%d/%Y")

cbo_data$type <- str_replace_all(cbo_data$type, "Labor Force Participation Rate, 16 Years or Older", "Labor Force Participation Rate")
cbo_data$type <- str_replace_all(cbo_data$type, "Employment, Total Nonfarm (Establishment Survey)", "Employment Level (CES)")

cbo_data <- cbo_data %>% select(date, value, source = projection_date, type) %>% mutate(lineversion = "second")

cboa <- cps_jobs_data %>% filter(seasonal == "S") %>% filter(periodicity_code == "M") %>% filter(date >= "2020-01-01") %>%
  filter(series_id %in% c("LNS11300000")) %>%
  select(date, value) %>% mutate(source = "Actual", type = "Labor Force Participation Rate", lineversion = "first")

maxDate <- max(cboa$date)

cboe <- ces_data %>% filter(series_id %in% "CES0000000001") %>% filter(date >= "2020-01-01") %>%
  select(date, value) %>% mutate(source = "Actual", type = "Employment Level (CES)", lineversion = "first")
  
cbot <- rbind(cboa, cboe, cbo_data)

actual_cbot <- cbot %>% filter(source == "Actual")

ces_dates <- unique(cboe$date)
ces_dates <- sort(ces_dates, decreasing = TRUE)
ces_dates <- ces_dates[seq(1, length(ces_dates), 6)]

border_color <- "#424242" 

cbot %>%
  filter(date > "2020-01-01" & date <= "2024-01-01") %>%
ggplot(aes(date, value, color=source, linetype=source)) + geom_line(size=1.5) + facet_wrap(~type, scales = "free") +
  geom_line(data = actual_cbot, aes(date,value), color=border_color, size=2) +
  geom_line(data = actual_cbot, aes(date,value), size=1.2) +
  theme_lass +
  theme(legend.position=c(0.35,0.55)) +
  scale_x_date(date_labels = "%b\n%Y", breaks = ces_dates) +
  scale_color_manual(values=c("#4F97D7", "#FF7F0E", "#2CA02C")) +
  scale_linetype_manual(values = c("solid","dotted","dotted")) +
  labs(title="The Labor Market is Not Only Beating Pre-American Rescue Plan Projections, It's Beating Pre-Covid Ones Too",
       subtitle="Solid line is actual, dotted lines are CBO projections by date.",
       caption="BLS, CBO. Dotted lines are CBO quarterly 10-Year Economic Projections, Jan 2020 and Feb 2021. Author's calculations. Mike Konczal, Roosevelt Institute",
       x=NA, y=NA) +
  theme(axis.text=element_text(size=13), legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(), legend.text = element_text(size=22, color="white"), panel.background = element_blank(),
        strip.text = element_text(face = "bold", color="white", hjust = 0.5, size = 22))

ggsave("graphics/cbo_projects.png", width = 19, height=10.68, dpi="retina")

