# This script does some data analysis on jobs.
# Written by: Mike Konczal
# Last Updated: 3-12-2022

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")
library(tidyverse)
library(ggtext)

##### SET UP SOME THINGS #####
#source(file = "1_load_cps_jobs_data.R")
#load("data/cps_jobs_data.RData")

##### FIRST GRAPHIC: lfp BY GROUPS #####

lfp <- cps_jobs_data %>% filter(seasonal == "S") %>% filter(periodicity_code == "M") %>% filter(grepl("Labor Force Participation", series_title))

MonthsToLag <- 12

lfp <- lfp %>%
  group_by(series_id) %>%  arrange(date) %>%
  mutate(val_org = value[date=="2020-01-01"]) %>%
  mutate(value_lagm = lag(value, MonthsToLag) / val_org) %>%
  mutate(value_1m = value / val_org) %>%
  mutate(value_diff = value_1m - value_lagm) %>%
  mutate(lagged_date_lagm = lag(date, MonthsToLag)) %>%
  filter(date == max(date)) %>% ungroup()

# WRITE OUT THE SERIES ID WE WANT
graphic_series <- c("LNS11300000","LNS11300001","LNS11300002","LNS11300003","LNS11300006","LNS11300009","LNS11300024","LNS11300036","LNS11300048","LNS11300060",
                    "LNS11300089","LNS11300091","LNS11300093","LNS11324887","LNS11327659","LNS11327660","LNS11327662","LNS11327689","LNS11300004",
                    "LNS11300005","LNS11300007","LNS11324230", "LNS11300008")

lfp <- lfp %>% filter(series_id %in% graphic_series)

lfp$series_title <- str_replace_all(lfp$series_title, "\\(Seas\\) Labor Force Participation Rate", "All")
lfp$series_title <- str_replace_all(lfp$series_title, "All - ", fixed(""))
lfp$series_title <- str_replace_all(lfp$series_title, " or African American", fixed(""))
lfp$series_title <- str_replace_all(lfp$series_title, ". & over", ("+"))
lfp$series_title <- str_replace_all(lfp$series_title, "Less than a High School", ("No HS"))
lfp$series_title <- str_replace_all(lfp$series_title, "High School Graduates, No College", ("HS, No College"))
lfp$series_title <- str_replace_all(lfp$series_title, "Bachelor's degree and higher", ("Bachelor and higher"))
lfp$series_title <- str_replace_all(lfp$series_title, "Some College or Associate Degree", ("Some College or Associate"))

Older <- lfp %>% filter(series_id == "LNS11324230") %>% mutate(series_title = "55 yrs+")

lfp <- lfp %>%
  filter(!series_id == "LNS11324230") %>% rbind(Older) %>%
  arrange(desc(series_id)) %>% mutate(g2_series_title=factor(series_title, series_title)) %>%
  mutate(display_value = value_1m*(value_1m > value_lagm))

lfp$display_value <- na_if(lfp$display_value, 0)

MaxMonth <- format(lfp$date[1], "%B")
MaxYear <- format(lfp$date[1], "%Y")

LaggedMonth <- format(lfp$lagged_date_lagm[1], "%B")
LaggedYear <- format(lfp$lagged_date_lagm[1], "%Y")

Full_Title <- paste("Labor Force Participation Rates as a Proportion of Pre-Pandemic Level, <span style = 'color:#bc5090;'>", LaggedMonth, " ", LaggedYear, "</span> and <span style = 'color:#01579B;'>", MaxMonth, " ", MaxYear, "</span>", sep="")

ggplot(lfp) +
  geom_segment( aes(x=g2_series_title, xend=g2_series_title, y=value_1m, yend=value_lagm), color="grey") +
  geom_point( aes(x=g2_series_title, y=value_1m), color="#01579B", size=3 ) +
  geom_point( aes(x=g2_series_title, y=value_lagm), color="#bc5090", size=3 ) +
  coord_flip()+
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_markdown(size = 35, hjust = 0.5),
        plot.subtitle = element_markdown(size = 28, margin=margin(9,0,15,0))) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.title.position = "plot",
    axis.text = element_text(size = 12),
    axis.ticks.y = element_blank()) +
  labs(x="",y="", subtitle=Full_Title, title="Labor Force Participation is Increasing Over the Past Year Across All Categories") +
  geom_hline(yintercept=1, color = "grey", alpha=0.4, linetype="dashed") +
  labs(caption = "Pre-pandemic level is the category's LFP ratio in January 2020.
       The category equals 1.0 when it returns to its January 2020 rate.
       Data: BLS, CPS. Seasonally adjusted. Author's calculations. Mike Konczal, Roosevelt Institute") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(), panel.background = element_blank()) +
  theme(panel.background = element_blank(), axis.line.y = element_blank()) +
  geom_text(aes(x=g2_series_title, y=display_value, label=(round(display_value, 2))), nudge_y = 0.004, nudge_x = 0.05, color = "#01579B", check_overlap = TRUE) +
  geom_text(aes(x=g2_series_title, y=value_lagm, label=(round(value_lagm, 2))), nudge_y = -0.004, nudge_x = 0.05, color = "#bc5090", check_overlap = TRUE) +
  theme(plot.caption = element_text(size=14, margin=margin(19,0,11,0)))
ggsave("graphics/LFP_race.png", width = 19, height=10.68, dpi="retina")

