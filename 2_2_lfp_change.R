# This script does some data analysis on jobs.
# Written by: Mike Konczal
# Last Updated: 3-12-2022

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")
library(tidyverse)
library(ggtext)

##### SET UP SOME THINGS #####
#source(file = "1_load_cps_jobs_data.R")
#load("data/cps_jobs_data.RData")

##### FIRST GRAPHIC: EPOP BY GROUPS #####

lfp <- cps_jobs_data %>% filter(seasonal == "S") %>% filter(periodicity_code == "M") %>% filter(grepl("Labor Force Participation", series_title))


### EDIT FROM HERE BUT LET'S CLEAN UP THE FIRST FIRST ###
epop <- epop %>%
  group_by(series_id) %>%  arrange(date) %>%
  mutate(val_org = value[date=="2020-01-01"]) %>%
  mutate(value_3m = lag(value, 2) / val_org) %>%
  mutate(value_1m = value / val_org) %>%
  mutate(value_diff = value_1m - value_3m) %>%
  mutate(lagged_date_3m = lag(date, 2)) %>%
  filter(date == max(date)) %>% ungroup()

# WRITE OUT THE SERIES ID WE WANT
graphic_series <- c("LNS12300000","LNS12300001","LNS12300002","LNS12300003","LNS12300006","LNS12300009","LNS12300024","LNS12300036","LNS12300048","LNS12300060",
                    "LNS12300089","LNS12300091","LNS12300093","LNS12324887","LNS12327659","LNS12327660","LNS12327662","LNS12327689","LNS12300004",
                    "LNS12300005","LNS12300007","LNS12300008","LNS12324230")

epop <- epop %>% filter(series_id %in% graphic_series)

epop$series_title <- str_replace_all(epop$series_title, "\\(Seas\\) Employment-Population Ratio", "All")
epop$series_title <- str_replace_all(epop$series_title, "All - ", fixed(""))
epop$series_title <- str_replace_all(epop$series_title, " or African American", fixed(""))
epop$series_title <- str_replace_all(epop$series_title, ". & over", ("+"))
epop$series_title <- str_replace_all(epop$series_title, "Less than a High School", ("No HS"))
epop$series_title <- str_replace_all(epop$series_title, "High School Graduates, No College", ("HS, No College"))
epop$series_title <- str_replace_all(epop$series_title, "Bachelor's degree and higher", ("Bachelor and higher"))
epop$series_title <- str_replace_all(epop$series_title, "Some College or Associate Degree", ("Some College or Associate"))

Older <- epop %>% filter(series_id == "LNS12324230") %>% mutate(series_title = "55 yrs+")

epop <- epop %>%
  filter(!series_id == "LNS12324230") %>% rbind(Older) %>%
  arrange(desc(series_id)) %>% mutate(g2_series_title=factor(series_title, series_title))

MaxMonth <- format(epop$date[1], "%B")
MaxYear <- format(epop$date[1], "%Y")

LaggedMonth <- format(epop$lagged_date_3m[1], "%B")
LaggedYear <- format(epop$lagged_date_3m[1], "%Y")

Full_Title <- paste(" Employment-Population Ratio as a Proportion of Pre-Pandemic Level, <span style = 'color:#03A9F4;'>", LaggedMonth, " ", LaggedYear, "</span> and <span style = 'color:#01579B;'>", MaxMonth, " ", MaxYear, "</span>", sep="")

ggplot(epop) +
  geom_segment( aes(x=g2_series_title, xend=g2_series_title, y=value_1m, yend=value_3m), color="grey") +
  geom_point( aes(x=g2_series_title, y=value_1m), color="#01579B", size=3 ) +
  geom_point( aes(x=g2_series_title, y=value_3m), color="#03A9F4", size=3 ) +
  coord_flip()+
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_markdown()) +
  ggtitle(Full_Title) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    plot.title.position = "plot",
    axis.text = element_text(size = 12),
    axis.ticks.y = element_blank()) +
  xlab("") +
  ylab("") +
  geom_hline(yintercept=1, linetype="solid", color = "black", alpha=0.5) +
  labs(caption = "Data: BLS, CPS, Seasonally-Adjusted data; Author's Calculations. Mike Konczal, Roosevelt Institute")
ggsave("graphics/EPOP_race.png", dpi="retina")


epop$g2_series_title