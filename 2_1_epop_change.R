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

MonthsToLag <- 12
epop <- cps_jobs_data %>% filter(seasonal == "S") %>% filter(periodicity_code == "M") %>% filter(grepl("Employment-Population Ratio", series_title))

epop <- epop %>%
  group_by(series_id) %>%  arrange(date) %>%
  mutate(val_org1 = value[date=="2020-01-01"]) %>%
  mutate(val_org2 = value[date=="2019-12-01"]) %>%
  mutate(val_org3 = value[date=="2019-11-01"]) %>%
  mutate(val_org = (val_org1+val_org2+val_org3)/3) %>%
  mutate(value_lagm = lag(value, MonthsToLag) / val_org) %>%
  mutate(value_1m = value / val_org) %>%
  mutate(value_diff = value_1m - value_lagm) %>%
  mutate(lagged_date_lagm = lag(date, MonthsToLag)) %>%
  filter(date == max(date)) %>% ungroup()

# WRITE OUT THE SERIES ID WE WANT
graphic_series <- c("LNS12300000","LNS12300001","LNS12300002","LNS12300003","LNS12300006","LNS12300009","LNS12300024","LNS12300036","LNS12300048","LNS12300060",
                    "LNS12300089","LNS12300091","LNS12300093","LNS12324887","LNS12327659","LNS12327660","LNS12327662","LNS12327689","LNS12300004",
                    "LNS12300005","LNS12300007","LNS12324230", "LNS12300008")

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
  arrange(desc(series_id)) %>% mutate(g2_series_title=factor(series_title, series_title)) %>%
  mutate(display_value = value_1m*(value_1m > value_lagm))

epop$display_value <- na_if(epop$display_value, 0)

MaxMonth <- format(epop$date[1], "%B")
MaxYear <- format(epop$date[1], "%Y")

LaggedMonth <- format(epop$lagged_date_lagm[1], "%B")
LaggedYear <- format(epop$lagged_date_lagm[1], "%Y")

Full_Title <- paste("Employment-Population Ratio as a Proportion of Pre-Pandemic Rate, <span style = 'color:#bc5090;'>", LaggedMonth, " ", LaggedYear, "</span> and <span style = 'color:#01579B;'>", MaxMonth, " ", MaxYear, "</span>", sep="")

ggplot(epop) +
  geom_segment( aes(x=g2_series_title, xend=g2_series_title, y=value_1m, yend=value_lagm), color="grey") +
  geom_point( aes(x=g2_series_title, y=value_1m), color="#01579B", size=5 ) +
  geom_point( aes(x=g2_series_title, y=value_lagm), color="#bc5090", size=5 ) +
  coord_flip()+
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_markdown(size = 35, hjust = 0.5),
        plot.subtitle = element_markdown(size = 28, margin=margin(9,0,15,0))) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.title.position = "plot",
    axis.text = element_text(size = 20),
    axis.ticks.y = element_blank()) +
  labs(x="",y="", subtitle=Full_Title, title="Employment Increasing Across All Categories, Approaching Pre-Pandemic Rate") +
  geom_hline(yintercept=1, color = "grey", alpha=0.4, linetype="dashed") +
  labs(caption = "The category equals 1.0 when it returns to its pre-pandemic rate.
       Pre-pandemic rate is defined as the average of the category's monthly EPOP ratio from Nov 19 to Jan 20.
       Data: BLS, CPS. Seasonally adjusted. Author's calculations. Mike Konczal, Roosevelt Institute") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(), panel.background = element_blank()) +
  theme(panel.background = element_blank(), axis.line.y = element_blank()) +
  geom_text(aes(x=g2_series_title, y=display_value, label=(round(display_value, 2))), nudge_y = 0.005, nudge_x = 0.05, color = "#01579B", check_overlap = TRUE, size=6) +
  geom_text(aes(x=g2_series_title, y=value_lagm, label=(round(value_lagm, 2))), nudge_y = -0.005, nudge_x = 0.05, color = "#bc5090", check_overlap = TRUE, size=6) +
  theme(plot.caption = element_text(size=14, lineheight=1.05, margin=margin(19,0,11,0)))
ggsave("graphics/EPOP_race.png", width = 19, height=10.68, dpi="retina")
