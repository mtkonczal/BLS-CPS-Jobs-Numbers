# This script does some data analysis on jobs.
# Written by: Mike Konczal
# Last Updated: 3-12-2022

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")
library(tidyverse)
library(ggtext)
library(ggrepel)

##### SET UP SOME THINGS #####
#source(file = "1_a_load_bls_cps_jobs_data.R")
#source(file = "1_b_load_bls_ces_jobs_data.R")
#load("data/cps_jobs_data.RData")
#load("data/ces_data.RData")

cboa <- cps_jobs_data %>% filter(seasonal == "S") %>% filter(periodicity_code == "M") %>% filter(date >= "2021-06-01") %>%
  filter(series_id %in% c("LNS11300000","LNS12000000", "LNS14000000")) %>%
  select(date, value, series_title) %>% mutate(source = "Actual")
maxDate <- max(cboa$date)

cboe <- ces_data %>% filter(series_id %in% "CES0000000001") %>% filter(date >= "2021-01-01") %>%
  select(date, value, series_title) %>% mutate(source = "Actual")
  
cbot <- rbind(cboa, cboe)
cbot$series_title <- str_replace_all(cbot$series_title, "\\(Seas\\) ", fixed(""))
cbot$series_title <- str_replace_all(cbot$series_title, "\\(Seas\\) ", fixed(""))
cbot$series_title <- str_replace_all(cbot$series_title, "Employment Level", "Employment Level (Household, in Thousands)")
cbot$series_title <- str_replace_all(cbot$series_title, "Employment \\(CES\\)", "Employment Level (Establishment, in Thousands)")
cbot$series_title <- str_replace_all(cbot$series_title, "All employees, thousands, total nonfarm, seasonally adjusted", "Employment Level (Establishment, in Thousands)")
cbot$series_title <- str_replace_all(cbot$series_title, "Civilian Labor Force Level", "Employment Level (Household, in Thousands)")


cbot <- cbot %>% mutate(display_valueAll = value*(date == maxDate))
cbot <- cbot %>% mutate(display_valueE = value*(date == maxDate)*(series_title == "Employment Level (Establishment, in Thousands)")) %>% mutate(display_valueE = floor(display_valueE))
cbot <- cbot %>% mutate(display_valueEH = value*(date == maxDate)*(series_title == "Employment Level (Household, in Thousands)")) %>% mutate(display_valueEH = floor(display_valueEH))
cbot <- cbot %>% mutate(display_valueLFP = value*(date == maxDate)*(series_title == "Labor Force Participation Rate"))
cbot <- cbot %>% mutate(display_valueU = value*(date == maxDate)*(series_title == "Unemployment Rate"))
cbot$display_valueAll <- na_if(cbot$display_valueAll, 0)
cbot$display_valueE <- na_if(cbot$display_valueE, 0)
cbot$display_valueEH <- na_if(cbot$display_valueE, 0)
cbot$display_valueLFP <- na_if(cbot$display_valueLFP, 0)
cbot$display_valueU <- na_if(cbot$display_valueU, 0)

cbot$series_title <- str_replace_all(cbot$series_title, "Unemployment Rate", "Unemployment Rate (Household)")
cbot$series_title <- str_replace_all(cbot$series_title, "Labor Force Participation Rate", "Labor Force Participation Rate (Household)")

ggplot(cbot, aes(date, value)) + geom_line(size=1.2, color="#01579B") + facet_wrap(facets = "series_title", scales = "free") +
  theme_classic() +
  theme(legend.position='none') +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_color_manual(values = c("#01579B")) +
  scale_linetype_manual(values = c("solid")) +
  labs(title="After Months of Flatlining, a Welcome Jump in Household Survey Data",
       subtitle="Unemployment rose, but so did Employment and Participation Ratios, a positive sign.",
       caption="Household and establishment refer to the respective surveys. Seasonally adjusted. Author's calculations. Mike Konczal, Roosevelt Institute",
       x="", y="") +
  theme(strip.text = element_text(face = "bold", color = "black", hjust = 0.5, size = 21),
        plot.title = element_text(size = 25, face="bold"), plot.subtitle = element_text(size=20, margin=margin(9,0,15,0),lineheight=1.05),
        plot.caption = element_text(size=14, margin=margin(19,0,11,0), lineheight=1.05),
        plot.title.position = "plot") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()) +
  theme(axis.text=element_text(size=11), legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(), legend.text = element_text(size=15, color="#222222"), panel.background = element_blank()) +
  geom_point(aes(x=date, y=display_valueAll), size=3) +
  geom_label_repel(aes(x=date, y=display_valueAll, color="#01579B", label=round(display_valueAll,1)), size=5, box.padding = unit(0.2,"in"))

ggsave("graphics/survey_debate.png", width = 19, height=10.68, dpi="retina")


cbot %>% filter(series_title %in% c("Employment Level (Establishment, in Thousands)", "Employment Level (Household, in Thousands)")) %>%
  filter(date >= "2021-09-01") %>%
  group_by(series_title) %>%
  mutate(value = value/value[date=="2022-01-01"]) %>%
  ungroup() %>%
ggplot(aes(date, value)) + geom_line(size=1.2, color="#01579B") + facet_wrap(facets = "series_title", scales = "free") +
  theme_classic() +
  theme(legend.position='none') +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 months") +
  scale_color_manual(values = c("#01579B")) +
  scale_linetype_manual(values = c("solid")) +
  labs(title="The Two Surveys Are Telling Two Different Stories",
       subtitle="The generally more accurate establishment survey is more optmistic than the household survey, going on several months now",
       caption="Household and establishment refer to the respective surveys. Seasonally adjusted.\nDotted red line is CBO quarterly 10-Year Economic Projections, February 2021.\nAuthor's calculations. Mike Konczal, Roosevelt Institute",
       x="", y="") +
  theme(strip.text = element_text(face = "bold", color = "black", hjust = 0.5, size = 21),
        plot.title = element_text(size = 25, face="bold"), plot.subtitle = element_text(size=20, margin=margin(9,0,15,0),lineheight=1.05),
        plot.caption = element_text(size=14, margin=margin(19,0,11,0), lineheight=1.05),
        plot.title.position = "plot") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()) +
  theme(axis.text=element_text(size=11), legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(), legend.text = element_text(size=15, color="#222222"), panel.background = element_blank()) #+
  #geom_point(aes(x=date, y=display_valueAll), size=3) +
  #geom_label_repel(aes(x=date, y=display_valueAll, color="#01579B", label=round(display_valueAll,1)), size=5, box.padding = unit(0.2,"in"))

ggsave("graphics/survey_debate2.png", width = 19, height=10.68, dpi="retina")