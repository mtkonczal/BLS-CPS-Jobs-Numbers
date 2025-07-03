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

##### SET UP SOME THINGS #####
#source(file = "1_a_load_bls_cps_jobs_data.R")
#source(file = "1_b_load_bls_ces_jobs_data.R")
#load("data/cps_jobs_data.RData")
#load("data/ces_data.RData")

U <- cps_jobs_data %>% filter(series_title=="(Seas) Unemployment Rate") %>%
  filter(periodicity_code == "M") %>%
  select(date, unrate = value)

current_U <- U %>% filter(date == max(date)) %>% select(unrate)
current_U <- as.numeric(current_U)

JG <- ces_data %>% filter(series_title=="All employees, thousands, total nonfarm, seasonally adjusted") %>%
  select(date, series_title, year, employment = value) %>% mutate(job_growth = employment - lag(employment,1)) %>%
  mutate(job_growth_rate = job_growth/lag(employment,1)) %>% mutate(current = (year > 2020)) %>%
  left_join(U, by="date") %>% mutate(current2 = unrate <= current_U) 

current_ER <- JG %>% filter(date == max(date)) %>% select(job_growth_rate)
current_ER <- as.numeric(current_ER)

JG <- JG %>% mutate(display_valueAll = job_growth_rate*(date == max(date)))
JG$display_valueAll <- na_if(JG$display_valueAll, 0)

maxdate <- max(JG$date)
maxdate <- as.character(format(maxdate, format="%B %Y"))
label_text <- paste("Unemployment at or Below ", current_U, "%, ", maxdate, " Value", sep="")
JG <- JG %>% mutate(current3 = ifelse(current2 == TRUE, as.character(label_text), "Unemployment Below 4%"))
subtitleNumbers <- paste("Monthly CES employment growth as a percent of total employment, only low unemployment months; Line equals ", maxdate, "'s ", round(current_ER*100,2), " percent value", sep="")

JG %>% filter(unrate < 4) %>% ggplot(aes(date, job_growth_rate, color=current3)) + geom_point(size=2) + theme_classic() +
  geom_hline(yintercept = current_ER, linetype="longdash", color="#01579B") +
  theme(legend.position='bottom', legend.title = element_blank()) +
  scale_color_manual(values = c("#01579B", "#bc5090")) +
  labs(title="Job Growth Remains Strong Compared to Other, More Volatile, Periods of Low Unemployment",
       subtitle=subtitleNumbers,
       caption="CES Employment, CPS Unemployment, Author's Calculations. Mike Konczal, Roosevelt Institute",
       x="", y="Job Growth as a Percent of Total Jobs") +
  theme(plot.title = element_text(size = 30, face="bold"), plot.subtitle = element_text(size=20, margin=margin(9,0,15,0)),
        plot.caption = element_text(size=12),
        plot.title.position = "plot") +
  theme(axis.text=element_text(size=20), legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(), legend.text = element_text(size=15, color="#222222"), panel.background = element_blank()) +
  scale_y_continuous(labels = percent)

ggsave("graphics/JG_lowU.png", width = 19, height=10.68, dpi="retina")

percent_higher <- JG %>% filter(date > "1980-01-01") %>%
  filter(date < "2020-04-01" | date > "2020-07-01") %>%
  select(date, job_growth_rate) %>% mutate(higherQ = job_growth_rate < current_ER) %>%
  summarize(sum(higherQ)/n())
percent_higher <- 100*round(as.numeric(percent_higher),2)
percent_higher <- paste(percent_higher, " Percent", sep="")

# Not used, here for reference sake
percent_positive <- JG %>% filter(date > "1980-01-01") %>%
  filter(date < "2020-04-01" | date > "2020-07-01") %>% filter(job_growth_rate > 0) %>%
  select(date, job_growth_rate) %>% mutate(higherQ = job_growth_rate < current_ER) %>%
  summarize(sum(higherQ)/n())
percent_positive <- 100*round(as.numeric(percent_positive),2)
percent_positive <- paste(percent_positive, " percent", sep="")

title2 <- paste("A Remarkable Month, ", maxdate, " Job Growth is Higher Than ", percent_higher, " Of Months Since 1980", sep="")
subtitleNumbers2 <- paste("Monthly CES employment growth as a percent of total employment; Line equals ", maxdate, "'s ", round(current_ER*100,2), " percent value", sep="")

JG %>% filter(date > "1980-01-01") %>%
  filter(date < "2020-04-01" | date > "2020-07-01") %>%
  ggplot(aes(date, job_growth_rate, color="#01579B")) + geom_point(size=2) + theme_classic() +
  geom_hline(yintercept = current_ER, size=2, linetype="longdash", color="darkred") +
  theme(legend.position='none', legend.title = element_blank()) +
  scale_color_manual(values = c("#01579B", "#bc5090")) +
  labs(title=title2,
       subtitle=subtitleNumbers2,
       caption=paste("April to July 2020 Excluded as graphical outliers. The value is ", percent_positive, " if only positive job growth months are included. CES Employment, Author's Calculations. Mike Konczal, Roosevelt Institute", sep=""),
       x="", y="Job Growth as a Percent of Total Jobs") +
  theme(plot.title = element_text(size = 30, face="bold"), plot.subtitle = element_text(size=20, margin=margin(9,0,15,0)),
        plot.caption = element_text(size=12),
        plot.title.position = "plot") +
  theme(axis.text=element_text(size=20), legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(), legend.text = element_text(size=15, color="#222222"), panel.background = element_blank()) +
  scale_y_continuous(labels = percent) +
  geom_point(aes(date,display_valueAll), shape = 21, color="black", fill="darkred", size=5)

ggsave("graphics/JG_1980.png", width = 19, height=10.68, dpi="retina")