# This script does some data analysis on jobs.
# Written by: Mike Konczal
# Last Updated: 3-12-2022

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")
library(tidyverse)
library(ggtext)

##### SET UP SOME THINGS #####
#source(file = "1_load_cps_jobs_data.R")
#load("data/cps_jobs_data.RData")

cbop <- read_csv("data/CBO_prepandemic_labor_projections.csv") %>% filter(series_title != "Employment (CES)") %>%
  filter(date <= "2022-05-01")

cboa <- cps_jobs_data %>% filter(seasonal == "S") %>% filter(periodicity_code == "M") %>% filter(date >= "2021-01-01") %>%
  filter(series_id %in% c("LNS11300000","LNS12000000","LNS14000000")) %>%
  select(date, value, series_title) %>% mutate(source = "Actual")

cbot <- rbind(cboa, cbop)
cbot <- cbot %>% mutate(display_value =   value*(date == "2022-02-01"))
cbot$display_value <- na_if(cbot$display_value, 0)

cbot$series_title <- str_replace_all(cbot$series_title, "\\(Seas\\) ", fixed(""))
cbot$series_title <- str_replace_all(cbot$series_title, "Employment Level", "Employment Level (in Thousands)")
cbot$source <- str_replace_all(cbot$source, "CBO Projections", "CBO Projections, February 2021")
cbot$source <- str_replace_all(cbot$source, "Actual", "Actual Values")

ggplot(cbot, aes(date, value, color=source, linetype=source, size=source)) + geom_line(size=1.2) + facet_wrap(facets = "series_title", scales = "free") +
  theme_classic() +
  theme(legend.position='bottom') +
  scale_x_date(date_labels = "%b %y") +
  scale_color_manual(values = c("steelblue", "darkred")) +
  scale_linetype_manual(values = c("solid","dashed")) +
  labs(title="Compared to Previous Projections, the Entire Labor Market is Stronger As a Result of the American Rescue Plan",
       subtitle="There are 3 million more jobs, labor force participation is 0.3 percent higher, and unemployment is 1.3 percent lower, compared to CBO projections from Feb 2021, before the ARP passed.",
       caption="Testing by Mike Konczal",
       x="", y="") +
  theme(strip.text = element_text(face = "bold", color = "black", hjust = 0.5, size = 18),
        plot.title = element_text(size = 25, face="bold"), plot.subtitle = element_text(size=18, margin=margin(9,0,15,0)),
        plot.caption = element_text(size=14, margin=margin(19,0,11,0)),
        plot.title.position = "plot") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()) +
  theme(axis.text=element_text(size=14), legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(), legend.text = element_text(size=18, color="#222222"), panel.background = element_blank())

ggsave("graphics/cbo_projects.png", dpi="retina", units=c("in"), width=21)