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

cbop <- read_csv("data/CBO_prepandemic_labor_projections.csv") %>% #filter(series_title != "(Seas) Employment Level") %>%
  filter(date <= "2022-10-01")

cboa <- cps_jobs_data %>% filter(seasonal == "S") %>% filter(periodicity_code == "M") %>% filter(date >= "2021-01-01") %>%
  filter(series_id %in% c("LNS11300000","LNS12000000", "LNS14000000")) %>%
  select(date, value, series_title) %>% mutate(source = "Actual")
maxDate <- max(cboa$date)

cboe <- ces_data %>% filter(series_id %in% "CES0000000001") %>% filter(date >= "2021-01-01") %>%
  select(date, value, series_title) %>% mutate(source = "Actual")
  
cbot <- rbind(cboa, cboe, cbop)
cbot$series_title <- str_replace_all(cbot$series_title, "\\(Seas\\) ", fixed(""))
cbot$series_title <- str_replace_all(cbot$series_title, "\\(Seas\\) ", fixed(""))
cbot$series_title <- str_replace_all(cbot$series_title, "Employment Level", "Employment Level (Household, in Thousands)")
cbot$series_title <- str_replace_all(cbot$series_title, "Employment \\(CES\\)", "Employment Level (Establishment, in Thousands)")
cbot$series_title <- str_replace_all(cbot$series_title, "All employees, thousands, total nonfarm, seasonally adjusted", "Employment Level (Establishment, in Thousands)")
cbot$series_title <- str_replace_all(cbot$series_title, "Civilian Labor Force Level", "Employment Level (Household, in Thousands)")
cbot$source <- str_replace_all(cbot$source, "CBO Projections", "CBO Projections, February 2021")
cbot$source <- str_replace_all(cbot$source, "Actual", "Actual Values")


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

calculation <- cbot %>% group_by(source, series_title) %>% mutate(Qvalue = (value + lag(value,1) + lag(value,2))/3) %>% ungroup() %>%
  filter(date == maxDate) %>% group_by(series_title) %>% arrange(desc(source)) %>% mutate(diffQ = Qvalue - lag(Qvalue)) %>% ungroup() %>%
  filter(source == "Actual Values") %>% select(date, series_title, diffQ)

numberU <- calculation %>% filter(series_title=="Unemployment Rate") %>% select(diffQ) %>% mutate(diffQ = abs(round(diffQ, digits=2)))
numberLFP <- calculation %>% filter(series_title=="Labor Force Participation Rate") %>% select(diffQ) %>% round(digits=3)
numberE <- calculation %>% filter(series_title=="Employment Level (Establishment, in Thousands)") %>% select(diffQ) %>% mutate(diffQ = diffQ/1000) %>% round(digits=2)
numberEH <- calculation %>% filter(series_title=="Employment Level (Household, in Thousands)") %>% select(diffQ) %>% mutate(diffQ = diffQ/1000) %>% round(digits=2)
subtitleNumbers <- paste("Over the last quarter, there's a monthly average of between", numberE, "(establishment survey) and", numberEH, "(household) million more jobs than the CBO's
projections from before the American Rescue Plan passed. The CBO predicted the recovery would flatline in early 2022. Instead it's continuing strong.", sep=" ")


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

cbot <- cbot %>% mutate(display_valueAll = if_else(date == max(cboe$date),value, as.numeric(NA)))

cbot %>%
  filter(date > "2020-01-01" & date < "2024-01-01") %>%
ggplot(aes(date, value, color=source, linetype=source, size=source)) + geom_line(size=1.2) + facet_wrap(~type, scales = "free") +
  theme_classic() +
  theme(legend.position='none') +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_color_manual(values = c("#01579B", "darkred","purple")) +
  scale_linetype_manual(values = c("solid","dashed","dashed")) +
  labs(title="Compared to Prior Projections, the Labor Market is Still Recovering Strong From the American Rescue Plan",
       subtitle=subtitleNumbers,
       caption="Household and establishment refer to the respective surveys. Seasonally adjusted.\nDotted red line is CBO quarterly 10-Year Economic Projections, February 2021.\nAuthor's calculations. Mike Konczal, Roosevelt Institute",
       x="", y="") +
  theme(strip.text = element_text(face = "bold", color = "black", hjust = 0.5, size = 21),
        plot.title = element_text(size = 25, face="bold"), plot.subtitle = element_text(size=20, margin=margin(9,0,15,0),lineheight=1.05),
        plot.caption = element_text(size=14, margin=margin(19,0,11,0), lineheight=1.05),
        plot.title.position = "plot") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank()) +
  theme(axis.text=element_text(size=11), legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(), legend.text = element_text(size=15, color="#222222"), panel.background = element_blank()) +
  geom_point(aes(x=date, y=display_valueAll), size=3) +
  geom_label_repel(aes(x=date, y=display_valueAll, label=round(display_valueAll,1)), size=5, box.padding = unit(0.2,"in"))

ggsave("graphics/cbo_projects.png", width = 19, height=10.68, dpi="retina")