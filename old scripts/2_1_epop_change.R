# This script does some data analysis on jobs.
# Written by: Mike Konczal
# Last Updated: 3-12-2022

setwd("/Users/mkonczal/Documents/GitHub/BLS-CPS-Jobs-Numbers/")
library(tidyverse)
library(ggtext)
library(lubridate)
library(tidytext)

##### SET UP SOME THINGS #####
#source(file = "1_load_cps_jobs_data.R")
#load("data/cps_jobs_data.RData")

##### FIRST GRAPHIC: EPOP BY GROUPS #####

# Filter out EPOP categories
epop <- cps_jobs_data %>% filter(seasonal == "S") %>% filter(periodicity_code == "M") %>%
  filter(grepl("Employment-Population Ratio", series_title))


epop %>% filter(date == max(date)) %>%
  select(series_id, series_title) %>%
  distinct(series_title, .keep_all = TRUE) 


df <- epop %>% filter(series_id %in% c("LNS12300061","LNS12300062")) %>%
  select(series_title, date, value) %>%
  pivot_wider(names_from = series_title, values_from = value) %>%
  clean_names()

df %>%
  filter(seas_employment_population_ratio_25_54_yrs_men >= seas_employment_population_ratio_25_54_yrs_men[date == max(date)]) %>%
  arrange(desc(date))

df %>%
  filter(seas_employment_population_ratio_25_54_yrs_women >= seas_employment_population_ratio_25_54_yrs_women[date == max(date)]) %>%
  arrange(desc(date))


# We are now setting this to a specific date, January 2021 here, rather than X months ago.
# Uncomment "MonthsToLag" with a numerical value to go back to X months ago.
LagMonth <- as.Date("2022-01-01")
MaxMonth <- epop %>% select(date) %>% filter(date == max(date)) %>% distinct(date) %>% pull(date)
MonthsToLag <- interval(LagMonth, MaxMonth) %/% months(1)
MonthsToLag <- 12



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

# Write out the series we want
graphic_series <- c("LNS12300000","LNS12300001","LNS12300002","LNS12300003","LNS12300006","LNS12300009","LNS12300036","LNS12300060",
                    "LNS12300089","LNS12300091","LNS12300093","LNS12327659","LNS12327660","LNS12327662","LNS12327689","LNS12300004",
                    "LNS12300005","LNS12300007","LNS12324230", "LNS12300008", "LNS12300012")

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

epop %>% mutate(g2_series_title = fct_reorder(g2_series_title, value_1m)) %>%
ggplot(aes(x=g2_series_title, y=value_1m, label=round(value_1m,3))) + geom_bar(stat="identity",size=0) +
theme_lass + coord_flip() +
scale_y_continuous(limits=c(0.8,1.2),oob = rescale_none) +
  labs(caption = "The category equals 1.0 when it returns to its pre-pandemic rate.
       Pre-pandemic rate is average monthly EPOP ratio from Nov 19 to Jan 20 for group.
       Data: BLS, CPS. Seasonally adjusted. Author's calculations. Mike Konczal, Roosevelt Institute",
       title="Feb 2023 Employment-to-Population Rate Compared to Prepandemic Baseline") +
  geom_hline(yintercept=1, color="white",alpha=0.4) +
  geom_text(    hjust = -0.5, size = 3,
                position = position_dodge(width = 1)) +
  theme(plot.title = element_text(size=14))
  
ggsave("graphics/EPOP_race2.png", width = 12, height=6.75, dpi="retina")
  

#### Prime-Age EPOP Recovered in 31 Months ####

GR_compare <- cps_jobs_data %>% filter(series_id == "LNS12300060") %>%
  mutate(value = value/100) %>%
  filter(date > "2000-01-01") %>%
  mutate(val_org1 = value[date=="2020-01-01"]) %>%
  mutate(val_org2 = value[date=="2019-12-01"]) %>%
  mutate(val_org3 = value[date=="2019-11-01"]) %>%
  mutate(val_org = (val_org1+val_org2+val_org3)/3) %>%
  mutate(GRval_org1 = value[date=="2008-01-01"]) %>%
  mutate(GRval_org2 = value[date=="2007-12-01"]) %>%
  mutate(GRval_org3 = value[date=="2007-11-01"]) %>%
  mutate(GRval_org = (GRval_org1+GRval_org2+GRval_org3)/3)

GR_compare <- GR_compare %>% mutate(GRval_org = GRval_org*(date >= "2008-01-01" & date <= "2019-01-01"))
GR_compare$GRval_org <- na_if(GR_compare$GRval_org, 0)
GR_compare <- GR_compare %>% mutate(val_org = val_org*(date > "2020-01-01"))
GR_compare$val_org <- na_if(GR_compare$val_org, 0)
GR_compare <- GR_compare %>% mutate(old_school = value[date == max(date)])
GR_compare$old_school <- if_else(GR_compare$date >= "2001-05-01",GR_compare$old_school, as.numeric(NA))

GR_compare %>%
  ggplot(aes(date,value)) + geom_line(size=2) +
  geom_line(aes(date,GRval_org), color="darkred", size=1.2, linetype="dashed") +
  geom_line(aes(date,val_org), color="darkred", size=1.2, linetype="dashed") +
  geom_line(aes(date,old_school), color="darkred", size=1.2, linetype="dashed") +
  scale_y_continuous(labels = percent) + theme_lass +
  labs(title="Prime-Age EPOP Highest in 22 Years", subtitle="Employment-to-population ratio, 25-54 years old, seasonally-adjusted. Red line is three month average prior.",
       caption="CPS, Seasonally Adjusted, Author's Calculations. Mike Konczal, Roosevelt Institute",
       x="", y="Employment to Population Ratio") +
  theme(plot.title = element_text(size = 25),
        plot.caption = element_text(size=12),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=20, margin=ggplot2::margin(9,0,15,0),lineheight=1.05)) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20),
        legend.text.align = 0, legend.background = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=15), panel.background = element_blank())
  
ggsave("graphics/GR_EPOP_race.png", width = 19, height=10.68, dpi="retina")

GR_compare %>% select(GRval_org) %>% filter(!is.na(GRval_org)) %>% summarize(n())
GR_compare %>% select(val_org) %>% filter(!is.na(val_org)) %>% summarize(n())


#### Prime-Age EPOP Changes ####
unadjusted_categories <- c("LNU02300060",
                           "LNU02300061",
                           "LNU02300062",
                           "LNU02300063",
                           "LNU02300064",
                           "LNU02300065",
                           "LNU02300066",
                           "LNU02300067",
                           "LNU02300068",
                           "LNU02300069",
                           "LNU02300070",
                           "LNU02300071",
                           "LNU02332249",
                           "LNU02332330",
                           "LNU02332371")

epops_values <- cps_jobs_data %>% filter(series_id %in% unadjusted_categories) %>%
  filter((date >= "2019-11-01" & date <= "2020-02-01") | (date >= "2022-11-01" & date <= "2023-03-01")) %>%
  mutate(after_pandemic = if_else(year(date) %in% c(2019,2020), 0, 1)) %>%
  group_by(after_pandemic, series_title) %>%
  summarize(epop = mean(value)) %>% ungroup()


epop <- epops_values %>% group_by(series_title) %>% summarize(diff = epop[after_pandemic==TRUE]-epop[after_pandemic==FALSE]) %>%
  arrange(desc(diff)) %>% mutate(diff = diff/100)

epop$series_title <- str_replace_all(epop$series_title, "\\(Unadj\\) Employment-Population Ratio", "All")
epop$series_title <- str_replace_all(epop$series_title, "\\(Unadj\\) Employment-population ratio", "All")
epop$series_title <- str_replace_all(epop$series_title, "All - ", fixed(""))
epop$series_title <- str_replace_all(epop$series_title, "25-54 yrs.", "25-54 yrs")
epop$series_title <- str_replace_all(epop$series_title, "25-54 yrs ", "25-54 yrs, ")
epop$series_title <- str_replace_all(epop$series_title, "25 to 54 years", "25-54 yrs")
epop$series_title <- str_replace_all(epop$series_title, " or African American", fixed(""))
epop$series_title <- str_replace_all(epop$series_title, ". & over", ("+"))
epop$series_title <- str_replace_all(epop$series_title, "Less than a High School", ("No HS"))
epop$series_title <- str_replace_all(epop$series_title, "High School Graduates, No College", ("HS, No College"))
epop$series_title <- str_replace_all(epop$series_title, "Bachelor's degree and higher", ("Bachelor and higher"))
epop$series_title <- str_replace_all(epop$series_title, "Some College or Associate Degree", ("Some College or Associate"))

epop %>%
  mutate(category = as.factor(series_title), name = reorder_within(series_title, diff, category)) %>%
  ggplot(aes(name, diff)) +
  geom_col(size=0) +
  coord_flip() +
  scale_x_reordered() +
  theme_lass +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  theme(panel.grid.major.y = element_line(size=0)) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot") +
  labs(y = NULL,
       x = NULL,
       title = "Increase in Prime-age EPOP Versus Early 2019",
       subtitle = "Seasonally-unadjusted",
       caption ="BLS, CPI, seasonally adjusted values only. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = "none")

ggsave("graphics/prime_EPOP_changes.png", dpi="retina", width = 8, height=12, units = "in")


cps_jobs_data %>% filter(series_title == "(Unadj) Employment-population ratio - 25 to 54 years, Asian, women") %>%
  ggplot(aes(date,value)) + geom_line() + theme_classic()



#### Unemployment Rate ####


cps_jobs_data %>% filter(series_id %in% c("LNS14000006","LNS14000003")) %>%
  ggplot(aes(date,value,color=series_title)) + geom_line() + theme_classic() + theme(legend.position = "bottom")

cps_jobs_data %>% filter(series_id %in% c("LNS14000006","LNS14000003")) %>%
  group_by(date) %>% summarize(black_u = value[series_id== "LNS14000006"],
                               relative_diff = value[series_id == "LNS14000006"]/value[series_id == "LNS14000003"],
                               absolute_diff = value[series_id == "LNS14000006"] - value[series_id == "LNS14000003"]) %>%
  ggplot(aes(date, absolute_diff)) + geom_line() + geom_line(aes(date,black_u, color="red")) +
  geom_line(aes(date,relative_diff, color="purple"))


cps_jobs_data %>% filter(series_id %in% c("LNS14000006","LNS14000003")) %>%
  group_by(date) %>% summarize(relative_diff = value[series_id == "LNS14000006"]/value[series_id == "LNS14000003"],
                               absolute_diff = value[series_id == "LNS14000006"] - value[series_id == "LNS14000003"])




long_epop <- cps_jobs_data %>% filter(series_id == "LNS12300060") %>%
  select(date,value) %>%
  mutate(last_value = value[date==max(date)]) %>%
  mutate(higher = value > last_value)

total_months_higher <- sum(long_epop$higher)

cps_jobs_data %>% filter(series_id == "LNS12300060") %>%
  summarize(total_above_current =
              sum(value > value[date == max(date)]))

a <- long_epop[long_epop$higher]


long_epop %>%
  mutate(value = value/100) %>%
  mutate(last_value = last_value/100) %>%
  ggplot(aes(date,value)) + geom_line(size=1.5) +
  geom_line(aes(date,last_value), color="#A4CCCC", linetype="dashed") +
  theme_lass +
  theme(plot.title.position = "plot") +
  labs(y = NULL,
       x = NULL,
       title = paste("There Are Only", total_months_higher, "Months With Higher Prime-Age EPOP Than Right Now"),
       subtitle = "Employment-to-population ratio, 25-54 year olds, monthly, seasonally adjusted.",
       caption ="BLS, CPS. Author's calculation. Mike Konczal") +
  scale_y_continuous(labels = scales::percent)

ggsave("graphics/long_epop.png",  width = 12, height=8, dpi="retina")


##### Men and Women ####
cps_jobs_data %>% filter(series_id %in% c("LNS12300062","LNS12300060")) %>%
  ggplot(aes(date, value, color=series_title)) + geom_line() +
  theme_lass