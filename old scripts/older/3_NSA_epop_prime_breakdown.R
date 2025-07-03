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
library(tidytext)

##### SET UP SOME THINGS #####
#source(file = "1_a_load_bls_cps_jobs_data.R")
#source(file = "1_b_load_bls_ces_jobs_data.R")

# NOTHING HERE YET

unique(cps_jobs_data$seasonal)

a <- cps_jobs_data %>% filter(seasonal == "U", date == "2023-01-01", ages_code == 33)

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
  filter((date >= "2019-11-01" & date <= "2020-02-01") | (date >= "2022-11-01" & date <= "2023-02-01")) %>%
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
       title = "Increase in EPOP versus prepandemic baseline",
       subtitle = "TBD",
       caption ="BLS, CPI, seasonally adjusted values only. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = "none")

ggsave("price_increases.png", dpi="retina", width = 8, height=12, units = "in")
  
  
cps_jobs_data %>% filter(series_title == "(Unadj) Employment-population ratio - 25 to 54 years, Asian, women") %>%
  ggplot(aes(date,value)) + geom_line() + theme_classic()