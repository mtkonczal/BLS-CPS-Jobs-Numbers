
annual_change <- ces_data %>% filter(series_id == "CES0000000001") %>%
  mutate(change12 = value - lag(value, 12)) %>%
  filter(month(date) == 12) %>%
  filter(year %in% c(2019,2023))
  

ces_last <- ces_data %>% filter(series_id == "CES0000000001") %>% filter(date == max(date)) %>% pull(value)
ces_2019 <- ces_data %>% filter(series_id == "CES0000000001") %>% filter(date == "2019-12-01") %>% pull(value)

jobs_chart <- ces_data %>%
  filter(seasonal == "S") %>%
  filter((display_level <= 2 & data_type_code == 1)) %>%
  group_by(industry_name) %>%
  mutate(
    change12 = value - lag(value, 12)
  ) %>%
  filter(year %in% c(2019, 2023),
         month(date) == 12) %>%
  select(date, industry_name, change12) %>%
  filter(!(industry_name %in% c("Total nonfarm", "Service-providing")))
  

# Reorder the levels of industry_name
jobs_chart2<- jobs_chart %>%
  mutate(industry_name = as.factor(industry_name),
         industry_name = fct_relevel(industry_name, 
                                     "Total private", "Private service-providing", "Goods-producing",
                                     "Construction", "Financial activities", "Government","Information", "Leisure and hospitality", "Manufacturing", "Mining and logging",
                                     "Other services", "Private education and health services", "Professional and business services", "Trade, transportation, and utilities"),
         industry_name = fct_rev(industry_name))

jobs_chart2 %>%
  mutate(year = as.factor(year(date))) %>%
  ggplot(aes(industry_name, change12, fill=year)) + geom_col(position = "dodge", size=0) +
  coord_flip() +
  theme_lass +
  theme(legend.position = c(0.8,0.4)) +
  labs(subtitle = "Jobs added over 12-months, 2023 and 2019, thousands of jobs.",
       caption = "CES, seasonally-adjusted. Mike Konczal, Roosevelt Institute.",
       title="Private Job Growth Faster Across Categories in 2023 against 2019")