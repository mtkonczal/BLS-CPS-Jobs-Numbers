library(tidyverse)
library(govMacroTools)
library(lubridate)


jobs <- getBLSFiles("ces", "konczal@gmail.com")

#reminder on display_levels
jobs %>%
  filter(seasonal == "S", data_type_code == "01") %>%
  filter(date == max(date) %m-% months(1)) %>%
  group_by(display_level) %>%
  reframe(n = n(),
          total_jobs = sum(value))


df <- jobs %>%
  filter(seasonal == "S", data_type_code == "01", display_level == 5) %>%
  filter(date <= max(date) %m-% months(1)) %>%
  group_by(series_title) %>%
  mutate(value = value - lag(value,1)) %>%
  ungroup()


library(tidyverse)

# Assuming your tibble is named df with columns: date, industry_name, value
# Example: df <- tibble(date, industry_name, value)

top_25_concentration <- df %>%
  group_by(date) %>%
  arrange(date, desc(value)) %>%  
  mutate(rank = row_number(),
         total_industries = n(),
         pct_rank = rank / total_industries) %>%
  mutate(top_25 = pct_rank <= 0.10) %>%
  summarise(
    total_gain = sum(value),
    top25_gain = sum(value[top_25]),
    pct_top25 = top25_gain / total_gain
  ) %>%
  ungroup()

# View result
tail(top_25_concentration, 15)


top_25_concentration %>%
  filter(year(date) >= 2013) %>%
  ggplot(aes(date, pct_top25)) + geom_line()



gini_top_half <- df %>%
  group_by(date) %>%
  arrange(date, desc(value)) %>%
  mutate(rank = row_number(),
         total_industries = n(),
         pct_rank = rank / total_industries) %>%
  filter(pct_rank <= 0.5) %>%  # keep only top half
  summarise(
    gini_top_half = ineq::Gini(value)
  ) %>%
  ungroup()

# View the resulting Gini coefficients per date
gini_top_half %>%
  filter(year(date) >= 1991) %>%
  mutate(gini_top_half_na = if_else(year(date) %in% c(2007,2008,2009, 2020), NA, gini_top_half)) %>%
  mutate(gini_top_half_na = gini_top_half_na + lag(gini_top_half_na,1) + lag(gini_top_half_na,2),
         gini_top_half_na = gini_top_half_na/3) %>%
  ggplot(aes(date, gini_top_half_na)) + geom_line()




df <- jobs %>%
  filter(data_type_code == "01", display_level == 5, period == "M13") %>%
  group_by(series_title) %>%
  mutate(value = value - lag(value,1)) %>%
  ungroup() %>%
  select(year, series_title, industry_name, value)


negative_pct <- df %>%
  filter(!is.na(value)) %>%
  filter(year >= 1991) %>%
  group_by(year, supersector_name) %>%
  summarise(
    total_industries = n(),
    negative_count = sum(value <= 0),
    percent_negative = negative_count / total_industries
  ) %>%
  ungroup()

negative_pct %>%
  ggplot(aes(year, percent_negative)) + geom_line() +
  facet_wrap(~supersector_name, scales = "free") +
  theme_classic()


negative_pct %>%
  ggplot(aes(year, percent_negative)) + geom_line() +  geom_point()
  

wages <- jobs %>%
  filter(data_type_code == "03", display_level == 5, period == "M13") %>%
  select(year, industry_name, wages = value)

df2 <- df %>%
  filter(!is.na(value)) %>%
  filter(year >= 2007) %>%
  mutate(negative_flat = value <= 0) %>%
  left_join(wages, by=c("year", "industry_name"))
  
df2 %>%
  filter(!is.na(wages)) %>%
  group_by(year, negative_flat) %>%
  reframe(avg_wages = mean(wages)) %>%
  ungroup() %>%
  ggplot(aes(year, avg_wages, color=negative_flat)) + geom_line() +
  theme_classic()