
# Feds ----
federal_jobs <- c(
  "Federal",
  "Federal, except U.S. Postal Service",
  "Federal hospitals",
  "Department of Defense",
  "U.S. Postal Service",
  "Other Federal government"
)

ces_data %>%
  filter(
         year >= 2024,
         seasonal == "S",
         data_type_code == 1,
         industry_name %in% federal_jobs) %>%
  ggplot(aes(date, value)) +
  geom_line() +
  facet_wrap(~industry_name, scales = "free")

df %>%
  filter(date == max(date) %m-% months(1)) %>%
  select(industry_name, display_level, value) %>%
  arrange(display_level)


# Business
ces_data %>%
  filter(
    year >= 2024,
    seasonal == "S",
    data_type_code == 1,
    industry_name %in% federal_jobs) %>%
  ggplot(aes(date, value)) +
  geom_line() +
  facet_wrap(~industry_name, scales = "free")

ces_data %>%
  filter(supersector_name == "Professional and business services") %>%
  select(industry_name, display_level) %>%
  distinct(industry_name, .keep_all = TRUE)


change_PBS <- ces_data %>%
  filter(supersector_name == "Professional and business services",
         seasonal == "S",
         data_type_code == "01") %>%
  group_by(industry_name) %>%
  mutate(year_change = value - lag(value, 12),
         year_percent_change = value/lag(value, 12) - 1) %>%
  ungroup() %>%
  filter(date == max(date) %m-% months(1)) %>%
  select(date, value, industry_name, display_level, year_change, year_percent_change)

View(change_PBS)

library(tidyverse)
library(lubridate)

# NBER recessions since 1990
recessions <- tribble(
  ~start,        ~end,
  "1990-07-01",  "1991-03-01",
  "2001-03-01",  "2001-11-01",
  "2007-12-01",  "2009-06-01",
  "2020-02-01",  "2020-04-01"
) %>%
  mutate(across(everything(), ymd))

ces_data %>%
  filter(industry_name == "Temporary help services",
         seasonal == "S",
         data_type_code == "01") %>%
  ggplot(aes(date, value)) +
  
  ## recession shading ------------
geom_rect(
  data = recessions,
  aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
  inherit.aes = FALSE,
  fill = "grey50",   # opaque grey
  alpha = 0.25       # tweak opacity to taste
) +
  ## main series ------------------
geom_line(size = 1) +
  
  labs(
    subtitle = "Total employment: Temporary help services. Monthly, thousands, seasonally adjusted.",
    title     = "What’s going on with temp help in this recovery?",
    caption   = "BLS: CES. Mike Konczal",
    x = NULL, y = NULL
  ) +
  theme_classic(base_size = 18) +
  theme(plot.title.position = "plot")




