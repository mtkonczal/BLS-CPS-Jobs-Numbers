library(tidyverse)
library(ggtext)
library(blsR)
library(govMacroTools)
library(scales)
library(zoo)
library(tidyverse)
library(govMacroTools)
library(lubridate)
library(viridis)
source("scripts/03_graphic_scripts.R")


young_unrate <- get_n_series_table(
  c("LNS14000037","LNS14000038"),
  api_key = bls_get_key(),
  start_year = 2017,
  end_year = 2025,
  tidy = TRUE
)

young_unrate <- young_unrate %>%
  mutate(men_unrate_20_24 = LNS14000037,
         date = as.Date(paste0(year,"/",month,"/",1)),
         women_unrate_20_24 = LNS14000038)

MI_dates <- sort(unique(young_unrate$date), decreasing = TRUE)
MI_dates <- MI_dates[seq(1, length(MI_dates), 6)]


# Set your custom colors
manual_colors <- c(
  "Men, 20-24, Unemployment" = "#2c3254",   # Example: dark blue
  "Female, 20-24, Unemployment" = "#ff8361" # Example: soft red
)

# Unemployment Rate ----
young_unrate %>%
  select(-year, -month, -LNS14000037, -LNS14000038) %>%
  pivot_longer(c(men_unrate_20_24, women_unrate_20_24), names_to = "type", values_to = "value") %>%
  mutate(
    value = value / 100,
    type = if_else(type == "men_unrate_20_24", "Men, 20-24, Unemployment", "Female, 20-24, Unemployment"),
    dateTag = if_else(date == max(date) | date == as.Date("2019-12-01"), value, NA_real_),
    pointTag = if_else(date == max(date) | date == as.Date("2019-12-01"), value, NA_real_),
    value = if_else(year(date) %in% 2020:2021, NA_real_, value)
  ) %>%
  ggplot(aes(date, value, color = type, label = percent(dateTag))) +
  geom_line(size = 1.2) +
  geom_text(aes(date, dateTag), nudge_x = 80, show.legend = FALSE) +
  geom_point(aes(date, pointTag), size = 2, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = manual_colors) +
  theme_esp() +
  labs(
    title = "Youth Unemployment Increasing in 2025, Especially for Men.",
    subtitle = "Unemployment rate, 20–24 year olds, seasonally adjusted. BLS, CPS. 2020–2021 Removed for Visibility.",
    caption = "Mike Konczal"
  ) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates) +
  theme(legend.position = "top")

ggsave("graphics/g2a_young_unemployment_gender.png", dpi = "retina", width = 12, height = 6.75, units = "in")

