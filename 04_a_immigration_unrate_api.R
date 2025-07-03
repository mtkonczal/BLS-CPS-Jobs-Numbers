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



bls_api <- "996d4e4af85f43f3ac301805891cbf6e"
bls_set_key(bls_api)

native_unrate <- get_n_series_table(
  c("LNU04073413","LNU04073395"),
  api_key = bls_get_key(),
  start_year = 2017,
  end_year = 2025,
  tidy = TRUE
)

native_unrate <- native_unrate %>%
  mutate(
    date = as.Date(paste0(year, "/", month, "/", 1)),
    native_unrate = LNU04073413 / 100,
    foreign_unrate = LNU04073395 / 100
  ) %>%
  arrange(date) %>%
  mutate(
    native_unrate = rollmean(native_unrate, k = 12, fill = NA, align = "right"),
    foreign_unrate = rollmean(foreign_unrate, k = 12, fill = NA, align = "right")
  )

MI_dates <- sort(unique(native_unrate$date), decreasing = TRUE)
MI_dates <- MI_dates[seq(1, length(MI_dates), 6)]


# Set your custom colors
manual_colors <- c(
  "Native Unemployment Rate" = "#2c3254",   # Example: dark blue
  "Foreign-Born Unemployment Rate" = "#ff8361" # Example: soft red
)

# Unemployment Rate ----
native_unrate %>%
  select(-year, -month, -LNU04073413, -LNU04073395) %>%
  pivot_longer(c(native_unrate, foreign_unrate), names_to = "type", values_to = "value") %>%
  mutate(
    type = if_else(type == "native_unrate", "Native Unemployment Rate", "Foreign-Born Unemployment Rate"),
    dateTag = if_else(date == max(date) | date == as.Date("2019-12-01"), value, NA_real_),
    pointTag = if_else(date == max(date) | date == as.Date("2019-12-01"), value, NA_real_)
  ) %>%
  filter(year(date) >= 2022) %>%
  ggplot(aes(date, value, color = type, label = percent(dateTag))) +
  geom_line(size = 1.2) +
  geom_text(aes(date, dateTag), nudge_x = 50, show.legend = FALSE) +
  geom_point(aes(date, pointTag), size = 2, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = manual_colors) +
  theme_esp() +
  labs(
    title = "Native versus Foreign Born Unemployment Rates",
    subtitle = "Unemployment rate, 12-month average of seasonally unadjusted values. BLS, CPS. 2020–2021 Removed for Visibility.",
    caption = "Mike Konczal"
  ) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates) +
  theme(legend.position = "top")

ggsave("graphics/g4a_native_born_unrate.png", dpi = "retina", width = 12, height = 6.75, units = "in")

