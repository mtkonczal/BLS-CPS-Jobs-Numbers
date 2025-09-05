library(tidyverse)
library(ggtext)
library(blsR)
library(scales)
library(lubridate)
library(viridis)
library(tidyusmacro)
source("scripts/graphic_scripts.R")

# ── Pull CPS LEVELS (seasonally unadjusted) and compute rates manually ─────────
# Native-born: LF = LNU01073413, Unemp = LNU03073413
# Foreign-born: LF = LNU01073395, Unemp = LNU03073395

lvl <- get_n_series_table(
  c("LNU01073413",  # Native LF
    "LNU03073413",  # Native Unemp
    "LNU01073395",  # Foreign LF
    "LNU03073395"), # Foreign Unemp
  api_key   = bls_get_key(),
  start_year = 2017,
  end_year   = 2025,
  tidy       = TRUE
)

manual_rates <- lvl %>%
  transmute(
    date  = as.Date(paste0(year, "/", month, "/1")),
    lf_nat    = LNU01073413,   # thousands
    unemp_nat = LNU03073413,   # thousands
    lf_for    = LNU01073395,
    unemp_for = LNU03073395
  ) %>%
  arrange(date) %>%
  mutate(
    native_unrate_manual  = unemp_nat / lf_nat,      # exact ratio
    foreign_unrate_manual = unemp_for / lf_for
  )

# X-axis breaks every ~6 months, include latest tick
MI_dates_manual <- sort(unique(manual_rates$date), decreasing = TRUE)
MI_dates_manual <- MI_dates_manual[seq(1, length(MI_dates_manual), 6)]
MI_dates_manual <- sort(MI_dates_manual)

# ESP palette
manual_colors <- c(
  "Native Unemployment Rate"       = "#2c3254",  # ESP Warm Navy
  "Foreign-Born Unemployment Rate" = "#ff8361"   # ESP Warm Red
)

# ── Graphic A (manual): Native vs Foreign across dates ─────────────────────────
pA <- manual_rates %>%
  select(date, native_unrate_manual, foreign_unrate_manual) %>%
  pivot_longer(
    c(native_unrate_manual, foreign_unrate_manual),
    names_to = "type",
    values_to = "value"
  ) %>%
  mutate(
    type = if_else(type == "native_unrate_manual",
                   "Native Unemployment Rate",
                   "Foreign-Born Unemployment Rate"),
    dateTag  = if_else(date == max(date) | date == as.Date("2019-12-01"), value, NA_real_),
    pointTag = dateTag
  ) %>%
  filter(year(date) >= 2022) %>%
  ggplot(aes(date, value, color = type, label = percent(dateTag, accuracy = 0.01))) +
  geom_line(size = 1.2) +
  geom_text(aes(date, dateTag), nudge_x = 50, show.legend = FALSE) +
  geom_point(aes(date, pointTag), size = 2, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = manual_colors) +
  theme_esp() +
  labs(
    title = "Native versus Foreign-Born Unemployment Rates",
    subtitle = "Seasonally unadjusted values; manually calculated from CPS levels (Unemployed / Labor Force). 2020–2021 removed for visibility.",
    caption = "Mike Konczal",
    x = NULL, y = NULL
  ) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates_manual) +
  theme(legend.position = "top") +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(r = 30))

ggsave("graphics/g4a_native_foreign_manual.png", pA, dpi = "retina", width = 12, height = 6.75, units = "in")


# ── Graphic B (manual): Native-only across dates, dotted backline + final label ─
# Prep last-point helpers for dotted backline and label
last_pt <- manual_rates %>%
  filter(year(date) >= 2021) %>%
  slice_max(order_by = date, n = 1) %>%
  transmute(
    last_date = date,
    last_val  = native_unrate_manual,
    seg_start = as.Date("2021-01-01"))  # how far back to draw dotted line

manual_rates %>%
  filter(year(date) >= 2021) %>%
  ggplot(aes(date, native_unrate_manual)) +
  geom_line(color = "#2c3254", size = 1.2) +
  # Dotted horizontal line "going backwards" from the final value
  geom_segment(
    data = last_pt,
    aes(x = seg_start, xend = last_date, y = last_val, yend = last_val),
    inherit.aes = FALSE,
    linetype = "dotted",
    linewidth = 0.7,
    color = "#2c3254"
  ) +
  # Final value label to the right of the last point
  geom_text(
    data = last_pt,
    aes(x = last_date + 30, y = last_val,
        label = scales::percent(last_val, accuracy = 0.01)),
    inherit.aes = FALSE,
    hjust = 0, vjust = 0.5,
    size = 4.2
  ) +
  geom_point(
    data = last_pt,
    aes(x = last_date, y = last_val),
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_esp() +
  labs(
    title = "Native-Born Unemployment Rate",
    subtitle = "Seasonally unadjusted values; manually calculated from CPS levels for extra digits.",
    caption = "Mike Konczal",
    x = NULL, y = "Unemployment Rate"
  ) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates_manual,
               expand = expansion(mult = c(0.01, 0.12))) +  # right padding for label
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(r = 35))

ggsave("graphics/g4b_native_only_manual.png", dpi = "retina", width = 12, height = 6.75, units = "in")


# ── Graphic C (manual): Native-only by month, lines for 2023–2025 + final label for latest year ─
dfC <- manual_rates %>%
  mutate(
    yr     = year(date),
    m_num  = month(date)               # numeric month to keep ordering stable
  ) %>%
  filter(yr %in% c(2023, 2024, 2025)) %>%
  group_by(yr, m_num) %>%
  summarise(native_unrate_manual = mean(native_unrate_manual, na.rm = TRUE), .groups = "drop") %>%
  mutate(m_lbl = factor(month.abb[m_num], levels = month.abb))

# Compute label only for the latest year present in dfC
latest_year <- max(dfC$yr, na.rm = TRUE)
label_pt <- dfC %>%
  filter(yr == latest_year) %>%
  arrange(m_num) %>%
  slice_tail(n = 1) %>%
  mutate(label_txt = scales::percent(native_unrate_manual, accuracy = 0.01))

pC <- ggplot(dfC, aes(x = m_lbl, y = native_unrate_manual,
                      group = factor(yr), color = factor(yr))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("2023" = "#70ad8f", "2024" = "#ff8361", "2025" = "#2c3254")) +
  scale_y_continuous(labels = scales::percent) +
  theme_esp() +
  labs(
    title = "Native-Born Unemployment Rate by Month Highest in Years",
    subtitle = "Seasonally unadjusted values; manually calculated from CPS levels for extra digits.",
    caption = "Inspired by Ben Zipperer, EPI. Mike Konczal",
    x = NULL, y = "Unemployment Rate", color = "Year"
  ) +
  theme(legend.position = "top") +
  # Label only the latest year's latest month
  geom_text(
    data = label_pt,
    aes(x = m_lbl, y = native_unrate_manual, label = label_txt, color = NULL),
    inherit.aes = FALSE,
    hjust = -0.2, vjust = 0.5, size = 4.2,
    show.legend = FALSE
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(r = 30))

ggsave("graphics/g4c_native_by_month_manual.png", pC, dpi = "retina", width = 12, height = 6.75, units = "in")
