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
  c(
    "LNU01073413", # Native LF
    "LNU03073413", # Native Unemp
    "LNU01073395", # Foreign LF
    "LNU03073395"
  ), # Foreign Unemp
  api_key = bls_get_key(),
  start_year = 2017,
  end_year = year(Sys.Date()),
  tidy = TRUE
)

lvl <- lvl %>%
  mutate(
    across(
      -c(year, month),
      as.numeric
    )
  )

manual_rates <- lvl %>%
  transmute(
    date = as.Date(paste0(year, "/", month, "/1")),
    lf_nat = LNU01073413, # thousands
    unemp_nat = LNU03073413, # thousands
    lf_for = LNU01073395,
    unemp_for = LNU03073395
  ) %>%
  arrange(date) %>%
  mutate(
    native_unrate_manual = unemp_nat / lf_nat, # exact ratio
    foreign_unrate_manual = unemp_for / lf_for
  )

# X-axis breaks every ~6 months, include latest tick
MI_dates_manual <- sort(unique(manual_rates$date), decreasing = TRUE)
MI_dates_manual <- MI_dates_manual[seq(1, length(MI_dates_manual), 6)]
MI_dates_manual <- sort(MI_dates_manual)

# ESP palette
manual_colors <- c(
  "Native Unemployment Rate" = "#2c3254", # ESP Warm Navy
  "Foreign-Born Unemployment Rate" = "#ff8361" # ESP Warm Red
)

# ── Graphic C (manual): Native-only by month, include 2023 onward + final label for latest year ─
latest_year <- max(year(manual_rates$date), na.rm = TRUE)
display_years <- seq(2023, latest_year)

dfC <- manual_rates %>%
  mutate(
    yr = year(date),
    m_num = month(date) # numeric month to keep ordering stable
  ) %>%
  filter(yr %in% display_years) %>%
  group_by(yr, m_num) %>%
  summarise(
    native_unrate_manual = mean(native_unrate_manual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(m_lbl = factor(month.abb[m_num], levels = month.abb))

# Compute label only for the latest year present in dfC
label_pt <- dfC %>%
  filter(yr == latest_year) %>%
  arrange(m_num) %>%
  slice_tail(n = 1) %>%
  mutate(label_txt = scales::percent(native_unrate_manual, accuracy = 0.01))

year_palette <- colorRampPalette(c("#70ad8f", "#ff8361", "#2c3254"))(
  length(display_years)
)
names(year_palette) <- as.character(display_years)

dfC <- dfC %>% filter(!is.na(native_unrate_manual))
pC <- ggplot(
  dfC,
  aes(
    x = m_lbl,
    y = native_unrate_manual,
    group = factor(yr),
    color = factor(yr)
  )
) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = year_palette) +
  scale_y_continuous(labels = scales::percent) +
  theme_esp() +
  labs(
    title = "Native-Born Unemployment Rate by Month Highest in Years",
    subtitle = "Seasonally unadjusted values; manually calculated from CPS levels for extra digits.",
    caption = "Inspired by Ben Zipperer, EPI. Mike Konczal, Economic Security Project.",
    x = NULL,
    y = "Unemployment Rate",
    color = "Year"
  ) +
  theme(legend.position = "top") +
  # Label only the latest year's latest month
  geom_text(
    data = label_pt,
    aes(x = m_lbl, y = native_unrate_manual, label = label_txt, color = NULL),
    inherit.aes = FALSE,
    hjust = -0.2,
    vjust = 0.5,
    size = 4.2,
    show.legend = FALSE
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(r = 30))

ggsave(
  "graphics/08_native_by_month_manual.png",
  pC,
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)
