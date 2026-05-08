# Deep dive into Education and Health Services supersector (code 65)
# Where is job growth coming from in 2025-2026 vs. prior years?
# Uses ALL sub-industries within the supersector.

positive_color <- "#2c3254"
esp_bg <- "#f4f2e4"

# --- Data prep ---
# Grab every sub-industry within supersector 65, excluding aggregates
# (display_level <= 2 are totals like "Education and health services" itself)
edhealth <- ces_data %>%
  filter(
    seasonal == "S",
    data_type_code == 1,
    supersector_code == "65",
    industry_display_level > 2
  ) %>%
  arrange(industry_name, date) %>%
  group_by(industry_name) %>%
  mutate(
    monthly_change = value - lag(value, 1),
    yoy_change = value - lag(value, 12),
    yoy_pct = value / lag(value, 12) - 1
  ) %>%
  ungroup()

# Also grab the supersector total for context
edhealth_total <- ces_data %>%
  filter(
    seasonal == "S",
    data_type_code == 1,
    supersector_code == "65",
    industry_display_level == 2
  ) %>%
  arrange(date) %>%
  mutate(
    monthly_change = value - lag(value, 1),
    yoy_change = value - lag(value, 12)
  )

latest_month <- max(edhealth$date, na.rm = TRUE)
n_industries <- n_distinct(edhealth$industry_name)
cat(sprintf("Ed & Health deep dive: %d sub-industries found.\n", n_industries))

# ============================================================
# CHART 1: Stacked monthly job gains by sub-industry, 2024-present
# Where within ed & health is the growth?
# ============================================================

chart1_df <- edhealth %>%
  filter(date >= "2024-01-01", !is.na(monthly_change))

MI_dates1 <- date_breaks_n(chart1_df$date, 6)

chart1_df %>%
  ggplot(aes(x = date, y = monthly_change, fill = industry_name)) +
  geom_col(position = "stack") +
  scale_x_date(breaks = MI_dates1, date_labels = "%b\n%Y") +
  labs(
    title = "Education & Health Job Gains by Sub-Industry",
    subtitle = "Monthly change in employment, thousands. All sub-industries within supersector.",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "BLS CES, seasonally adjusted. Mike Konczal, Economic Security Project."
  ) +
  theme_esp() +
  theme(
    plot.title = element_text(size = 22, face = "bold", color = positive_color),
    plot.subtitle = element_text(size = 14, color = positive_color),
    plot.caption = element_text(size = 10, color = "grey40"),
    legend.position = "right",
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(ncol = 1))

ggsave(
  "graphics/15a_edhealth_stacked_monthly.png",
  dpi = "retina",
  width = 14,
  height = 7.5,
  units = "in"
)

# ============================================================
# CHART 2: Average monthly job gains, 2022-2024 vs 2025+
# Which sub-industries accelerated or slowed?
# ============================================================

chart2_df <- edhealth %>%
  filter(!is.na(monthly_change)) %>%
  mutate(
    period = case_when(
      date >= "2025-01-01" ~ "2025-2026",
      date >= "2022-01-01" & date < "2025-01-01" ~ "2022-2024",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(industry_name, period) %>%
  summarize(
    avg_monthly_gain = mean(monthly_change, na.rm = TRUE),
    .groups = "drop"
  )

chart2_df %>%
  ggplot(aes(
    x = reorder(industry_name, avg_monthly_gain),
    y = avg_monthly_gain,
    fill = period
  )) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = c("2022-2024" = "grey60", "2025-2026" = "#B23A48")
  ) +
  coord_flip() +
  labs(
    title = "Where Did Education & Health Job Growth Change?",
    subtitle = "Average monthly job gain (thousands) by sub-industry.",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "BLS CES, seasonally adjusted. Mike Konczal, Economic Security Project."
  ) +
  theme_esp() +
  theme(
    plot.title = element_text(size = 22, face = "bold", color = positive_color),
    plot.subtitle = element_text(size = 14, color = positive_color),
    plot.caption = element_text(size = 10, color = "grey40"),
    legend.position = "top",
    axis.text.y = element_text(size = 9)
  )

ggsave(
  "graphics/15b_edhealth_period_comparison.png",
  dpi = "retina",
  width = 13,
  height = 10,
  units = "in"
)

# ============================================================
# CHART 3: Cumulative job growth indexed to Jan 2022
# Shows the trajectory of each sub-industry
# ============================================================

index_date <- as.Date("2022-01-01")

chart3_df <- edhealth %>%
  group_by(industry_name) %>%
  filter(any(date == index_date)) %>%
  mutate(
    base_value = value[date == index_date],
    indexed = value - base_value
  ) %>%
  ungroup() %>%
  filter(date >= index_date)

chart3_labels <- chart3_df %>%
  filter(date == max(date)) %>%
  mutate(label = paste0(industry_name, "  ", sprintf("%+.0f", indexed), "K"))

MI_dates3 <- date_breaks_n(chart3_df$date, 6)

chart3_df %>%
  ggplot(aes(x = date, y = indexed, color = industry_name)) +
  geom_line(linewidth = 1.1, show.legend = FALSE) +
  geom_text(
    data = chart3_labels,
    aes(x = date + days(15), y = indexed, label = label, color = industry_name),
    hjust = 0,
    size = 3,
    show.legend = FALSE
  ) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  scale_x_date(
    breaks = MI_dates3,
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.30))
  ) +
  labs(
    title = "Cumulative Job Growth Within Education & Health Since Jan 2022",
    subtitle = "Change in employment level (thousands) relative to January 2022.",
    x = NULL,
    y = NULL,
    caption = "BLS CES, seasonally adjusted. Mike Konczal, Economic Security Project."
  ) +
  coord_cartesian(clip = "off") +
  theme_esp() +
  theme(
    plot.title = element_text(size = 22, face = "bold", color = positive_color),
    plot.subtitle = element_text(size = 14, color = positive_color),
    plot.caption = element_text(size = 10, color = "grey40")
  )

ggsave(
  "graphics/15c_edhealth_cumulative.png",
  dpi = "retina",
  width = 14,
  height = 9,
  units = "in"
)

# ============================================================
# CHART 4: Share of supersector job gains by sub-industry
# Rolling 3-month avg to reduce noise
# ============================================================

chart4_df <- edhealth %>%
  filter(date >= "2023-01-01", !is.na(monthly_change)) %>%
  arrange(industry_name, date) %>%
  group_by(industry_name) %>%
  mutate(rolling3 = (value - lag(value, 3)) / 3) %>%
  ungroup() %>%
  filter(!is.na(rolling3)) %>%
  group_by(date) %>%
  mutate(share = rolling3 / sum(rolling3)) %>%
  ungroup()

MI_dates4 <- date_breaks_n(chart4_df$date, 6)

chart4_df %>%
  ggplot(aes(x = date, y = share, fill = industry_name)) +
  geom_area(position = "stack") +
  geom_hline(yintercept = c(0, 1), color = "grey40", linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(breaks = MI_dates4, date_labels = "%b\n%Y") +
  labs(
    title = "Which Sub-Industries Are Driving Education & Health Growth?",
    subtitle = "Share of 3-month average job gains within the supersector.",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "BLS CES, seasonally adjusted. Mike Konczal, Economic Security Project."
  ) +
  theme_esp() +
  theme(
    plot.title = element_text(size = 22, face = "bold", color = positive_color),
    plot.subtitle = element_text(size = 14, color = positive_color),
    plot.caption = element_text(size = 10, color = "grey40"),
    legend.position = "right",
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(ncol = 1))

ggsave(
  "graphics/15d_edhealth_share_of_growth.png",
  dpi = "retina",
  width = 14,
  height = 7.5,
  units = "in"
)

# ============================================================
# CHART 5: YoY % growth by sub-industry, faceted
# Are any sub-industries actually accelerating?
# ============================================================

chart5_df <- edhealth %>%
  filter(date >= "2018-01-01", !is.na(yoy_pct)) %>%
  mutate(
    yoy_pct = if_else(date >= "2020-03-01" & date <= "2021-12-01", NA, yoy_pct)
  )

MI_dates5 <- date_breaks_n(chart5_df$date, 4)

chart5_df %>%
  ggplot(aes(x = date, y = yoy_pct)) +
  geom_line(linewidth = 0.9, color = positive_color) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  facet_wrap(~industry_name, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_x_date(breaks = MI_dates5, date_labels = "%b\n%Y") +
  labs(
    title = "Year-Over-Year Employment Growth by Sub-Industry",
    subtitle = "Education & Health Services supersector. 2020-2021 removed.",
    x = NULL,
    y = NULL,
    caption = "BLS CES, seasonally adjusted. Mike Konczal, Economic Security Project."
  ) +
  theme_esp(base_size = 12) +
  theme(
    plot.title = element_text(size = 22, face = "bold", color = positive_color),
    plot.subtitle = element_text(size = 14, color = positive_color),
    plot.caption = element_text(size = 10, color = "grey40"),
    strip.text = element_text(size = 9, face = "bold"),
    axis.text = element_text(size = 7)
  )

ggsave(
  "graphics/15e_edhealth_yoy_faceted.png",
  dpi = "retina",
  width = 16,
  height = 12,
  units = "in"
)
