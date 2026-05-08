# Wage dispersion across diffusion index industries
# Uses data_type_code == 3 (average hourly earnings) for the ~250
# diffusion sub-industries to show whether wage growth is compressing
# or whether some industries are pulling away.

diffusion_industries <- cesDiffusionIndex

wage_data <- ces_data %>%
  filter(
    data_type_code == 3,
    seasonal == "S",
    industry_code %in% diffusion_industries$industry_code
  ) %>%
  arrange(industry_name, date) %>%
  group_by(industry_name) %>%
  mutate(
    wage_yoy = value / lag(value, 12) - 1
  ) %>%
  ungroup() %>%
  filter(!is.na(wage_yoy))

# Compute cross-industry distribution of YoY wage growth each month
wage_dispersion <- wage_data %>%
  group_by(date) %>%
  summarize(
    p10 = quantile(wage_yoy, 0.10, na.rm = TRUE),
    p25 = quantile(wage_yoy, 0.25, na.rm = TRUE),
    p50 = quantile(wage_yoy, 0.50, na.rm = TRUE),
    p75 = quantile(wage_yoy, 0.75, na.rm = TRUE),
    p90 = quantile(wage_yoy, 0.90, na.rm = TRUE),
    sd  = sd(wage_yoy, na.rm = TRUE),
    n   = n(),
    .groups = "drop"
  )

graph_start <- as.Date("2016-01-01")

plot_df <- wage_dispersion %>%
  filter(date >= graph_start) %>%
  # Blank out pandemic distortion
  mutate(across(p10:sd, ~ if_else(date >= "2020-03-01" & date <= "2021-12-01", NA, .x)))

MI_dates_wd <- date_breaks_n(plot_df$date, 6)

# --- Main fan chart: p10-p90, p25-p75, median ---
plot_df %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = p10, ymax = p90), fill = "#2c3254", alpha = 0.15) +
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#2c3254", alpha = 0.30) +
  geom_line(aes(y = p50), color = "#2c3254", linewidth = 1.4) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(breaks = MI_dates_wd, date_labels = "%b\n%Y") +
  labs(
    title = "Wage Growth Dispersion Across Industries",
    subtitle = "Year-over-year average hourly earnings growth, ~250 CES diffusion sub-industries.\nDark band: 25th-75th percentile. Light band: 10th-90th percentile. Line: median. 2020-2021 removed.",
    x = NULL,
    y = NULL,
    caption = "BLS CES, seasonally adjusted, data_type_code 03 (average hourly earnings). Mike Konczal, Economic Security Project."
  ) +
  theme_esp() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption.position = "plot"
  )

ggsave(
  "graphics/14_wage_dispersion.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

# --- Top and bottom movers table for latest month ---
latest_wage_month <- max(wage_data$date)

wage_latest <- wage_data %>%
  filter(date == latest_wage_month) %>%
  select(industry_name, wage_yoy, value) %>%
  arrange(desc(wage_yoy))

cat("\n--- Fastest wage growth (top 15) ---\n")
print(head(wage_latest, 15))
cat("\n--- Slowest wage growth (bottom 15) ---\n")
print(tail(wage_latest, 15))

write_csv(wage_latest, "data/14_wage_dispersion_latest.csv")
