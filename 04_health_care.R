positive_color <- "#2c3254" # Bright blue
negative_color <- "#ff8361" # Pale violet

liberation_day <- as.Date("2025-04-01")
display_start <- as.Date("2024-01-01")

health_jobs <- get_n_series_table(
  c(
    "CES6562000001",
    "CES0600000001",
    "CES4300000001",
    "CES0500000001",
    "CES4422000001"
  ),
  api_key = bls_get_key(),
  start_year = 2020,
  end_year = 2025,
  tidy = TRUE
)

health_jobs <- health_jobs %>%
  mutate(
    date = as.Date(paste0(year, "/", month, "/", 1)),
    ces_private = CES0500000001 - lag(CES0500000001, 1),
    health_care = CES6562000001 - lag(CES6562000001, 1),
    blue_collar = CES0600000001 + CES4422000001 + CES4300000001,
    blue_collar = blue_collar - lag(blue_collar, 1),
    other_private = ces_private - health_care - blue_collar
  )

# Private versus nonfederal jobs -----

MI_dates <- date_breaks_n(health_jobs$date, 6)

health_share_since_liberation <- health_jobs %>%
  filter(date >= liberation_day) %>%
  summarize(
    share = sum(health_care, na.rm = TRUE) / sum(ces_private, na.rm = TRUE)
  ) %>%
  pull(share)

title_health_jobs <- sprintf(
  "%s of All Private Jobs Since Liberation Day Are Health Care",
  scales::percent(health_share_since_liberation, accuracy = 1)
)

blue_collar_change_since_liberation <- health_jobs %>%
  filter(!is.na(blue_collar), date >= liberation_day) %>%
  summarize(
    change = sum(blue_collar, na.rm = TRUE),
    total_gains = sum(ces_private, na.rm = TRUE),
    share_of_gains = change / total_gains
  )

dec_2024_change <- health_jobs %>%
  filter(year == 2024) %>%
  summarize(
    health_care_change = sum(health_care, na.rm = TRUE),
    blue_collar_change = sum(blue_collar, na.rm = TRUE),
    total_private_change = sum(ces_private, na.rm = TRUE),
    health_share = health_care_change / total_private_change,
    blue_collar_pct = blue_collar_change / total_private_change
  )

subtitle_health_jobs <- sprintf(
  "Total private jobs, CES. Since Apr 2025, blue-collar jobs gained %s jobs (%s of total gains).\nDec 2024 vs Dec 2023: Health care and social assistance %s of gains; blue-collar jobs %s.",
  scales::comma(round(blue_collar_change_since_liberation$change * 1000)),
  scales::percent(
    blue_collar_change_since_liberation$share_of_gains,
    accuracy = 0.1
  ),
  scales::percent(dec_2024_change$health_share, accuracy = 0.1),
  scales::percent(dec_2024_change$blue_collar_pct, accuracy = 0.1)
)

plot_df <- health_jobs %>%
  select(date, health_care, blue_collar, other_private) %>%
  filter(date >= display_start) %>%
  pivot_longer(
    c(health_care, blue_collar, other_private),
    names_to = "type",
    values_to = "value"
  ) %>%
  mutate(
    type = recode(
      type,
      health_care = "Health Care and Social Assistance",
      blue_collar = "Blue-Collar Industries",
      other_private = "All Other Private Sector Jobs"
    )
  )


plot_df %>%
  ggplot(aes(x = date, y = value, fill = type)) +
  geom_col(position = "stack") +
  geom_text(
    data = plot_df %>% filter(year(date) == 2025),
    aes(label = comma(round(value))),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5
  ) +
  scale_fill_manual(
    breaks = c(
      "Health Care and Social Assistance",
      "Blue-Collar Industries",
      "All Other Private Sector Jobs"
    ),
    values = c(
      "Health Care and Social Assistance" = "#1B7F5A",
      "Blue-Collar Industries" = positive_color,
      "All Other Private Sector Jobs" = "#6A3D9A"
    )
  ) +
  labs(
    title = title_health_jobs,
    subtitle = subtitle_health_jobs,
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "CES. Total private. Blue-collar: mining, logging, construction, manufacturing, transportation, warehousing, and utilities (definition via Joey Politano). Mike Konczal, Economic Security Project."
  ) +
  theme_esp() +
  theme(legend.position = "top") +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates)

ggsave(
  "graphics/04_health_care.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)
