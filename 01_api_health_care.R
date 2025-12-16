positive_color <- "#2c3254" # Bright blue
negative_color <- "#ff8361" # Pale violet

title_health_jobs <- "107% of All Jobs Since Liberation Day Are Health Care/Private Education"

health_jobs <- get_n_series_table(
  c(
    "CES6500000001",
    "CES0000000001",
    "CES0500000001"
  ),
  api_key = bls_get_key(),
  start_year = 2020,
  end_year = 2025,
  tidy = TRUE
)

health_jobs <- health_jobs %>%
  mutate(
    date = as.Date(paste0(year, "/", month, "/", 1)),
    ces = CES0500000001 - lag(CES0500000001, 1),
    health = CES6500000001 - lag(CES6500000001, 1),
    nonhealth = ces - health
  )

# Private versus nonfederal jobs -----

MI_dates <- date_breaks_n(health_jobs$date, 6)


health_jobs %>%
  tail(8) %>%
  summarize(percent_health = sum(health) / sum(ces) - 1)

plot_df <- health_jobs %>%
  select(date, health, nonhealth) %>%
  filter(year(date) >= 2024) %>%
  pivot_longer(
    c(health, nonhealth),
    names_to = "type",
    values_to = "value"
  ) %>%
  mutate(
    type = recode(
      type,
      health = "Private Education and Health Services",
      nonhealth = "All Other Private Sector Jobs"
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
    values = c(
      "Private Education and Health Services" = "#1B7F5A",
      "All Other Private Sector Jobs" = "#6A3D9A"
    )
  ) +
  labs(
    title = title_health_jobs,
    subtitle = "CES Data, Thousands, All Private Sector Jobs.",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "CES. Seasonally-adjusted. Total nonfarm. Mike Konczal, Economic Security Project."
  ) +
  theme_esp() +
  theme(legend.position = "right") +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates)

ggsave(
  "graphics/health_jobs.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)
