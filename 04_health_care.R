library(patchwork)

bls_set_key(Sys.getenv("BLS_KEY"))

positive_color <- "#2c3254" # Bright blue
esp_bg <- "#f4f2e4"

liberation_day <- as.Date("2025-04-01")
display_start <- as.Date("2025-01-01")

jobs_mix <- get_n_series_table(
  c(
    "CES6562000001",
    "CES0500000001",
    "CES0500000010",
    "CES0600000001",
    "CES4300000001",
    "CES4422000001",
    "CES0000000001",
    "CES0000000010"
  ),
  api_key = bls_get_key(),
  start_year = 2020,
  end_year = year(Sys.Date()),
  tidy = TRUE
)

jobs_mix <- jobs_mix %>%
  mutate(
    date = as.Date(paste0(year, "/", month, "/", 1)),
    total_private = CES0500000001 - lag(CES0500000001, 1),
    women_private = CES0500000010 - lag(CES0500000010, 1),
    men_private = total_private - women_private,
    total_nonfarm = CES0000000001 - lag(CES0000000001, 1),
    health_care = CES6562000001 - lag(CES6562000001, 1),
    women = CES0000000010 - lag(CES0000000010, 1),
    men = total_nonfarm - women,
    blue_collar = CES0600000001 + CES4422000001 + CES4300000001,
    blue_collar = blue_collar - lag(blue_collar, 1),
    other_private = total_private - health_care - blue_collar
  )

MI_dates <- date_breaks_n(jobs_mix$date, 6)

private_shares_since_liberation <- jobs_mix %>%
  filter(date >= liberation_day) %>%
  summarize(
    health_share = sum(health_care, na.rm = TRUE) / sum(total_private, na.rm = TRUE),
    women_share = sum(women_private, na.rm = TRUE) / sum(total_private, na.rm = TRUE)
  )

overall_shares_since_liberation <- jobs_mix %>%
  filter(date >= liberation_day) %>%
  summarize(
    health_share = sum(health_care, na.rm = TRUE) / sum(total_nonfarm, na.rm = TRUE),
    women_share = sum(women, na.rm = TRUE) / sum(total_nonfarm, na.rm = TRUE)
  )

health_plot_df <- jobs_mix %>%
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
      other_private = "All Other Private Jobs"
    )
  )

health_plot <- health_plot_df %>%
  ggplot(aes(x = date, y = value, fill = type)) +
  geom_vline(
    xintercept = liberation_day,
    color = "#B23A48",
    linetype = "dotted",
    linewidth = 1
  ) +
  geom_col(position = "stack") +
  geom_text(
    data = health_plot_df,
    aes(label = comma(round(value))),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4.2
  ) +
  scale_fill_manual(
    breaks = c(
      "Health Care and Social Assistance",
      "Blue-Collar Industries",
      "All Other Private Jobs"
    ),
    values = c(
      "Health Care and Social Assistance" = "#1B7F5A",
      "Blue-Collar Industries" = positive_color,
      "All Other Private Jobs" = "#6A3D9A"
    )
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = NULL
  ) +
  theme_esp() +
  theme(plot.background = element_rect(fill = esp_bg, color = NA)) +
  theme(legend.position = "top") +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates)

gender_plot_df <- jobs_mix %>%
  select(date, women_private, men_private) %>%
  filter(date >= display_start) %>%
  pivot_longer(
    c(men_private, women_private),
    names_to = "gender",
    values_to = "jobs"
  ) %>%
  mutate(gender = recode(gender, men_private = "Men", women_private = "Women"))

gender_plot <- gender_plot_df %>%
  ggplot(aes(x = date, y = jobs, fill = gender)) +
  geom_vline(
    xintercept = liberation_day,
    color = "#B23A48",
    linetype = "dotted",
    linewidth = 1
  ) +
  geom_col(position = "stack") +
  geom_text(
    data = gender_plot_df,
    aes(label = comma(round(jobs))),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4.2
  ) +
  scale_fill_manual(values = c("Men" = "#2c3254", "Women" = "#ff8361")) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = NULL
  ) +
  theme_esp() +
  theme(plot.background = element_rect(fill = esp_bg, color = NA)) +
  theme(legend.position = "top") +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates)

combined_title <- sprintf(
  "Private sector since Liberation Day: %s of job gains went to health care; %s went to women",
  scales::percent(private_shares_since_liberation$health_share, accuracy = 1),
  scales::percent(private_shares_since_liberation$women_share, accuracy = 1)
)

combined_subtitle <- sprintf(
  "Overall: %s of job gains went to health care; %s went to women\nMonthly job gains, Current Employment Statistics, private sector only.",
  scales::percent(overall_shares_since_liberation$health_share, accuracy = 1),
  scales::percent(overall_shares_since_liberation$women_share, accuracy = 1)
)

combined_graphic <- health_plot + gender_plot +
  plot_annotation(
    title = combined_title,
    subtitle = combined_subtitle,
    caption = "CES, seasonally adjusted. Liberation Day marked by dotted red line.\nBlue-collar: mining, logging, construction, manufacturing, transportation, warehousing, and utilities (definition via Joey Politano). Mike Konczal, Economic Security Project.",
    theme = theme(
      plot.title = element_text(size = 24, face = "bold", color = positive_color),
      plot.subtitle = element_text(size = 16, color = positive_color),
      plot.caption = element_text(size = 11, color = "grey40")
      ,
      plot.background = element_rect(fill = esp_bg, color = NA)
    )
  )

health_plot

ggsave(
  "graphics/04_health_care.png",
  plot = health_plot,
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

gender_plot

ggsave(
  "graphics/02e_gender.png",
  plot = gender_plot,
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

combined_graphic

ggsave(
  "graphics/04_health_care_gender_combined.png",
  plot = combined_graphic,
  dpi = "retina",
  width = 16,
  height = 7.5,
  units = "in"
)
