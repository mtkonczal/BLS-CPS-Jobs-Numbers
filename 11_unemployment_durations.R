graphic_duration_title <- "Unemployment Duration Picks Up, Above 2024 Levels"

duration <- get_n_series_table(
  c(
    "LNS13008276",
    "LNS13008275"
  ),
  api_key = bls_get_key(),
  start_year = 2019,
  end_year = as.integer(format(Sys.Date(), "%Y")),
  tidy = TRUE
) %>%
  mutate(date = as.Date(paste0(year, "/", month, "/", 1))) %>%
  select(-year, -month) %>%
  pivot_longer(
    LNS13008276:LNS13008275,
    names_to = "type",
    values_to = "value"
  ) %>%
  mutate(
    type = if_else(
      type == "LNS13008276",
      "Median Weeks Unemployed",
      "Average Weeks Unemployed"
    )
  )

duration$value <- as.numeric(duration$value)

#### Graphic 1: Duration Length of Unemployment ####
g_dates <- duration %>% filter(date >= "2022-01-01")
g_dates <- unique(g_dates$date)
g_dates <- sort(g_dates, decreasing = TRUE)
g_dates <- g_dates[seq(1, length(g_dates), 6)]


duration %>%
  group_by(type) %>%
  mutate(pre_value = mean(value[year(date) == 2024])) %>%
  mutate(last_value = if_else(date == max(date), value, as.numeric(NA))) %>%
  ungroup() %>%
  mutate(pre_value = if_else(year(date) >= 2019, pre_value, as.numeric(NA))) %>%
  filter(date >= "2022-01-01") %>%
  ggplot(aes(date, value, color = type, label = last_value)) +
  geom_line(size = 1.2) +
  geom_point() +
  theme_esp() +
  geom_line(aes(date, pre_value, color = type), linetype = "dashed") +
  theme(legend.position = c(0.6, 0.9)) +
  scale_x_date(date_labels = "%B\n%Y", breaks = g_dates) +
  labs(
    title = graphic_duration_title,
    subtitle = "Average and median weeks of unemployment length, dotted line is average 2024 value.",
    caption = "BLS, CPS, Seasonally-Adjusted, Mike Konczal, Economic Security Project."
  ) +
  scale_color_manual(values = c("#2D779C", "#97BC56")) +
  geom_text(show.legend = FALSE, nudge_x = 90, size = 5.5)

ggsave("graphics/11_durations.png", width = 12, height = 9, dpi = "retina")
