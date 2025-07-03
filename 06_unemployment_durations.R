
graphic_duration_title <- "Unemployment Duration Picks Up, Near 2019 Levels"


#### Graphic 1: Duration Length of Unemployment ####
g_dates <- cps_jobs_data %>% filter(date >= "2017-01-01")
g_dates <- unique(g_dates$date)
g_dates <- sort(g_dates, decreasing = TRUE)
g_dates <- g_dates[seq(1, length(g_dates), 12)]

u_duration_series <- c("(Seas) Median Weeks Unemployed", "(Seas) Average Weeks Unemployed")

cps_jobs_data %>%
  filter(series_title %in% u_duration_series, periodicity_code == "M") %>%
  group_by(series_title) %>%
  mutate(pre_value = mean(value[year(date) == 2019])) %>%
  mutate(last_value = if_else(date == max(date), value, as.numeric(NA))) %>%
  ungroup() %>%
  mutate(pre_value = if_else(year(date) >= 2019, pre_value, as.numeric(NA))) %>%
  filter(date >= "2017-01-01") %>%
  mutate(series_title = str_remove(series_title, "\\(Seas\\)")) %>%
  ggplot(aes(date, value, color = series_title, label = last_value)) +
  geom_line(size = 1.2) +
  geom_point() +
  theme_esp() +
  geom_line(aes(date, pre_value, color = series_title), linetype = "dashed") +
  theme(legend.position = c(0.3, 0.9)) +
  scale_x_date(date_labels = "%B\n%Y", breaks = g_dates) +
  labs(
    title = graphic_duration_title,
    subtitle = "Average and median weeks of unemployment length, dotted line is average 2019 value.",
    caption = "BLS, CPS, Seasonally-Adjusted, Mike Konczal."
  ) +
  scale_color_manual(values = c("#2D779C", "#97BC56")) +
  geom_text(show.legend = FALSE, nudge_x = 90, size = 5.5)

ggsave("graphics/g9_durations.png", width = 12, height = 9, dpi = "retina")
