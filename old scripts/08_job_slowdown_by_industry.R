library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(scales)

slowdown_title <- "Slowdown in Job Growth is Across Industries"

# Last available date within the same filter universe
max_date <- ces_data %>%
  filter(seasonal == "S", data_type_code == 1) %>%
  summarise(md = max(date, na.rm = TRUE), .groups = "drop") %>%
  pull(md)

max_month <- month(max_date)
month_spread <- (max_month - 3):max_month

industry_changes <- ces_data %>%
  filter(display_level == 2, seasonal == "S", data_type_code == 1) %>%
  group_by(industry_name) %>%
  mutate(change = value - lag(value)) %>%
  # last 4 months (by month-of-year) in each year since 2024
  filter(month(date) %in% month_spread, year(date) >= 2024) %>%
  group_by(industry_name, yr = year(date)) %>%
  summarise(avg_change = mean(change, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = yr, values_from = avg_change) %>%
  mutate(diff = `2025` - `2024`) %>%
  arrange(diff)

# Subtitle with proper date formatting
sub_txt <- sprintf(
  "Slowdown = 2025 minus 2024, averaged over the last 4 months of each year, ending %s",
  format(max_date, "%B %Y")
)

ggplot(industry_changes, aes(x = reorder(industry_name, diff), y = diff)) +
  geom_col(fill = "#2c3254") + # ESP navy
  coord_flip(clip = "off") +
  # Labels just OUTSIDE the bar ends:
  # - positive bars: to the RIGHT (hjust < 0)
  # - negative bars: to the LEFT  (hjust > 1)
  geom_text(
    aes(
      label = number(diff, accuracy = 0.1, big.mark = ","),
      hjust = ifelse(diff >= 0, -0.15, 1.15)
    ),
    color = "#2c3254",
    size = 3.6
  ) +
  # Extra room on both sides for outside labels
  scale_y_continuous(
    expand = expansion(mult = c(0.12, 0.12)),
    labels = label_number(big.mark = ",")
  ) +
  labs(
    title = slowdown_title,
    subtitle = sub_txt,
    x = NULL,
    y = "Difference (2025 − 2024), avg monthly jobs",
    caption = "Mike Konczal, Economic Security Project."
  ) +
  theme_esp() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 30, 10, 30) # extra room for outside labels
  )

ggsave(
  "graphics/g99_slowdown_by_industry.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)
