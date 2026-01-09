library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

# Filter to Total nonfarm and Government
jobs_data <- ces_data %>%
  filter(data_type_code == 1,
         seasonal == "S",
         industry_name %in% c("Total nonfarm", "Government")) %>%
  arrange(industry_name, date)

# Pivot to wide format for diff calculation
jobs_wide <- jobs_data %>%
  select(date, industry_name, value) %>%
  tidyr::pivot_wider(names_from = industry_name, values_from = value) %>%
  arrange(date) %>%
  mutate(
    total_diff = `Total nonfarm` - lag(`Total nonfarm`),
    gov_diff = Government - lag(Government),
    gov_share = gov_diff / total_diff
  )

# Plot
jobs_wide %>%
  filter(year(date) >= 2022) %>%
ggplot(aes(x = date, y = gov_share)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Government Share of Total Monthly Job Gains",
    subtitle = "Based on CES 1-month changes, seasonally adjusted",
    x = NULL,
    y = "Share of Monthly Job Gains from Government",
    caption = "Source: BLS Current Employment Statistics (CES)"
  ) +
  theme_minimal(base_size = 14)