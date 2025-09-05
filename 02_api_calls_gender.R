library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)

gender <- unrate %>%
  mutate(
    diff_total = CES0000000001 - lag(CES0000000001, 1),
    diff_women = CES0000000010 - lag(CES0000000010, 1),
    diff_men   = diff_total - diff_women,
    share_women = diff_women / diff_total,
    date = as.Date(paste0(year,"/",month,"/",1))
  )

MI_dates <- date_breaks_n(gender$date, 6)

# ---- Compute shares for 2025 YTD and 2023–2024 combined ----
shares <- gender %>%
  mutate(period = case_when(
    year(date) %in% c(2023, 2024) ~ "2023–2024",
    year(date) >= 2025 ~ "2025",
    year(date) >= 2012 & year(date) <= 2019 ~ "2012-2019",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  summarise(
    women = sum(diff_women, na.rm = TRUE),
    total = sum(diff_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(share = ifelse(total == 0, NA_real_, women / total))

share_2324 <- shares %>% filter(period == "2023–2024") %>% pull(share)
share_2025  <- shares %>% filter(period == "2025") %>% pull(share)
share_0019  <- shares %>% filter(period == "2012-2019") %>% pull(share)

subtitle_text <- paste0(
  "CES Data, Total Nonfarm. Thousands. Women gained ",
  percent(share_2025, accuracy = 0.1),
  " of net job in 2025, YTD, vs ",
  percent(share_2324, accuracy = 0.1),
  " in 2023–2024."
)

# ---- Plot ----
plot_df <- gender %>%
  filter(year(date) >= 2023) %>%
  pivot_longer(c(diff_men, diff_women),
               names_to = "gender",
               values_to = "jobs") %>%
  mutate(gender = recode(gender,
                         diff_men = "Men",
                         diff_women = "Women"))



plot_df %>%
  ggplot(aes(x = date, y = jobs, fill = gender)) +
  geom_col(position = "stack") +
  geom_text(
    data = plot_df %>% filter(year(date) == 2025),
    aes(label = comma(round(jobs))),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.2
  ) +
  scale_fill_manual(values = c("Men" = "#2c3254", "Women" = "#ff8361")) +
  labs(
    title = "Monthly Change in Payroll Jobs by Gender",
    subtitle = subtitle_text,
    x = NULL, y = NULL,
    fill = NULL,
    caption = "CES. Seasonally-adjusted. Total nonfarm. Mike Konczal."
  ) +
  theme_esp() +
  theme(legend.position = "right") +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates)

ggsave("graphics/g5_gender.png", dpi = "retina", width = 12, height = 6.75, units = "in")


gender_description <- gender %>%
  filter(date >= "2025-05-01") %>%
  reframe(diff_men = sum(diff_men),
diff_total = sum(diff_total))

print(paste0("Men have gained ", gender_description$diff_men, " out of a total of ", gender_description$diff_total, " jobs since May, 2025."))