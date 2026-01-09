library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)


graphic_title_revisions <- "Job Revisions Turned Negative in Recent Months"
how_many_months <- 8

revisions_df <- read_csv("data/bls_ces_monthly_revisions.csv")
revisions_df <- revisions_df %>%
  mutate(
    non_na_count = rowSums(
      !is.na(select(., starts_with("sa_"), starts_with("nsa_")))
    )
  ) %>%
  arrange(year, month_num, non_na_count) %>%
  group_by(year, month_num) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(-non_na_count)

# Latest 2025 month with any estimate
latest_m <- revisions_df %>%
  filter(year == 2025) %>%
  filter(if_any(starts_with("sa_"), ~ !is.na(.))) %>%
  summarise(latest = max(month_num, na.rm = TRUE)) %>%
  pull(latest)

start_m <- max(1, latest_m - how_many_months + 1)

revisions_long <- revisions_df %>%
  filter(year == 2025, month_num >= start_m, month_num <= latest_m) %>%
  transmute(
    month = factor(
      month_num,
      levels = start_m:latest_m,
      labels = month.abb[start_m:latest_m],
      ordered = TRUE
    ),
    sa_1st,
    sa_2nd,
    sa_3rd
  ) %>%
  pivot_longer(starts_with("sa_"), names_to = "estimate", values_to = "jobs")

# Use the SAME dodging for bars and labels (centers labels)
pos <- position_dodge2(width = 0.65, preserve = "single", padding = 0)

ggplot(revisions_long, aes(x = month, y = jobs, fill = estimate)) +
  geom_col(position = pos, width = 0.65, na.rm = TRUE) +
  geom_text(
    aes(label = scales::comma(jobs), group = estimate),
    position = pos,
    vjust = -0.25,
    size = 5,
    na.rm = TRUE,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "sa_1st" = "#2c3254",
      "sa_2nd" = "#70ad8f",
      "sa_3rd" = "#ff8361"
    ),
    labels = c("1st Estimate", "2nd Estimate", "3rd Estimate"),
    name = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(
    subtitle = "Total Nonfarm Jobs: 1st, 2nd, and 3rd Estimates",
    title = graphic_title_revisions,
    x = "The Year 2025",
    y = "Jobs (thousands)",
    caption = "Source: BLS. Mike Konczal, Economic Security Project."
  ) +
  theme_esp() +
  theme(legend.position = "top", axis.text = ggplot2::element_text(size = 20))

ggsave(
  "graphics/01_job_revisions.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)
