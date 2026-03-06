library(dplyr)
library(lubridate)
library(scales)
library(blsR)
library(ggplot2)
library(tidyr)
library(gt)

if (!exists("unrate")) {
  stop("Expected `unrate` from 02_unrate_jobs.R. Source that script first.")
}
if (!exists("revisions_df")) {
  stop(
    "Expected `revisions_df` from 01_revisions_estimate.R. Source that script first."
  )
}

# Latest unemployment change and level
latest_date <- unrate %>%
  summarize(latest_date = max(date, na.rm = TRUE)) %>%
  pull(latest_date)

unrate_latest <- unrate %>%
  arrange(date) %>%
  mutate(unrate_change = unrate - lag(unrate)) %>%
  filter(date == latest_date) %>%
  select(unrate, unrate_change, ces) %>%
  slice(1)

unrate_direction <- if_else(
  unrate_latest$unrate_change >= 0,
  "increased",
  "decreased"
)
unrate_change_text <- percent(abs(unrate_latest$unrate_change), accuracy = 0.01)
unrate_level_text <- percent(unrate_latest$unrate, accuracy = 0.1)
latest_month_label <- format(latest_date, "%B")

# Health care share of December job gains (health care vs total nonfarm)
health_private <- get_n_series_table(
  c("CES6562000001", "CES0500000001"),
  api_key = bls_get_key(),
  start_year = year(latest_date) - 2,
  end_year = year(latest_date),
  tidy = TRUE
) %>%
  mutate(
    date = as.Date(paste0(year, "/", month, "/", 1)),
    health_care_change = CES6562000001 - lag(CES6562000001, 1),
    private_change = CES0500000001 - lag(CES0500000001, 1)
  )

health_dec <- health_private %>%
  filter(date == latest_date) %>%
  summarize(health_care_change = first(health_care_change))

total_jobs_added <- unrate_latest$ces
health_share <- health_dec$health_care_change / total_jobs_added

jobs_added_text <- paste0(comma(round(total_jobs_added)), ",000")
health_share_text <- percent(health_share, accuracy = 0.1)

# Revisions for latest two months
rev_value <- revisions_df %>%
  mutate(
    rev_value = coalesce(sa_rev_3rd_minus_1st, sa_rev_2nd_minus_1st)
  ) %>%
  filter(!is.na(rev_value)) %>%
  arrange(date) %>%
  slice_tail(n = 2)

rev_sum <- sum(rev_value$rev_value, na.rm = TRUE)
rev_direction <- if_else(rev_sum < 0, "reduced", "added")
rev_text <- paste0(comma(abs(round(rev_sum))), ",000")

# Private sector monthly average over the last 12 months
private_12m <- health_private %>%
  filter(!is.na(private_change), date <= latest_date) %>%
  arrange(date) %>%
  slice_tail(n = 12)

avg_12m <- mean(private_12m$private_change, na.rm = TRUE)
avg_12m_text <- paste0(comma(round(avg_12m)), ",000")

paragraph <- paste0(
  "In ",
  latest_month_label,
  ", unemployment ",
  unrate_direction,
  " ",
  unrate_change_text,
  " to ",
  unrate_level_text,
  ". ",
  jobs_added_text,
  " total new jobs were added, with ",
  health_share_text,
  " being in health care. Revisions ",
  rev_direction,
  " jobs ",
  rev_text,
  " over the past two months.\n\n",
  "Across the last 12 months, the private sector added just ",
  avg_12m_text,
  " jobs a month. Let's dig in. /1"
)

# Federal vs blue-collar job losses since Dec 2024 ----
positive_color <- "#2c3254" # Bright blue
negative_color <- "#ff8361" # Pale violet

race_start <- as.Date("2025-01-01")
race_end <- as.Date("2026-01-01")

race_jobs <- get_n_series_table(
  c(
    "CES9091000001",
    "CES0600000001",
    "CES4300000001",
    "CES4422000001"
  ),
  api_key = bls_get_key(),
  start_year = 2024,
  end_year = 2026,
  tidy = TRUE
) %>%
  mutate(
    date = as.Date(paste0(year, "/", month, "/", 1)),
    blue_collar_level = CES0600000001 + CES4300000001 + CES4422000001
  )

race_baseline <- race_jobs %>%
  filter(date == race_start) %>%
  summarize(
    federal_base = first(CES9091000001),
    blue_collar_base = first(blue_collar_level)
  )

race_plot <- race_jobs %>%
  mutate(
    federal_change = CES9091000001 - race_baseline$federal_base,
    blue_collar_change = blue_collar_level - race_baseline$blue_collar_base
  ) %>%
  filter(date >= race_start, date <= race_end) %>%
  select(date, federal_change, blue_collar_change) %>%
  pivot_longer(
    c(federal_change, blue_collar_change),
    names_to = "type",
    values_to = "value"
  ) %>%
  mutate(
    type = recode(
      type,
      federal_change = "Federal Jobs",
      blue_collar_change = "Blue-Collar Jobs"
    )
  )

race_last <- race_plot %>%
  group_by(type) %>%
  slice_max(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    label = comma(round(value)),
    label_date = date + days(2)
  )

race_loss <- race_plot %>%
  group_by(type) %>%
  slice_max(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    baseline = if_else(
      type == "Federal Jobs",
      race_baseline$federal_base,
      race_baseline$blue_collar_base
    ),
    loss_pct = value / baseline,
    loss_label = percent(loss_pct, accuracy = 0.01)
  ) %>%
  select(type, loss_label) %>%
  pivot_wider(names_from = type, values_from = loss_label)

race_loss_line <- sprintf(
  "As a percent of jobs, across 2025 that was Federal %s; Blue-collar %s.",
  race_loss$`Federal Jobs`,
  race_loss$`Blue-Collar Jobs`
)

race_dates <- date_breaks_n(race_plot$date, 6)

race_plot %>%
  ggplot(aes(x = date, y = value, color = type)) +
  geom_hline(yintercept = 0, color = "grey75", linewidth = 0.9) +
  geom_line(linewidth = 1.4) +
  geom_point(data = race_last, size = 3) +
  geom_text(
    data = race_last,
    aes(x = label_date, y = value, label = label, color = type),
    hjust = 0,
    size = 8,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Federal Jobs" = negative_color,
      "Blue-Collar Jobs" = positive_color
    )
  ) +
  scale_x_date(
    date_labels = "%b\n%Y",
    breaks = race_dates,
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  labs(
    title = "Instead of Netting Out, Both Federal and Blue-Collar Workers Lost Jobs",
    subtitle = paste0(
      "Change since Dec 2024, indexed to zero. ",
      race_loss_line
    ),
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "CES, seasonally adjusted. Blue-collar: mining, logging, construction, manufacturing, transportation, warehousing, and utilities (definition via Joey Politano). Mike Konczal, Economic Security Project."
  ) +
  theme_esp() +
  theme(legend.position = "top")

ggsave(
  "graphics/03_federal_blue_collar_race.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

# Private payroll levels: yesterday vs today with revisions (gt table) ----
private_revision_wide <- tibble::tribble(
  ~series, ~yesterday, ~today,
  "Total Private December 2025", NA_real_, 136115,
  "Total Private November 2025", 136148, 136078,
  "Total Private January 2025", 135461, 135461,
  "Difference, Latest to Jan", 687, 654
) %>%
  mutate(series = factor(series, levels = rev(unique(series))))

private_revision_gt <- private_revision_wide %>%
  mutate(
    yesterday = if_else(is.na(yesterday), "", comma(yesterday)),
    today = if_else(is.na(today), "", comma(today))
  ) %>%
  gt(rowname_col = "series") %>%
  cols_label(
    yesterday = "As of Yesterday (1/8)",
    today = "As of Today With Revisions (1/9)"
  ) %>%
  tab_header(
    title = "Private Payroll Levels Were Revised Slightly Lower",
    subtitle = "Yesterday (1/8) vs today with revisions (1/9), thousands of jobs"
  ) %>%
  tab_source_note(
    source_note = "Source: ALFRED (FRED), CES, seasonally adjusted. Mike Konczal, Economic Security Project."
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      weight = px(2)
    ),
    locations = cells_body(
      rows = series == "Difference, Latest to Jan"
    )
  ) %>%
  tab_options(
    table.font.size = 16,
    data_row.padding = px(8),
    heading.title.font.size = 20,
    heading.subtitle.font.size = 14
  )

gtsave(
  private_revision_gt,
  "graphics/03_private_revisions_comparison_table.png",
  zoom = 2
)
