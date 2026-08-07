bls_set_key(Sys.getenv("BLS_KEY"))

positive_color <- "#2c3254" # Bright blue
esp_bg <- "#f4f2e4"

liberation_day <- as.Date("2025-04-01")
display_start <- as.Date("2025-01-01")

# Gender chart is anchored to the start of the term, not Liberation Day.
term_start <- as.Date("2025-01-01")
term_start_label <- format(term_start, "%B %Y")

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
    other_jobs = total_nonfarm - health_care - blue_collar
  )

MI_dates <- date_breaks_n(jobs_mix$date, 6)

overall_shares_since_liberation <- jobs_mix %>%
  filter(date >= liberation_day) %>%
  summarize(
    health_share = sum(health_care, na.rm = TRUE) /
      sum(total_nonfarm, na.rm = TRUE),
    women_share = sum(women, na.rm = TRUE) / sum(total_nonfarm, na.rm = TRUE)
  )

private_shares_since_term <- jobs_mix %>%
  filter(date >= term_start) %>%
  summarize(
    women_share = sum(women_private, na.rm = TRUE) /
      sum(total_private, na.rm = TRUE)
  )

overall_shares_since_term <- jobs_mix %>%
  filter(date >= term_start) %>%
  summarize(
    women_share = sum(women, na.rm = TRUE) / sum(total_nonfarm, na.rm = TRUE)
  )

health_plot_df <- jobs_mix %>%
  select(date, health_care, blue_collar, other_jobs) %>%
  filter(date >= display_start) %>%
  pivot_longer(
    c(health_care, blue_collar, other_jobs),
    names_to = "type",
    values_to = "value"
  ) %>%
  mutate(
    type = recode(
      type,
      health_care = "Health Care and Social Assistance",
      blue_collar = "Blue-Collar Industries",
      other_jobs = "All Other Jobs"
    )
  )

health_title <- sprintf(
  "Since Liberation Day: %s of all job gains went to health care",
  scales::percent(overall_shares_since_liberation$health_share, accuracy = 1)
)

health_title <- "Broader Job Growth in 2026"

health_subtitle <- "Monthly job gains, Current Employment Statistics, total nonfarm."

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
      "All Other Jobs"
    ),
    values = c(
      "Health Care and Social Assistance" = "#1B7F5A",
      "Blue-Collar Industries" = positive_color,
      "All Other Jobs" = "#6A3D9A"
    )
  ) +
  labs(
    title = health_title,
    subtitle = health_subtitle,
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "CES, seasonally adjusted. Liberation Day marked by dotted red line.\nBlue-collar: mining, logging, construction, manufacturing, transportation, warehousing, and utilities (definition via Joey Politano). Mike Konczal, Economic Security Project."
  ) +
  theme_esp() +
  theme(
    plot.title = element_text(size = 24, face = "bold", color = positive_color),
    plot.subtitle = element_text(size = 16, color = positive_color),
    plot.caption = element_text(size = 11, color = "grey40"),
    plot.background = element_rect(fill = esp_bg, color = NA),
    legend.position = "top"
  ) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates)

# ── Two-line share charts: gender, and health care ───────────────────────

gender_colors <- c("Men" = "#2c3254", "Women" = "#ff8361")
health_colors <- c("All Other Jobs" = "#2c3254", "Health Care" = "#1B7F5A")

# Trend panel reaches back far enough to show the two series moving together
# before they split.
trend_start <- as.Date("2023-01-01")
share_base_date <- term_start %m-% months(1)

nonfarm_levels <- jobs_mix %>%
  arrange(date) %>%
  transmute(
    date,
    total_level = CES0000000001,
    women_level = CES0000000010,
    men_level = CES0000000001 - CES0000000010,
    health_level = CES6562000001,
    other_level = CES0000000001 - CES6562000001
  )

share_base <- nonfarm_levels %>% filter(date == share_base_date)

if (nrow(share_base) != 1) {
  stop("No ", format(share_base_date, "%B %Y"), " baseline for the share charts.")
}

share_latest <- nonfarm_levels %>% slice_max(date, n = 1)

# A share of a NET change exceeds 100% whenever the other category loses jobs
# over the window. Health care is at ~150% since the term began because
# everything outside it shed 337k. That is intended: print the number as-is and
# let the two lines on the chart show why it is above 100.
share_text <- function(share) {
  if (is.na(share)) {
    return("an unknown share")
  }
  scales::percent(share, accuracy = 1)
}

share_of_change <- function(part_col) {
  (share_latest[[part_col]] - share_base[[part_col]]) /
    (share_latest$total_level - share_base$total_level)
}

share_of_12m <- function(part_col) {
  twelve_ago <- nonfarm_levels %>%
    filter(date == share_latest$date %m-% months(12))
  if (nrow(twelve_ago) != 1) {
    return(NA_real_)
  }
  (share_latest[[part_col]] - twelve_ago[[part_col]]) /
    (share_latest$total_level - twelve_ago$total_level)
}

# Builds the 12-month-change and cumulative-since-base frames for one pairing.
build_share_frames <- function(a_col, a_name, b_col, b_name) {
  twelve <- nonfarm_levels %>%
    mutate(
      !!a_name := .data[[a_col]] - lag(.data[[a_col]], 12),
      !!b_name := .data[[b_col]] - lag(.data[[b_col]], 12)
    ) %>%
    filter(date >= trend_start, !is.na(.data[[a_name]])) %>%
    select(date, all_of(c(a_name, b_name))) %>%
    pivot_longer(-date, names_to = "series", values_to = "jobs")

  cumulative <- nonfarm_levels %>%
    filter(date >= share_base_date) %>%
    transmute(
      date,
      !!a_name := .data[[a_col]] - share_base[[a_col]],
      !!b_name := .data[[b_col]] - share_base[[b_col]]
    ) %>%
    pivot_longer(-date, names_to = "series", values_to = "jobs")

  list(twelve = twelve, cumulative = cumulative)
}

end_labels <- function(df, signed = FALSE) {
  df %>%
    group_by(series) %>%
    slice_max(date, n = 1) %>%
    ungroup() %>%
    mutate(
      label = paste0(
        series,
        ": ",
        if (signed) if_else(jobs >= 0, "+", "") else "",
        comma(round(jobs)),
        "k"
      )
    )
}

two_line_chart <- function(df, colors, title, subtitle, caption,
                           break_by = "-6 months", signed = FALSE,
                           nudge = 45, right_pad = 0.22,
                           min_segment = 0.9) {
  ends <- end_labels(df, signed = signed)

  ggplot(df, aes(date, jobs, color = series)) +
    geom_hline(yintercept = 0, color = "grey30", linewidth = 0.4) +
    geom_line(linewidth = 1.7) +
    geom_point(data = ends, size = 3.2) +
    ggrepel::geom_text_repel(
      data = ends,
      aes(label = label),
      hjust = 0,
      nudge_x = nudge,
      direction = "y",
      seed = 42,
      size = 4.5,
      fontface = "bold",
      show.legend = FALSE,
      segment.color = "grey55",
      segment.size = 0.3,
      min.segment.length = min_segment
    ) +
    scale_color_manual(values = colors, guide = "none") +
    scale_y_continuous(labels = comma) +
    scale_x_date(
      date_labels = "%b\n%Y",
      breaks = seq(max(df$date), min(df$date), by = break_by),
      expand = expansion(mult = c(0.03, right_pad))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      caption = caption
    ) +
    theme_esp() +
    theme(
      plot.title = element_text(size = 22, face = "bold", color = positive_color),
      plot.subtitle = element_text(size = 14, color = positive_color),
      plot.caption = element_text(size = 11, color = "grey40"),
      plot.background = element_rect(fill = esp_bg, color = NA),
      panel.grid.major.x = element_blank()
    )
}

# ---- Gender ----

gender_frames <- build_share_frames(
  "men_level", "Men",
  "women_level", "Women"
)

gender_share_12m <- share_of_12m("women_level")
gender_share_term <- share_of_change("women_level")

gender_caption <- "CES, seasonally adjusted, total nonfarm employment. Mike Konczal, Economic Security Project."

gender_12m_plot <- two_line_chart(
  gender_frames$twelve,
  gender_colors,
  title = paste0(
    "Women Gained ",
    share_text(gender_share_12m),
    " of the Job Growth Over the Past Year"
  ),
  subtitle = paste0(
    "Since ", term_start_label, ", women gained ",
    share_text(gender_share_term), " of net job growth.\n",
    "Change in total employment over the prior 12 months, thousands of jobs."
  ),
  caption = gender_caption
)

gender_cum_plot <- two_line_chart(
  gender_frames$cumulative,
  gender_colors,
  title = paste0(
    "Since ", term_start_label, ", Women Gained ",
    share_text(gender_share_term),
    " of the Net Job Growth"
  ),
  subtitle = paste0(
    "Over the past year, women gained ",
    share_text(gender_share_12m), " of net job growth.\n",
    "Cumulative change in total employment since ",
    format(share_base_date, "%B %Y"), ", thousands of jobs."
  ),
  caption = gender_caption,
  break_by = "-3 months",
  signed = TRUE,
  nudge = 25,
  right_pad = 0.22
)

# ---- Health care and social assistance ----

health_frames <- build_share_frames(
  "other_level", "All Other Jobs",
  "health_level", "Health Care"
)

health_share_12m <- share_of_12m("health_level")
health_share_term <- share_of_change("health_level")

health_share_caption <- "CES, seasonally adjusted, total nonfarm employment. Health care and social assistance versus all other jobs. Mike Konczal, Economic Security Project."

health_12m_plot <- two_line_chart(
  health_frames$twelve,
  health_colors,
  title = paste0(
    "Health Care Accounted for ",
    share_text(health_share_12m),
    " of the Job Growth Over the Past Year"
  ),
  subtitle = paste0(
    "Since ", term_start_label, ", health care and social assistance accounted for ",
    share_text(health_share_term), " of net job growth.\n",
    "Change in total employment over the prior 12 months, thousands of jobs."
  ),
  caption = health_share_caption
)

health_cum_plot <- two_line_chart(
  health_frames$cumulative,
  health_colors,
  title = paste0(
    "Since ", term_start_label, ", Health Care Accounted for ",
    share_text(health_share_term),
    " of the Net Job Growth"
  ),
  subtitle = paste0(
    "Over the past year, health care and social assistance accounted for ",
    share_text(health_share_12m), " of net job growth.\n",
    "Cumulative change in total employment since ",
    format(share_base_date, "%B %Y"), ", thousands of jobs."
  ),
  caption = health_share_caption,
  break_by = "-3 months",
  signed = TRUE,
  nudge = 25,
  right_pad = 0.22
)

ggsave(
  "graphics/04_health_care.png",
  plot = health_plot,
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

ggsave(
  "graphics/04_gender_12m.png",
  plot = gender_12m_plot,
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

ggsave(
  "graphics/04_gender_cumulative.png",
  plot = gender_cum_plot,
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

ggsave(
  "graphics/04_health_care_12m.png",
  plot = health_12m_plot,
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

ggsave(
  "graphics/04_health_care_cumulative.png",
  plot = health_cum_plot,
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)
