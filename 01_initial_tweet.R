library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(lubridate)
library(patchwork)
source("scripts/graphic_scripts.R")

positive_color <- "#2c3254"
negative_color <- "#ff8361"
green_color <- "#70ad8f"
esp_bg <- "#f4f2e4"

# Left panel shows the current calendar year. Early in the year that is too few
# bars to read a trend, so back-fill from the prior year up to this floor.
min_months_shown <- 6
ur_months_shown <- 24 # window for the unemployment rate panel
# Every month from here forward gets a printed value label. Anchored at the
# Nov 2025 peak so the panel reads as the decline off that peak; bump it
# forward when the labels start crowding.
ur_label_start <- as.Date("2025-11-01")

if (!exists("unrate")) {
  stop("Expected `unrate` to be loaded before sourcing 01_initial_tweet.R.")
}

if (!exists("revisions_df")) {
  stop(
    "Expected `revisions_df` to be loaded before sourcing 01_initial_tweet.R."
  )
}

# ── Left panel: Job revisions (1st, 2nd, 3rd estimates) ──────────────────

rev_available <- revisions_df %>%
  mutate(date = as.Date(date)) %>%
  filter(if_any(c(sa_1st, sa_2nd, sa_3rd), ~ !is.na(.))) %>%
  arrange(date)

focus_year <- year(max(rev_available$date))

months_this_year <- rev_available %>% filter(year(date) == focus_year)

plot_months <- if (nrow(months_this_year) >= min_months_shown) {
  months_this_year
} else {
  tail(rev_available, min_months_shown)
}

mixed_years <- n_distinct(year(plot_months$date)) > 1

revisions_long <- plot_months %>%
  transmute(
    date,
    month_label = if (mixed_years) {
      format(date, "%b\n%Y")
    } else {
      format(date, "%b")
    },
    sa_1st,
    sa_2nd,
    sa_3rd
  ) %>%
  mutate(month_label = factor(month_label, levels = month_label)) %>%
  pivot_longer(
    starts_with("sa_"),
    names_to = "estimate",
    values_to = "jobs"
  ) %>%
  mutate(
    estimate = factor(estimate, levels = c("sa_1st", "sa_2nd", "sa_3rd")),
    # Zero-height placeholders so months without a 2nd/3rd estimate still hold
    # their dodge slots; otherwise the newest month's lone bar drifts to center.
    jobs_bar = coalesce(jobs, 0)
  )

# Pace comparison, both from the current published CES level so they are
# apples-to-apples: prior year is Dec-over-Dec / 12, focus year is
# latest-month-over-prior-Dec / months elapsed. Neither is a 1st print, so the
# dashed line is not directly comparable to the individual 1st-estimate bars.
prior_year <- focus_year - 1

ces_level <- unrate %>%
  filter(!is.na(CES0000000001)) %>%
  select(date, CES0000000001)

ces_at <- function(d) {
  v <- ces_level$CES0000000001[ces_level$date == as.Date(d)]
  if (length(v) == 1) v else NA_real_
}

prior_dec <- ces_at(paste0(prior_year, "-12-01"))
prior_prior_dec <- ces_at(paste0(prior_year - 1, "-12-01"))
latest_ces_date <- max(ces_level$date)

prior_avg <- (prior_dec - prior_prior_dec) / 12
ytd_avg <- (ces_at(latest_ces_date) - prior_dec) / month(latest_ces_date)

if (is.na(prior_avg)) {
  warning("Could not compute ", prior_year, " average monthly job growth.")
}
if (is.na(ytd_avg) || year(latest_ces_date) != focus_year) {
  warning("Could not compute ", focus_year, " year-to-date job growth pace.")
  ytd_avg <- NA_real_
}

pos <- position_dodge(width = 0.8)

p_left <- ggplot(
  revisions_long,
  aes(x = month_label, y = jobs_bar, fill = estimate)
) +
  geom_col(position = pos, width = 0.74) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.4) +
  geom_text(
    aes(label = comma(jobs), vjust = if_else(jobs >= 0, -0.45, 1.35)),
    position = pos,
    size = 3.6,
    fontface = "bold",
    color = positive_color,
    na.rm = TRUE
  ) +
  scale_fill_manual(
    values = c(
      "sa_1st" = positive_color,
      "sa_2nd" = green_color,
      "sa_3rd" = negative_color
    ),
    labels = c("1st estimate", "2nd estimate", "3rd estimate"),
    name = NULL,
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.12, 0.14))
  ) +
  labs(
    title = "Monthly Job Growth and Revisions",
    subtitle = "Total nonfarm payrolls, change in thousands, seasonally adjusted",
    x = NULL,
    y = NULL
  ) +
  theme_esp() +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 11),
    axis.text.x = element_text(size = 12, color = positive_color),
    panel.grid.major.x = element_blank()
  )

# ── Right panel: Unemployment rate ───────────────────────────────────────

unrate_plot <- unrate %>%
  filter(!is.na(unrate)) %>%
  arrange(date) %>%
  filter(date > max(date) %m-% months(ur_months_shown)) %>%
  # Push each label away from the line: above local peaks, below local troughs.
  # At the endpoints there is only one neighbour, so compare against that.
  mutate(
    prev_u = lag(unrate),
    next_u = lead(unrate),
    local_mid = case_when(
      !is.na(prev_u) & !is.na(next_u) ~ (prev_u + next_u) / 2,
      !is.na(prev_u) ~ prev_u,
      TRUE ~ next_u
    ),
    nudge_dir = if_else(unrate >= local_mid, 1, -1)
  )

ur_span <- diff(range(unrate_plot$unrate))

ur_labels <- unrate_plot %>%
  filter(date >= ur_label_start) %>%
  mutate(nudge_y = nudge_dir * ur_span * 0.09)

ur_breaks <- seq(max(unrate_plot$date), min(unrate_plot$date), by = "-3 months")

focus_year_start <- as.Date(paste0(focus_year, "-01-01"))

# Hard right edge, just wide enough for the final value label. The shaded band
# is drawn out to exactly this date, so it clips flush instead of stretching
# the panel (an annotation past the limit would widen the scale).
ur_x_max <- max(unrate_plot$date) + days(40)

p_right <- ggplot(unrate_plot, aes(date, unrate)) +
  annotate(
    "rect",
    xmin = focus_year_start,
    xmax = ur_x_max,
    ymin = -Inf,
    ymax = Inf,
    fill = positive_color,
    alpha = 0.06
  ) +
  geom_line(linewidth = 1.1, color = positive_color) +
  geom_point(data = ur_labels, size = 2.8, color = positive_color) +
  geom_text_repel(
    data = ur_labels,
    aes(label = percent(unrate, accuracy = 0.01)),
    nudge_y = ur_labels$nudge_y,
    color = positive_color,
    size = 3.9,
    fontface = "bold",
    seed = 42,
    # Both axes: on a steep run like the drop off the Nov 2025 peak, a
    # vertical-only nudge lands the label back on the line either way.
    box.padding = 0.45,
    point.padding = 0.35,
    max.overlaps = Inf,
    min.segment.length = 0.3,
    segment.color = "grey55",
    segment.size = 0.3
  ) +
  annotate(
    "text",
    # Centred in the shaded band so it stays on-panel however wide the band is.
    x = focus_year_start + (max(unrate_plot$date) - focus_year_start) / 2,
    y = max(unrate_plot$unrate),
    label = focus_year,
    hjust = 0.5,
    size = 4.5,
    fontface = "bold",
    color = alpha(positive_color, 0.45)
  ) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  scale_x_date(
    date_labels = "%b\n%Y",
    breaks = ur_breaks,
    limits = c(min(unrate_plot$date), ur_x_max),
    expand = expansion(mult = c(0.02, 0))
  ) +
  labs(
    title = "Unemployment Rate",
    subtitle = paste0(
      "Calculated from CPS levels, last ",
      ur_months_shown,
      " months"
    ),
    x = NULL,
    y = NULL
  ) +
  theme_esp() +
  theme(
    axis.text.x = element_text(size = 12, color = positive_color),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80")
  )

# ── Combine side by side ─────────────────────────────────────────────────

latest_jobs <- revisions_long %>%
  filter(estimate == "sa_1st", !is.na(jobs)) %>%
  slice_max(date, n = 1) %>%
  pull(jobs)

latest_unrate <- unrate %>%
  filter(!is.na(unrate)) %>%
  slice_max(date, n = 1) %>%
  pull(unrate)

latest_month_name <- format(max(unrate$date, na.rm = TRUE), "%B %Y")

combined_title <- paste0(
  latest_month_name,
  ": ",
  comma(round(latest_jobs)),
  "k Jobs Added, ",
  percent(latest_unrate, accuracy = 0.01),
  " Unemployment"
)

combined_subtitle <- if (!is.na(ytd_avg) && !is.na(prior_avg)) {
  paste0(
    focus_year,
    " has averaged ",
    comma(round(ytd_avg)),
    "k jobs a month, against ",
    comma(round(prior_avg)),
    "k a month in ",
    prior_year,
    "."
  )
} else {
  NULL
}

combined <- p_left +
  p_right +
  plot_annotation(
    title = combined_title,
    subtitle = combined_subtitle,
    caption = "BLS, CES & CPS, seasonally adjusted. Mike Konczal, Economic Security Project.",
    theme = theme(
      plot.title = element_text(
        size = 22,
        face = "bold",
        color = positive_color
      ),
      plot.subtitle = element_text(size = 13, color = positive_color),
      plot.caption = element_text(size = 11, color = "grey40"),
      plot.background = element_rect(fill = esp_bg, color = NA),
      plot.margin = margin(12, 16, 8, 12)
    )
  )

combined

ggsave(
  "graphics/01_initial_tweet.png",
  plot = combined,
  dpi = "retina",
  width = 15,
  height = 7,
  units = "in"
)

# ── Generate tweet text ──────────────────────────────────────────────────

# Revisions. BLS reports the combined revision as previously published vs.
# current, i.e. only the revision booked in *this* release. For the month
# getting its third estimate that is 3rd-minus-2nd; for the month getting its
# second it is 2nd-minus-1st. Using the cumulative 3rd-minus-1st double-counts
# the revision the prior release already reported.
rev_value <- revisions_df %>%
  mutate(
    rev_value = coalesce(sa_rev_3rd_minus_2nd, sa_rev_2nd_minus_1st)
  ) %>%
  filter(!is.na(rev_value)) %>%
  arrange(date) %>%
  slice_tail(n = 2)

rev_sum <- sum(rev_value$rev_value, na.rm = TRUE)
rev_direction <- if_else(rev_sum < 0, "revised down", "revised up")
rev_text <- paste0(comma(abs(round(rev_sum))), "k")

tweet <- paste0(
  latest_month_name,
  " jobs report: ",
  comma(round(latest_jobs)),
  "k jobs added, unemployment at ",
  percent(latest_unrate, accuracy = 0.01),
  ". ",
  "Prior two months ",
  rev_direction,
  " ",
  rev_text,
  ".\n\n",
  "Let's dig in. /1"
)

cat("\n", strrep("─", 60), "\n")
cat("TWEET:\n\n")
cat(tweet, "\n")
cat(strrep("─", 60), "\n")
cat("Character count:", nchar(tweet), "\n")

writeLines(tweet, "tweet.txt")
cat("Tweet saved to tweet.txt\n")
