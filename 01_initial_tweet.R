library(tidyverse)
library(ggtext)
library(blsR)
library(tidyusmacro)
library(scales)
library(lubridate)
library(patchwork)
source("scripts/graphic_scripts.R")

positive_color <- "#2c3254"
negative_color <- "#ff8361"
green_color <- "#70ad8f"

how_many_months <- 8

if (!exists("unrate")) {
  stop("Expected `unrate` to be loaded before sourcing 01_initial_tweet.R.")
}

if (!exists("revisions_df")) {
  stop(
    "Expected `revisions_df` to be loaded before sourcing 01_initial_tweet.R."
  )
}

# ── Left panel: Job revisions (1st, 2nd, 3rd estimates) ──────────────────

latest_year <- max(revisions_df$year[!is.na(revisions_df$sa_1st)])

latest_m <- revisions_df %>%
  filter(year == latest_year) %>%
  filter(if_any(starts_with("sa_"), ~ !is.na(.))) %>%
  summarise(latest = max(month_num, na.rm = TRUE)) %>%
  pull(latest)

# Walk back across year boundary if needed
all_months <- revisions_df %>%
  filter(!is.na(sa_1st)) %>%
  arrange(year, month_num) %>%
  tail(how_many_months)

revisions_long <- all_months %>%
  transmute(
    date = as.Date(date),
    month_label = format(date, "%b\n%Y"),
    sa_1st,
    sa_2nd,
    sa_3rd
  ) %>%
  mutate(month_label = factor(month_label, levels = month_label)) %>%
  pivot_longer(
    starts_with("sa_"),
    names_to = "estimate",
    values_to = "jobs"
  )

pos <- position_dodge2(width = 0.65, preserve = "single", padding = 0)

p_left <- ggplot(
  revisions_long,
  aes(x = month_label, y = jobs, fill = estimate)
) +
  geom_col(position = pos, width = 0.65, na.rm = TRUE) +
  geom_text(
    aes(label = comma(jobs), group = estimate),
    position = pos,
    vjust = -0.25,
    size = 4,
    na.rm = TRUE,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "sa_1st" = positive_color,
      "sa_2nd" = green_color,
      "sa_3rd" = negative_color
    ),
    labels = c("1st Estimate", "2nd Estimate", "3rd Estimate"),
    name = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(
    title = "Monthly Jobs: 1st, 2nd, 3rd Estimates",
    subtitle = "Total nonfarm, thousands",
    x = NULL,
    y = NULL
  ) +
  theme_esp() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10),
    axis.text.x = element_text(size = 11)
  )

# ── Right panel: Unemployment rate (last 12 months, labeled) ──────────────

unrate_plot <- unrate %>%
  filter(!is.na(unrate)) %>%
  filter(date > max(date) %m-% months(24)) %>%
  mutate(
    dateTag = if_else(
      date >= max(date) %m-% months(how_many_months - 1),
      round(unrate, 4),
      NA_real_
    )
  )

ur_breaks <- seq(max(unrate_plot$date), min(unrate_plot$date), by = "-4 months")

p_right <- ggplot(
  unrate_plot,
  aes(date, unrate, label = percent(dateTag, 0.01))
) +
  geom_line(linewidth = 1.2, color = positive_color) +
  geom_point(aes(date, dateTag), size = 3.5, color = positive_color) +
  geom_text(
    aes(date, dateTag),
    nudge_x = 30,
    color = positive_color,
    size = 3.5,
    na.rm = TRUE
  ) +
  scale_y_continuous(labels = percent) +
  scale_x_date(
    date_labels = "%b\n%Y",
    breaks = ur_breaks
  ) +
  labs(
    title = "Unemployment Rate",
    subtitle = "Calculated from CPS levels",
    x = NULL,
    y = NULL
  ) +
  theme_esp() +
  theme(
    panel.grid.major.y = element_line(color = "grey80")
  )

# ── Combine side by side ─────────────────────────────────────────────────

latest_jobs <- revisions_long %>%
  filter(estimate == "sa_1st") %>%
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
  percent(latest_unrate, accuracy = 0.1),
  " Unemployment"
)

esp_bg <- "#f4f2e4"

combined <- p_left +
  p_right +
  plot_annotation(
    title = combined_title,
    caption = "BLS, CES & CPS, seasonally adjusted. Mike Konczal, Economic Security Project.",
    theme = theme(
      plot.title = element_text(
        size = 18,
        face = "bold",
        color = positive_color
      ),
      plot.caption = element_text(size = 11, color = "grey40"),
      plot.background = element_rect(fill = esp_bg, color = NA)
    )
  )

combined

ggsave(
  "graphics/01_initial_tweet.png",
  plot = combined,
  dpi = "retina",
  width = 16,
  height = 7.5,
  units = "in"
)

# ── Generate tweet text ──────────────────────────────────────────────────

# Revisions
rev_value <- revisions_df %>%
  mutate(
    rev_value = coalesce(sa_rev_3rd_minus_1st, sa_rev_2nd_minus_1st)
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
  "k jobs added. ",
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
