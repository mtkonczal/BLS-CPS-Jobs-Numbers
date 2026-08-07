library(tidyverse)
library(ggtext)
library(blsR)
library(scales)
library(lubridate)
library(patchwork)
source("scripts/graphic_scripts.R")

bls_set_key(Sys.getenv("BLS_KEY"))

# Prime-age (25-54) participation and employment. The point of these two is to
# check whether a flat unemployment rate is being held up by people leaving the
# labor force: if LFPR falls alongside EPOP, the unemployment rate can hold
# steady while fewer people are actually working.

positive_color <- "#2c3254"
green_color <- "#70ad8f"

# Post-recovery window. Starting in 2019 puts the COVID collapse on the axis and
# squeezes the 2pp band these series actually move in.
prime_start <- as.Date("2022-01-01")
benchmark_year <- 2019
benchmark_label <- paste(benchmark_year, "average")

prime_age <- get_n_series_table(
  c(
    "LNS11300060", # prime-age (25-54) labor force participation rate
    "LNS12300060" # prime-age (25-54) employment-population ratio
  ),
  api_key = bls_get_key(),
  start_year = benchmark_year,
  end_year = as.integer(format(Sys.Date(), "%Y")),
  tidy = TRUE
) %>%
  mutate(
    date = as.Date(paste0(year, "/", month, "/1")),
    lfpr = suppressWarnings(as.numeric(LNS11300060)) / 100,
    epop = suppressWarnings(as.numeric(LNS12300060)) / 100
  ) %>%
  arrange(date) %>%
  select(date, lfpr, epop)

# Months with no observation are kept as NA rather than dropped, so the line
# breaks instead of drawing a straight segment across them. The CPS was not
# collected in October 2025.
missing_months <- prime_age %>% filter(is.na(lfpr) | is.na(epop)) %>% pull(date)

benchmarks <- prime_age %>%
  filter(year(date) == benchmark_year) %>%
  summarize(
    lfpr = mean(lfpr, na.rm = TRUE),
    epop = mean(epop, na.rm = TRUE)
  )

prime_plot_data <- prime_age %>% filter(date >= prime_start)

prime_breaks <- seq(
  max(prime_plot_data$date),
  min(prime_plot_data$date),
  by = "-6 months"
)

prime_caption <- paste0(
  "BLS, CPS, ages 25-54, seasonally adjusted. Dotted line is the ",
  benchmark_year,
  " average.",
  if (length(missing_months) > 0) {
    paste0(
      " No observation for ",
      paste(format(missing_months, "%B %Y"), collapse = ", "),
      "."
    )
  } else {
    ""
  },
  " Mike Konczal, Economic Security Project."
)

prime_age_chart <- function(
  measure,
  line_color,
  label,
  show_benchmark_label = TRUE,
  caption = NULL,
  title_size = 22,
  subtitle_size = 14
) {
  series <- prime_plot_data %>%
    transmute(date, value = .data[[measure]])

  latest <- series %>% filter(!is.na(value)) %>% slice_max(date, n = 1)
  prior <- series %>%
    filter(!is.na(value), date < latest$date) %>%
    slice_max(date, n = 1)

  benchmark <- benchmarks[[measure]]

  month_change <- latest$value - prior$value
  gap_to_benchmark <- latest$value - benchmark

  subtitle <- sprintf(
    "%s %.1f pp from %s, and %.1f pp %s the %s.",
    if_else(month_change >= 0, "Up", "Down"),
    abs(month_change) * 100,
    format(prior$date, "%B %Y"),
    abs(gap_to_benchmark) * 100,
    if_else(gap_to_benchmark >= 0, "above", "below"),
    benchmark_label
  )

  plot <- ggplot(series, aes(date, value)) +
    # Reference line carries the series colour so it reads as that series'
    # own benchmark rather than as a second variable.
    geom_hline(
      yintercept = benchmark,
      linetype = "dotted",
      color = line_color,
      linewidth = 0.9
    ) +
    geom_line(linewidth = 1.7, color = line_color, na.rm = TRUE) +
    geom_point(data = latest, size = 3.4, color = line_color) +
    geom_text(
      data = latest,
      aes(label = percent(value, accuracy = 0.1)),
      hjust = 0,
      nudge_x = 30,
      size = 5.2,
      fontface = "bold",
      color = line_color
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 0.1),
      breaks = scales::breaks_pretty(n = 7)
    ) +
    scale_x_date(
      date_labels = "%b\n%Y",
      breaks = prime_breaks,
      expand = expansion(mult = c(0.02, 0.11))
    ) +
    labs(
      title = paste0(label, ": ", percent(latest$value, accuracy = 0.1)),
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      caption = caption
    ) +
    theme_esp(base_size = 14) +
    theme(
      plot.title = element_text(
        size = title_size,
        face = "bold",
        color = positive_color
      ),
      plot.subtitle = element_text(size = subtitle_size, color = positive_color),
      plot.caption = element_text(size = 10, color = "grey40"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey82")
    )

  if (show_benchmark_label) {
    # Anchored bottom-right: both series sit well above their benchmark by the
    # end of the window, so the space under the line there stays clear.
    plot <- plot +
      annotate(
        "text",
        x = max(series$date),
        y = benchmark,
        label = benchmark_label,
        hjust = 1,
        vjust = 1.6,
        size = 4,
        fontface = "bold",
        color = line_color
      )
  }

  plot
}

# ---- Standalone charts ----

prime_lfp_plot <- prime_age_chart(
  "lfpr",
  green_color,
  "Prime-Age Labor Force Participation Rate",
  caption = prime_caption
)

prime_epop_plot <- prime_age_chart(
  "epop",
  positive_color,
  "Prime-Age Employment-Population Ratio",
  caption = prime_caption
)

ggsave(
  "graphics/03a_prime_age_lfp.png",
  plot = prime_lfp_plot,
  dpi = "retina",
  width = 11,
  height = 6,
  units = "in"
)

ggsave(
  "graphics/03b_prime_age_epop.png",
  plot = prime_epop_plot,
  dpi = "retina",
  width = 11,
  height = 6,
  units = "in"
)

# ---- Combined: the two side by side ----
# Benchmark label only on the left panel, and one shared caption.

prime_combined <- prime_age_chart(
  "lfpr",
  green_color,
  "Labor Force Participation Rate",
  show_benchmark_label = TRUE,
  title_size = 17,
  subtitle_size = 12
) +
  prime_age_chart(
    "epop",
    positive_color,
    "Employment-Population Ratio",
    show_benchmark_label = FALSE,
    title_size = 17,
    subtitle_size = 12
  ) +
  plot_annotation(
    title = "Prime-Age Participation and Employment Both Fell in the Latest Month",
    subtitle = "Ages 25-54. A falling participation rate can hold the unemployment rate down even as employment drops.",
    caption = prime_caption,
    theme = theme(
      plot.title = element_text(
        size = 22,
        face = "bold",
        color = positive_color
      ),
      plot.subtitle = element_text(size = 14, color = positive_color),
      plot.caption = element_text(size = 10, color = "grey40"),
      plot.background = element_rect(fill = "#f4f2e4", color = NA)
    )
  )

ggsave(
  "graphics/03c_prime_age_lfp_epop.png",
  plot = prime_combined,
  dpi = "retina",
  width = 16,
  height = 6.5,
  units = "in"
)

prime_lfp_plot
prime_epop_plot
prime_combined
