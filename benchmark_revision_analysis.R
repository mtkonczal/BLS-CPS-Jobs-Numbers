library(tidyverse)
library(ggtext)
library(blsR)
library(scales)
library(lubridate)
source("scripts/graphic_scripts.R")

# Set to 1 to pull and cache pre-revision data (before release)
# Set to 2 to pull post-revision data AND build all graphics
STEP <- 2

# Ensure output directories exist
dir.create("data", showWarnings = FALSE)
dir.create("graphics", showWarnings = FALSE)

# BLS API key — set in .Renviron via usethis::edit_r_environ()
bls_set_key(Sys.getenv("BLS_KEY"))

# Series IDs: total nonfarm + supersectors (SA, monthly, thousands)
ces_series <- c(
  "CES0000000001", # Total nonfarm
  "CES0500000001", # Total private
  "CES1000000001", # Mining and logging
  "CES2000000001", # Construction
  "CES3000000001", # Manufacturing
  "CES4000000001", # Trade, transportation, utilities
  "CES4142000001", # Retail trade
  "CES5000000001", # Information
  "CES5500000001", # Financial activities
  "CES6000000001", # Professional and business services
  "CES6500000001", # Education and health services
  "CES7000000001", # Leisure and hospitality
  "CES8000000001", # Other services
  "CES9000000001" # Government
)

# Industry labels for display (supersectors only, excluding retail subset)
industry_labels <- c(
  "CES0500000001" = "Total private",
  "CES1000000001" = "Mining and logging",
  "CES2000000001" = "Construction",
  "CES3000000001" = "Manufacturing",
  "CES4000000001" = "Trade, transport., utilities",
  "CES5000000001" = "Information",
  "CES5500000001" = "Financial activities",
  "CES6000000001" = "Prof. and business services",
  "CES6500000001" = "Education and health services",
  "CES7000000001" = "Leisure and hospitality",
  "CES8000000001" = "Other services",
  "CES9000000001" = "Government"
)

# Colors matching project conventions
positive_color <- "#2c3254"
negative_color <- "#ff8361"
old_color <- "#A4CCCC"
revised_color <- "#2c3254"
highlight_color <- "#ff8361"
bar_muted <- "#8da0b5"

# ── Step 1: Pull and cache pre-revision data (run Feb 10) ──────────────────
if (STEP == 1) {
  cat("Step 1: Pulling pre-revision CES data…\n")
  pre <- tryCatch(
    get_n_series_table(
      ces_series,
      api_key = bls_get_key(),
      start_year = 2023,
      end_year = 2026,
      tidy = TRUE
    ),
    error = function(e) {
      stop(
        "BLS API request failed. Check your API key and try again.\n",
        e$message
      )
    }
  )
  pre <- pre %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-")))
  saveRDS(pre, "data/pre_revision_ces.rds")
  cat("Pre-revision data saved to data/pre_revision_ces.rds\n")
  cat("Set STEP <- 2 after the release and re-run.\n")
}

# ── Step 2: Pull post-revision data and build graphics ─────────────────────
if (STEP == 2) {
  cat("Step 2: Pulling post-revision CES data…\n")
  post <- tryCatch(
    get_n_series_table(
      ces_series,
      api_key = bls_get_key(),
      start_year = 2023,
      end_year = 2026,
      tidy = TRUE
    ),
    error = function(e) {
      stop(
        "BLS API request failed. The release may not be up yet.\n",
        e$message
      )
    }
  )
  post <- post %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-")))
  saveRDS(post, "data/post_revision_ces.rds")
  cat("Post-revision data saved to data/post_revision_ces.rds\n")

  cat("Building graphics…\n")

  pre <- readRDS("data/pre_revision_ces.rds")

  # ── Helper: pivot to long form with monthly change ──
  make_monthly_change <- function(df, vintage_label) {
    df %>%
      pivot_longer(
        cols = all_of(ces_series),
        names_to = "series_id",
        values_to = "level"
      ) %>%
      mutate(level = as.numeric(level)) %>%
      group_by(series_id) %>%
      arrange(date) %>%
      mutate(monthly_change = level - lag(level, 1)) %>%
      ungroup() %>%
      mutate(vintage = vintage_label)
  }

  pre_long <- make_monthly_change(pre, "Pre-revision")
  post_long <- make_monthly_change(post, "Revised")

  # ────────────────────────────────────────────────────────────────────────

  # Graphic 1: Monthly NFP Change, Old vs. Revised
  # ────────────────────────────────────────────────────────────────────────

  nfp_compare <- bind_rows(pre_long, post_long) %>%
    filter(series_id == "CES0000000001") %>%
    filter(date >= "2024-04-01") %>%
    filter(!is.na(monthly_change))

  # Cumulative revision over benchmark window Apr 2024–Mar 2025
  cumulative <- nfp_compare %>%
    filter(date >= "2024-04-01", date <= "2025-03-01") %>%
    group_by(vintage) %>%
    summarize(total = sum(monthly_change, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = vintage, values_from = total)
  cum_revision <- cumulative$Revised - cumulative$`Pre-revision`

  revision_title <- paste0(
    "Monthly Job Gains: Before and After Benchmark Revision (",
    comma(round(cum_revision)),
    "k Total Revision)"
  )

  nfp_compare %>%
    mutate(vintage = factor(vintage, levels = c("Pre-revision", "Revised"))) %>%
    ggplot(aes(x = date, y = monthly_change, fill = vintage)) +
    geom_col(position = position_dodge(width = 25), width = 22, alpha = 0.9) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    scale_fill_manual(
      values = c("Pre-revision" = old_color, "Revised" = revised_color),
      name = NULL
    ) +
    scale_x_date(
      date_labels = "%b\n%Y",
      breaks = seq(
        as.Date("2024-04-01"),
        max(nfp_compare$date),
        by = "2 months"
      )
    ) +
    labs(
      title = revision_title,
      subtitle = "Seasonally adjusted monthly change in total nonfarm payrolls, thousands",
      caption = "Source: Bureau of Labor Statistics, Current Employment Statistics. Pre-revision data cached Feb 10, 2026.",
      x = NULL,
      y = NULL
    ) +
    theme_esp() +
    theme(
      legend.position = "top",
      panel.grid.major.y = element_line(color = "grey80")
    )

  ggsave(
    "graphics/01_revision_comparison.png",
    dpi = "retina",
    width = 12,
    height = 6.75,
    units = "in"
  )
  cat("Saved graphics/01_revision_comparison.png\n")

  # ────────────────────────────────────────────────────────────────────────
  # Graphic 2: Industry Revision Decomposition at March 2025
  # ────────────────────────────────────────────────────────────────────────

  # Supersector series (exclude total nonfarm and retail subset)
  supersector_ids <- names(industry_labels)

  industry_rev <- bind_rows(pre_long, post_long) %>%
    filter(series_id %in% supersector_ids) %>%
    filter(date == "2025-03-01") %>%
    select(series_id, vintage, level) %>%
    pivot_wider(names_from = vintage, values_from = level) %>%
    mutate(
      revision = Revised - `Pre-revision`,
      industry = industry_labels[series_id],
      positive = revision >= 0
    )

  # Total nonfarm revision for reference
  total_nfp_rev <- bind_rows(pre_long, post_long) %>%
    filter(series_id == "CES0000000001", date == "2025-03-01") %>%
    select(vintage, level) %>%
    pivot_wider(names_from = vintage, values_from = level) %>%
    mutate(revision = Revised - `Pre-revision`) %>%
    pull(revision)

  industry_rev %>%
    mutate(
      industry = fct_reorder(industry, revision),
      text_hjust = if_else(revision >= 0, 1.1, -0.1)
    ) %>%
    ggplot(aes(x = revision, y = industry, fill = positive)) +
    geom_col(show.legend = FALSE) +
    geom_vline(xintercept = 0, linewidth = 0.4) +
    geom_text(
      aes(
        label = comma(round(revision)),
        hjust = text_hjust,
        color = positive
      ),
      size = 4.5,
      fontface = "bold",
      show.legend = FALSE
    ) +
    annotate(
      "text",
      x = max(abs(industry_rev$revision)) * 0.6,
      y = 1,
      label = paste0(
        "Total nonfarm revision: ",
        comma(round(total_nfp_rev)),
        "k"
      ),
      hjust = 0,
      size = 4,
      fontface = "italic",
      color = "grey40"
    ) +
    scale_fill_manual(values = c("TRUE" = "#2D779C", "FALSE" = "#A4546A")) +
    scale_color_manual(values = c("TRUE" = "#2D779C", "FALSE" = "#A4546A")) +
    labs(
      title = "Benchmark Revisions by Industry, March 2025",
      subtitle = "Difference between revised and pre-revision employment levels, thousands",
      caption = "Source: Bureau of Labor Statistics, CES benchmark revision.",
      x = NULL,
      y = NULL
    ) +
    theme_esp() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey80"),
      axis.text.y = element_text(size = 13)
    )

  ggsave(
    "graphics/02_industry_revisions.png",
    dpi = "retina",
    width = 12,
    height = 6.75,
    units = "in"
  )
  cat("Saved graphics/02_industry_revisions.png\n")

  # ────────────────────────────────────────────────────────────────────────
  # Graphic 3: January 2026 in Context (revised series only)
  # ────────────────────────────────────────────────────────────────────────

  jan_context <- post_long %>%
    filter(series_id == "CES0000000001") %>%
    filter(date >= "2025-01-01") %>%
    filter(!is.na(monthly_change))

  # Trailing 6-month average (Jul 2025–Dec 2025)
  trailing_avg <- jan_context %>%
    filter(date >= "2025-07-01", date <= "2025-12-01") %>%
    summarize(avg = mean(monthly_change, na.rm = TRUE)) %>%
    pull(avg)

  jan_context <- jan_context %>%
    mutate(
      is_jan_2026 = date == "2026-01-01",
      fill_color = if_else(is_jan_2026, highlight_color, bar_muted),
      label = if_else(
        date >= max(date) %m-% months(5),
        comma(round(monthly_change)),
        NA_character_
      )
    )

  jan_context %>%
    ggplot(aes(x = date, y = monthly_change)) +
    geom_col(aes(fill = fill_color), show.legend = FALSE) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    geom_hline(
      yintercept = trailing_avg,
      linetype = "dashed",
      color = positive_color,
      linewidth = 0.8
    ) +
    annotate(
      "text",
      x = max(jan_context$date) - days(10),
      y = trailing_avg + max(jan_context$monthly_change, na.rm = TRUE) * 0.06,
      label = paste0("6-mo avg: ", comma(round(trailing_avg)), "k"),
      hjust = 1,
      size = 4.5,
      color = positive_color,
      fontface = "bold"
    ) +
    geom_text(
      aes(label = label),
      vjust = -0.4,
      color = positive_color,
      size = 4.5
    ) +
    scale_fill_identity() +
    scale_x_date(
      date_labels = "%b\n%Y",
      breaks = jan_context$date
    ) +
    labs(
      title = "Monthly Job Gains on the Revised Series",
      subtitle = "Seasonally adjusted monthly change in total nonfarm payrolls, thousands",
      caption = "Source: Bureau of Labor Statistics, CES (post-benchmark revision).",
      x = NULL,
      y = NULL
    ) +
    theme_esp() +
    theme(
      panel.grid.major.y = element_line(color = "grey80")
    )

  ggsave(
    "graphics/03_january_in_context.png",
    dpi = "retina",
    width = 12,
    height = 6.75,
    units = "in"
  )
  cat("Saved graphics/03_january_in_context.png\n")

  cat("\nAll three graphics saved to graphics/\n")
}
