library(tidyverse)
library(scales)
library(lubridate)
source("scripts/graphic_scripts.R")

ces_data <- readRDS("data/ces.rds")

# Edit this tibble to change the tech-sector definition used below.
tech_sector_definition <- tribble(
  ~display_name, ~industry_name,
  "Software publishers", "Software publishers",
  "Custom computer programming services", "Custom computer programming services",
  "Computing infrastructure, data processing, web hosting, and related",
  "Computing infrastructure providers, data processing, web hosting, and related services",
  "Computer systems design services", "Computer systems design services",
  "Web search portals and all other information services",
  "Web search portals, libraries, archives, and other information services",
  "Streaming services, social networks, and related",
  "Media streaming distribution services, social networks, and other media networks and content providers"
)

jobs_code <- 1
wages_code <- 3
analysis_start <- as.Date("2020-01-01")
weights_start <- as.Date("2018-01-01")
weights_end <- NULL

build_sector_panel <- function(
    ces_data,
    sector_definition,
    jobs_code = 1,
    wages_code = 3
) {
  sector_panel <- ces_data %>%
    filter(
      seasonal == "S",
      industry_name %in% sector_definition$industry_name,
      data_type_code %in% c(jobs_code, wages_code)
    ) %>%
    select(date, industry_name, data_type_code, value) %>%
    inner_join(sector_definition, by = "industry_name") %>%
    mutate(
      series = case_when(
        data_type_code == jobs_code ~ "jobs",
        data_type_code == wages_code ~ "wages"
      )
    ) %>%
    select(date, display_name, series, value) %>%
    pivot_wider(names_from = series, values_from = value)

  complete_months <- sector_panel %>%
    group_by(date) %>%
    summarize(
      has_all_series = n() == nrow(sector_definition) &&
        all(!is.na(jobs)) &&
        all(!is.na(wages)),
      .groups = "drop"
    ) %>%
    filter(has_all_series) %>%
    pull(date)

  sector_panel %>%
    filter(date %in% complete_months) %>%
    arrange(display_name, date)
}

compute_fixed_weights <- function(
    sector_panel,
    start_date = NULL,
    end_date = NULL
) {
  filtered_panel <- sector_panel %>%
    filter(
      date >= coalesce(start_date, min(date, na.rm = TRUE)),
      date <= coalesce(end_date, max(date, na.rm = TRUE))
    )

  filtered_panel %>%
    group_by(display_name) %>%
    summarize(avg_jobs = mean(jobs, na.rm = TRUE), .groups = "drop") %>%
    mutate(weight = avg_jobs / sum(avg_jobs))
}

build_sector_aggregate <- function(sector_panel, sector_weights) {
  sector_panel %>%
    inner_join(sector_weights, by = "display_name") %>%
    group_by(date) %>%
    summarize(
      tech_jobs = sum(jobs, na.rm = TRUE),
      tech_wage = sum(weight * wages, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(date) %>%
    mutate(
      jobs_yoy = tech_jobs / lag(tech_jobs, 12) - 1,
      wages_yoy = tech_wage / lag(tech_wage, 12) - 1
    )
}

make_supply_demand_plot <- function(aggregate_df, analysis_start) {
  plot_df <- aggregate_df %>%
    filter(date >= analysis_start) %>%
    drop_na(jobs_yoy, wages_yoy)

  latest_date <- max(plot_df$date, na.rm = TRUE)

  label_df <- plot_df %>%
    filter(month(date) == 1, date != latest_date) %>%
    mutate(label = format(date, "%Y"))

  latest_df <- plot_df %>%
    filter(date == latest_date) %>%
    mutate(label = format(date, "%b %Y"))

  plot_df %>%
    ggplot(aes(jobs_yoy, wages_yoy)) +
    geom_vline(xintercept = 0, color = "grey70", linewidth = 0.6) +
    geom_hline(yintercept = 0, color = "grey70", linewidth = 0.6) +
    geom_path(color = "#2c3254", linewidth = 1.3) +
    geom_point(color = "#2c3254", size = 2.2) +
    geom_point(
      data = latest_df,
      color = "#ff8361",
      size = 4
    ) +
    geom_text(
      data = label_df,
      aes(label = label),
      nudge_x = 0.0025,
      nudge_y = 0.0015,
      size = 4.2,
      color = "#2c3254"
    ) +
    geom_text(
      data = latest_df,
      aes(label = label),
      nudge_x = 0.0025,
      nudge_y = 0.0015,
      size = 4.4,
      color = "#ff8361",
      fontface = "bold"
    ) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    theme_esp(base_size = 15) +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(size = 11),
      plot.caption.position = "plot",
      panel.grid.major = element_line(color = "grey82"),
      plot.margin = margin(12, 18, 16, 30)
    ) +
    labs(
      title = "Tech Jobs and Wages Move Like a Demand Story",
      subtitle = paste0(
        "Aggregate across six tech industries. X-axis is year-over-year employment growth; ",
        "Y-axis is year-over-year wage growth.\nWages are average hourly earnings weighted by each industry's average employment level. ",
        "Through ",
        format(latest_date, "%B %Y"),
        "."
      ),
      x = "Tech employment growth, year-over-year",
      y = "Tech wage growth, year-over-year",
      caption = paste(
        "BLS CES, seasonally adjusted.",
        "\nThe web-search/info-services bucket uses the current CES industry label",
        "'Web search portals, libraries, archives, and other information services'",
        "to keep a wage series available."
      )
    )
}

tech_sector_panel <- build_sector_panel(
  ces_data = ces_data,
  sector_definition = tech_sector_definition,
  jobs_code = jobs_code,
  wages_code = wages_code
)

tech_sector_weights <- compute_fixed_weights(
  sector_panel = tech_sector_panel,
  start_date = weights_start,
  end_date = weights_end
)

tech_sector_aggregate <- build_sector_aggregate(
  sector_panel = tech_sector_panel,
  sector_weights = tech_sector_weights
)

tech_supply_demand_plot <- make_supply_demand_plot(
  aggregate_df = tech_sector_aggregate,
  analysis_start = analysis_start
)

tech_supply_demand_plot

ggsave(
  "graphics/99_tech_supply_demand.png",
  plot = tech_supply_demand_plot,
  dpi = "retina",
  width = 11.5,
  height = 8,
  units = "in"
)
