library(tidyverse)
library(ggtext)
library(blsR)
library(scales)
library(lubridate)
source("scripts/graphic_scripts.R")

bls_set_key(Sys.getenv("BLS_KEY"))

pull_month_value <- function(values, dates, target_date) {
  matched_value <- values[dates == target_date]

  if (length(matched_value) == 0) {
    return(NA_real_)
  }

  matched_value[[1]]
}

lfp_epop_series <- c(
  "LNS11300000", # overall LFPR
  "LNS12300000", # overall EPOP
  "LNS11300060", # prime-age LFPR
  "LNS12300060" # prime-age EPOP
)

lfp_epop_raw <- get_n_series_table(
  lfp_epop_series,
  api_key = bls_get_key(),
  start_year = 2018,
  end_year = as.integer(format(Sys.Date(), "%Y")),
  tidy = TRUE
) %>%
  filter(if_all(all_of(lfp_epop_series), ~ .x != "-")) %>%
  mutate(
    date = as.Date(paste0(year, "/", month, "/1")),
    LNS11300000 = as.numeric(LNS11300000) / 100,
    LNS12300000 = as.numeric(LNS12300000) / 100,
    LNS11300060 = as.numeric(LNS11300060) / 100,
    LNS12300060 = as.numeric(LNS12300060) / 100
  )

lfp_epop <- lfp_epop_raw %>%
  transmute(
    date,
    overall_lfpr = LNS11300000,
    overall_epop = LNS12300000,
    prime_lfpr = LNS11300060,
    prime_epop = LNS12300060
  )

last_month <- max(lfp_epop$date, na.rm = TRUE)
plot_start <- as.Date("2019-01-01")
month_breaks <- sort(unique(lfp_epop$date), decreasing = TRUE)
month_breaks <- month_breaks[seq(1, length(month_breaks), 4)]

levels_long <- lfp_epop %>%
  pivot_longer(
    cols = -date,
    names_to = c("group", "measure"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  mutate(
    group = recode(
      group,
      overall = "Overall, 16+",
      prime = "Prime-age, 25-54"
    ),
    measure = recode(
      measure,
      lfpr = "Labor-force participation rate",
      epop = "Employment-population ratio"
    )
  )

baseline_df <- levels_long %>%
  filter(date >= as.Date("2019-01-01"), date <= as.Date("2019-12-01")) %>%
  group_by(group, measure) %>%
  summarize(baseline = mean(value, na.rm = TRUE), .groups = "drop")

levels_labels <- levels_long %>%
  group_by(group, measure) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  mutate(
    label_date = date + days(30),
    label = percent(value, accuracy = 0.1)
  )

lfp_epop_colors <- c(
  "Labor-force participation rate" = "#70ad8f",
  "Employment-population ratio" = "#2c3254"
)

levels_long %>%
  filter(date >= plot_start) %>%
  ggplot(aes(date, value, color = measure)) +
  geom_hline(
    data = baseline_df,
    aes(yintercept = baseline, color = measure),
    linetype = "dotted",
    linewidth = 0.9,
    show.legend = FALSE
  ) +
  geom_line(linewidth = 1.8, show.legend = FALSE) +
  geom_point(
    data = levels_labels,
    size = 3.2,
    show.legend = FALSE
  ) +
  geom_text(
    data = levels_labels,
    aes(label_date, value, label = label),
    hjust = 0,
    size = 5.1,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(~group, ncol = 1) +
  scale_color_manual(values = lfp_epop_colors) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_date(
    breaks = month_breaks,
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.16))
  ) +
  coord_cartesian(clip = "off") +
  theme_esp(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 15),
    strip.text = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 12),
    panel.grid.major.y = element_line(color = "grey82")
  ) +
  labs(
    title = "Prime-Age EPOP and LFPR Are the Cleaner Read on Labor-Market Slack",
    subtitle = "Seasonally adjusted. Dotted lines are 2019 averages, the pre-pandemic benchmark many labor-market analysts still watch.",
    x = "",
    y = "",
    caption = "BLS CPS. Mike Konczal, Economic Security Project."
  )

ggsave(
  "graphics/03a_lfp_epop_levels.png",
  dpi = "retina",
  width = 12,
  height = 9,
  units = "in"
)

change_summary <- bind_rows(
  tibble(
    group = "Overall, 16+",
    component = c("LFPR", "EPOP", "LFPR - EPOP"),
    change = c(
      100 *
        (pull_month_value(lfp_epop$overall_lfpr, lfp_epop$date, last_month) -
          pull_month_value(
            lfp_epop$overall_lfpr,
            lfp_epop$date,
            last_month %m-% months(3)
          )),
      100 *
        (pull_month_value(lfp_epop$overall_epop, lfp_epop$date, last_month) -
          pull_month_value(
            lfp_epop$overall_epop,
            lfp_epop$date,
            last_month %m-% months(3)
          )),
      100 *
        ((pull_month_value(lfp_epop$overall_lfpr, lfp_epop$date, last_month) -
          pull_month_value(lfp_epop$overall_epop, lfp_epop$date, last_month)) -
          (pull_month_value(
            lfp_epop$overall_lfpr,
            lfp_epop$date,
            last_month %m-% months(3)
          ) -
            pull_month_value(
              lfp_epop$overall_epop,
              lfp_epop$date,
              last_month %m-% months(3)
            )))
    )
  ),
  tibble(
    group = "Prime-age, 25-54",
    component = c("LFPR", "EPOP", "LFPR - EPOP"),
    change = c(
      100 *
        (pull_month_value(lfp_epop$prime_lfpr, lfp_epop$date, last_month) -
          pull_month_value(
            lfp_epop$prime_lfpr,
            lfp_epop$date,
            last_month %m-% months(3)
          )),
      100 *
        (pull_month_value(lfp_epop$prime_epop, lfp_epop$date, last_month) -
          pull_month_value(
            lfp_epop$prime_epop,
            lfp_epop$date,
            last_month %m-% months(3)
          )),
      100 *
        ((pull_month_value(lfp_epop$prime_lfpr, lfp_epop$date, last_month) -
          pull_month_value(lfp_epop$prime_epop, lfp_epop$date, last_month)) -
          (pull_month_value(
            lfp_epop$prime_lfpr,
            lfp_epop$date,
            last_month %m-% months(3)
          ) -
            pull_month_value(
              lfp_epop$prime_epop,
              lfp_epop$date,
              last_month %m-% months(3)
            )))
    )
  )
) %>%
  mutate(
    component = factor(component, levels = c("LFPR", "EPOP", "LFPR - EPOP")),
    label = sprintf("%+.1f pp", change)
  )

component_colors <- c(
  "LFPR" = "#70ad8f",
  "EPOP" = "#2c3254",
  "LFPR - EPOP" = "#ff8361"
)

change_summary %>%
  ggplot(aes(component, change, fill = component)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.5) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(
      y = change + if_else(change >= 0, 0.08, -0.08),
      label = label
    ),
    size = 5,
    fontface = "bold"
  ) +
  facet_wrap(~group, ncol = 1) +
  scale_fill_manual(values = component_colors) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = " pp")) +
  theme_esp(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 15),
    strip.text = element_text(size = 16, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey82")
  ) +
  labs(
    title = "LFPR Gains Are Helpful Only If EPOP Keeps Up",
    subtitle = paste0(
      "Three-month change through ",
      format(last_month, "%B %Y"),
      ". LFPR - EPOP is the nonemployment share of the population, so a rise there is the more worrisome part."
    ),
    x = "",
    y = "",
    caption = "BLS CPS. Mike Konczal, Economic Security Project."
  )

ggsave(
  "graphics/03b_lfp_epop_change.png",
  dpi = "retina",
  width = 12,
  height = 8.5,
  units = "in"
)

gender_sector_raw <- get_n_series_table(
  c(
    "CES0500000001", # total private
    "CES0500000010", # private women
    "CES9091000001", # federal total
    "CES9091000010", # federal women
    "CES9092000001", # state total
    "CES9092000010", # state women
    "CES9093000001", # local total
    "CES9093000010" # local women
  ),
  api_key = bls_get_key(),
  start_year = 2024,
  end_year = as.integer(format(Sys.Date(), "%Y")),
  tidy = TRUE
) %>%
  mutate(
    date = as.Date(paste0(year, "/", month, "/1")),
    state_local_total = CES9092000001 + CES9093000001,
    state_local_women = CES9092000010 + CES9093000010
  ) %>%
  transmute(
    date,
    private_total = CES0500000001,
    private_women = CES0500000010,
    private_men = private_total - private_women,
    federal_total = CES9091000001,
    federal_women = CES9091000010,
    federal_men = federal_total - federal_women,
    state_local_total,
    state_local_women,
    state_local_men = state_local_total - state_local_women
  )

sector_last_month <- gender_sector_raw %>%
  filter(if_all(-date, ~ !is.na(.x))) %>%
  summarize(last_complete_month = max(date, na.rm = TRUE)) %>%
  pull(last_complete_month)

sector_baseline <- as.Date("2024-12-01")

gender_sector_growth <- bind_rows(
  tibble(
    sector = "Private",
    men = pull_month_value(
      gender_sector_raw$private_men,
      gender_sector_raw$date,
      sector_last_month
    ) -
      pull_month_value(
        gender_sector_raw$private_men,
        gender_sector_raw$date,
        sector_baseline
      ),
    women = pull_month_value(
      gender_sector_raw$private_women,
      gender_sector_raw$date,
      sector_last_month
    ) -
      pull_month_value(
        gender_sector_raw$private_women,
        gender_sector_raw$date,
        sector_baseline
      )
  ),
  tibble(
    sector = "Federal",
    men = pull_month_value(
      gender_sector_raw$federal_men,
      gender_sector_raw$date,
      sector_last_month
    ) -
      pull_month_value(
        gender_sector_raw$federal_men,
        gender_sector_raw$date,
        sector_baseline
      ),
    women = pull_month_value(
      gender_sector_raw$federal_women,
      gender_sector_raw$date,
      sector_last_month
    ) -
      pull_month_value(
        gender_sector_raw$federal_women,
        gender_sector_raw$date,
        sector_baseline
      )
  ),
  tibble(
    sector = "State/local",
    men = pull_month_value(
      gender_sector_raw$state_local_men,
      gender_sector_raw$date,
      sector_last_month
    ) -
      pull_month_value(
        gender_sector_raw$state_local_men,
        gender_sector_raw$date,
        sector_baseline
      ),
    women = pull_month_value(
      gender_sector_raw$state_local_women,
      gender_sector_raw$date,
      sector_last_month
    ) -
      pull_month_value(
        gender_sector_raw$state_local_women,
        gender_sector_raw$date,
        sector_baseline
      )
  )
) %>%
  pivot_longer(c(men, women), names_to = "gender", values_to = "change") %>%
  mutate(
    sector = factor(sector, levels = c("Private", "Federal", "State/local")),
    gender = recode(gender, men = "Men", women = "Women"),
    label = comma(round(change))
  )

sector_totals <- gender_sector_growth %>%
  group_by(sector) %>%
  summarize(total = sum(change, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    label = comma(round(total)),
    y = total + if_else(total >= 0, 55, -55)
  )

gender_sector_growth %>%
  ggplot(aes(sector, change, fill = gender)) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.5) +
  geom_col(width = 0.72, color = NA) +
  geom_text(
    aes(label = if_else(abs(change) >= 25, label, "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_text(
    data = sector_totals,
    aes(sector, y, label = label),
    inherit.aes = FALSE,
    color = "#2c3254",
    size = 5.4,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("Men" = "#2c3254", "Women" = "#ff8361")) +
  scale_y_continuous(
    labels = label_number(big.mark = ",", suffix = "k"),
    expand = expansion(mult = c(0.08, 0.14))
  ) +
  coord_flip() +
  theme_esp(base_size = 16) +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 15),
    panel.grid.major.x = element_line(color = "grey82"),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = "Job Growth Since December 2024, by Sector and Gender",
    subtitle = paste0(
      "Seasonally adjusted CES employment, in thousands. Change from Dec. 2024 to ",
      format(sector_last_month, "%b %Y"),
      ".\nState/local combines state and local government. ",
      format(sector_last_month, "%b %Y"),
      " is the latest month with complete gender detail."
    ),
    x = "",
    y = "",
    caption = "BLS CES. Mike Konczal, Economic Security Project."
  )

ggsave(
  "graphics/03c_gender_sector_growth.png",
  dpi = "retina",
  width = 12,
  height = 7.5,
  units = "in"
)
