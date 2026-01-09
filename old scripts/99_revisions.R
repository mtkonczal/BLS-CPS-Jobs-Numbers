library(tidyverse)
library(slider)        # for rolling means
library(govMacroTools)
library(scales)
library(lubridate)

# ---------- Toggle ----------
use_3m <- FALSE  # TRUE = plot 3-month avg, FALSE = plot single-month values
# ----------------------------

# Colors
esp_blue  <- "#1F4E79"
esp_coral <- "#FF8361"

# Prep
df <- read_csv("data/job_revisions.csv") %>%
  filter(!is.na(Date), !is.na(SA_2nd_minus_1st)) %>%
  rename(date = Date) %>%
  arrange(date)

max_date    <- max(df$date)
min_date    <- max_date %m-% months(30)
date_breaks <- seq(min_date, max_date, by = "5 months")

# Pivot & compute rolling averages
df_long <- df %>%
  select(date, SA_2nd_minus_1st, SA_3rd_minus_1st) %>%
  pivot_longer(-date, names_to = "series_raw", values_to = "value") %>%
  mutate(series = recode(series_raw,
                         SA_2nd_minus_1st = "2nd to 1st Month Revision",
                         SA_3rd_minus_1st = "3rd to 1st Month Revision"),
         .after = series_raw) %>%
  group_by(series) %>%
  mutate(value_3m   = slide_dbl(value, mean, .before = 2, .complete = TRUE),
         value_plot = if (use_3m) value_3m else value) %>%
  ungroup()

# Optional helper for title tweak
title_suffix <- if (use_3m) " (3-Month Avg.)" else ""

ggplot(df_long, aes(date, value_plot, color = series)) +
  geom_line(linewidth = 1.4, lineend = "round") +
  scale_color_manual(values = c("2nd to 1st Month Revision" = esp_blue,
                                "3rd to 1st Month Revision" = esp_coral),
                     name = NULL) +
  scale_x_date(
    breaks = date_breaks,
    labels = ~ format(., "%b\n%Y")
  ) +
  labs(
    title    = paste0("Revisions Have Gotten More Negative in 2025", title_suffix),
    subtitle = "1-Month and 2-Month Revisions to CES Jobs Numbers",
    y = NULL,
    x = NULL,
    caption = "Source: BLS, Mike Konczal."
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_vline(xintercept = as.Date("2024-12-01"), linewidth = 0.4, color = "grey50") +
  theme_esp() +
  theme(
    plot.margin         = margin(10, 50, 10, 10),
    plot.title.position = "plot",
    legend.position     = c(0.98, 0.98),
    legend.justification= c(1, 1),
    legend.background   = element_rect(fill = alpha("white", 0.7), color = NA),
    legend.key.width    = unit(1.4, "lines")
  ) +   theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )


ggsave("graphics/revisions.png", dpi = "retina", width = 12, height = 6.75, units = "in")

