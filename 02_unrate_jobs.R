library(tidyverse)
library(ggtext)
library(blsR)
library(tidyusmacro)
library(scales)
library(zoo)
library(tidyverse)
library(lubridate)
library(viridis)
source("scripts/graphic_scripts.R")


positive_color <- "#2c3254" # Bright blue
negative_color <- "#ff8361" # Pale violet

# Diffusion index panel starts here; earlier months only feed the 3-month average.
diffusion_start <- as.Date("2018-01-01")

required_unrate_columns <- c(
  "LNS13000000",
  "LNS11000000",
  "CES0500000021",
  "CES0000000001",
  "CES0000000010",
  "CES9091000001",
  "date",
  "unrate",
  "diffusion",
  "ces",
  "federal",
  "nonfederal"
)

if (!exists("unrate") || !all(required_unrate_columns %in% names(unrate))) {
  bls_set_key(Sys.getenv("BLS_KEY"))

  unrate <- get_n_series_table(
    c(
      "LNS13000000",
      "LNS11000000",
      "CES0500000021",
      "CES0000000001",
      "CES0000000010",
      "CES9091000001"
    ),
    api_key = bls_get_key(),
    start_year = 2011,
    end_year = 2026,
    tidy = TRUE
  ) %>%
    mutate(
      LNS11000000 = as.numeric(LNS11000000),
      LNS13000000 = as.numeric(LNS13000000),
      unrate = LNS13000000 / LNS11000000,
      date = as.Date(paste0(year, "/", month, "/", 1)),
      diffusion = CES0500000021 / 100,
      ces = CES0000000001 - lag(CES0000000001, 1),
      federal = CES9091000001,
      nonfederal = CES0000000001 - federal,
      federal = CES9091000001 - lag(CES9091000001, 1),
      nonfederal = nonfederal - lag(nonfederal, 1)
    )
}


# Diffusion index ----
# CES0500000021: "Diffusion indexes, 1-month span, total private, seasonally
# adjusted." Share of private industries adding jobs over the month, with
# unchanged industries counted as half. 50% is the neutral line.

diffusion_df <- unrate %>%
  filter(!is.na(diffusion)) %>%
  arrange(date) %>%
  # Rolled before the window filter so January 2018 already has a full window.
  mutate(diffusion_3m = rollmean(diffusion, 3, align = "right", fill = NA)) %>%
  filter(date >= diffusion_start)

latest_diffusion <- diffusion_df %>% slice_max(date, n = 1)

latest_3m <- diffusion_df %>%
  filter(!is.na(diffusion_3m)) %>%
  slice_max(date, n = 1)

# Headline follows the data rather than being retyped each month.
title3 <- if (latest_diffusion$diffusion > 0.5) {
  "More Than Half of Industries Are Gaining Jobs"
} else {
  "Fewer Than Half of Industries Are Gaining Jobs"
}

# Hard right edge leaves room for the two end labels without an Inf-on-Date
# annotation stretching the panel.
diffusion_x_max <- max(diffusion_df$date) %m+% months(14)

# Each label carries the colour of the line it describes: faint for the monthly
# series, solid for the 3-month average.
diffusion_end_labels <- bind_rows(
  latest_diffusion %>%
    transmute(
      date,
      value = diffusion,
      label = paste0("Latest month: ", percent(diffusion, 1)),
      label_color = alpha(positive_color, 0.55)
    ),
  latest_3m %>%
    transmute(
      date,
      value = diffusion_3m,
      label = paste0("3-month average: ", percent(diffusion_3m, 1)),
      label_color = positive_color
    )
)

ggplot(diffusion_df, aes(date, diffusion)) +
  # Below 50% more industries are cutting than adding.
  annotate(
    "rect",
    xmin = diffusion_start,
    xmax = diffusion_x_max,
    ymin = -Inf,
    ymax = 0.5,
    fill = negative_color,
    alpha = 0.07
  ) +
  geom_hline(
    yintercept = 0.5,
    color = negative_color,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  geom_line(color = positive_color, linewidth = 0.5, alpha = 0.45) +
  geom_line(
    aes(y = diffusion_3m),
    color = positive_color,
    linewidth = 1.4,
    na.rm = TRUE
  ) +
  geom_point(
    data = latest_diffusion,
    color = positive_color,
    size = 2.6,
    alpha = 0.5
  ) +
  geom_point(
    data = latest_3m,
    aes(y = diffusion_3m),
    color = positive_color,
    size = 3
  ) +
  geom_text_repel(
    data = diffusion_end_labels,
    aes(date, value, label = label, color = label_color),
    inherit.aes = FALSE,
    hjust = 0,
    nudge_x = 40,
    direction = "y",
    seed = 42,
    size = 4.2,
    fontface = "bold",
    show.legend = FALSE,
    segment.color = "grey55",
    segment.size = 0.3,
    min.segment.length = 0.3
  ) +
  scale_color_identity() +
  scale_y_continuous(
    labels = percent,
    breaks = seq(0, 0.8, 0.1),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_x_date(
    date_labels = "%b\n%Y",
    breaks = date_breaks_gg(12, max(diffusion_df$date)),
    limits = c(diffusion_start, diffusion_x_max),
    expand = expansion(mult = c(0.01, 0))
  ) +
  labs(
    title = title3,
    subtitle = "Share of 250 private-sector subindustries making up the labor market that added jobs over the month.",
    x = NULL,
    y = NULL,
    caption = "BLS, CES, 1-month diffusion index, total private, seasonally adjusted. Mike Konczal, Economic Security Project."
  ) +
  theme_esp() +
  theme(
    plot.subtitle = element_text(size = 12, lineheight = 1.2),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    axis.text = element_text(size = 11)
  )

ggsave(
  "graphics/02d_diffusion_index.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)


# Sahm Rule Graphic -----
sahm_df <- make_Sahm(unrate %>% rename(unemployment = unrate)) %>%
  select(date, sahm_level = diff)

sahm_df %>%
  filter(year(date) > 2022) %>%
  ggplot(aes(date, sahm_level)) +
  geom_line(linewidth = 1.5) +
  geom_hline(yintercept = 0.005) +
  scale_y_continuous(labels = percent) +
  theme_esp() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  labs(
    title = "Progress But Still Near a Threshold",
    subtitle = "Sahm Rule: 3-Month Average of Unemployment Minus Lowest Unemployment Over Past Year; Recession Prediction at 0.5%",
    x = "",
    y = "",
    caption = "Dividing labor force and uemployed levels for extra decimals. Mike Konczal, Economic Security Project."
  ) +
  scale_x_date(
    breaks = date_breaks_gg(6, max(unrate$date)),
    date_labels = "%b %Y"
  ) +
  theme(
    plot.subtitle = element_text(face = "bold", size = 12),
    plot.caption = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    plot.title.position = "plot",
    legend.position = c(0.3, 0.8)
  )

ggsave(
  "graphics/02f_sahm_rule.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)
