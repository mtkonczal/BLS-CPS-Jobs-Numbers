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


##### TITLES #####
title1 <- "Unemployment Is Increasing"
title2 <- "Weak Initial Number, and Now First Month of Negative Job Growth"
title2_private <- "Federal Buyouts Show Up in Data"
title3 <- "More Industries Are Losing Jobs"
title_gender <- "Men have gained -56 out of a total of 107 jobs since May"

# Set up your bls KEY and put it in your .Renviron using usethis::edit_r_environ()
bls_set_key(Sys.getenv("BLS_KEY"))


positive_color <- "#2c3254" # Bright blue
negative_color <- "#ff8361" # Pale violet

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
  end_year = 2025,
  tidy = TRUE
)

unrate <- unrate %>%
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


# Unemployment Rate ----
unrate %>%
  filter(date > max(date) %m-% months(24)) %>%
  filter(!is.na(unrate)) %>%
  mutate(
    dateTag = if_else(
      date >= max(date) %m-% months(5) | date == "2024-12-01",
      round(unrate, 4),
      NA
    )
  ) %>%
  ggplot(aes(date, unrate, label = percent(dateTag, 0.01))) +
  geom_line(linewidth = 1.2, color = positive_color) +
  geom_text(
    aes(date, dateTag),
    nudge_x = 35,
    color = positive_color,
    size = 5.5
  ) +
  geom_point(aes(date, dateTag), size = 4, color = positive_color) +
  scale_y_continuous(label = percent) +
  theme_esp() +
  labs(
    title = title1,
    subtitle = "Unemployment Rate, Manually Calculated",
    caption = "Mike Konczal, Economic Security Project."
  ) +
  scale_x_date(
    date_labels = "%b\n%Y",
    breaks = date_breaks_gg(6, max(unrate$date))
  ) +
  scale_fill_brewer(palette = "Paired") +
  theme(
    panel.grid.major.y = element_line(color = "grey80")
  )

ggsave(
  "graphics/02a_unrate.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)


# CES Jobs Gained ----
unrate %>%
  mutate(
    dateTag = if_else(date >= max(date) %m-% months(5), ces, NA),
    ces3m = ces + lag(ces, 1) + lag(ces, 2),
    ces3m = ces3m / 3,
    fill_color = if_else(date == max(date), positive_color, negative_color),
    text_color = if_else(date == max(date), positive_color, negative_color)
  ) %>%
  filter(date >= "2023-01-01") %>%
  ggplot(aes(date, ces, label = dateTag)) +
  geom_col(aes(fill = fill_color), size = 0, show.legend = FALSE) +
  geom_text(aes(color = text_color), nudge_y = 10, show.legend = FALSE) +
  labs(
    title = title2,
    subtitle = "Monthly jobs gained. CES",
    caption = "Mike Konczal, Economic Security Project."
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_esp() +
  scale_x_date(
    date_labels = "%b\n%Y",
    breaks = date_breaks_gg(6, max(unrate$date))
  ) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )

ggsave(
  "graphics/02b_jobs_gained.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)


# Private versus nonfederal jobs -----

MI_dates <- date_breaks_n(unrate$date, 6)


plot_df <- unrate %>%
  select(date, federal, nonfederal) %>%
  filter(year(date) >= 2024) %>%
  pivot_longer(
    c(federal, nonfederal),
    names_to = "type",
    values_to = "value"
  ) %>%
  mutate(
    type = recode(
      type,
      federal = "Federal Public Jobs",
      nonfederal = "All Other Jobs"
    )
  )


plot_df %>%
  ggplot(aes(x = date, y = value, fill = type)) +
  geom_col(position = "stack") +
  geom_text(
    data = plot_df %>% filter(year(date) == 2025),
    aes(label = comma(round(value))),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5
  ) +
  scale_fill_manual(
    values = c("Federal Public Jobs" = "#1B7F5A", "All Other Jobs" = "#6A3D9A")
  ) +
  labs(
    title = title2_private,
    subtitle = "CES Data, Thousands.",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "CES. Seasonally-adjusted. Total nonfarm. Mike Konczal, Economic Security Project."
  ) +
  theme_esp() +
  theme(legend.position = "right") +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates)

ggsave(
  "graphics/02c_federal_vs_rest.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)


# Diffusion index ----
unrate %>%
  #  filter(date >= "2023-07-01") %>%
  filter(year(date) >= 2017) %>%
  mutate(
    dateTag = if_else(date >= max(date) %m-% months(0), round(diffusion, 4), NA)
  ) %>%
  ggplot(aes(date, diffusion, label = percent(dateTag))) +
  geom_line(linewidth = 1.2) +
  geom_text(aes(date, dateTag), nudge_x = 70) +
  geom_point(aes(date, dateTag)) +
  scale_y_continuous(label = percent) +
  theme_esp() +
  labs(
    title = title3,
    subtitle = "Percent of Job Categories That Gained Jobs",
    caption = "Mike Konczal, Economic Security Project."
  ) +
  scale_x_date(
    date_labels = "%b\n%Y",
    breaks = date_breaks_gg(6, max(unrate$date))
  ) +
  scale_fill_brewer(palette = "Paired") +
  geom_hline(yintercept = 0.5, color = negative_color, linetype = "dashed") +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )

ggsave(
  "graphics/02d_diffusion_index.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)


# Male jobs -----
gender <- unrate %>%
  mutate(
    diff_total = CES0000000001 - lag(CES0000000001, 1),
    diff_women = CES0000000010 - lag(CES0000000010, 1),
    diff_men = diff_total - diff_women,
    share_women = diff_women / diff_total,
    date = as.Date(paste0(year, "/", month, "/", 1))
  )

MI_dates <- date_breaks_n(gender$date, 6)

shares <- gender %>%
  mutate(
    period = case_when(
      year(date) %in% c(2023, 2024) ~ "2023–2024",
      year(date) >= 2025 ~ "2025",
      year(date) >= 2012 & year(date) <= 2019 ~ "2012-2019",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  summarise(
    women = sum(diff_women, na.rm = TRUE),
    total = sum(diff_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(share = ifelse(total == 0, NA_real_, women / total))

share_2324 <- shares %>% filter(period == "2023–2024") %>% pull(share)
share_2025 <- shares %>% filter(period == "2025") %>% pull(share)
share_0019 <- shares %>% filter(period == "2012-2019") %>% pull(share)

subtitle_text <- paste0(
  "CES Data, Total Nonfarm. Thousands. Women gained ",
  percent(share_2025, accuracy = 0.1),
  " of net job in 2025, YTD, vs ",
  percent(share_2324, accuracy = 0.1),
  " in 2023–2024."
)

plot_df <- gender %>%
  filter(year(date) >= 2023) %>%
  pivot_longer(
    c(diff_men, diff_women),
    names_to = "gender",
    values_to = "jobs"
  ) %>%
  mutate(gender = recode(gender, diff_men = "Men", diff_women = "Women"))


gender_description <- gender %>%
  filter(date >= "2025-05-01") %>%
  reframe(diff_men = sum(diff_men), diff_total = sum(diff_total))

gender_description <- paste0(
  "Men have gained ",
  gender_description$diff_men,
  " out of a total of ",
  gender_description$diff_total,
  " jobs since May 2025."
)


plot_df %>%
  ggplot(aes(x = date, y = jobs, fill = gender)) +
  geom_col(position = "stack") +
  geom_text(
    data = plot_df %>% filter(year(date) == 2025),
    aes(label = comma(round(jobs))),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.2
  ) +
  scale_fill_manual(values = c("Men" = "#2c3254", "Women" = "#ff8361")) +
  labs(
    title = gender_description,
    subtitle = subtitle_text,
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "CES. Seasonally-adjusted. Total nonfarm. Mike Konczal, Economic Security Project."
  ) +
  theme_esp() +
  theme(legend.position = "right") +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates)

ggsave(
  "graphics/02e_gender.png",
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


# CES Jobs Gained ----
unrate %>%
  mutate(
    dateTag = if_else(date >= max(date) %m-% months(5), ces, NA_real_),
    ces3m = (ces + lag(ces, 1) + lag(ces, 2)) / 3,
    fill_color = if_else(date == max(date), positive_color, negative_color),
    text_color = if_else(date == max(date), positive_color, negative_color),
    vjust_lab = if_else(ces < 0, 1.2, -0.3) # BELOW if negative, ABOVE if positive
  ) %>%
  filter(date >= as.Date("2023-01-01")) %>%
  ggplot(aes(date, ces)) +
  geom_col(aes(fill = fill_color), size = 0, show.legend = FALSE) +
  geom_text(
    aes(
      label = ifelse(!is.na(dateTag), scales::comma(dateTag), ""),
      color = text_color,
      vjust = vjust_lab
    ),
    na.rm = TRUE,
    show.legend = FALSE
  ) +
  labs(
    title = title2,
    subtitle = "Monthly jobs gained. CES",
    caption = "Mike Konczal, Economic Security Project."
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_esp() +
  scale_x_date(
    date_labels = "%b\n%Y",
    breaks = date_breaks_gg(6, max(unrate$date))
  ) +
  theme(
    panel.grid.major.y = element_line(color = "grey80")
  )

ggsave(
  "graphics/02g_jobs_gained_fixed.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)
