library(tidyverse)
library(ggtext)
library(blsR)
library(govMacroTools)
source("scripts/03_graphic_scripts.R")

bls_api <- "996d4e4af85f43f3ac301805891cbf6e"
bls_set_key(bls_api)

make_date <- function(x){
  x$date <- as.Date(paste(x$month, "01", x$year, sep="/"), "%m/%d/%Y")
  return(x)
}

positive_color <- "#4F97D7" # Bright blue
negative_color <- "#BA68C8" # Pale violet

unrate <- get_n_series_table(
  c("LNS13000000","LNS11000000","CES0500000021","CES0000000001"),
  api_key = bls_get_key(),
  start_year = 2019,
  end_year = 2025,
  tidy = TRUE
)

unrate <- unrate %>%
  mutate(unrate = LNS13000000/LNS11000000,
         date = as.Date(paste0(year,"/",month,"/",1)),
         diffusion = CES0500000021/100,
         ces = CES0000000001 - lag(CES0000000001,1))

MI_dates <- sort(unique(unrate$date), decreasing = TRUE)
MI_dates <- MI_dates[seq(1, length(MI_dates), 6)]

##### TITLES #####
title1 <- "Unemployment Ticks Up in Feburary"
title2 <- "Job Gains Slow in February"
title3 <- "Diffusion Index Falls"



# unrate day
unrate %>%
  filter(date > max(date) %m-% months(24)) %>%
  mutate(dateTag = if_else(date >= max(date) %m-% months(5), round(unrate, 4), NA)) %>%
  ggplot(aes(date, unrate, label = percent(dateTag))) +
  geom_line(size = 1.2, color=positive_color) +
  geom_text(aes(date, dateTag), nudge_x = 25, color=positive_color) +
  geom_point(aes(date, dateTag), size = 4, color=positive_color) +
  scale_y_continuous(label = percent) +
  theme_lass +
  labs(
    title = title1,
    subtitle = "Unemployment Rate, Manually Calculated",
    caption = "Mike Konczal, Economic Security Project"
  ) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates) +
  scale_fill_brewer(palette = "Paired")

ggsave("graphics/new_jobs.png", dpi = "retina", width = 12, height = 6.75, units = "in")


# jobs day
unrate %>%
  mutate(
    dateTag = if_else(date >= max(date) %m-% months(5), ces, NA),
    ces3m = ces + lag(ces, 1) + lag(ces, 2),
    ces3m = ces3m / 3
  ) %>%
  filter(date >= "2023-01-01") %>%
  ggplot(aes(date, ces, label = dateTag)) +
  geom_col(fill=negative_color, size=0) +
  geom_text(aes(date, dateTag), nudge_y = 10, color=negative_color) +
  theme_lass +
  labs(
    title = title2,
    subtitle = "Jobs Gained CES",
    caption = "Mike Konczal, Economic Security Project"
  ) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates)

ggsave("graphics/new_jobs2.png", dpi = "retina", width = 12, height = 6.75, units = "in")


# Diffusion index
unrate %>%
  filter(date >= "2023-07-01") %>%
  mutate(dateTag = if_else(date >= max(date) %m-% months(5), round(diffusion, 4), NA)) %>%
  ggplot(aes(date, diffusion, label=percent(dateTag))) + geom_line(size=1.2) +
  geom_text(aes(date, dateTag), nudge_x = 20) +
  geom_point(aes(date, dateTag)) +
  scale_y_continuous(label = percent) +
  theme_lass +
  labs(title = title3,
       subtitle = "Percent of Job Categories That Gained Jobs",
       caption = "Mike Konczal, Economic Security Project") +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates) +
  scale_fill_brewer(palette = "Paired") +
  geom_hline(yintercept = 0.5, color=negative_color, linetype="dashed")
  

ggsave("graphics/new_jobs3.png", dpi = "retina", width = 12, height = 6.75, units = "in")



