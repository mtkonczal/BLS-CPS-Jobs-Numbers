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
title1 <- "Unemployment Picks Up in July"
title2 <- "Revisions Wipe Out Job Gains in Recent Months"
title3 <- "Only Half of Industries Are Adding Jobs"

# Set up your bls KEY and put it in your .Renviron using usethis::edit_r_environ()
bls_set_key(Sys.getenv("BLS_KEY"))


positive_color <- "#2c3254" # Bright blue
negative_color <- "#ff8361" # Pale violet

unrate <- get_n_series_table(
  c("LNS13000000","LNS11000000","CES0500000021","CES0000000001"),
  api_key = bls_get_key(),
  start_year = 2011,
  end_year = 2025,
  tidy = TRUE
)

unrate <- unrate %>%
  mutate(unrate = LNS13000000/LNS11000000,
         date = as.Date(paste0(year,"/",month,"/",1)),
         diffusion = CES0500000021/100,
         ces = CES0000000001 - lag(CES0000000001,1))


# Unemployment Rate ----
unrate %>%
  filter(date > max(date) %m-% months(24)) %>%
  mutate(dateTag = if_else(date >= max(date) %m-% months(2), round(unrate, 4), NA)) %>%
  ggplot(aes(date, unrate, label = percent(dateTag, 0.01))) +
  geom_line(size = 1.2, color=positive_color) +
  geom_text(aes(date, dateTag), nudge_x = 25, color=positive_color) +
  geom_point(aes(date, dateTag), size = 4, color=positive_color) +
  scale_y_continuous(label = percent) +
  theme_esp() +
  labs(
    title = title1,
    subtitle = "Unemployment Rate, Manually Calculated",
    caption = "Mike Konczal"
  ) +
  scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks_gg(6, max(unrate$date))) +
  scale_fill_brewer(palette = "Paired") +
  theme(
    panel.grid.major.y = element_line(color = "grey80")
  )

ggsave("graphics/g1_unrate.png", dpi = "retina", width = 12, height = 6.75, units = "in")



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
    caption = "Mike Konczal"
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_esp() +
  scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks_gg(6, max(unrate$date))) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )

ggsave("graphics/g2_jobs_gained.png", dpi = "retina", width = 12, height = 6.75, units = "in")


# Diffusion index ----
unrate %>%
#  filter(date >= "2023-07-01") %>%
  filter(year(date) >= 2017) %>%
  mutate(dateTag = if_else(date >= max(date) %m-% months(0), round(diffusion, 4), NA)) %>%
  ggplot(aes(date, diffusion, label=percent(dateTag))) + geom_line(size=1.2) +
  geom_text(aes(date, dateTag), nudge_x = 70) +
  geom_point(aes(date, dateTag)) +
  scale_y_continuous(label = percent) +
  theme_esp() +
  labs(title = title3,
       subtitle = "Percent of Job Categories That Gained Jobs",
       caption = "Mike Konczal, Economic Security Project") +
  scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks_gg(6, max(unrate$date))) +
  scale_fill_brewer(palette = "Paired") +
  geom_hline(yintercept = 0.5, color=negative_color, linetype="dashed")  +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  )
  
ggsave("graphics/g3_diffusion_index.png", dpi = "retina", width = 12, height = 6.75, units = "in")




# Sahm Rule Graphic -----
df <- make_Sahm(unrate %>% rename(unemployment = unrate)) %>% select(date, sahm_level = diff)

df %>%
  filter(year(date)> 2022) %>%
  ggplot(aes(date, sahm_level)) + geom_line(size = 1.5) +
  geom_hline(yintercept = 0.005) +
  scale_y_continuous(labels = percent) +
  theme_esp() +
  scale_color_brewer(palette="Set1") +
  theme(legend.title = element_blank(), legend.text = element_text(size=14)) +
  labs(
    title    = "Progress But Still Near a Threshold",
    subtitle = "Sahm Rule: 3-Month Average of Unemployment Minus Lowest Unemployment Over Past Year; Recession Prediction at 0.5%",
    x        = "",
    y        = "",
    caption  = "Dividing labor force and uemployed levels for extra decimals. Mike Konczal"
  ) +
  scale_x_date(
    breaks = date_breaks_gg(6, max(unrate$date)),
    date_labels = "%b %Y"
  ) +
  theme(
    plot.subtitle          = element_text(face = "bold", size = 12),
    plot.caption = element_text(size = 12),    
    axis.title          = element_text(face = "bold"),
    plot.title.position = "plot",
    legend.position = c(0.3,0.8)
  )

ggsave("graphics/g4_sahm_rule.png", dpi = "retina", width = 12, height = 6.75, units = "in")