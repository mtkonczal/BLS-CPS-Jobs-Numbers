library(scales)
library(zoo)
library(tidyverse)
library(govMacroTools)
library(lubridate)
library(viridis)

# Generate 100 dates, starting from the maximum date in the input vector and going back X months at a time
generate_dates <- function(dates, X) {
  max_date <- max(dates)
  generated_dates <- seq(max_date, length.out = 100, by = paste0("-", X, " months"))
  return(generated_dates)
}

#jobs_old <- getFRED("PAYEMS")

# Sahm Rule
bls_set_key("c4793807cb6d499ba947c6e321b66436")

ur <- get_n_series_table(
  list(ur_level ='LNS13000000', lf_level = 'LNS11000000', current_ces = 'CES0000000001'),
  start_year = 2015, end_year = 2025, tidy=TRUE
) %>%
  make_date() %>%
  mutate(urate = ur_level/lf_level)

tail(ur)

df <- ur %>%
  left_join(jobs_old, by="date") %>%
  filter(year(date) >= 2022) %>%
  select(date, previous = payems, current_ces) %>%
#  mutate(previous = previous - lag(previous,1),
#         current_ces = current_ces - lag(current_ces,1)) %>%
  pivot_longer(previous:current_ces, values_to = "value", names_to = "type") %>%
  filter(!is.na(value)) %>%
  mutate(type = case_when(
    type == "previous"    ~ "Values in 2024",
    type == "current_ces"   ~ "Values Now",
    TRUE                            ~ type  # fallback in case of unexpected values
  ))

df %>%
  group_by(type) %>%
  mutate(value = value - lag(value)) %>%
  ungroup() %>%
  ggplot(aes(date, value, color=type)) + geom_line(size = 1.5) +
  theme_classic(base_size = 18) +
  scale_color_brewer(palette="Set1") +
  theme(legend.title = element_blank(), legend.text = element_text(size=14)) +
  labs(
    title    = "As Understood at the time, CES jobs were overstated by 20K a month.",
    subtitle = "Monthly employment change, CES Survey. Numbers as of last month and now.",
    x        = "",
    y        = "",
    caption  = "CES. Mike Konczal"
  ) +
  scale_x_date(
    breaks = generate_dates(df$date, 12),
    date_labels = "%b %Y"
  ) +
  theme(
    plot.subtitle          = element_text(face = "bold", size = 12),
    plot.caption = element_text(size = 12),    
    axis.title          = element_text(face = "bold"),
    plot.title.position = "plot",
    legend.position = c(0.8,0.5)
  )

ggsave("tester2.png", width = 12, height = 6, units = "in", dpi = "retina")


ur %>% filter(date == "2021-01-01" | date == "2024-12-01")

df %>%
  group_by(type) %>%
  mutate(diff = value - lag(value)) %>%
  filter(year(date) == 2024) %>%
  reframe(avg = mean(diff))

-137-14-64+10-23-31-56-7-15+1+49+51
