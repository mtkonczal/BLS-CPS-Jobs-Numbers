

all_unrate <- cps_jobs_data %>%
  filter(series_id %in% c("LNS13000000", "LNS11000000")) %>%
  group_by(date) %>%
  reframe(unrate = value[series_id == "LNS13000000"]/value[series_id == "LNS11000000"]) %>%
  ungroup() %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  reframe(unrate = mean(unrate))


all_unrate <- all_unrate %>%
#  filter(month(date) == month_filter) %>%
  mutate(lagged = lag(unrate),
         change = unrate - lag(unrate),
         abs_change = abs(change))

###
all_unrate %>%
ggplot(aes(date, change)) + geom_line()

all_unrate %>%
  ggplot(aes(unrate, change)) + geom_point()



all_unrate %>% 
  mutate(is_latest_year = year == max(year, na.rm = TRUE)) %>%   # flag the latest year
  ggplot(aes(unrate, change, colour = is_latest_year)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("FALSE" = "grey60", "TRUE" = "#2c3254")) +
  guides(colour = "none") +                                      # hide legend (optional)
  labs(
    title = "Unemployment vs. Year-over-Year Change",
    subtitle = paste0("Latest year highlighted: ", max(all_unrate$year, na.rm = TRUE)),
    x = "Unemployment rate",
    y = "Change (pct-pt)"
  ) +
  theme_esp()



# Graphic 1 ----
library(dplyr)
library(ggplot2)
library(scales)
library(glue)

all_unrate %>% 
  slice_min(abs_change, n = 20, with_ties = TRUE) %>%
  ggplot(aes(reorder(factor(year), abs_change), abs_change,
             fill = year == max(year))) +
  geom_col(width = 0.7) +
  geom_text(aes(label = percent(abs_change, accuracy = 0.01)),
            hjust = -0.15, size = 5) +                        # 2️⃣ bigger
  scale_fill_manual(values = c("FALSE" = "grey70",
                               "TRUE"  = "#2c3254")) +
  coord_flip(clip = "off") +
  scale_y_continuous(labels = percent_format(accuracy = 0.01),
                     expand  = expansion(mult = c(0, 0.25))) +
  
  labs(
    title = "Twenty Smallest Year-Over-Year Moves in Unemployment",
    subtitle = glue("{month.name[month_filter]} {max(all_unrate$year)} vs. a year earlier"),
    x = NULL, y = NULL
  ) +
  theme_esp() +
  theme(
    legend.position = "none"
  )



---






all_unrate %>% 
  ggplot(aes(reorder(factor(year), change), change)) +
  # lollipop stem
  geom_segment(aes(xend = factor(year), yend = 0),
               linewidth = 0.8, colour = "grey80") +
  # lollipop head
  geom_point(size = 4,
             aes(colour = year == max(year))) +
  scale_colour_manual(values = c("FALSE" = "grey60",
                                 "TRUE"  = "#2c3254")) +
  coord_flip() +
  labs(
    title = "Smallest Year-over-Year Change in Unemployment on Record",
    subtitle = glue::glue("{month.name[month_filter]} {max(all_unrate$year)} vs. a year earlier"),
    x = NULL, y = "Pct-point change"
  ) +
  theme_esp() +
 theme(legend.position = "none")



all_unrate %>% 
  ggplot(aes(year, change, fill = year == max(year))) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = c("FALSE" = "grey70",
                               "TRUE"  = "#70ad8f")) +
  geom_text(
    data = subset(all_unrate, year == max(year)),
    aes(label = scales::percent(change, accuracy = 0.01)),
    nudge_y = 0.04, colour = "#70ad8f", size = 3.5
  ) +
  labs(
    title = "Year-to-Year Change in the Unemployment Rate",
    subtitle = glue::glue("{month.name[month_filter]} comparisons — latest bar sets a record low"),
    x = NULL, y = "Pct-point change"
  ) + theme(legend.position = "none")


all_unrate %>% 
  mutate(dummy = "change") %>%  # everything on one axis
  ggplot(aes(dummy, change)) +
  geom_boxplot(fill = "grey92", width = 0.35, outlier.shape = NA) +
  geom_jitter(width = 0.05, colour = "grey70", alpha = 0.6, size = 1) +
  geom_point(data = subset(all_unrate, year == max(year)),
             aes(y = change), colour = "#ff8361", size = 4) +
  geom_text(data = subset(all_unrate, year == max(year)),
            aes(y = change, label = "Record-low"), 
            nudge_x = 0.15, colour = "#ff8361", hjust = 0) +
  scale_y_continuous(name = "Pct-point change (YoY)") +
  labs(
    title = "Latest 12-Month Drop Is the Smallest on Record",
    subtitle = glue::glue("{month.name[month_filter]} unemployment vs. same month prior year")
  ) +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())