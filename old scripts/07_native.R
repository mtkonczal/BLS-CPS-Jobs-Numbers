library(tidyverse)
library(lubridate)
library(scales)
library(seasonal) # seas(), seasadj()
library(forecast)
library(tidyusmacro)

cps_jobs_data <-getBLSFiles("cps", "konczal@gmail.com")

# ---- Your function (used as-is) ----
seasonal_adjust <- function(x, date) {
  ts_x <- ts(
    x,
    start     = c(year(min(date)), month(min(date))),
    frequency = 12
  )
  as.numeric(seasadj(seas(ts_x)))
}

# ---- Native-born unemployment (rate in decimals) ----
unrate_native <- cps_jobs_data %>%
  filter(
    series_id == "LNU04073413",
    period != "M13"
  ) %>%
  mutate(value = value / 100) %>%
  select(date, series_title, value)

# ---- Prime-age (25–54) native-born EPOP (level in decimals) ----
prime_pop_native <- c("LNU00073417", "LNU00073418", "LNU00073419")
prime_emp_native <- c("LNU02073417", "LNU02073418", "LNU02073419")


prime_pop_native <- c("LNU00073417", "LNU00073418", "LNU00073419")
prime_emp_native <- c("LNU02073417", "LNU02073418", "LNU02073419")

cps_jobs_data %>% filter(series_id == "LNU02073417") %>%
  select(series_title)

cps_jobs_data %>% filter(series_title == "(Unadj) Employment Level - Foreign born, 25 to 34 years") %>%
  select(series_id)


prime_epop_native <- cps_jobs_data %>%
  filter(
    series_id %in% c(prime_pop_native, prime_emp_native),
    period != "M13"
  ) %>%
  group_by(date) %>%
  summarise(
    prime_emp = sum(value[lfst_text == "Employed"], na.rm = TRUE),
    prime_pop = sum(value[lfst_text == "Civilian noninstitutional population"], na.rm = TRUE),
    value = prime_emp / prime_pop,
    series_title = "(Unadj) Prime-Age (25-54) Employment-Population Ratio - Native Born",
    .groups = "drop"
  ) %>%
  select(date, series_title, value)

# ---- Combine, seasonally adjust with your function, and pivot ----
df_sa <- bind_rows(unrate_native, prime_epop_native) %>%
  group_by(series_title) %>%
  mutate(sa_value = seasonal_adjust(value, date)) %>%
  ungroup() %>%
  filter(year(date) >= 2021) %>%
  pivot_longer(c(value, sa_value), names_to = "adjustment", values_to = "val") %>%
  mutate(
    adjustment = recode(adjustment, value = "NSA", sa_value = "SA"),
    adjustment = factor(adjustment, levels = c("NSA", "SA"))
  )

# --- ESP colors (dynamic mapping) ---
esp_colors <- df_sa %>%
  distinct(series_title) %>%
  mutate(col = ifelse(str_detect(series_title, regex("unemployment", ignore_case = TRUE)),
    esp_navy, "#ff8361"
  )) %>%
  deframe()

df_native <- bind_rows(unrate_native, prime_epop_native) %>%
  filter(year(date) >= 2021)



MI_dates <- sort(unique(df$date), decreasing = TRUE)
MI_dates <- MI_dates[seq(1, length(MI_dates), 12)]

# --- Last value per series for dotted horizontal reference line ---
last_vals <- df_native %>%
  group_by(series_title) %>%
  summarise(
    min_date = min(date, na.rm = TRUE),
    last_date = max(date, na.rm = TRUE),
    last_val = value[which.max(date)],
    label = 100*round(last_val, digits = 3),
    .groups = "drop"
  )

# --- Plot: NSA only; dotted segment to last_date + value label ---
ggplot(df_native, aes(date, value, color = series_title)) +
  theme_esp() +
  geom_line(aes(linetype = "Series"), linewidth = 1.2) +
  # dotted horizontal segment that stops at each series' last_date
  geom_segment(
    data = last_vals,
    aes(
      x = min_date, xend = last_date, y = last_val, yend = last_val,
      color = series_title, linetype = "Last value"
    ),
    linewidth = 1.2, show.legend = TRUE
  ) +
  # mark and label the last value
  geom_point(
    data = last_vals,
    aes(x = last_date, y = last_val, color = series_title),
    size = 2.2, show.legend = FALSE
  ) +
  geom_text(
    data = last_vals,
    aes(x = last_date, y = last_val, label = label, color = series_title),
    hjust = -0.1, vjust = 0.5, size = 5.5, show.legend = FALSE
  ) +
  facet_wrap(~series_title, scales = "free_y") +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = esp_colors, name = "Series") +
  scale_linetype_manual(
    values = c("Series" = "solid", "Last value" = "dotted"),
    name = NULL,
    guide = guide_legend(override.aes = list(color = "grey50"))
  ) +
  labs(
    title = "Labor Market Indicators Got Worse For Native-Born Americans in July",
    subtitle = "Values seasonally unadjusted. Native Prime EPOP manually calculated.",
    x = NULL, y = NULL,
    caption = "Source: CPS (BLS). Mike Konczal"
  ) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
  ) +
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
  ) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates, expand = expansion(mult = c(0.02, 0.12)))

ggsave("graphics/g99_native_born.png", dpi = "retina", width = 12, height = 6.75, units = "in")
