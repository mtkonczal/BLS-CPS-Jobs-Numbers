
max_month <- month(max(ces_data$date, na.rm = TRUE))
c((max_month - 3):max_month)

ces_data %>%
  filter(display_level == 2) %>%
  filter(seasonal == "S") %>%
  filter(data_type_code == 1) %>%
  group_by(industry_name) %>%
  mutate(change = value - lag(value,1)) %>%
  filter(month(date) %in% c((max_month - 3):max_month)) %>%
  filter(year(date) >= 2024) %>%
  group_by(industry_name, year) %>%
  reframe(avg_change = mean(change)) %>%
  ungroup()
 



ces_data %>%
  filter(display_level == 2) %>%
  filter(seasonal == "S") %>%
  filter(data_type_code == 1) %>%
  group_by(industry_name) %>%
  reframe(date = date,
  change = value - lag(value, 1),
value3 = change + lag(change, 1) + lag(change, 2),
value3 = value3/3) %>%
  ungroup() %>%
  filter(year(date)>=2023) %>%
  ggplot(aes(date, value3)) +
  geom_line() +
  facet_wrap(~industry_name, scales = "free") +
  geom_hline(yintercept = 0)


ces_data

ces_data %>%
  filter(display_level == 2) %>%
  filter(seasonal == "S") %>%
  filter(data_type_code == 1) %>%
  filter()
  group_by(industry_name) %>%
  reframe(date = date,
  change = value - lag(value, 1),
value3 = change + lag(change, 1) + lag(change, 2),
value3 = value3/3) %>%
  ungroup() %>%
  filter(year(date)>=2023) %>%
  ggplot(aes(date, value3)) +
  geom_line() +
  facet_wrap(~industry_name, scales = "free") +
  geom_hline(yintercept = 0)



ces_data %>% filter(seasonal == "S", data_type_code == 1) %>%
  filter(date == max(date)) %>%
  group_by(display_level) %>%
  reframe(n = n(),
sum = sum(value))



ces_data %>% filter(seasonal == "S", data_type_code == 1) %>%
  filter(display_level == 4) %>%
  group_by(industry_name) %>%
  reframe(date = date,
  m_change = value - lag(value, 1)) %>%
  ungroup() %>%
  mutate(month_lab = fct_inorder(format(date, "%b"))) %>%
ggplot(aes(x = month_lab, y = m_change)) +
  geom_boxplot(outliers = FALSE, width = 0.7, fill = "#70ad8f")


jg <- ces_data %>% filter(seasonal == "S", data_type_code == 1) %>%
  filter(date <= max(date) %m-% months(1)) %>%
  filter(industry_name %in% cesDiffusionIndex$industry_title)


jg2 <- jg %>%
  group_by(industry_name) %>%
  reframe(change_2025 = value[date == max(date)] - value[date == "2024-12-01"],
change_2024 = value[date == "2024-12-01"] - value[date == "2023-12-01"],
diff = change_2025 - change_2024) 



library(tidyverse)
library(lubridate)
library(scales)

# --- Compute quintiles by 2024 change ---
jg2 <- jg2 %>%
  mutate(quintile = ntile(change_2024, 5))  # 1 = slowest, 5 = fastest

# --- Join quintile back to original monthly data ---
jg_cat <- jg %>%
  inner_join(jg2 %>% select(industry_name, quintile), by = "industry_name")

View(jg_cat %>% distinct(industry_name, .keep_all = TRUE) %>% select(industry_name, quintile))
# --- Aggregate total job level by quintile ---
jg_sum <- jg_cat %>%
  group_by(date, quintile) %>%
  summarise(total_jobs = sum(value), .groups = "drop") %>%
  arrange(quintile, date)

# --- Normalize to 2023-12 baseline so we’re showing changes ---
jg_sum <- jg_sum %>%
  group_by(quintile) %>%
  mutate(change_since_2023 = total_jobs - lag(total_jobs,1 )) %>%
  ungroup() %>%
  filter(date >= "2024-01-01")

# --- Plot ---
ggplot(jg_sum, aes(x = date, y = change_since_2023, color = factor(quintile))) +
  geom_line(size = 1.2) +
  scale_color_brewer(palette = "RdYlBu", direction = -1,
                     name = "2024 Job Growth Quintile",
                     labels = c("1 = Slowest", "2", "3", "4", "5 = Fastest")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Job Growth by Industry Quintiles (2023–2025)",
    subtitle = "Industries grouped by 2024 job change (fastest to slowest)",
    y = "Change in Jobs Since Dec 2023",
    x = NULL
  ) +
  theme_minimal(base_family = "Public Sans") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 18)
  )

ces_data %>%
  filter(seasonal == "S",
data_type_code == 1) %>%
  filter(date == max(date)) %>%
  group_by(display_level) %>%
  reframe(n = n(),
sum = sum(value))

ces_data %>% filter(display_level == 2) %>%
  distinct(industry_name, .keep_all = TRUE) %>%
  select(industry_name, supersector_name)





library(tidyverse)
library(lubridate)
library(scales)

# jg already defined:
# jg <- ces_data %>% filter(seasonal == "S", data_type_code == 1) %>%
#   filter(date <= max(date) %m-% months(1)) %>%
#   filter(industry_name %in% cesDiffusionIndex$industry_title)

# 1) Monthly change per industry
chg <- jg %>%
  arrange(industry_name, date) %>%
  group_by(industry_name) %>%
  mutate(m_change = value - lag(value)) %>%
  ungroup()

# 2) Within-month quintiles by that month’s change
#    (1 = slowest/most negative, 5 = fastest)
chg_q <- chg %>%
  group_by(date) %>%
  mutate(
    quintile = if (sum(!is.na(m_change)) >= 5) ntile(m_change, 5) else NA_integer_
  ) %>%
  ungroup()

# 3) Aggregate: sum of that month’s change within each quintile
q_sum <- chg_q %>%
  filter(!is.na(quintile)) %>%
  group_by(date, quintile) %>%
  summarise(
    total_monthly_change = sum(m_change, na.rm = TRUE),
    n_industries = n(),
    .groups = "drop"
  ) %>%
  filter(year(date)>= 2024)

# 4) Plot: evolution of total (non-cumulative) monthly change by quintile
ggplot(q_sum, aes(date, total_monthly_change, color = factor(quintile))) +
  geom_line(size = 1.1) +
  scale_color_brewer(
    palette = "RdYlBu", direction = -1,
    name = "Within-Month Quintile\n(1=Slowest, 5=Fastest)",
    labels = c("1", "2", "3", "4", "5")
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Monthly Job Change by Within-Month Quintiles",
    subtitle = "Industries split into quintiles each month by their MoM job change",
    x = NULL, y = "Sum of Monthly Job Change"
  ) +
  (if (exists("theme_esp")) theme_esp() else theme_minimal(base_family = "Public Sans")) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16)
  )


library(tidyverse)
library(lubridate)
library(scales)
library(forcats)

# Start from your 'jg' (already filtered to latest month-1 and valid industries)
# jg <- ces_data %>% filter(seasonal == "S", data_type_code == 1) %>%
#   filter(date <= max(date) %m-% months(1)) %>%
#   filter(industry_name %in% cesDiffusionIndex$industry_title)

# 1) Compute monthly change per industry
chg <- jg %>%
  arrange(industry_name, date) %>%
  group_by(industry_name) %>%
  mutate(m_change = value - lag(value)) %>%
  ungroup()

# 2) Restrict to 2025
chg_2025 <- chg %>%
  filter(year(date) == 2025)

# 3) Make a month label in calendar order
chg_2025 <- chg_2025 %>%
  mutate(month_lab = fct_inorder(format(date, "%b")))

# 4) Boxplots: one box per month, across *all* industries
ggplot(chg_2025, aes(x = month_lab, y = m_change)) +
  geom_boxplot(outliers = FALSE, width = 0.7, fill = "#70ad8f") +
  labs(
    title = "Distribution of Industry Monthly Job Changes (2025)",
    subtitle = "Each box is the across-industry distribution for that month",
    x = NULL,
    y = "Monthly Job Change"
  ) +
  scale_y_continuous(labels = comma) +
  (if (exists("theme_esp")) theme_esp() else theme_minimal(base_family = "Public Sans")) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(vjust = 1)
  )

# Optional: fix extremes if a few outliers dominate the scale
# lims <- quantile(chg_2025$m_change, c(0.01, 0.99), na.rm = TRUE)
# + coord_cartesian(ylim = lims)





library(dplyr)
library(tidyr)
library(ggplot2)

max_month <- month(max(ces_data$date, na.rm = TRUE))
month_spread <- c((max_month - 3):max_month)


industry_changes <- ces_data %>%
  filter(display_level == 2,
         seasonal == "S",
         data_type_code == 1) %>%
  group_by(industry_name) %>%
  mutate(change = value - lag(value)) %>%
  # last 4 months ending max_date, years 2024+
  filter(month(date) %in% ((max_month - 3):max_month),
         year(date) >= 2024) %>%
  group_by(industry_name, yr = year(date)) %>%
  summarise(avg_change = mean(change, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = yr, values_from = avg_change) %>%
  mutate(diff = `2025` - `2024`) %>%
  arrange(diff)

# Build subtitle that explains the 4-month window up to max_date
sub_txt <- paste0(
  "Slowdown = 2025 average minus 2024 average over the last 4 months, ",
  "ending {format(max_date, '%B %Y')}"
)

ggplot(industry_changes, aes(x = reorder(industry_name, diff), y = diff)) +
  geom_col(fill = "#2c3254") +  # ESP navy
  coord_flip(clip = "off") +
  # Put labels just outside the bars
  geom_text(
    aes(
      label = round(diff, 1),
      hjust = ifelse(diff < 0, -0.15, 1.15)  # after flip: left of neg bars, right of pos bars
    ),
    color = "#2c3254",
    size = 3.6
  ) +
  # Add room so outside labels are visible
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.12))) +
  labs(
    title = "Change in Average Monthly Job Growth, 2025 vs 2024",
    subtitle = sub_txt,
    x = NULL,
    y = "Difference (2025 2024), avg monthly jobs"
  ) +
  theme_esp() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 30, 10, 30) # extra room for outside labels
  )