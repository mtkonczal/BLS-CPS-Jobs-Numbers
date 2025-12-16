title_cyc <- "Cyclical Industries Slowing"
graph_start_year <- 2016


library(broom)
# Plot Correlation Diffusion ----

#diffusion_industries <- read_csv("Good Scripts to Run/ces_diffusion_index_series/Manufacturing DI series-Table 1.csv")
diffusion_industries <- cesDiffusionIndex

diffusion_unrate <- unrate %>% select(date, unrate)

# Keep all years in the raw data
diffusion_jobs <- ces_data %>%
  filter(
    data_type_code == 1,
    industry_code %in% diffusion_industries$industry_code,
    seasonal == "S"
  ) %>%
  left_join(diffusion_unrate, by = "date") %>%
  select(date, total_jobs = value, unrate, industry_name)

# Step 1: Calculate 6-month changes using ALL years
job_changes <- diffusion_jobs %>%
  arrange(industry_name, date) %>%
  group_by(industry_name) %>%
  mutate(
    emp_change_6m = (total_jobs - lag(total_jobs, 6)) / lag(total_jobs, 6),
    unrate_change_6m = unrate - lag(unrate, 6)
  ) %>%
  ungroup()

# Step 2: Restrict to 1991–2019 for regression estimation
regression_sample <- job_changes %>%
  filter(year(date) >= 1991, year(date) <= 2019)

industry_coefs <- regression_sample %>%
  filter(!is.na(emp_change_6m), !is.na(unrate_change_6m)) %>%
  group_by(industry_name) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(emp_change_6m ~ unrate_change_6m, data = .x)),
    tidy_model = map(model, ~ tidy(.x))
  ) %>%
  unnest(tidy_model) %>%
  filter(term == "unrate_change_6m") %>%
  select(industry_name, estimate)


# Step 3: Classify sensitivity groups
quantiles <- quantile(industry_coefs$estimate, probs = c(0.33, 0.66))

#Fix
industry_coefs <- industry_coefs %>%
  mutate(
    sensitivity_group = case_when(
      estimate >= quantiles[2] ~ "Low",
      estimate >= quantiles[1] ~ "Medium",
      TRUE ~ "High"
    )
  ) %>%
  mutate(
    sensitivity_group = factor(
      sensitivity_group,
      levels = c("Low", "Medium", "High")
    )
  )

MI_datesCYC <- sort(
  ces_data %>%
    filter(seasonal == "S") %>%
    filter(date <= max(date) %m-% months(1)) %>%
    distinct(date) %>%
    pull(),
  decreasing = TRUE
)
MI_datesCYC <- MI_datesCYC[seq(1, length(MI_datesCYC), 48)]

# Job growth by sensititvity: 1 month ----
ces_data %>%
  filter(seasonal == "S", data_type_code == 1, year >= graph_start_year) %>%
  filter(date <= max(date) %m-% months(1)) %>%
  inner_join(industry_coefs, by = "industry_name") %>%
  group_by(series_title) %>%
  mutate(job_growth = (value - lag(value, 3)) / 3) %>%
  ungroup() %>%
  group_by(date, sensitivity_group) %>%
  reframe(job_growth = sum(job_growth)) %>%
  ungroup() %>%
  mutate(
    job_growth = if_else(year(date) %in% c(2020, 2021), NA, job_growth)
  ) %>%
  ggplot(aes(date, job_growth, color = sensitivity_group)) +
  geom_line(show.legend = FALSE, size = 1.2) +
  theme_esp() +
  facet_wrap(~sensitivity_group) +
  geom_hline(yintercept = 0, size = 1) +
  labs(
    subtitle = "Three month average job gain by cyclical sensitivity. 2020-2021 removed.",
    y = "",
    x = "",
    title = title_cyc,
    caption = "250 Diffusion sub-industries 6-month job gains correlated against unemployment changes on 1991 to 2019 data. BLS. Mike Konczal, ESP."
  ) +
  theme(plot.title.position = "plot") +
  scale_x_date(
    breaks = MI_datesCYC,
    date_labels = "%b\n%Y"
  )

ggsave(
  "graphics/g8_cyclical_industries.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)


# Find current monthly cyclicality ----

ces_data %>%
  filter(seasonal == "S", data_type_code == 1) %>%
  filter(date == max(date)) %>%
  group_by(display_level) %>%
  reframe(sum = sum(value), n = n())
