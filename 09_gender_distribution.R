
# Keep all years in the raw data
diffusion_jobs <- ces_data %>% filter(data_type_code %in% c(1,10),
                                 industry_code %in% cesDiffusionIndex$industry_code,
                                 seasonal == "S")



ces_data %>% select(data_type_code, data_type_text) %>%
  distinct(data_type_code, .keep_all = TRUE)

gender_jobs <- diffusion_jobs %>%
  select(date, value, data_type_code, industry_name) %>%
  pivot_wider(names_from = data_type_code, values_from = value) %>%
  clean_names() %>%
  rename(total_jobs = x1,
  women_jobs = x10) %>%
  filter(!is.na(women_jobs)) %>%
  mutate(men_jobs = total_jobs - women_jobs,
  percent_men = men_jobs/total_jobs)


gender_jobs %>%
  filter(date == max(date)) %>%
  reframe(total = sum(total_jobs),
total_women = sum(women_jobs),
total_men = sum(men_jobs),
number_industries = n(),
number_majority_men = sum(percent_men > 0.5, na.rm = TRUE))

gender_jobs %>%
  filter(date == max(date)) %>%
  ggplot(aes(x=percent_men)) +
  geom_histogram()

gender_jobs %>%
  filter(year(date) >= 2024) %>%
  mutate(majority_men = percent_men >= 0.5) %>%
  group_by(date, majority_men) %>%
  reframe(total_jobs = sum(total_jobs)) %>%
  ungroup() %>%
  group_by(majority_men) %>%
  mutate(change = total_jobs - lag(total_jobs,1)) %>%
  ungroup() %>%
  ggplot(aes(date, change, color = majority_men)) +
  geom_line()





# women health care


# Keep all years in the raw data
first_take <- ces_data %>% filter(data_type_code %in% c(1,10),
                                 seasonal == "S",
                                industry_name %in% c("Total nonfarm", "Private education and health services"))



library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(scales)

# Start from your filtered pull
first_take <- ces_data %>%
  filter(
    data_type_code %in% c(1, 10),         # 1 = total employees, 10 = women employees
    seasonal == "S",
    industry_name %in% c("Total nonfarm", "Private education and health services")
  )

# 1) Build women/men levels for each industry/date
levels_by_industry <- first_take %>%
  select(date, industry_name, data_type_code, value) %>%
  mutate(sex_tag = if_else(data_type_code == 10, "Women", "Total")) %>%
  select(-data_type_code) %>%
  pivot_wider(names_from = sex_tag, values_from = value) %>%
  mutate(Men = Total - Women) %>%
  select(date, industry_name, Women, Men)

# 2) Split into Health and Total; compute "Other" = Total Nonfarm − Health
total_lvl <- levels_by_industry %>%
  filter(industry_name == "Total nonfarm") %>%
  select(date, Women_total = Women, Men_total = Men)

health_lvl <- levels_by_industry %>%
  filter(industry_name == "Private education and health services") %>%
  select(date, Women_health = Women, Men_health = Men)

other_lvl <- total_lvl %>%
  inner_join(health_lvl, by = "date") %>%
  transmute(
    date,
    sector = "Other",
    Women = Women_total - Women_health,
    Men   = Men_total   - Men_health
  )

health_tidy <- health_lvl %>%
  transmute(
    date,
    sector = "Health",
    Women = Women_health,
    Men   = Men_health
  )

# 3) Tidy long + compute monthly change for each bucket/sex
changes_2025 <- bind_rows(health_tidy, other_lvl) %>%
  pivot_longer(c(Women, Men), names_to = "sex", values_to = "value") %>%
  group_by(sector, sex) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(change = value - lag(value)) %>%
  ungroup() %>%
  filter(year(date) == 2025) %>%
  drop_na(change)

# Optional: nicer legend labels & ordering
changes_2025 <- changes_2025 %>%
  mutate(cat = factor(paste(sector, sex, sep = " — "),
                      levels = c("Health — Women", "Health — Men",
                                 "Other — Women",  "Other — Men")))

# 4) Plot: stacked columns for the four categories each month of 2025
ggplot(changes_2025, aes(x = date, y = change, fill = cat)) +
  geom_col() +
  scale_x_date(date_breaks = "1 month", labels = label_date("%b")) +
  labs(
    title    = "Monthly Change in Jobs, by Sector and Sex (2025)",
    subtitle = "Seasonally adjusted CES. Health vs. all other industries; Women and Men.\nChange is current month minus prior month.",
    x = NULL,
    y = "Change in jobs (thousands)",
    fill = NULL
  ) +
  theme_minimal(base_family = "Public Sans") +
  theme(panel.grid.major.x = element_blank())
