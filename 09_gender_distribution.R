
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
