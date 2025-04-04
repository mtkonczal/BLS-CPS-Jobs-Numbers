library(tidyverse)
library(lubridate)
library(govMacroTools)
library(scales)

ces_jobs <- getBLSFiles("ces", "konczal@gmail.com")


# Dufusion Analysis ----
diffusion_index <- ces_jobs %>%
  filter(seasonal == "S", data_type_code == "01", display_level == 5) %>%
  filter(date <= max(date) %m-% months(1)) %>%
  group_by(series_title) %>%
  mutate(value = value - lag(value,1)) %>%
  mutate(
    diffusion_number_BLS = case_when(
      value > 0 ~ 1,
      value == 0 ~ 0.5,
      TRUE ~ 0
    ),
    diffusion_number_zero = case_when(
      value > 0 ~ 1,
      value == 0 ~ 0,
      TRUE ~ 0
    )
  ) %>%
ungroup() %>%
group_by(date) %>%
  reframe(
    n = n(),
    diffusion_number_BLS = sum(diffusion_number_BLS) / n,
    diffusion_number_zero = sum(diffusion_number_zero) / n,
    diff = diffusion_number_BLS - diffusion_number_zero
  ) %>%
  ungroup()

diffusion_index %>%
  filter(year(date) > 1990) %>%
  ggplot() +
  geom_line(aes(date, diffusion_number_BLS)) +
  geom_line(aes(date, diffusion_number_zero), color="purple")



diffusion_index %>%
  filter(year(date) > 1990) %>%
  ggplot(aes(date, diff)) + geom_line()


ces_jobs %>%
  filter(seasonal == "S", data_type_code == "01", display_level == 5) %>%
  filter(date <= max(date) %m-% months(1)) %>%
  group_by(series_title) %>%
  mutate(value = value - lag(value,1)) %>%
  group_by(date) %>%
  reframe(percent_zero = sum(value == 0)/n()) %>%
  ungroup() %>%
  filter(year(date) > 1991) %>%
  ggplot(aes(date, percent_zero)) + geom_line()



# Manufacturing versus Services -----

df <- ces_jobs %>% filter(seasonal == "S", data_type_code == "03", date == "2025-01-01")

unique(df$supersector_name)

ces_jobs %>% filter(seasonal == "S", data_type_code == "01", date == "2025-01-01") %>%
  mutate(is_manufacturing = supersector_name %in% c("Nondurable Goods","Durable Goods")) %>%
  group_by(display_level, is_manufacturing) %>%
  reframe(n = n(),
          total = sum(value))

df %>%
  mutate(is_manufacturing = supersector_name %in% c("Nondurable Goods","Durable Goods")) %>%
  filter(display_level == 4) %>%
  mutate(industry_name = fct_reorder(industry_name, value)) %>% # reorder industry_name by value
  ggplot(aes(industry_name, value, fill=is_manufacturing)) + 
  geom_col() +
  coord_flip() +
  theme_classic()



# Plot comparing durable vs nondurable goods average hourly wages
df %>%
  mutate(is_manufacturing = supersector_name %in% c("Nondurable Goods", "Durable Goods")) %>%
  filter(display_level == 3) %>%
  mutate(industry_name = fct_reorder(industry_name, value),
         is_manufacturing = factor(is_manufacturing, levels = c(TRUE, FALSE), labels = c("Durable/Nondurable Goods", "Other Industries"))) %>%
  ggplot(aes(industry_name, value, fill = is_manufacturing)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("#D55E00", "#0072B2"), name = NULL) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Average Hourly Wages: Durable vs Nondurable Goods Industries",
    subtitle = "Comparing average hourly wages across industries (BLS CES Data)",
    x = "Industry",
    y = "Average Hourly Wage",
    caption = "Source: Bureau of Labor Statistics, Current Employment Statistics (CES)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title.position = "plot"
  )



ces_jobs %>% filter(seasonal == "S", data_type_code == "03", date == "2025-01-01") %>%
  filter(display_level == 2) %>%
  select(series_title, value) %>%
  arrange(desc(value))