library(tidyverse)
library(ggtext)

library(govMacroTools)

ces_data <- getBLSFiles("ces", "rortybomb@gmail.com")
cps_jobs_data <- getBLSFiles("cps", "rortybomb@gmail.com")

source("scripts/03_graphic_scripts.R")


three_six_wages_title <- "Average Hourly Earnings Remain Volatile"
three_six_wages(ces_data, three_six_wages_title)
ggsave("graphics/wages_3_6.png", dpi = "retina", width = 12, height = 8, units = "in")


total_title <- "Relax, it's a Soft Landing: Job Growth Remains Steady"
total_jobs_graphic(ces_data, total_title)
ggsave("graphics/total_jobs.png", dpi = "retina", width = 12, height = 6.75, units = "in")


detailed_compare_2019(ces_data)
ggsave("graphics/compare_2019_detail.png",  width = 15, height=15, dpi="retina")

double_jobs_chart(ces_data, "Hello there")
ggsave("graphics/double_jobs.png", dpi = "retina", width = 12, height = 8, units = "in")

black_white_comparison(cps_jobs_data)
ggsave("graphics/black_white_comparsion.png", dpi = "retina", width = 12, height = 8, units = "in")

by_type_title <- "Manually calculated categories, percent of labor force"
unemployment_rate_by_type(cps_jobs_data, by_type_title)
ggsave("graphics/u_by_type.png", dpi = "retina", width = 12, height = 8, units = "in")

duration_title <- "Unemployment duration slowly adjusting"
draw_u_duration(cps_jobs_data, duration_title)
ggsave("graphics/duration.png", dpi = "retina", width = 12, height = 8, units = "in")


make_jobs_chart(ces_data)

df <- ces_data %>%
  filter(date == "2024-01-01", seasonal == "S", data_type_code == 3)
three_six_wages(ces_data, graphic_title = "Services Wages", series_analysis = "CES0800000003")

source("2_3_CBO_projections.R")

#source("annual_wrapup.R")




##### Notepad ####

ces_data %>% filter(date == "2024-01-01", seasonal == "S", data_type_code == "1") %>%
  group_by(display_level) %>%
  summarize(total = sum(value))

ces_data %>% filter(data_type_code == 1, seasonal == "S", date == "2024-01-01", display_level == 3) 


df <- ces_data %>% filter(data_type_code == 1, seasonal == "S", month(date) == 1, display_level > 3, display_level <= 6) %>%
  group_by(year, display_level) %>%
  mutate(total_jobs = sum(value)) %>%
  ungroup() %>%
  mutate(per_jobs = value/total_jobs) %>%
  group_by(industry_name) %>%
  summarize(display_level = display_level[date == "2024-01-01"],
            value_2024 = value[date == "2024-01-01"],
            value_2019 = value[date == "2019-01-01"],
            percent_2024 = 100*per_jobs[date == "2024-01-01"],
            percent_2019 = 100*per_jobs[date == "2019-01-01"],
            diff_number = value_2024 - value_2019,
            diff_percent = percent_2024 - percent_2019) %>%
  ungroup() %>%
  arrange(diff_percent)
View(df)

write_csv(df, "job_category_changes_2019_2024.csv")


top_10 <- df %>%
  mutate(pos = diff_percent > 0) %>%
  group_by(pos, display_level) %>%
  mutate(level = abs(diff_percent)) %>%
  arrange(level) %>%
  top_n(10, level) %>%
  ungroup() %>%
  select(industry_name, display_level, pos) %>%
  mutate(keep_this = 1)

df %>%
  left_join(top_10, by=c("industry_name","display_level")) %>%
  filter(keep_this == 1) %>%
  mutate(industry_name = str_replace_all(industry_name, "and other general merchandise", "and merchandise")) %>%
  mutate(industry_name = reorder_within(industry_name, diff_percent, display_level)) %>%
  ggplot(aes(industry_name, diff_percent, fill=pos)) +
  geom_col(size=0) +
  scale_x_reordered() +
  facet_grid(rows = vars(display_level), cols = NULL, scales = "free") +
  coord_flip() +
  theme_lass +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  theme(panel.grid.major.y = element_line(size=0)) +
  theme(plot.title.position = "plot") +
  labs(y = NULL,
       x = NULL,
       subtitle = "Change in the percent of employment, top/bottom 10 occupation categories, broken out by occupational detail category, employment survey, seasonally adjusted",
       caption ="BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = c(0.75,0.5)) +
  geom_text(aes(y = diff_percent + 0.045*sign(diff_percent), label = round(diff_percent,2)), color="white", size=3) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(size = 18))


ggsave("difference_percentages.png", dpi = "retina", width = 18, height = 30, units = "in")




top_10 <- compare_2019 %>%
  mutate(pos = difference > 0) %>%
  group_by(pos) %>%
  mutate(level = abs(difference)) %>%
  arrange(level) %>%
  top_n(10, level) %>%
  ungroup() %>%
  select(industry_name)


compare_2019 %>%
  filter(industry_name %in% top_10$industry_name) %>%
  mutate(industry_name = str_replace_all(industry_name, "and other general merchandise", "and merchandise")) %>%
  mutate(industry_name = reorder_within(industry_name, difference, months_category)) %>%
  ggplot(aes(industry_name, difference, label=difference_label, fill=positive)) +
  geom_col(size=0) +
  scale_x_reordered() +
  coord_flip() +
  theme_lass +
  theme(panel.grid.major.x = element_line(size=0.5)) +
  theme(panel.grid.major.y = element_line(size=0)) +
  theme(plot.title.position = "plot") +
  labs(y = NULL,
       x = NULL,
       title = graphic_title,
       subtitle = "Total Jobs growth last month versus Dec 2019, 5th-level detail category by occupations, employment survey, seasonally adjusted",
       caption ="BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal") +
  theme(axis.text.y = element_text(size=12, face="plain"),
        legend.position = c(0.75,0.5)) +
  geom_text(aes(y = difference_label + 40 * sign(difference_label), label = difference_label), color="white", size=6, ) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(size = 18))



ces_df %>% filter(data_type_code == 1, seasonal == "S", date >= "2024-05-01") %>%
  group_by(display_level, date) %>%
  summarize(n = n())



df <- ces_data %>% filter(data_type_code == 1, seasonal == "S", display_level == 4) %>%
  group_by(industry_code) %>%
  mutate(change3 = value - lag(value,3),
         change3P = value/lag(value, 3) - 1) %>%
  ungroup() %>%
  filter(!is.na(change3)) %>%
  group_by(date) %>%
  summarize(Q25 = quantile(change3, 0.10),
            Q75 = quantile(change3, 0.90),
            median = median(change3)) %>%
  ungroup()

df %>%
  filter(year(date) >= 2015) %>%
  mutate(Q25 = if_else(year(date) %in% c(2020,2021), NA, Q25),
         Q75 = if_else(year(date) %in% c(2020,2021), NA, Q75),
         median = if_else(year(date) %in% c(2020,2021), NA, median)) %>%
  ggplot(aes(x = date)) + geom_ribbon(aes(ymin=Q25, ymax=Q75), fill="skyblue") +
  geom_line(aes(y = median), color="black") +
  theme_lass +
  labs(subtitle = "25th, median, and 75th quantiles for 3-month employment change in 84 distinct (display-level 4) CES subindustries.",
       caption = "Thousands of jobs. 2020-2021 excluded from graphic as outliers. 84 categories cover 100% of jobs. Mike Konczal, Roosevelt Institute.", y="3-month change",
       title="Employment Growth is Broad.") +
  theme(plot.title.position = "plot")


ces_data %>%
  filter(data_type_code == 1, seasonal == "S", display_level == 5) %>%
  group_by(industry_code) %>%
  mutate(neg = value - lag(value, 1) < 0) %>%
  filter(!is.na(neg)) %>%
  ungroup() %>%
  filter(year(date) >= 2000, date <= max(date) %m-% months(1)) %>%
  group_by(date) %>%
  summarize(percent_neg = sum(neg) / n(),
            emp_neg = sum(value[neg==TRUE]/sum(value))) %>%
  ggplot(aes(date, percent_neg)) +
  geom_line(size=1.2) +
  labs(
    subtitle = "Percent of 241 Level-5 subindustries with negative month-over-month job growth.",
    caption = "Mike Konczal, Roosevelt Institute."
  ) +
  theme_classic(base_size = 22) +
  scale_y_continuous(label = percent) +
  theme(plot.title.position = "plot")



dl_post <- ces_data %>% filter(data_type_code == 1, seasonal == "S", display_level <= 2)
#saveRDS(df, "data/2023_ces_data.rds")
dl_0_2_2023 <- readRDS("data/2023_ces_data.rds") %>% mutate(type = "Original 2023 data")
#ces_data %>% filter(data_type_code == 1, seasonal == "S", display_level <= 2) %>%
dl_factor <- dl_0_2_2023 %>% select(industry_code, industry_name, display_level) %>% distinct(industry_code, .keep_all = TRUE) %>% pull(industry_name)

dl_post %>%
  mutate(type = "Revised 2023 data") %>%
  rbind(dl_0_2_2023) %>%
  group_by(type, industry_name) %>%
  mutate(diff = value - lag(value, 12)) %>%
  filter(date == "2023-12-01") %>%
  arrange(display_level, industry_code) %>%
  ungroup() %>%
  mutate(chart_type = case_when(
    industry_name %in% c("Total nonfarm", "Goods-producing", "Private service-providing", "Total private") ~ "Total",
    industry_name %in% c("Mining and logging", "Construction", "Manufacturing") ~ "Goods",
    TRUE ~ "Services"
  )) %>%
  mutate(chart_type = factor(chart_type, levels = c("Total", "Goods", "Services"))) %>%
  filter(industry_name != "Service-providing") %>%
  mutate(industry_nameF = fct_rev(factor(industry_name, levels=dl_factor))) %>%
  ggplot(aes(x = industry_nameF, y = diff, fill=type)) +
  geom_bar(position="dodge", stat="identity", linewidth=0) +
  coord_flip() +
  theme_lass +
  geom_text(aes(y = diff + 175 * sign(diff), label = diff, group = type), color="white", size=5, position = position_dodge(width = 0.9)) +
  labs(title = "Annual and Monthly Updates Net Find More Jobs in 2023.", caption="Mike Konczal, Roosevelt Institute.") +
  theme(legend.position = "bottom")


df <- ces_data %>% filter(seasonal == "S", data_type_code == 1)

unique(df$naics_code)

df %>% filter(date == "2024-01-01") %>%
  group_by(display_level) %>%
  summarize(min = min(nchar(naics_code)),
            max = max(nchar(naics_code)))

df %>% group_by(series_id) %>%
  mutate(positive = value > lag(value,1)) %>%
  ungroup() %>%
  filter(year(date) == 2024, display_level == 5) %>%
  group_by(date, display_level) %>%
  summarize(diffusion_index = sum(positive)/n())


df %>% filter(date == "2024-01-01", display_level == 5)



# prime 25-54 LFP men/women
View(
cps_jobs_data %>% filter(date == "2024-01-01")
)


cps_jobs_data %>% filter(series_id %in% c("LNS11300061","LNS11300062")) %>%
  ggplot(aes(date, value)) + geom_line() +
  facet_wrap(~series_id)