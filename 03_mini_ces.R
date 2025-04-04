library(tidyverse)
library(ggtext)
library(blsR)
library(govMacroTools)
source("scripts/03_graphic_scripts.R")

mini_ces <- getBLSFiles("ces_allemp", "konczal@gmail.com")

mini_ces <- mini_ces %>% filter(year(date)>2010) %>% mutate(data_type_code = as.numeric(data_type_code))
  
ces_last <- mini_ces %>% filter(series_id == "CES0000000001") %>% filter(date == max(date)) %>% pull(value)

chart_date <- format(max(mini_ces$date, na.rm = TRUE), "%B %Y")
  


mini_ces %>% filter(date == max(date)) %>%
  select(series_title, display_level)



jobs_chart <- mini_ces %>%
  filter(seasonal == "S") %>%
  filter(display_level <= 2 | industry_name %in% c("State government", "Local government", "Federal")) %>%
  group_by(industry_name) %>%
  reframe(
    change = value[date == max(date)] - value[date == max(date) %m-% months(1)],
    change3 = value[date == max(date)] - value[date == max(date) %m-% months(3)],
    change12 = value[date == max(date)] - value[date == max(date) %m-% months(12)],
    series_id = series_id[date == max(date)]
  ) %>%
  mutate(
    change3 = round(change3 / 3),
    change12 = round(change12 / 12)
  ) %>%
  mutate(
    chart_type = case_when(
      industry_name %in% c("Total nonfarm", "Goods-producing", "Private service-providing", "Total private") ~ "Total",
      industry_name %in% c("Mining and logging", "Construction", "Manufacturing") ~ "Goods",
      industry_name %in% c("State government", "Local government", "Federal") ~ "Government",
      TRUE ~ "Services"
    )
  ) %>%
  mutate(chart_type = factor(chart_type, levels = c("Total", "Government", "Goods", "Services"))) %>%
  filter(industry_name != "Service-providing" & industry_name != "Government")

jobs_chart %>%
  mutate(industry_name = if_else(industry_name == "Total nonfarm", "All jobs", industry_name)) %>%
  arrange(industry_name) %>%
  select(-series_id) %>%
  gt(groupname_col = "chart_type") %>%
  row_group_order(groups = c("Total", "Goods", "Services", "Government")) %>%
  tab_header(
    title = md(paste0("**Summary of the ", chart_date, " Jobs Number**")),
    subtitle = "All numbers in thousands"
  ) %>%
  cols_label(
    change = "Last-Month",
    change3 = "3-Month",
    change12 = "12-Month",
    industry_name = ""
  ) %>%
  tab_source_note(
    source_note = "BLS CES data, author's calculations. Mike Konczal, Economic Security Project"
  ) %>%
  opt_stylize(style = 6, color = "blue") %>%
  tab_spanner(
    label = "Employment Change",
    columns = c(change, change3, change12)
  ) %>%
  sub_missing(missing_text = "") %>%
  cols_align(columns = c(change3, change12), align = "center") %>%
  cols_align(columns = c(industry_name), align = "right") %>%
  cols_align(columns = industry_name, align = "left") %>%
  gtsave(filename = "graphics/jobs_chart.png")
