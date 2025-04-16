# This script looks at employment growth both:
# - in periods of low unemployment, and
# - since 1980
# It creates two graphics in the graphics folder.
# It requires CES (for employment) and CPS (for unemployment rate) numbers.
# Written by: Mike Konczal, Roosevelt Institute
# Last Updated: 7/6/2021


library(tidyverse)
library(ggtext)
library(ggrepel)
library(scales)
library(lubridate)
library(tidytext)
library(gt)
library(hrbrthemes)



theme_lass <-   theme_modern_rc() + theme(legend.position = "none", legend.title = element_blank(),
                                          panel.grid.major.y = element_line(size=0.5),
                                          panel.grid.minor.y = element_blank(),
                                          plot.title.position = "plot",
                                          axis.title.x = element_blank(),
                                          axis.title.y = element_blank(),
                                          plot.title = element_text(size = 25, face="bold"),
                                          plot.subtitle = element_text(size=15, color="white"),
                                          plot.caption = element_text(size=10, face="italic"),
                                          legend.text = element_text(size=12),
                                          axis.text.y = element_text(size=12, face="bold"),
                                          axis.text.x = element_text(size=12, face="bold"),
                                          strip.text = element_text(face = "bold", color="white", hjust = 0.5, size = 10),
                                          strip.background = element_blank()) +
  theme(text = element_text(family = "Larsseit"),
        plot.title = element_text(family = "Larsseit"),
        plot.subtitle = element_text(family = "Larsseit"),
        plot.caption = element_text(family="Larsseit"),
        strip.text = element_text(family="Larsseit"))



total_jobs_graphic <- function(ces_data, graphic_title = "Default title", length = 3) {
  ces_dates <- unique(ces_data$date)
  ces_dates <- sort(ces_dates, decreasing = TRUE)
  ces_dates <- ces_dates[seq(1, length(ces_dates), 3)]

  jobs_growth <- ces_data %>%
    filter(series_id == "CES0000000001") %>%
    arrange(date) %>%
    mutate(jobs_difference = value - lag(value, 1)) %>%
    mutate(jobs_difference_label = if_else(date >= max(date, na.rm = TRUE) %m-% months(6), jobs_difference, as.numeric(NA))) %>%
    mutate(before_trend = (value[date == "2020-01-01"] - value[date == "2018-01-01"]) / 24) %>%
    filter(date >= "2021-01-01")

  avg_job_growth <- mean(jobs_growth$before_trend)

  jobs_growth %>%
    ggplot(aes(date, jobs_difference, label = jobs_difference_label)) +
    geom_bar(stat = "identity") +
    geom_text(nudge_y = 30) +
    theme_lass +
    geom_line(aes(date, before_trend), color = "red", size = 1.2) +
    labs(
      y = NULL,
      x = NULL,
      title = graphic_title,
      subtitle = paste("Total job growth, employment survey, seasonally-adjusted. Red line reflects 2018-19 average growth of ", avg_job_growth, " thousand.", sep = ""),
      caption = "BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal"
    ) +
    scale_x_date(date_labels = "%b\n%Y", breaks = ces_dates)
}


detailed_compare_2019 <- function(ces_data, graphic_title = "Top 10 Job Growers/Losers Since 2019 of 241 Industries, in Thousands"){

  compare_2019 <- ces_data %>% filter(seasonal == "S", (display_level == 5), data_type_code == 1) %>% group_by(industry_name) %>%
    summarize(before = value[date == "2019-12-01"],
              after = value[date == max(date)]) %>%
    mutate(difference = after - before,
           months_category = "Since End of 2019",
           difference_label = round(difference),
           positive = difference_label >= 0) %>%
    ungroup()
  
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

}

double_jobs_chart <- function(ces_data, graphic_title = "Default Title", months_difference = 6) {
  one_month <- ces_data %>%
    filter(seasonal == "S", (display_level == 2), data_type_code == 1) %>%
    group_by(industry_name) %>%
    summarize(
      before = value[date == max(date) %m-% months(1)],
      after = value[date == max(date)]
    ) %>%
    mutate(difference = after - before, months_category = "Last 1 Month")

  one_month_total <- sum(one_month$difference)
  one_month$months_category <- paste("Last 1 Month, ", one_month_total, " thousand jobs", sep = "")

  double_jobs_graphic <- ces_data %>%
    filter(seasonal == "S", (display_level == 2), data_type_code == 1) %>%
    group_by(industry_name) %>%
    summarize(
      before = value[date == max(date) %m-% months(months_difference)],
      after = value[date == max(date)]
    ) %>%
    mutate(difference = after - before, difference = difference / months_difference) %>%
    arrange(desc(difference))

  six_months_total <- sum(double_jobs_graphic$difference)
  double_jobs_graphic$months_category <- paste("Last 6 Months, Average Monthly, ", round(six_months_total), " thousand jobs", sep = "")

  double_jobs_graphic <- double_jobs_graphic %>%
    rbind(one_month) %>%
    mutate(difference_label = round(difference)) %>%
    mutate(positive = difference_label >= 0)

  double_jobs_graphic %>%
    mutate(industry_name = reorder_within(industry_name, difference, months_category)) %>%
    ggplot(aes(industry_name, difference, label = difference_label, fill = positive)) +
    geom_col(size = 0) +
    scale_x_reordered() +
    facet_wrap(~months_category, scales = "free_y", ncol = 1) +
    coord_flip() +
    theme_lass +
    theme(panel.grid.major.x = element_line(size = 0.5)) +
    theme(panel.grid.major.y = element_line(size = 0)) +
    theme(plot.title.position = "plot") +
    labs(
      y = NULL,
      x = NULL,
      title = graphic_title,
      subtitle = "Job growth last month versus average of the last three months, by occupations, employment survey, seasonally adjusted",
      caption = "BLS, CES, seasonally adjusted values. Author's calculation. Mike Konczal"
    ) +
    theme(
      axis.text.y = element_text(size = 12, face = "plain"),
      legend.position = c(0.75, 0.5)
    ) +
    geom_text(aes(y = difference_label + 4 * sign(difference_label), label = difference_label), color = "white", size = 6, ) +
    scale_y_continuous(position = "right") +
    theme(legend.position = "none") +
    theme(strip.text.x = element_text(size = 15))
}

black_white_comparison <- function(cps_jobs_data, graphic_title = "Default Title", date_break_length = 36) {
  b_w_breaks <- unique(cps_jobs_data$date)
  b_w_breaks <- sort(b_w_breaks, decreasing = TRUE)
  b_w_breaks <- b_w_breaks[seq(1, length(b_w_breaks), date_break_length)]

  cps_jobs_data %>%
    filter(series_id %in% c("LNS14000006", "LNS14000003")) %>%
    group_by(date) %>%
    summarize(
      black_u = value[series_id == "LNS14000006"],
      relative_diff = value[series_id == "LNS14000006"] / value[series_id == "LNS14000003"],
      absolute_diff = value[series_id == "LNS14000006"] - value[series_id == "LNS14000003"]
    ) %>%
    pivot_longer(black_u:absolute_diff, names_to = "type", values_to = "values") %>%
    ungroup() %>%
    group_by(type) %>%
    mutate(dotted_line = values[date == max(date)]) %>%
    mutate(
      type = str_replace_all(type, "black_u", "Black Unemployment"),
      type = str_replace_all(type, "absolute_diff", "Black Unemployment Minus White Unemployment"),
      type = str_replace_all(type, "relative_diff", "Black Unemployment Divided By White Unemployment"),
    ) %>%
    ungroup() %>%
    mutate(type_F = factor(type, levels = c("Black Unemployment", "Black Unemployment Minus White Unemployment", "Black Unemployment Divided By White Unemployment"))) %>%
    mutate(values = values / 100, dotted_line = dotted_line / 100) %>%
    filter(year(date) > 2001) %>%
    ggplot(aes(date, values, color = type_F)) +
    geom_line() +
    geom_line(aes(date, dotted_line, color = type_F), linetype = "dashed") +
    theme_lass +
    theme(legend.position = c(0.25, 0.85)) +
    labs(title = "Black-White Unemployment Gaps Among Lowest Levels Across Measures", caption = "Dotted Line is Last Value. BLS, CPS, Seasonally-Adujusted. Mike Konczal, Roosevelt Institute") +
    scale_y_continuous(labels = percent) +
    scale_x_date(date_labels = "%B\n%Y", breaks = b_w_breaks)
  
}

unemployment_rate_by_type <- function(cps_jobs_data, graphic_title = "Default title.", start_date = "2022-01-01", axis_months = 6){
  job_leavers <- cps_jobs_data %>% filter(series_id %in% c("LNS13023705","LNS11000000")) %>%
    group_by(date) %>%
    reframe(better_percent = value[series_id == "LNS13023705"]/value[series_id == "LNS11000000"]) %>%
    filter(!is.na(better_percent)) %>%
    mutate(series_title = "Job Leavers Unemployment Rate") %>%
    ungroup()
  
  entrants <- cps_jobs_data %>% filter(series_id %in% c("LNS13023557","LNS13023569","LNS11000000")) %>%
    group_by(date) %>%
    reframe(better_percent = (value[series_id == "LNS13023557"]+value[series_id == "LNS13023569"])/value[series_id == "LNS11000000"]) %>%
    filter(!is.na(better_percent)) %>%
    mutate(series_title = "New Entrants and Reentrants") %>%
    ungroup()
  
  on_temporary_layoff <- cps_jobs_data %>% filter(series_id %in% c("LNS13023653","LNS11000000")) %>%
    group_by(date) %>%
    reframe(better_percent = value[series_id == "LNS13023653"]/value[series_id == "LNS11000000"]) %>%
    filter(!is.na(better_percent)) %>%
    mutate(series_title = "Job Losers on Temporary Layoff") %>%
    ungroup()
  
  not_on_temporary_layoff <- cps_jobs_data %>% filter(series_id %in% c("LNS13025699","LNS11000000")) %>%
    group_by(date) %>%
    reframe(better_percent = value[series_id == "LNS13025699"]/value[series_id == "LNS11000000"]) %>%
    filter(!is.na(better_percent)) %>%
    mutate(series_title = "Job Losers Not on Temporary Layoff") %>%
    ungroup()
  
  urate_mine <- rbind(job_leavers,entrants,on_temporary_layoff,not_on_temporary_layoff) %>% group_by(date) %>% summarize(this_u_rate_works_maybe = sum(better_percent)) %>% ungroup()
  year_delay <- start_date
  
  g_dates <- unique(job_leavers$date)
  g_dates <- sort(g_dates, decreasing = TRUE)
  g_dates <- g_dates[seq(1, length(g_dates), axis_months)]
  
  rbind(job_leavers,entrants,on_temporary_layoff,not_on_temporary_layoff) %>%
    rename(value = better_percent) %>%
    group_by(series_title) %>%
    mutate(pre_value = mean(value[year(date)==2019])) %>%
    mutate(last_value = if_else(date == max(date), value, as.numeric(NA))) %>%
    ungroup() %>%
    mutate(pre_value = if_else(year(date)>=2019,pre_value,as.numeric(NA))) %>%
    filter(date >= year_delay) %>%
    ggplot(aes(date,value, color=series_title,label=label_percent()(round(last_value,3)))) +
    geom_line(size=1.2) +
    #geom_point(size=2) +
    theme_lass + facet_wrap(~series_title, scales = "free") +
    scale_y_continuous(labels = percent) +
    scale_x_date(date_labels = "%b\n%Y", breaks=g_dates) +
    labs(title=graphic_title,
         subtitle="Unemployment rate contribution, by category of unemployment. Dotted line is average 2019 value.",
         caption="BLS, CPS, Seasonally-Adjusted, Mike Konczal") +
    scale_color_manual(values=c("#6EA4BF","#2D779C", "#97BC56","#E2E47E")) +
    geom_line(aes(date,pre_value,color=series_title), linetype="dashed") +
    theme(strip.text.x = element_text(size = 15))
    #geom_text(show.legend=FALSE, nudge_x = 50, size = 5)
  
}

draw_u_duration <- function(cps_jobs_data, graphic_title = "Default graphic.") {

  g_dates <- sort(unique(cps_jobs_data$date), decreasing = TRUE)
  g_dates <- g_dates[seq(1, length(g_dates), 12)]

  u_duration_series <- c("(Seas) Median Weeks Unemployed", "(Seas) Average Weeks Unemployed")

  cps_jobs_data %>%
    filter(series_title %in% u_duration_series, periodicity_code == "M") %>%
    group_by(series_title) %>%
    mutate(pre_value = mean(value[year(date) == 2019])) %>%
    mutate(last_value = if_else(date == max(date), value, as.numeric(NA))) %>%
    ungroup() %>%
    mutate(pre_value = if_else(year(date) >= 2019, pre_value, as.numeric(NA))) %>%
    filter(date >= "2017-01-01") %>%
    mutate(series_title = str_remove(series_title, "\\(Seas\\)")) %>%
    ggplot(aes(date, value, color = series_title, label = last_value)) +
    geom_line(size = 2) +
    theme_lass +
    geom_line(aes(date, pre_value, color = series_title), linetype = "dashed") +
    theme(legend.position = c(0.3, 0.9)) +
    scale_x_date(date_labels = "%b\n%Y", breaks = g_dates) +
    labs(
      title = graphic_title,
      subtitle = "Average and median weeks of unemployment length, dotted line is average 2019 value.",
      caption = "BLS, CPS, Seasonally-Adjusted, Mike Konczal"
    ) +
    scale_color_manual(values = c("#2D779C", "#97BC56")) +
    geom_text(show.legend = FALSE, nudge_x = 60, size = 5.5)
}

three_six_wages <- function(ces_data, graphic_title = "Wages trend default title.", series_analysis = "CES0500000003") {
  
  ces_data <- ces_data %>% filter(series_id == series_analysis)
  
  AHE <- ces_data %>%
    select(date, value) %>%
    mutate(ThreeMonth = (value / lag(value, 3))^4 - 1) %>%
    mutate(SixMonth = (value / lag(value, 6))^2 - 1) %>%
    mutate(YoY = (value / lag(value, 12)) - 1) %>%
    select(-value, YoY) %>%
    pivot_longer(ThreeMonth:SixMonth, names_to = "time_length", values_to = "change") %>%
    mutate(time_length = str_replace_all(time_length, "SixMonth", "6-Month Change")) %>%
    mutate(time_length = str_replace_all(time_length, "ThreeMonth", "3-Month Change")) %>%
    mutate(last_value = ifelse(date == max(date), change, NA))

  one_month_change <- ces_data %>%
    select(date, value) %>%
    mutate(one_month = value/lag(value,1)) %>%
    mutate(one_month = one_month^12-1) %>%
    select(date, one_month)


  MI_dates <- sort(unique(ces_data$date), decreasing = TRUE)
  MI_dates <- MI_dates[seq(1, length(MI_dates), 12)]

  date_start <- "2018-01-01"
  date_end <- "2020-01-01"
  date_period <- interval(date_start, date_end)
  date_period <- date_period %/% months(1)

  pre_AHE <- ces_data %>%
    filter((date == date_start | date == date_end)) %>%
    mutate(change = value / lag(value, 1)) %>%
    filter(!is.na(change)) %>%
    mutate(change = change^(12 / date_period) - 1) %>%
    select(change)
  pre_AHE <- as.numeric(pre_AHE)

  AHE %>%
    filter(date > "2020-12-01") %>%
    left_join(one_month_change, by = "date") %>%
    mutate(one_month = if_else(one_month == lag(one_month, 1), as.double(NA), one_month)) %>%
    ggplot(aes(date, change, color = time_length, label = label_percent()(round(last_value, 3)))) +
    geom_line(size = 3) +
    geom_col(aes(date, one_month), alpha = 0.25, size = 0, show.legend = FALSE) +
    labs(
      x = "", y = "",
      title = graphic_title,
      subtitle = paste("Annualized, monthly average hourly earnings of all employees, total private.\nBars are 1-month annualized. Dotted line represents 2018-2019 value of ", round(100 * pre_AHE, 1), "%.", sep = ""), # , round(pre_core,3)*100, "%.", sep=""),
      caption = "Dotted line is annualized, BLS, Author's calculations. Mike Konczal, Roosevelt Institute."
    ) +
    theme_lass +
    geom_hline(yintercept = pre_AHE, linetype = "dashed", color = "#A4CCCC") +
    scale_fill_brewer(palette = "Paired") +
    theme(panel.grid.major.y = element_line(size = 0.5)) +
    theme(plot.title.position = "plot") +
    scale_y_continuous(labels = percent) +
    scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates) +
    theme(legend.position = c(0.85, 0.80), legend.text = element_text(size = 15)) +
    scale_color_manual(values = c("#2D779C", "#A4CCCC")) +
    geom_text(show.legend = FALSE, nudge_x = 44, size = 5.5)
}

make_jobs_chart <- function(ces_data) {
  
  ces_data <- ces_data %>% filter(year(date)>2010) %>% mutate(data_type_code = as.numeric(data_type_code))
  
  ces_last <- ces_data %>% filter(series_id == "CES0000000001") %>% filter(date == max(date)) %>% pull(value)
  ces_2019 <- ces_data %>% filter(series_id == "CES0000000001") %>% filter(date == "2019-12-01") %>% pull(value)
  
  jobs_chart <- ces_data %>%
    filter(seasonal == "S") %>%
    filter(display_level <= 2, data_type_code == 1) %>%
    group_by(industry_name) %>%
    reframe(
      change = value[date == max(date)] - value[date == max(date) %m-% months(1)],
      change3 = value[date == max(date)] - value[date == max(date) %m-% months(3)],
      change12 = value[date == max(date)] - value[date == max(date) %m-% months(12)],
      change2019 = value[date == "2019-12-01"]/ces_2019,
      last = value[date == max(date)]/ces_last,
      series_id = series_id[date == max(date)]
    ) %>%
    mutate(change3 = round(change3/3),
           change12 = round(change12/12))
  
industry_timeline <- ces_data %>%
  filter(series_id %in% jobs_chart$series_id) %>%
  filter(date >= "2020-01-01") %>%
  group_by(series_id) %>%
  mutate(value = value - value[date == "2020-01-01"]) %>%
  summarise(Timeline = list(c(value)))
  
  
  jobs_chart <- jobs_chart %>%
    mutate(chart_type = case_when(
      industry_name %in% c("Total nonfarm", "Goods-producing", "Private service-providing", "Total private") ~ "Total",
      industry_name %in% c("Mining and logging", "Construction", "Manufacturing") ~ "Goods",
      TRUE ~ "Services"
    )) %>%
    mutate(chart_type = factor(chart_type, levels = c("Total", "Goods", "Services"))) %>%
    filter(industry_name != "Service-providing") 

  wage_data <- ces_data %>%
    filter(seasonal == "S") %>%
    filter((display_level <= 2 & data_type_code == 3)) %>%
    group_by(industry_name) %>%
    summarize(last_change = value[date == max(date)]/value[date == max(date) %m-% months(1)],
              last_6change = value[date == max(date)]/value[date == max(date) %m-% months(6)],
              last_change = last_change^12-1,
              last_6change = last_6change^2-1)
  
  chart_date <- format(max(ces_data$date, na.rm = TRUE), "%B %Y")

  jobs_chart %>%
    left_join(wage_data, by = "industry_name") %>%
    #left_join(industry_timeline, by = "series_id") %>%
    mutate(industry_name = if_else(industry_name == "Total nonfarm", "All jobs", industry_name)) %>%
    arrange(industry_name) %>%
    select(-series_id) %>%
#    mutate(last = format(last, big.mark=","),
#           change2019 = format(change2019, big.mark=",")) %>%
    gt(groupname_col = "chart_type") %>%
    row_group_order(groups = c("Total", "Goods", "Services")) %>%
    tab_header(title = md(paste0("**Summary of the ", chart_date, " Jobs Number**")),
               subtitle = "All numbers in thousands, wage data annualized") %>%
    cols_label(
      change = "Last-Month",
      last = "Current",
      change3 = ("3-Month"),
      change12 = ("12-Month"),
      change2019 = "2019",
      last_change = "This\nMonth",
      last_6change = "Last 6\nMonths",
      industry_name = ""
    ) %>%
    tab_source_note(
      source_note = "BLS data, author's calculations. 2019 is value in December 2019. Mike Konczal, Roosevelt Institute"
    ) %>%
    opt_stylize(style = 6, color = "blue") %>%
    fmt_percent(columns = c(last_change, last_6change, last, change2019),
                            decimals = 1) %>%
    tab_spanner(
      label = "Employment Change",
      columns = c(change, change3, change12)
    ) %>%
    tab_spanner(
      label = "Employment (%)",
      columns = c(change2019, last)
    ) %>%
    tab_spanner(
      label = "Average Hourly Earnings",
      columns = c(last_change, last_6change)
    ) %>%
    sub_missing(missing_text = "") %>%
    cols_align(columns = c(last, change2019, change3, change12, last_6change, last_change),
      align = "center") %>%
    cols_align(columns = c(industry_name),
      align = "right") %>%
    cols_align(columns = industry_name,
               align = "left") %>%
    gtsave(., filename="graphics/jobs_chart.png")
  
}
