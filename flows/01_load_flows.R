source("../scripts/01_load_cps_jobs.R")
# Lfst_code
#70	Labor Force Flows Employed to Employed
#71	Labor Force Flows Unemployed to Employed
#72	Labor Force Flows Not in Labor Force to Employed
#73	Marginal Inflows to Employed
#74	Labor Force Flows Employed to Unemployed
#75	Labor Force Flows Unemployed to Unemployed
#76	Labor Force Flows Not in Labor Force to Unemployed
#77	Marginal Inflows to Unemployed
#78	Labor Force Flows Employed to Not in Labor Force
#79	Labor Force Flows Unemployed to Not in Labor Force
#80	Labor Force Flows Not in Labor Force to Not in Labor Force
#81	Marginal Inflows to Not in Labor Force




# This tries to do the CEA graphic of in and out - not there yet, try again tomorrow with their graphics
flows_data <- cps_jobs_data %>%
  filter(seasonal == "S", year(date) >= 2023, sexs_code == 0) %>%
  group_by(date) %>%
  summarize(
    u_level = value[lfst_code == 74] + value[lfst_code == 75] + value[lfst_code == 76] + value[lfst_code == 77],
    lf_level = u_level + value[lfst_code == 70] + value[lfst_code == 71] + value[lfst_code == 72] + value[lfst_code == 73],
    u_rate = u_level/lf_level,
    net_EU = (value[lfst_code == 74] - value[lfst_code == 71])/lf_level,
    net_NU = (value[lfst_code == 76] - value[lfst_code == 79])/lf_level
  ) %>%
  ungroup() %>%
  mutate(change_u = u_rate - lag(u_rate,1)) %>%
  pivot_longer(net_EU:net_NU, names_to = "type", values_to = "values")


flows_data %>%
  filter(year(date) >= 2023) %>%
  ggplot(aes(date, values, fill = type)) +
  geom_col() +
  geom_line(aes(date, change_u), color="purple")


ulevel <- getFRED("UNEMPLOY")

cps_jobs_data %>% filter(seasonal == "S", sexs_code == 0, year(date) >= 2022) %>%
  filter(lfst_code %in% c(71,74)) %>%
  left_join(ulevel, by="date") %>%
  mutate(percent_exiting = value/unemploy) %>%
  ggplot(aes(date, value, color=series_title)) + geom_line()



###
  flows_data <- cps_jobs_data %>%
    filter(seasonal == "S", year(date) >= 2017, sexs_code == 0) %>%
    group_by(date) %>%
    summarize(
      u_level = value[lfst_code == 74] + value[lfst_code == 75] + value[lfst_code == 76] + value[lfst_code == 77],
      lf_level = u_level + value[lfst_code == 70] + value[lfst_code == 71] + value[lfst_code == 72] + value[lfst_code == 73],
      u_rate = u_level / lf_level,
      entering_unrate = (value[lfst_code == 74] + value[lfst_code == 76]) / lf_level,
      leaving_unrate = -(value[lfst_code == 71] + value[lfst_code == 79]) / lf_level
    ) %>%
    ungroup() %>%
    mutate(change_u = u_rate - lag(u_rate, 1)) %>%
    pivot_longer(entering_unrate:leaving_unrate, names_to = "type", values_to = "values")

  
  ## 1. keep just the months you want to show
  flows_recent <- flows_data %>% 
    filter(year(date) >= 2024)
  
  ## 2. grab the latest observation for each flow type
  label_df <- flows_recent %>% 
    group_by(type) %>% 
    slice_max(date, n = 3, with_ties = FALSE) %>% 
    ungroup() %>% 
    mutate(
      lbl = percent(values, accuracy = 0.01),
      vjust = ifelse(values > 0, -0.4, 1.4),        # above / below the bar
      y_pos = values * 1.05                         # little nudge away from bar end
    )
  
  ## 3. make the plot
  flows_recent %>%
    ggplot(aes(date, values, fill = type)) +
    geom_col(width = 25) +                          # wide bars look more like columns
    geom_line(aes(y = change_u), colour = "purple",
              linewidth = 1.2) +
    geom_text(data = label_df,
              aes(y = y_pos, label = lbl, vjust = vjust),
              colour = "black", size = 3.5) +
    scale_fill_manual(values = c(
      entering_unrate = "#2c7fb8",  # teal
      leaving_unrate  = "#de5957"   # coral
    ),
    labels = c("Entering unemployment", "Leaving unemployment"),
    name = NULL) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(title = "Net Flows In and Out of Unemployment",
         subtitle = "Seasonally adjusted CPS flow rates, 2024-present",
         y = "percentage-point contribution to Δu",
         x = NULL) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "top",
      legend.justification = "left",
      legend.box.spacing = unit(0.1, "cm")
    )
  
  
  lf <- cps_jobs_data %>%
    filter(seasonal == "S", year(date) >= 2024, sexs_code == 0) %>%
    group_by(date) %>%
    summarize(
      u_level = value[lfst_code == 74] + value[lfst_code == 75] + value[lfst_code == 76] + value[lfst_code == 77],
      lf_level = u_level + value[lfst_code == 70] + value[lfst_code == 71] + value[lfst_code == 72] + value[lfst_code == 73]
    ) %>%
    ungroup() %>%
    select(date, lf_level, u_level) %>%
    mutate(u_change = u_level/lf_level,
           u_change = u_change - lag(u_change, 1))
  
cps_jobs_data %>%
    filter(seasonal == "S", year(date) >= 2024, sexs_code == 0, lfst_code %in% c(71, 74, 76, 79)) %>%
  left_join(lf, by="date") %>%
  mutate(value = if_else(lfst_code %in% c(71, 79), -value, value),
         value = value/lf_level) %>%
  group_by(series_title) %>%
  mutate(last_labels = if_else(date >= max(date) %m-% months(4), value, NA)) %>%
  ungroup() %>%
  ggplot() +
  geom_col(data = lf,
           aes(date, u_change),
           fill = "grey70") +

  geom_line(data = cps_jobs_data %>%
              filter(seasonal == "S",
                     year(date) >= 2024,
                     sexs_code == 0,
                     lfst_code %in% c(71, 74, 76, 79)) %>%
              left_join(lf, by = "date") %>%
              mutate(value = if_else(lfst_code %in% c(71, 79), -value, value),
                     value = value / lf_level),
            aes(date, value, colour = series_title)) +
  geom_point(aes(date, last_labels)) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_text(aes(date, last_labels, label = last_labels)) +
  scale_y_continuous(label = percent)





cps_jobs_data %>%
  filter(seasonal == "S", year(date) >= 2023, sexs_code == 0) %>%
  filter(lfst_code %in% c(74,71,76,79)) %>%
  select(series_title, series_id, lfst_code) %>%
  distinct(series_title, .keep_all = TRUE)

entering_unrate = (value[lfst_code == 74] + value[lfst_code == 76]) / lf_level,
leaving_unrate = -(value[lfst_code == 71] + value[lfst_code == 79]) / lf_level


filter(seasonal == "S", year(date) >= 2024, sexs_code == 0) %>%
  group_by(date) %>%
  summarize(
    u_level = value[lfst_code == 74] + value[lfst_code == 75] + value[lfst_code == 76] + value[lfst_code == 77],
    lf_level = u_level + value[lfst_code == 70] + value[lfst_code == 71] + value[lfst_code == 72] + value[lfst_code == 73]
  ) %>%