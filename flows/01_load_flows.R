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
  geom_line(aes(date, change_u))


ulevel <- getFRED("UNEMPLOY")

cps_jobs_data %>% filter(seasonal == "S", sexs_code == 0, year(date) >= 2022) %>%
  filter(lfst_code %in% c(71,74)) %>%
  left_join(ulevel, by="date") %>%
  mutate(percent_exiting = value/unemploy) %>%
  ggplot(aes(date, value, color=series_title)) + geom_line()