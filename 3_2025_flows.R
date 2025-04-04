
library(tidyverse)
library(lubridate)
library(ggrepel)
library(scales)

#cps_jobs_data %>% filter(series_id %in% flows_series_ids)

flows_series_ids <- c("LNS17000000","LNS17100000","LNS17200000","LNS17400000","LNS17500000",
                      "LNS17600000","LNS17800000","LNS17900000","LNS18000000","clf16ov")

flows_series_titles <- c("EE","UE","NE","EU","UU","NU","EN","UN","NN","labor_force")


unrate <- as_tibble(getFRED(c("UNEMPLOY","clf16ov"))) %>%
  mutate(urate = unemploy/clf16ov) %>% select(date, urate, labor_force_level = clf16ov)

flows <- as_tibble(getFRED(flows_series_ids, rename_variables = flows_series_titles)) %>% filter(!is.na(EE))


#### Job finding rate:
flows %>%
  select(date, UE, NE) %>%
  pivot_longer(UE:NE, names_to = "type", values_to = "values") %>%
  left_join(unrate, by="date") %>%
  mutate(values_lf = if_else(year(date) == 2020, NA, values/labor_force_level)) %>%
  filter(year(date) >= 2016) %>%
  ggplot(aes(date, values_lf, color=type)) +
  geom_line() +
  theme_classic(base_size = 18)


flows %>%
  select(date, UE, NE) %>%
  pivot_longer(UE:NE, names_to = "type", values_to = "values") %>%
  left_join(unrate, by = "date") %>%
  mutate(values_lf = if_else(year(date) == 2020, NA_real_, values / labor_force_level)) %>%
  filter(year(date) >= 2016) %>%
  ggplot(aes(x = date, y = values_lf, color = type)) +
  geom_line() +
  theme_classic(base_size = 18) +
  scale_color_manual(
    name = NULL,
    values = c("UE" = "red", "NE" = "blue"),
    labels = c("UE" = "Unemployed to Employed",
               "NE" = "Not in the Labor Force to Employed")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = c(0.8,0.5)) +
  labs(title="People Aren't Finding Jobs Without Being Unemployed",
       subtitle="Transition Flows As a Percent of Labor Force. Regular Line Value, Dotted Loess Regresion.",
       x="",
       y="",
       caption="2020 removed as outliers. Mike Konczal") +
  geom_smooth(linetype="dashed", se = FALSE, alpha=0.5, show.legend = FALSE)
ggsave("graphics/flows1.png", dpi = "retina", width = 12, height = 6.75, units = "in")



flows %>%
  select(date, UE, NE) %>%
  pivot_longer(UE:NE, names_to = "type", values_to = "values") %>%
  left_join(unrate, by="date") %>%
  mutate(values_lf = if_else(year(date) == 2020, NA, values/labor_force_level)) %>%
  filter(year(date) >= 2018) %>%
  ggplot(aes(date, values_lf, fill=type)) +
  geom_bar(stat="identity") +
  theme_classic() +
  facet_wrap(~type)

flows %>%
  filter(year(date) > 2020) %>%
  select(date, UE, NE, NU) %>%
  pivot_longer(UE:NU, names_to = "type", values_to = "values") %>%
  left_join(unrate, by="date") %>%
  mutate(values_lf = values) %>%
  ggplot(aes(date, values_lf, fill=type)) +
  geom_line() +
  theme_classic() +
  facet_wrap(~type)
  

# Ins and Outs - what's the value here?
flows %>%
  select(date, EU, UE, NU, NE) %>%
  left_join(unrate, by="date") %>%
  reframe(date = date,
          ins_of_unemployment = if_else(year(date) == 2020, NA, (EU+NU)/labor_force_level),
         outs_of_unemployment = if_else(year(date) == 2020, NA, (UE+NE)/labor_force_level)) %>%
  pivot_longer(ins_of_unemployment:outs_of_unemployment, names_to = "type", values_to = "value") %>%
  ggplot(aes(date, value, color=type)) +
  geom_line() +
  theme_classic(base_size = 11) +
  scale_y_continuous(label = percent) +
  labs(y = "",
       x= "",
       subtitle = "Flows into Employment, From Unemployment and Not-in-Labor Force, as a percent of Labor Force. Blue line is three month average. Orange line is latest value projected back.",
       caption = "2020 removed as outlier.") +
  theme(plot.title.position = "plot")

ggsave("data_day_markdowns/jobs day/job_finding_rate.png", dpi="retina")


flows %>%
  select(date, UE, NE) %>%
  left_join(unrate, by="date") %>%
  mutate(values_lf = if_else(year(date) == 2020, NA, (UE + NE)/labor_force_level),
         values_lf_smooth = if_else(year(date) == 2020, NA, (values_lf + lag(values_lf) + lag(values_lf, 2))/3),
         hline_max = values_lf_smooth[date == max(date)]) %>%
  ggplot(aes(date, values_lf)) +
  geom_line(alpha=0.5) +
  geom_line(aes(date, values_lf_smooth), color="#2D779C", size=1) +
  theme_classic(base_size = 11) +
  geom_line(aes(date, hline_max), linetype = "dotted", color="orange", size=1.5) +
  scale_y_continuous(label = percent) +
  labs(y = "",
       x= "",
       subtitle = "Flows into Employment, From Unemployment and Not-in-Labor Force, as a percent of Labor Force. Blue line is three month average. Orange line is latest value projected back.",
       caption = "2020 removed as outlier.") +
  theme(plot.title.position = "plot")



flows %>%
  filter(year(date) != 2020) %>%
  select(date, UE, NE) %>%
  left_join(unrate, by="date") %>%
  mutate(values_lf = (UE + NE)/labor_force_level,
         values_lf_smooth = (values_lf + lag(values_lf) + lag(values_lf, 2))/3,
         low_u = if_else(year(date) > 1997 & year(date) < 2001, "90s", "All Else"),
         low_u = if_else(year(date) > 2021, "Pandemic", low_u)) %>%
  ggplot(aes(urate, values_lf, color=low_u)) +
  geom_point() +
  theme_classic()




#### 
flows %>%
  filter(year(date) >= 2022) %>%
  select(date, UE, NE, EU, EN) %>%
  pivot_longer(UE:EN, names_to = "type", values_to = "values") %>%
  mutate(to_employment = if_else(type %in% c("UE","NE"), "to employment - level","out of employment - level"),
         label_name = if_else(date == max(date), type, NA)) %>%
  ggplot(aes(date, values, color=type, label=label_name)) + geom_line(size=1.2) +
  theme_classic(base_size = 22) +
  facet_wrap(~to_employment, scales = "free") +
  geom_text(nudge_y = -400, label.size = NA, size =12) +
  geom_smooth(se = FALSE, size=1.2, linetype = "dashed") +
  labs(caption="Dashed line smoothed via loess regression. Mike Konczal",
       subtitle = "The Not-in-labor-Force to Employment Channel (NE) Has Fallen During the Past Year") +
  theme(legend.position = "None",
        plot.title.position = "plot")


  

unrate_type <- as_tibble(getFRED(c("LNS13023705","LNS13023557","LNS13023569","LNS13023653","LNS13025699","CLF16OV"))) %>%
  filter(!is.na(lns13023705 ))


job_leavers <- unrate_type %>%
  mutate(job_leavers = lns13023705/clf16ov,
         new_entrants_and_reentrants = (lns13023557+lns13023569)/clf16ov,
         job_losers_on_temporary_layoff = lns13023653/clf16ov,
         job_losers_not_on_temporary_layoff = lns13025699/clf16ov)


year_delay <- max(job_leavers$date) %m-% months(22)

g_dates <- unique(job_leavers$date)
g_dates <- sort(g_dates, decreasing = TRUE)
g_dates <- g_dates[seq(1, length(g_dates), 3)]

job_leavers %>%
  filter(date >= year_delay) %>%
  pivot_longer(job_leavers:job_losers_not_on_temporary_layoff, names_to = "type", values_to = "value") %>%
  mutate(type = str_replace_all(type, "_", " ")) %>%
  ggplot(aes(date, value)) +
  geom_line(size=1.2) + geom_point(size=2) + theme_classic() + facet_wrap(~type, scales = "free") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%b\n%Y", breaks=g_dates) +
  labs(subtitle="Unemployment rate contribution, by category of unemployment.",
       caption="BLS, CPS, Seasonally-Adjusted") #+
#  scale_color_manual(values=c("#6EA4BF","#2D779C", "#97BC56","#E2E47E")) +
#  geom_line(aes(date,pre_value,color=series_title), linetype="dashed") +
#  geom_text(show.legend=FALSE, nudge_x = 25, size = 4)

job_leavers$job_losers_on_temporary_layoff


unrate <- as_tibble(getFRED(c("UNEMPLOY","clf16ov"))) %>%
  mutate(urate = unemploy/clf16ov) %>% select(date, urate) %>%
  mutate(urate_increase = lead(urate,3)-urate)

df<- job_leavers %>%
  mutate(leavers_diff = job_losers_on_temporary_layoff - lag(job_losers_on_temporary_layoff,1)) %>%
  select(date, job_losers_on_temporary_layoff, leavers_diff) %>%
  mutate(leavers_lead = lead(job_losers_on_temporary_layoff, 6) - job_losers_on_temporary_layoff) %>%
  left_join(unrate, by="date") %>%
  mutate(leavers_diff_squared =leavers_diff*leavers_diff)

a <- lm(leavers_lead ~ leavers_diff, data = df %>% filter(year(date) <= 2019))
summary(a)




library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

