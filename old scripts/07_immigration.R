
# Native born unemployment ----
#Look at immigration
unique(cps_jobs_data$born_text)
max_date <- max(cps_jobs_data$date, na.rm = TRUE)

nt_series <- cps_jobs_data %>% filter(date >= max_date %m-% months(1)) %>%
  filter(born_text == "Native born") %>%
  distinct(series_title)

#View(nt_series)

# unrate foreign born: LNU04073395
cps_jobs_data %>%
  filter(series_id %in% c("LNU04073413")) %>%
  ggplot(aes(date, value, color=series_title)) +
  geom_line(show.legend = FALSE) +
  labs(subtitle = "(Unadj) Unemployment Rate - Native born") +
  theme_classic(base_size = 20) 

ggsave("graphics/g10_native_unrate.png", width = 12, height = 9, dpi = "retina")

# Prime EPOP ----
# This was an issue because the overall workforce is flat on native-born workers
# as the population ages. But in the last few years prime has remained steady.

prime_pop_foreign <- c("LNU00073399", "LNU00073400", "LNU00073401")
prime_emp_foreign <- c("LNU02073399", "LNU02073400", "LNU02073401")

prime_pop_native <- c("LNU00073417", "LNU00073418", "LNU00073419")
prime_emp_native <- c("LNU02073417", "LNU02073418", "LNU02073419")

prime_epop_native <- cps_jobs_data %>%
  filter(series_id %in% c(prime_pop_native, prime_emp_native, prime_pop_foreign, prime_emp_foreign)) %>%
  group_by(date, born_text) %>%
  summarize(prime_emp = sum(value[lfst_text == "Employed"]),
            prime_pop = sum(value[lfst_text == "Civilian noninstitutional population"]),
            prime_emp_pop = prime_emp/prime_pop) %>%
  ungroup()


prime_epop_native %>%
  ggplot(aes(date, prime_emp_pop, color=born_text)) +
  geom_line(show.legend = FALSE) +
  labs(subtitle = "Manually calculated prime-age 25-54 EPOP.") +
  theme_classic(base_size = 20) +
  facet_wrap(~born_text) +
  scale_y_continuous(label = percent)

ggsave("graphics/g11_native_foreign_prime_epop.png", width = 12, height = 9, dpi = "retina")




prime_epop_native %>%
  filter(born_text == "Native born",
         year(date) >= 2015) %>%
  mutate(highlight_dots = if_else(month(date) == month(max(date)), prime_emp_pop, NA)) %>%
  ggplot(aes(date, prime_emp_pop)) +
  geom_point(aes(date, highlight_dots), size = 4) +
  geom_line(show.legend = FALSE) +
  labs(subtitle = "Manually calculated prime-age 25-54 EPOP for Native Born workers.\nNot Seasonally Adjusted, Dots are April values.",
       caption = "Mike Konczal") +
  theme_classic(base_size = 20) +
  scale_y_continuous(label = percent) +
  theme(plot.title.position = "plot")

ggsave("graphics/g12_native_prime_epop.png", width = 12, height = 9, dpi = "retina")