library(scales)

remote_codes_industry <- c("LNU0201B67E",
"LNU0201B680",
"LNU0201B684",
"LNU0201B688",
"LNU0201B68A",
"LNU0201B686",
"LNU0201B68C",
"LNU0201B68E",
"LNU0201B690",
"LNU0201B692",
"LNU0201B694",
"LNU0201B682",
"LNU0201B696",
"LNU0201B698",
"LNU0201B69A",
"LNU0201B69C",
"LNU0201B69E",
"LNU0201B6A0",
"LNU0201B6A2",
"LNU0201B6A4",
"LNU0201B6A6",
"LNU0201B6A8",
"LNU0201B6AC",
"LNU0201B6AA",
"LNU0201B6AE",
"LNU0201B6B0",
"LNU0201B6B2",
"LNU0201B6B4",
"LNU0201B6B6",
"LNU0201B6B8",
"LNU0201B6BA",
"LNU0201B6BC")

remote_codes_government <- c("LNU0201B678", "LNU0201B66C", "LNU0201B66E")

remote_sector_raw <- get_n_series_table(
  c("LNU0201B678","LNU0201B66C","LNU0201B66E"),
  api_key = bls_get_key(),
  start_year = 2022,
  end_year = 2025,
  tidy = TRUE
)


telework_series <- tribble(
  ~series_id,    ~series_title,
  "LNU0201B66C", "Federal government workers",
  "LNU0201B66E", "State government workers",
  "LNU0201B678", "Private workers (excluding self-employed)"
)


remote_sector <- remote_sector_raw %>%
  mutate(date = as.Date(paste0(year,"/",month,"/",1))) %>%
  select(-year, -month) %>%
  pivot_longer(-date, names_to = "series_id", values_to = "value") %>%
  left_join(telework_series, by="series_id") %>%
  mutate(value = as.numeric(value)/100)


remote_sector %>%
  mutate(dateTag = if_else(date == max(date), value, NA)) %>%
  ggplot(aes(date, value, color = series_title)) +
  geom_line(size = 1.2) +
  geom_text(aes(date, dateTag, color=series_title, label = percent(dateTag)), nudge_x = 35, show.legend = FALSE) +
  geom_point(aes(date, dateTag, color=series_title), size = 4, show.legend = FALSE) +
  scale_y_continuous(label = percent)  +
    labs(title = "Federal Government Workers Now Not Working From Home",
         subtitle = "Percent of total at work. Persons who teleworked or worked at home, some or all hours. Not seasonally adjusted.",
         caption = "Current population survey. Series starts in October 2022. Mike Konczal") +
  scale_fill_brewer(palette = "Paired") +
  theme(plot.title.position = "plot",
        legend.position = "top")

ggsave("graphics/g12_public_private_remote.png", dpi = "retina", width = 12, height = 6.75, units = "in")