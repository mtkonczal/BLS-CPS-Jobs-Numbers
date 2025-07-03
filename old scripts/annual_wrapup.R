

min_year <- 2000
max_year <- max(ces_data$year)

annual_graphic <- ces_data %>%
  filter(series_id == "CES0000000001") %>%
  mutate(
    absolute_change = value - lag(value, 12),
    absolute_change = absolute_change / 1000,
    percent_change = value/lag(value, 12) - 1,
    change_label = round(absolute_change, 1),
    positive = absolute_change > 0
  ) %>%
  group_by(year) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  filter(year >= min_year)

check_comparison <- annual_graphic %>%
  select(date, year, absolute_change, percent_change)

text_color <- "#FFFFFF"     # White text
positive_color <- "#4F97D7" # Bright blue
negative_color <- "#BA68C8" # Pale violet
border_color <- "#424242" 
 
max_value <- annual_graphic$absolute_change[annual_graphic$year == max(annual_graphic$year)]
max_rounded_value <- annual_graphic$change_label[annual_graphic$year == max(annual_graphic$year)]

annual_graphic %>%
  ggplot(aes(year, absolute_change, fill = positive)) +
  geom_col(size = 0) +
  theme_lass +
  coord_flip() +
  geom_text(aes(
    y = absolute_change / 2,
    label = change_label
  ), color = text_color, size = 6) +
  scale_fill_manual(values = c(`TRUE` = positive_color, `FALSE` = negative_color)) +
  scale_x_continuous(breaks = c(min_year:max_year)) +
  scale_y_continuous(breaks = c(-10,-5,0,max_rounded_value,5)) +
  labs(title="2023 had the 5th highest job growth in the 21st century.",
       subtitle= paste0("Millions of new jobs created each year, employer CES survey. ", max_year, "'s value is ", max_value, " million."),
       y = "Millions of new jobs added per year",
       caption = "Change is 12-month ending December each year. Mike Konczal, Roosevelt Institute.") +
  theme(axis.title.x = element_text(size=12, color="white")) +
  geom_hline(yintercept = annual_graphic$absolute_change[annual_graphic$date == max(annual_graphic$date)],
             color=border_color, size=1.5) +
  geom_hline(yintercept = annual_graphic$absolute_change[annual_graphic$date == max(annual_graphic$date)],
             color=positive_color, size=1)

ggsave("graphics/annual_changes.png", dpi = "retina", width = 10, height = 8.5, units = "in")