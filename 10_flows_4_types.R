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

offset <- 0.00025 # vertical distance for nudging text

title_flows_g1 <- "Not in LF to Employment Behind Small Unemployment Increase in 2025"
start_year <- 2024
label_horizon <- 4

in_out_unrate <- c("LNS17100000", "LNS17400000", "LNS17600000", "LNS17900000")

flows_lookup <- tribble(
  ~series_title                                               , ~series_id    , ~lfst_code ,
  "(Seas) Labor Force Flows Unemployed to Employed"           , "LNS17100000" ,         71 ,
  "(Seas) Labor Force Flows Employed to Unemployed"           , "LNS17400000" ,         74 ,
  "(Seas) Labor Force Flows Not in Labor Force to Unemployed" , "LNS17600000" ,         76 ,
  "(Seas) Labor Force Flows Unemployed to Not in Labor Force" , "LNS17900000" ,         79
)


flows_raw <- get_n_series_table(
  in_out_unrate,
  api_key = bls_get_key(),
  start_year = 2022,
  end_year = as.integer(format(Sys.Date(), "%Y")),
  tidy = TRUE
) %>%
  mutate(date = as.Date(paste0(year, "/", month, "/", 1))) %>%
  select(-year, -month) %>%
  pivot_longer(
    LNS17100000:LNS17900000,
    names_to = "series_id",
    values_to = "value"
  ) %>%
  inner_join(flows_lookup, by = "series_id")

flows_raw$value <- as.numeric(flows_raw$value)

MI_dates <- sort(unique(flows_raw$date), decreasing = TRUE)
MI_dates <- MI_dates[seq(1, length(MI_dates), 3)]

lf <- unrate %>%
  select(date, lf_level = LNS11000000, u_change = unrate) %>%
  mutate(
    u_change = u_change - lag(u_change, 1),
    last_labels = if_else(
      date >= max(date) %m-% months(label_horizon),
      u_change,
      NA
    )
  ) %>%
  filter(year(date) >= start_year)

flows <- flows_raw %>%
  filter(year(date) >= 2024, series_id %in% in_out_unrate) %>%
  inner_join(lf, by = "date") %>%
  mutate(
    value = if_else(lfst_code %in% c(71, 79), -value, value),
    value = value / lf_level
  ) %>%
  mutate(
    last_labels = if_else(
      date >= max(date) %m-% months(label_horizon),
      value,
      NA
    )
  )


## 1.  DATA FOR LABELS ----------------------------------------------------------
# labels for the line/point series
flows_lab <- flows %>% # <- your object from earlier
  filter(!is.na(last_labels)) %>% # only the most-recent points
  mutate(label_y = last_labels + offset) # nudge *up*

# labels for the geom_col series
lf_lab <- lf %>% # <- your lf object
  filter(!is.na(last_labels)) %>% # last few bars
  mutate(label_y = last_labels + sign(last_labels) * offset) # up if +, down if –

legend_x <- as.Date("2024-03-01") # left-half

legend_df <- tibble(
  series_title = unique(flows$series_title),
  x = legend_x,
  label = str_remove(series_title, "^\\(Seas\\) Labor Force Flows\\s+")
)

adds_subtracts <- min(flows$date) + 0.5 * (max(flows$date) - min(flows$date)) # left-half

legend_df <- flows %>%
  filter(date == legend_x) %>%
  select(series_title, y = value) %>%
  right_join(legend_df, by = "series_title")

legend_df <- flows %>%
  filter(date == max(date)) %>%
  mutate(
    label = str_remove(series_title, "^\\(Seas\\) Labor Force Flows\\s+"),
    label = str_replace_all(label, "Labor Force", "LF")
  ) %>%
  select(series_title, x = date, y = value, label)


## 2.  PLOT --------------------------------------------------------------------
flows %>%
  ggplot() +
  # bars: monthly Δ unemployment share
  geom_col(data = lf, aes(date, u_change), fill = "#2c3254", colour = "black") +

  # coloured lines + points
  geom_line(aes(date, value, colour = series_title), linewidth = 1) +
  geom_point(aes(date, value, colour = series_title), size = 2.5) +

  # labels for the lines/points
  geom_text(
    data = flows_lab,
    aes(
      date,
      label_y,
      label = percent(last_labels, accuracy = .001),
      colour = series_title
    ),
    vjust = 0, # vjust = 0 centres vertically; we already nudged up
    show.legend = FALSE,
    size = 5
  ) +

  # labels for the bars
  geom_text(
    data = lf_lab,
    aes(date, label_y, label = percent(last_labels, accuracy = .01)),
    vjust = if_else(lf_lab$last_labels >= 0, 0, 1), # above or below
    show.legend = FALSE,
    size = 5,
    color = "#2c3254"
  ) +

  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title = title_flows_g1,
    subtitle = "Percent of labor force.",
    y = "",
    x = "",
    caption = "BLS CPS. 'Marginal inflows to unemployment' ignored for now but drive small discrepencies. Mike Konczal"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  scale_x_date(
    breaks = MI_dates,
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0, 0.20)) # ← add 12 % on the right
  ) +
  geom_text(
    # coloured descriptions
    data = legend_df,
    aes(x, y, colour = series_title, label = label),
    hjust = 0,
    size = 4.5,
    nudge_x = 15,
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = adds_subtracts,
    y = 0.005,
    label = "Adds to Unemployment",
    hjust = 0.5,
    size = 11.5,
    fontface = "bold",
    color = "#2c3254",
    alpha = 0.7
  ) +
  annotate(
    "text",
    x = adds_subtracts,
    y = -0.005,
    label = "Subtracts From Unemployment",
    hjust = 0.5,
    size = 11.5,
    fontface = "bold",
    color = "#2c3254",
    alpha = 0.7
  ) +
  annotate(
    "text",
    x = as.Date("2024-03-01"),
    y = -0.001,
    label = "Change in Unemployment",
    hjust = 0,
    size = 5,
    fontface = "bold",
    color = "#2c3254"
  ) +
  geom_hline(yintercept = 0)

ggsave(
  "graphics/10_flows_unrate.png",
  dpi = "retina",
  width = 12,
  height = 8,
  units = "in"
)


flows_clean <- flows %>%
  mutate(
    series_label = case_when(
      series_id == "LNS17600000" ~ "Not in LF -> Unemployed",
      series_id == "LNS17400000" ~ "Employed -> Unemployed",
      series_id == "LNS17900000" ~ "Unemployed -> Not in LF",
      series_id == "LNS17100000" ~ "Unemployed -> Employed"
    ),
    series_label = factor(
      series_label,
      levels = c(
        "Not in LF -> Unemployed",
        "Employed -> Unemployed",
        "Unemployed -> Not in LF",
        "Unemployed -> Employed"
      )
    )
  )

flows_clean_labels <- flows_clean %>%
  group_by(series_label) %>%
  filter(date >= max(date) %m-% months(1)) %>%
  ungroup() %>%
  mutate(
    label_date = date + days(24),
    label = percent(value, accuracy = 0.01)
  )

flows_clean_colors <- c(
  "Not in LF -> Unemployed" = "#c65d08",
  "Employed -> Unemployed" = "#2a9d6f",
  "Unemployed -> Not in LF" = "#d93b8e",
  "Unemployed -> Employed" = "#6c6bb0"
)

flows_clean %>%
  ggplot(aes(date, value, color = series_label)) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.5) +
  geom_line(linewidth = 1.8, show.legend = FALSE) +
  geom_point(size = 2.6, show.legend = FALSE) +
  geom_text(
    data = flows_clean_labels,
    aes(label_date, value, label = label),
    hjust = 0,
    size = 5.1,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(~series_label, ncol = 2, scales = "free_y") +
  scale_color_manual(values = flows_clean_colors) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_date(
    breaks = MI_dates,
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.14))
  ) +
  labs(
    title = "Labor-Force Flows Behind the 2025 Unemployment Increase",
    subtitle = "Monthly flows as a share of the labor force. Top row adds to unemployment; bottom row subtracts from unemployment.",
    x = "",
    y = "",
    caption = "BLS CPS. 'Marginal inflows to unemployment' ignored for now but drive small discrepancies. Mike Konczal"
  ) +
  coord_cartesian(clip = "off") +
  theme_esp(base_size = 16) +
  theme(
    strip.text = element_text(size = 17, face = "bold"),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 15),
    plot.caption.position = "plot",
    axis.text.x = element_text(size = 12),
    panel.grid.major.x = element_line(color = "grey82"),
    panel.grid.major.y = element_line(color = "grey88")
  )

ggsave(
  "graphics/10_flows_unrate_clean.png",
  dpi = "retina",
  width = 13,
  height = 9,
  units = "in"
)

# --- Unemployed -> Not in Labor Force standalone plots ---

u_to_nlf <- flows_raw %>%
  filter(series_id == "LNS17900000") %>%
  inner_join(
    unrate %>%
      mutate(date = as.Date(paste0(year, "/", month, "/", 1))) %>%
      select(date, lf_level = LNS11000000),
    by = "date"
  ) %>%
  mutate(value = value / lf_level) %>%
  filter(year(date) >= 2022)

u_to_nlf_labels <- u_to_nlf %>%
  filter(date == max(date)) %>%
  mutate(
    label_date = date + days(24),
    label = percent(value, accuracy = 0.01)
  )

u_to_nlf_dates <- date_breaks_n(u_to_nlf$date, 8)

u_to_nlf_base <- u_to_nlf %>%
  ggplot(aes(date, value)) +
  geom_line(linewidth = 1.8, color = "#d93b8e") +
  geom_point(size = 2.6, color = "#d93b8e") +
  geom_text(
    data = u_to_nlf_labels,
    aes(label_date, value, label = label),
    hjust = 0,
    size = 5.1,
    fontface = "bold",
    color = "#d93b8e"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 0.01)) +
  scale_x_date(
    breaks = u_to_nlf_dates,
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.14))
  ) +
  labs(
    title = "Unemployed Workers Leaving the Labor Force",
    subtitle = "Monthly flow from unemployed to not in the labor force, as a share of the labor force.",
    x = "",
    y = "",
    caption = "BLS CPS, seasonally adjusted. Mike Konczal"
  ) +
  coord_cartesian(clip = "off") +
  theme_esp(base_size = 16) +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 15),
    plot.caption.position = "plot",
    axis.text.x = element_text(size = 12),
    panel.grid.major.x = element_line(color = "grey82"),
    panel.grid.major.y = element_line(color = "grey88")
  )

# Version without trend line
u_to_nlf_base

ggsave(
  "graphics/10_u_to_nlf.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)

# Version with trend line
u_to_nlf_base +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "grey40",
    linetype = "dashed",
    linewidth = 1
  )

ggsave(
  "graphics/10_u_to_nlf_trend.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)
