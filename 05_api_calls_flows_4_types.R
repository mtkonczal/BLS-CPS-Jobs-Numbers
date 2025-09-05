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




offset <- 0.00025  # vertical distance for nudging text

title_flows_g1 <- "Not in LF to Employment Behind Small Unemployment Increase in 2025"
start_year <- 2024
label_horizon <- 4

in_out_unrate <-   c("LNS17100000","LNS17400000","LNS17600000","LNS17900000")

flows_lookup <- tribble(
  ~series_title,                                              ~series_id,   ~lfst_code,
  "(Seas) Labor Force Flows Unemployed to Employed",          "LNS17100000", 71,
  "(Seas) Labor Force Flows Employed to Unemployed",          "LNS17400000", 74,
  "(Seas) Labor Force Flows Not in Labor Force to Unemployed","LNS17600000", 76,
  "(Seas) Labor Force Flows Unemployed to Not in Labor Force","LNS17900000", 79
)


flows_raw <- get_n_series_table(
  in_out_unrate,
  api_key = bls_get_key(),
  start_year = 2022,
  end_year = 2025,
  tidy = TRUE
) %>%
  mutate(date = as.Date(paste0(year,"/",month,"/",1))) %>%
  select(-year, -month) %>%
  pivot_longer(LNS17100000:LNS17900000, names_to = "series_id", values_to = "value") %>%
  inner_join(flows_lookup, by="series_id")



MI_dates <- sort(unique(flows_raw$date), decreasing = TRUE)
MI_dates <- MI_dates[seq(1, length(MI_dates), 3)]

lf <- unrate %>%
  select(date, lf_level = LNS11000000, u_change = unrate) %>%
  mutate(
    u_change = u_change - lag(u_change, 1),
    last_labels = if_else(date >= max(date) %m-% months(label_horizon), u_change, NA)) %>%
  filter(year(date) >= start_year)

flows <- flows_raw %>%
  filter(year(date) >= 2024, series_id %in% in_out_unrate) %>%
  inner_join(lf, by = "date") %>%
  mutate(
    value = if_else(lfst_code %in% c(71, 79), -value, value),
    value = value / lf_level
  ) %>%
  mutate(last_labels = if_else(date >= max(date) %m-% months(label_horizon), value, NA))





## 1.  DATA FOR LABELS ----------------------------------------------------------
# labels for the line/point series
flows_lab <- flows %>%                                   # <- your object from earlier
  filter(!is.na(last_labels)) %>%                        # only the most-recent points
  mutate(label_y = last_labels + offset)                 # nudge *up*

# labels for the geom_col series
lf_lab <- lf %>%                                         # <- your lf object
  filter(!is.na(last_labels)) %>%                        # last few bars
  mutate(label_y = last_labels + sign(last_labels)*offset)  # up if +, down if –

legend_x <- as.Date("2024-03-01")  # left-half

legend_df <- tibble(
  series_title = unique(flows$series_title),
  x            = legend_x,
  label        = str_remove(series_title, "^\\(Seas\\) Labor Force Flows\\s+")
)

adds_subtracts <- min(flows$date) + 0.5 * (max(flows$date) - min(flows$date))  # left-half

legend_df <- flows %>%
  filter(date == legend_x) %>%
  select(series_title, y = value) %>%
  right_join(legend_df, by="series_title")

legend_df <- flows %>%
  filter(date == max(date)) %>%
  mutate(label = str_remove(series_title, "^\\(Seas\\) Labor Force Flows\\s+"),
         label = str_replace_all(label, "Labor Force", "LF")) %>%
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
    aes(date, label_y,
        label  = percent(last_labels, accuracy = .001),
        colour = series_title),
    vjust = 0,     # vjust = 0 centres vertically; we already nudged up
    show.legend = FALSE, size = 5
  ) +
  
  # labels for the bars
  geom_text(
    data = lf_lab,
    aes(date, label_y,
        label = percent(last_labels, accuracy = .01)),
    vjust = if_else(lf_lab$last_labels >= 0, 0, 1),  # above or below
    show.legend = FALSE, size = 5, color = "#2c3254"
  ) +
  
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title    = title_flows_g1,
    subtitle = "Percent of labor force.",
    y        = "",
    x        = "",
    caption  = "BLS CPS. 'Marginal inflows to unemployment' ignored for now but drive small discrepencies. Mike Konczal"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position      = "none",
    plot.title.position  = "plot",
    plot.caption.position = "plot"
  ) +
  scale_x_date(
    breaks      = MI_dates,
    date_labels = "%b\n%Y",
    expand      = expansion(mult = c(0, 0.20))   # ← add 12 % on the right
  ) +
  geom_text(                                           # coloured descriptions
    data = legend_df,
    aes(x, y, colour = series_title, label = label),
    hjust = 0, size = 4.5, nudge_x = 15, show.legend = FALSE
  ) +
  annotate("text", x = adds_subtracts,  y =  0.005, label = "Adds to Unemployment",
           hjust = 0.5, size = 11.5, fontface = "bold", color = "#2c3254", alpha = 0.7) +
  annotate("text", x = adds_subtracts,  y = -0.005, label = "Subtracts From Unemployment",
           hjust = 0.5, size = 11.5, fontface = "bold", color = "#2c3254", alpha = 0.7) +
  annotate("text", x = as.Date("2024-03-01"),  y =  -0.001, label = "Change in Unemployment",
           hjust = 0, size = 5, fontface = "bold", color="#2c3254") +
  geom_hline(yintercept = 0)

ggsave("graphics/g7_4flows_unrate.png", dpi = "retina", width = 12, height = 8, units = "in")
