# ---- Goods employment vs log-linear projection (BLS CES) ----

actual_color <- "#0B4F6C" # deep teal
trend_color <- "#B23A48" # dark brick red

title_goods <- "Employment in Goods-Producing Jobs Has Fallen Under Trump"

proj_start <- as.Date("2022-12-01")
proj_end <- as.Date("2024-12-01")

goods <- get_n_series_table(
  c(
    "CES0600000001", # Goods-producing employment
    "CES0000000001" # Total nonfarm (optional; not used below)
  ),
  api_key = bls_get_key(),
  start_year = 2020,
  end_year = 2025,
  tidy = TRUE
) %>%
  mutate(
    date = as.Date(paste0(year, "/", month, "/", 1)),
    goods = CES0600000001,
    goods_proj = logLinearProjection(date, goods, proj_start, proj_end)
  )

# Date breaks like your bottom chart
MI_dates_goods <- date_breaks_n(goods$date, 6)

plot_goods <- goods %>%
  filter(date >= proj_start) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = goods, color = "Goods employment"), linewidth = 1.2) +
  geom_line(
    aes(y = goods_proj, color = "Log-linear trend"),
    linewidth = 1.2,
    linetype = "22"
  ) +
  scale_color_manual(
    values = c(
      "Goods employment" = actual_color,
      "Log-linear trend" = trend_color
    )
  ) +
  scale_x_date(date_labels = "%b\n%Y", breaks = MI_dates_goods) +
  labs(
    title = title_goods,
    subtitle = "BLS CES Goods-producing employment (thousands). Dashed line is a log-linear regression from Dec 2022 to Dec 2024.",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: BLS, CES. Seasonally adjusted. Mike Konczal, Economic Security Project."
  ) +
  theme_esp() +
  theme(legend.position = "right")

plot_goods

ggsave(
  "graphics/goods_loglinear_projection.png",
  plot = plot_goods,
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)
