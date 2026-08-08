# ==============================================================================
# 10b_flows_deep_dive.R
#
# Six ways to see the CPS gross-flows relationship behind the unemployment rate.
# Motivating question: how can the unemployment rate FALL in a month when
# household employment also FALLS?
#
# Data layer lives in scripts/flows_data.R (shared with 10c_flows_bathtub.R).
# Requires BLS_KEY in .Renviron.
#
# Outputs (graphics/):
#   10b_1_flow_map.png        net-flow map between E / U / NLF, latest month
#   10b_2_waterfall.png       exact decomposition of the latest change in u
#   10b_3_contrib_bars.png    which channel moved the rate, every month
#   10b_4_rates_panel.png     transition (hazard) rates vs. history
#   10b_5_hire_fire.png       job-finding vs. layoff rate scatter
#   10b_6_counterfactual.png  steady-state u if one rate returned to 2019
# ==============================================================================

source("scripts/graphic_scripts.R")
source("scripts/flows_data.R")

# ==============================================================================
# CHART 1: THE FLOW MAP -- where the people actually went, latest month
# ==============================================================================
# Net flows between the three states. This is the chart that answers the
# question in one picture: the net circulation runs E -> NLF -> U -> E, which
# drains both employment and unemployment into non-participation.

nodes <- tibble(
  state = c("EMPLOYED", "UNEMPLOYED", "NOT IN\nLABOR FORCE"),
  x     = c(1.0, 5.0, 3.0),
  y     = c(3.2, 3.2, 1.0),
  level = c(L$E, L$U, L$N),
  chg   = c(L$dE, L$dU, L$dN)
) %>%
  mutate(lab = paste0(state, "\n", comma(round(level)), "k\n",
                      if_else(chg >= 0, "+", ""), comma(round(chg)), "k"))

arrows_df <- tibble(
  value = c(L$UE - L$EU, L$EN - L$NE, L$NU - L$UN),
  what  = c("found jobs", "left the labor force", "started looking"),
  x     = c(4.30, 1.32, 3.62),  y    = c(3.20, 2.82, 1.45),
  xend  = c(1.70, 2.48, 4.68),  yend = c(3.20, 1.58, 2.78),
  lx    = c(3.00, 1.72, 4.28),  ly   = c(3.52, 2.05, 2.05),
  hj    = c(0.50, 1.00, 0.00)
) %>%
  mutate(text = paste0("net ", comma(round(value)), "k\n", what),
         lw   = rescale(abs(value), to = c(1.3, 3.4),
                        from = c(0, max(abs(value)))))

margin_txt <- paste0("+", comma(round(L$r_E + L$r_U + L$r_N)),
                     "k population growth, almost all of it\nlanding outside the labor force")

p1 <- ggplot() +
  geom_segment(data = arrows_df,
               aes(x = x, y = y, xend = xend, yend = yend, linewidth = I(lw)),
               colour = NAVY, alpha = 0.85, lineend = "butt",
               arrow = arrow(length = unit(0.42, "cm"), type = "closed")) +
  geom_text(data = arrows_df, aes(lx, ly, label = text, hjust = hj),
            size = 5.2, lineheight = 0.95, colour = NAVY, fontface = "bold") +
  geom_label(data = nodes, aes(x, y, label = lab),
             size = 5.6, lineheight = 1.15, fontface = "bold",
             colour = "#f4f2e4", fill = NAVY,
             label.padding = unit(0.55, "lines"), label.r = unit(0.35, "lines")) +
  annotate("text", x = 3.0, y = 0.12, label = margin_txt,
           size = 4.4, colour = "grey35", lineheight = 1.05) +
  scale_x_continuous(limits = c(0.0, 6.0)) +
  scale_y_continuous(limits = c(-0.05, 4.0)) +
  labs(
    title    = paste0("Both Employment and Unemployment Drained Into Non-Participation in ", latest_lab),
    subtitle = paste0(
      "Net monthly flows between labor market states. The circulation runs employment to out-of-the-labor-force ",
      "to unemployment\nto employment, which is how the unemployment rate fell ",
      sprintf("%.2f", abs(100*L$du_act)), " points in a month when employment fell ",
      comma(round(abs(L$dE))), ",000."
    ),
    caption  = CAPTION
  ) +
  theme_esp(base_size = 16) +
  theme(
    plot.title         = element_text(size = 23, face = "bold"),
    plot.subtitle      = element_text(size = 14, lineheight = 1.15),
    axis.text          = element_blank(),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.caption.position = "plot"
  )

ggsave("graphics/10b_1_flow_map.png", p1, dpi = "retina",
       width = 13, height = 7.5, units = "in")

# ==============================================================================
# CHART 2: THE WATERFALL -- exact arithmetic of the latest change in u
# ==============================================================================
# Grey ticks mark where each bar would have ended at that flow's average
# contribution over the previous twelve readings, so a short bar is visibly a
# flow running below its own recent norm.

prev12 <- dat %>%
  filter(!is.na(du_act), date < latest) %>%
  slice_max(date, n = 12) %>%
  summarize(across(all_of(FLOW_LEVELS), mean)) %>%
  pivot_longer(everything(), names_to = "flow", values_to = "base_pp") %>%
  mutate(base_pp = 100 * base_pp)

wf <- L %>%
  select(all_of(FLOW_LEVELS)) %>%
  pivot_longer(everything(), names_to = "flow", values_to = "pp") %>%
  mutate(pp = 100 * pp, flow = factor(flow, levels = FLOW_LEVELS)) %>%
  arrange(flow) %>%
  left_join(prev12, by = "flow") %>%
  mutate(end = cumsum(pp), start = end - pp, idx = row_number(),
         ref = start + base_pp)

wf_total <- tibble(flow = "Change in the\nunemployment rate", idx = nrow(wf) + 1,
                   start = 0, end = sum(wf$pp), pp = sum(wf$pp))

wf_all <- bind_rows(wf %>% mutate(flow = as.character(flow)), wf_total) %>%
  mutate(flow  = factor(flow, levels = c(FLOW_LEVELS, wf_total$flow)),
         fill  = if_else(idx > nrow(wf), "TOTAL", as.character(flow)),
         lab   = paste0(if_else(pp >= 0, "+", ""), sprintf("%.2f", pp)),
         lab_y = pmax(start, end, ref, na.rm = TRUE) + 0.05)

p2 <- ggplot(wf_all, aes(x = idx, fill = fill)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.6) +
  geom_segment(data = wf, aes(x = idx + 0.42, xend = idx + 1.58, y = end, yend = end),
               inherit.aes = FALSE, colour = "grey55", linetype = "22", linewidth = 0.5) +
  geom_rect(aes(xmin = idx - 0.42, xmax = idx + 0.42, ymin = start, ymax = end),
            colour = NA) +
  geom_segment(data = wf, aes(x = idx - 0.5, xend = idx + 0.5, y = ref, yend = ref),
               inherit.aes = FALSE, colour = "grey25", linewidth = 0.9) +
  geom_text(aes(x = idx, y = lab_y, label = lab),
            size = 5, fontface = "bold", colour = NAVY, vjust = 0) +
  annotate("segment", x = 0.55, xend = 3.45, y = 2.30, yend = 2.30,
           colour = RED, linewidth = 0.7) +
  annotate("text", x = 2.0, y = 2.36, label = "PUSHES THE RATE UP",
           colour = RED, size = 4.6, fontface = "bold", vjust = 0) +
  annotate("segment", x = 3.55, xend = 6.45, y = 2.30, yend = 2.30,
           colour = "#20705a", linewidth = 0.7) +
  annotate("text", x = 5.0, y = 2.36, label = "PUSHES THE RATE DOWN",
           colour = "#20705a", size = 4.6, fontface = "bold", vjust = 0) +
  scale_fill_manual(values = c(FLOW_COLS, TOTAL = NAVY), guide = "none") +
  scale_x_continuous(breaks = wf_all$idx,
                     labels = str_wrap(as.character(wf_all$flow), 13),
                     expand = expansion(mult = 0.03)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, style_positive = "plus"),
                     expand = expansion(mult = c(0.06, 0.10))) +
  labs(
    title    = paste0("What Actually Moved the Unemployment Rate in ", latest_lab),
    subtitle = paste0(
      "Exact contribution of each gross flow to the monthly change in the unemployment rate, in percentage points. ",
      "The\nrate is the small residual of six large, offsetting flows. Black ticks mark each flow's average ",
      "contribution over\nthe previous twelve readings. Employment-to-non-participation carries a weight of only ",
      sprintf("%.2f", L$u_lag), ", because a\nworker who leaves the labor force shrinks the numerator and the denominator together."
    ),
    x = NULL, y = "Percentage points", caption = CAPTION
  ) +
  theme_esp(base_size = 16) +
  theme(
    plot.title         = element_text(size = 23, face = "bold"),
    plot.subtitle      = element_text(size = 13, lineheight = 1.2),
    axis.title.y       = element_text(size = 12, colour = "grey35"),
    axis.text.x        = element_text(size = 12, lineheight = 1.0, face = "bold"),
    panel.grid.major.x = element_blank(),
    plot.caption.position = "plot"
  )

ggsave("graphics/10b_2_waterfall.png", p2, dpi = "retina",
       width = 13, height = 8.5, units = "in")

# ==============================================================================
# ==============================================================================
# CHART 3: WHICH CHANNEL MOVED THE RATE, MONTH BY MONTH
# ==============================================================================
# Replacement for 10_flows_unrate.png. The four gross flows in and out of
# unemployment, each converted into percentage points of the rate, stacked so
# they sum EXACTLY to the monthly change. Inflows stack above zero, outflows
# below. The unlabeled grey remainder carries the two flows between employment
# and non-participation (which the rate barely feels, weight ~0.04) plus the CPS
# population and sample margin, so the accounting still closes.

CH <- c("Employed to Unemployed",
        "Non-Labor Force to Unemployed",
        "Unemployed to Employed",
        "Unemployed to Non-Labor Force",
        "remainder")
CH_COLS <- setNames(c(RED, ORANGE, GREEN, BLUE, GREY), CH)
CH_LAB  <- CH[1:4]   # the remainder is drawn but not named

chan <- dat %>%
  transmute(
    date,
    !!CH[1] := `Employed to Unemployed`,
    !!CH[2] := `Not in LF to Unemployed`,
    !!CH[3] := `Unemployed to Employed`,
    !!CH[4] := `Unemployed to Not in LF`,
    !!CH[5] := `Employed to Not in LF` + `Not in LF to Employed` +
               `Population / sample margin`,
    du_act
  )

# the five stacked pieces must still reproduce the change exactly
stopifnot(max(abs(rowSums(chan[, CH]) - chan$du_act), na.rm = TRUE) < 1e-15)

start_bars <- latest %m-% months(11)

bars <- chan %>%
  filter(date >= start_bars) %>%
  select(-du_act) %>%
  pivot_longer(-date, names_to = "channel", values_to = "pp") %>%
  mutate(pp = 100 * pp, channel = factor(channel, levels = CH)) %>%
  # midpoint of each piece within its stack, for labels drawn inside the bars
  group_by(date) %>%
  arrange(channel, .by_group = TRUE) %>%
  mutate(mid = if_else(pp > 0, cumsum(pmax(pp, 0)), cumsum(pmin(pp, 0))) - pp/2) %>%
  ungroup()

line_df <- chan %>%
  filter(date >= start_bars, !is.na(du_act)) %>%
  transmute(date, du = 100 * du_act)

# De facto legend: each flow named in its own color, parked at the midpoint of
# its own segment in the final month. Direct labeling beats a legend box, and
# the segments are a full percentage point tall so nothing collides.
key <- bars %>%
  filter(date == latest, channel %in% CH_LAB) %>%
  mutate(y = mid, lab = as.character(channel))

# numbers printed inside the bars for the three most recent months
last3 <- sort(unique(line_df$date), decreasing = TRUE)[1:3]
inbar <- bars %>%
  filter(date %in% last3, channel %in% CH_LAB) %>%
  mutate(lab = sprintf("%+.2f", pp))

p3 <- ggplot() +
  geom_col(data = bars, aes(date, pp, fill = channel), width = 21,
           position = position_stack(reverse = TRUE)) +
  geom_text(data = inbar, aes(date, mid, label = lab),
            colour = "#f4f2e4", size = 4.1, fontface = "bold") +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.6) +
  geom_line(data = line_df, aes(date, du), colour = NAVY, linewidth = 1.4) +
  geom_point(data = line_df, aes(date, du), colour = NAVY, size = 2.6) +
  geom_point(data = line_df %>% filter(date == latest), aes(date, du),
             colour = NAVY, size = 5.5) +
  geom_text(data = key, aes(latest + days(26), y, label = lab, colour = channel),
            hjust = 0, vjust = 0.5, size = 4.8, fontface = "bold",
            show.legend = FALSE) +
  geom_label(data = line_df %>% filter(date == latest),
             aes(date + days(26), du, label = "CHANGE IN THE RATE"),
             hjust = 0, size = 4.8, fontface = "bold", colour = "#f4f2e4",
             fill = NAVY, label.padding = unit(0.35, "lines"),
             label.size = 0) +
  scale_fill_manual(values = CH_COLS, guide = "none") +
  scale_colour_manual(values = CH_COLS, guide = "none") +
  scale_x_date(breaks = date_breaks_every(dat$date[dat$date >= start_bars], 1),
               date_labels = "%b\n%Y", expand = expansion(mult = c(0.04, 0.34))) +
  scale_y_continuous(labels = label_number(accuracy = 0.5, style_positive = "plus")) +
  labs(
    title    = "The Unemployment Rate Is the Small Difference Between Four Big Flows",
    subtitle = paste0(
      "Each gross flow in and out of unemployment, converted into percentage points of the rate. Inflows stack above ",
      "zero,\noutflows below, and the pieces sum exactly to the navy line, which is the actual monthly change. ",
      "The unlabeled grey\nsliver holds the two flows between employment and non-participation, which the rate barely ",
      "feels, plus population\ngrowth and the CPS sample margin."
    ),
    x = NULL, y = "Contribution to the monthly change (pp)", caption = CAPTION
  ) +
  coord_cartesian(clip = "off") +
  theme_esp(base_size = 16) +
  theme(
    plot.title         = element_text(size = 23, face = "bold"),
    plot.subtitle      = element_text(size = 13, lineheight = 1.2),
    axis.title.y       = element_text(size = 12, colour = "grey35"),
    axis.text.x        = element_text(size = 11.5),
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    plot.caption.position = "plot"
  )

ggsave("graphics/10b_3_contrib_bars.png", p3, dpi = "retina",
       width = 15, height = 8.5, units = "in")

cat("\n", latest_lab, "channel contributions (pp):\n")
print(chan %>% filter(date == latest) %>%
        pivot_longer(-date) %>%
        mutate(value = round(100*value, 3),
               name = str_replace_all(name, "\n", " / ")) %>%
        select(name, value) %>% as.data.frame())

# ==============================================================================
# CHART 4: TRANSITION RATES -- the behavior, not the headcount
# ==============================================================================
# Levels of flows scale with the size of the origin stock, which makes them a
# poor read on behavior: the unemployed-to-employed flow can fall simply because
# there are fewer unemployed people. Normalizing each flow by its lagged origin
# stock gives the monthly transition probability, which is what actually
# changed.

rate_meta <- tribble(
  ~key,   ~label,                                                        ~short,
  "f_UE", "Job finding rate\nunemployed who found work",                 "Job finding rate\nunemployed to employed",
  "f_UN", "Dropout rate\nunemployed who stopped looking",                "Dropout rate\nunemployed to out of the LF",
  "s_EU", "Layoff rate\nemployed who became unemployed",                 "Layoff rate\nemployed to unemployed",
  "s_EN", "Exit rate\nemployed who left the labor force",                "Exit rate\nemployed to out of the LF",
  "e_NU", "Entry to unemployment\nnon-participants now looking",         "Entry to unemployment\nout of the LF to unemployed",
  "e_NE", "Entry to a job\nnon-participants who found work",             "Entry to a job\nout of the LF to employed"
)

rates <- dat %>%
  transmute(
    date, contiguous,
    f_UE = UE / lag(U), f_UN = UN / lag(U),
    s_EU = EU / lag(E), s_EN = EN / lag(E),
    e_NU = NU / lag(N), e_NE = NE / lag(N)
  ) %>%
  mutate(across(-c(date, contiguous), ~ if_else(contiguous, .x, NA_real_)))

rates_long <- rates %>%
  pivot_longer(-c(date, contiguous), names_to = "key", values_to = "rate") %>%
  inner_join(rate_meta, by = "key") %>%
  filter(!is.na(rate))

# percentile / rank of the latest value in the post-1994 history (which
# INCLUDES 2020, even though 2020 is dropped from the plotted lines)
rate_stats <- rates_long %>%
  group_by(key, label) %>%
  summarize(
    latest_v = rate[date == max(date)],
    phrase   = rank_phrase(rate[date >= as.Date("1994-01-01")],
                           rate[date == max(date)], "1994"),
    m2019    = mean(rate[year(date) == 2019]),
    .groups  = "drop"
  ) %>%
  mutate(facet = paste0(label, "\n",
                        percent(latest_v, accuracy = 0.01), " in ", latest_lab,
                        "  |  ", phrase))

rec <- read_csv("data/recession_dates.csv", show_col_types = FALSE) %>%
  filter(start_date >= as.Date("1990-01-01"))

plot_rates <- rates_long %>%
  inner_join(rate_stats %>% select(key, facet, m2019), by = "key") %>%
  filter(date >= as.Date("1996-01-01"), !(date %within% COVID)) %>%
  mutate(facet = factor(facet, levels = rate_stats$facet[match(rate_meta$key, rate_stats$key)]))

p4 <- ggplot(plot_rates, aes(date, rate)) +
  geom_rect(data = rec, inherit.aes = FALSE,
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = "grey60", alpha = 0.28) +
  geom_line(colour = NAVY, linewidth = 0.55, alpha = 0.85) +
  geom_hline(aes(yintercept = m2019), colour = RED, linetype = "22", linewidth = 0.8) +
  geom_point(data = plot_rates %>% filter(date == latest), colour = RED, size = 3.2) +
  facet_wrap(~facet, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               expand = expansion(mult = 0.01)) +
  labs(
    title    = "A Low-Firing, Low-Hiring Labor Market With an Unusual Leak Into Non-Participation",
    subtitle = paste0(
      "Monthly transition probabilities: each flow divided by the stock it came from. ",
      "Dashed red line is the 2019 average;\nred dot is ", latest_lab,
      ". Shaded bars are recessions. March through December 2020 omitted from the lines ",
      "(but not\nfrom the rankings), because the pandemic values are off this scale by an order of magnitude."
    ),
    x = NULL, y = NULL, caption = CAPTION
  ) +
  theme_esp(base_size = 15) +
  theme(
    plot.title      = element_text(size = 21, face = "bold"),
    plot.subtitle   = element_text(size = 13, lineheight = 1.2),
    strip.text      = element_text(size = 12.5, face = "bold", lineheight = 1.15),
    axis.text       = element_text(size = 11),
    panel.spacing   = unit(1.1, "lines"),
    plot.caption.position = "plot"
  )

ggsave("graphics/10b_4_rates_panel.png", p4, dpi = "retina",
       width = 13.5, height = 10, units = "in")

# ==============================================================================
# CHART 5: HIRE / FIRE SCATTER -- is this a frozen market or a weakening one?
# ==============================================================================
# Three-month averages of the job-finding rate and the layoff rate. A recession
# moves the economy up and to the left (more firing, less hiring). A frozen
# market moves down and to the left (less of both). The distinction is the whole
# argument about what a falling unemployment rate means right now.

roll3 <- function(x) zoo::rollapplyr(x, 3, mean, fill = NA)

hf <- rates %>%
  transmute(date, f = roll3(f_UE), s = roll3(s_EU)) %>%
  filter(!is.na(f), !is.na(s), date >= as.Date("1996-01-01"),
         !(date %within% interval(as.Date("2020-03-01"), as.Date("2021-12-01"))))

hf_hist  <- hf %>% filter(date < as.Date("2022-01-01"))
hf_path  <- hf %>% filter(date >= as.Date("2022-01-01"))
hf_marks <- hf %>%
  filter(date %in% c(as.Date("2019-12-01"), as.Date("2022-06-01"),
                     as.Date("2024-01-01"), latest)) %>%
  mutate(lab = format(date, "%b %Y"))

xr <- range(hf$f); yr <- range(hf$s)
pad <- function(r, f = 0.06) r + c(-1, 1) * diff(r) * f

p5 <- ggplot() +
  geom_point(data = hf_hist, aes(f, s), colour = "grey62", size = 1.9, alpha = 0.55) +
  geom_path(data = hf_path, aes(f, s, colour = date), linewidth = 1.5, lineend = "round") +
  geom_point(data = hf_path, aes(f, s, colour = date), size = 2.4) +
  geom_point(data = hf_marks, aes(f, s), shape = 21, size = 5.5,
             fill = NA, colour = NAVY, stroke = 1.3) +
  ggrepel::geom_text_repel(data = hf_marks, aes(f, s, label = lab),
                           size = 5, fontface = "bold", colour = NAVY,
                           box.padding = 1.1, min.segment.length = 0, seed = 1) +
  annotate("text", x = pad(xr)[2], y = pad(yr)[2], hjust = 1, vjust = 1,
           label = "CHURNING\nlots of hiring, lots of firing",
           size = 4.5, colour = "grey45", lineheight = 1.1, fontface = "bold") +
  annotate("text", x = pad(xr)[1], y = pad(yr)[2], hjust = 0, vjust = 1,
           label = "WEAKENING\nfiring up, hiring down",
           size = 4.5, colour = "grey45", lineheight = 1.1, fontface = "bold") +
  annotate("text", x = pad(xr)[1], y = pad(yr)[1], hjust = 0, vjust = 0,
           label = "FROZEN\nnobody hiring, nobody firing",
           size = 4.5, colour = "grey45", lineheight = 1.1, fontface = "bold") +
  scale_colour_gradient(low = "#e08b3c", high = RED, trans = "date", guide = "none") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  coord_cartesian(xlim = pad(xr), ylim = pad(yr)) +
  labs(
    title    = "Frozen, Not Collapsing: Hiring Out of Unemployment Is Weak, but So Is Firing",
    subtitle = paste0(
      "Three-month average monthly transition rates. Grey points are 1996 through 2019; the colored path runs from\n",
      "January 2022 to ", latest_lab, ". March 2020 through December 2021 omitted as pandemic distortion."
    ),
    x = "Share of the unemployed who found work (job-finding rate)",
    y = "Share of the employed who became unemployed (layoff rate)",
    caption = CAPTION
  ) +
  theme_esp(base_size = 16) +
  theme(
    plot.title    = element_text(size = 21, face = "bold"),
    plot.subtitle = element_text(size = 13, lineheight = 1.2),
    axis.title    = element_text(size = 13, colour = "grey30"),
    plot.caption.position = "plot"
  )

ggsave("graphics/10b_5_hire_fire.png", p5, dpi = "retina",
       width = 12.5, height = 9, units = "in")

# ==============================================================================
# CHART 6: COUNTERFACTUAL -- which flow is holding the rate where it is?
# ==============================================================================
# Treat the six transition rates as a three-state Markov chain and solve for the
# stationary distribution: the unemployment rate this set of flow rates would
# eventually produce. Then move one rate at a time back to its 2019 average,
# holding the other five at their current values. This separates "unemployment
# is low because people are finding work" from "unemployment is low because
# nobody is being let go."

steady_u <- function(p) {
  P <- matrix(c(1 - p$s_EU - p$s_EN, p$s_EU,             p$s_EN,
                p$f_UE,              1 - p$f_UE - p$f_UN, p$f_UN,
                p$e_NE,              p$e_NU,             1 - p$e_NE - p$e_NU),
              nrow = 3, byrow = TRUE)
  ev <- eigen(t(P))
  v  <- Re(ev$vectors[, which.min(abs(ev$values - 1))])
  v  <- v / sum(v)
  v[2] / (v[1] + v[2])
}

RKEYS <- rate_meta$key
cur3 <- rates %>% filter(date >= latest %m-% months(2)) %>%
  summarize(across(all_of(RKEYS), ~ mean(.x, na.rm = TRUE))) %>% as.list()
b19  <- rates %>% filter(year(date) == 2019) %>%
  summarize(across(all_of(RKEYS), mean)) %>% as.list()

u_now <- steady_u(cur3)
u_19  <- steady_u(b19)

cf <- map_dfr(RKEYS, function(k) {
  swapped <- cur3; swapped[[k]] <- b19[[k]]
  tibble(key = k, u_cf = steady_u(swapped),
         cur_rate = cur3[[k]], base_rate = b19[[k]])
}) %>%
  inner_join(rate_meta, by = "key") %>%
  mutate(
    effect = u_cf - u_now,
    detail = paste0("now ", percent(cur_rate, accuracy = 0.01),
                    ", 2019 ", percent(base_rate, accuracy = 0.01)),
    lab    = paste0(sprintf("%+.2f", 100*effect), " pp"),
    # A NEGATIVE effect means the 2019 value would deliver a LOWER steady-state
    # rate, i.e. today's value of that rate is currently holding unemployment UP.
    dir    = if_else(effect < 0, "holding u up", "holding u down")
  ) %>%
  arrange(effect) %>%
  mutate(short = factor(short, levels = short))

cat("\nSteady-state u at current 3-month flow rates:", percent(u_now, 0.01),
    "| at 2019 rates:", percent(u_19, 0.01), "\n")
print(cf %>% select(short, detail, effect_pp = effect) %>%
        mutate(effect_pp = round(100*effect_pp, 3)) %>% as.data.frame())

p6 <- ggplot(cf, aes(effect * 100, short, fill = dir)) +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.7) +
  geom_col(width = 0.62) +
  geom_text(aes(label = lab,
                hjust = if_else(effect > 0, -0.15, 1.15)),
            size = 5.2, fontface = "bold", colour = NAVY) +
  geom_text(aes(x = 0, label = detail,
                hjust = if_else(effect > 0, 1.12, -0.12)),
            size = 4.4, colour = "grey35") +
  scale_fill_manual(values = c(`holding u up` = RED, `holding u down` = "#20705a"),
                    guide = "none") +
  scale_x_continuous(labels = label_number(accuracy = 0.1, style_positive = "plus"),
                     expand = expansion(mult = 0.30)) +
  labs(
    title    = "Only the Collapse in Layoffs Is Holding Unemployment Down",
    subtitle = paste0(
      "Effect on the flow-implied steady-state unemployment rate of returning ONE transition rate to its 2019 average, ",
      "holding\nthe other five at their current three-month values. At today's flows the steady state is ",
      percent(u_now, 0.01), "; at 2019's flows it is ", percent(u_19, 0.01),
      ".\nA red bar means today's value of that rate is holding unemployment UP relative to 2019. ",
      "Five of six are red."
    ),
    x = "Percentage-point change in the steady-state unemployment rate if this one rate returned to 2019",
    y = NULL,
    caption = paste0("Effects do not sum to the total because the six rates interact.\n", CAPTION)
  ) +
  theme_esp(base_size = 16) +
  theme(
    plot.title         = element_text(size = 22, face = "bold"),
    plot.subtitle      = element_text(size = 13, lineheight = 1.2),
    axis.text.y        = element_text(size = 12.5, face = "bold", lineheight = 1.15),
    axis.title.x       = element_text(size = 12, colour = "grey35"),
    panel.grid.major.y = element_blank(),
    plot.caption.position = "plot"
  )

ggsave("graphics/10b_6_counterfactual.png", p6, dpi = "retina",
       width = 13.5, height = 8, units = "in")

cat("\nWrote 6 charts to graphics/10b_*.png\n")
