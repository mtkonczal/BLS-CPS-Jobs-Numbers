# ==============================================================================
# 10c_flows_bathtub.R
#
# The labor market as two bathtubs.
#
#   UNEMPLOYMENT TUB   fills from  E->U and NLF->U
#                      drains to   U->E and U->NLF
#
#   LABOR FORCE TUB    fills from  NLF->E and NLF->U
#                      drains to   E->NLF and U->NLF
#
# Two flows sit in both tubs. Keeping the tubs separate is the point: the
# unemployment tub can drain into the labor force tub's drain rather than into
# employment, which is how the unemployment rate falls in a month when
# employment also falls.
#
# Data layer lives in scripts/flows_data.R (shared with 10b_flows_deep_dive.R).
# Requires BLS_KEY in .Renviron.
#
# Outputs (graphics/):
#   10c_1_tub_unemployment.png  monthly ins vs. outs, unemployment tub
#   10c_2_tub_laborforce.png    monthly ins vs. outs, labor force tub
#   10c_3_tub_schematic.png     both tubs drawn, latest month
#   10c_4_water_line.png        actual level vs. the level the flows imply
#   10c_5_cumulative.png        what filled and drained each tub since 2024
# ==============================================================================

library(patchwork)
source("scripts/graphic_scripts.R")
source("scripts/flows_data.R")

IN_COL  <- "#c05a2e"  # water going in
OUT_COL <- "#2a7f8f"  # water going out

# Break lines and ribbons at the missing-survey gap instead of drawing across it
add_gap_groups <- function(d) {
  d %>%
    arrange(date) %>%
    mutate(step = round(as.numeric(date - lag(date)) / 30.4),
           grp  = cumsum(replace_na(step, 1) != 1)) %>%
    select(-step)
}

rollk <- function(x, k) zoo::rollapplyr(x, k, mean, fill = NA)

tubs <- dat %>%
  transmute(
    date, contiguous, U, LF, N, POP,
    u_in   = EU + NU,   u_out  = UE + UN,
    lf_in  = NE + NU,   lf_out = EN + UN,
    u_marg = r_U,       lf_marg = r_LF,
    u_lvl_chg = dU,     lf_lvl_chg = dLF
  ) %>%
  mutate(across(c(u_in, u_out, lf_in, lf_out),
                ~ if_else(contiguous, .x, NA_real_)),
         u_net = u_in - u_out, lf_net = lf_in - lf_out)

TL <- tubs %>% filter(date == latest)

cat(sprintf("\nUnemployment tub: level %sk | in %sk | out %sk | net %+.0fk | margin %+.0fk\n",
            comma(TL$U), comma(TL$u_in), comma(TL$u_out), TL$u_net, TL$u_marg))
cat(sprintf("Labor force tub:  level %sk | in %sk | out %sk | net %+.0fk | margin %+.0fk\n",
            comma(TL$LF), comma(TL$lf_in), comma(TL$lf_out), TL$lf_net, TL$lf_marg))
cat(sprintf("Turnover: unemployment %.1f%% out per month, labor force %.1f%% out per month\n",
            100*TL$u_out/TL$U, 100*TL$lf_out/TL$LF))

# ==============================================================================
# CHARTS 1 and 2: MONTHLY INS VS. OUTS
# ==============================================================================
# The classic bathtub picture. Two lines, and the gap between them shaded by
# sign: orange where the tub is filling, teal where it is draining. Because both
# lines are large and close together, the shaded gap IS the monthly change.
# Charts start in 2022 because the 2020 flows are an order of magnitude larger
# and would flatten everything else; the 2019 averages are carried in as dashed
# reference lines instead.

REF19 <- tubs %>%
  filter(year(date) == 2019) %>%
  summarize(across(c(u_in, u_out, lf_in, lf_out), mean))

tub_flow_chart <- function(d, in_lab, out_lab, ref_in, ref_out, ttl, sub) {
  dd <- d %>%
    filter(!is.na(inflow)) %>%
    add_gap_groups() %>%
    group_by(grp) %>%
    mutate(in3 = rollk(inflow, 3), out3 = rollk(outflow, 3)) %>%
    ungroup()
  rng <- diff(range(c(dd$inflow, dd$outflow)))
  last_row <- dd %>%
    filter(date == max(date)) %>%
    mutate(gapsize = abs(in3 - out3),
           push = if_else(gapsize < 0.16*rng, 0.09*rng, 0),
           y_in  = in3  + push * sign(in3 - out3 + 1e-9),
           y_out = out3 - push * sign(in3 - out3 + 1e-9))

  ggplot(dd, aes(x = date, group = grp)) +
    geom_ribbon(aes(ymin = out3, ymax = pmax(in3, out3)),
                fill = IN_COL, alpha = 0.35) +
    geom_ribbon(aes(ymin = pmin(in3, out3), ymax = out3),
                fill = OUT_COL, alpha = 0.35) +
    geom_hline(yintercept = ref_in,  colour = IN_COL,  linetype = "22", linewidth = 0.7) +
    geom_hline(yintercept = ref_out, colour = OUT_COL, linetype = "22", linewidth = 0.7) +
    geom_line(aes(y = inflow),  colour = IN_COL,  linewidth = 0.5, alpha = 0.55) +
    geom_line(aes(y = outflow), colour = OUT_COL, linewidth = 0.5, alpha = 0.55) +
    geom_line(aes(y = in3),  colour = IN_COL,  linewidth = 1.7) +
    geom_line(aes(y = out3), colour = OUT_COL, linewidth = 1.7) +
    geom_point(data = last_row, aes(y = in3),  colour = IN_COL,  size = 3.4) +
    geom_point(data = last_row, aes(y = out3), colour = OUT_COL, size = 3.4) +
    geom_text(data = last_row, aes(x = date + days(22), y = y_in,
                                   label = paste0(in_lab, "\n", comma(round(in3)), "k (3-mo avg)")),
              hjust = 0, vjust = 0.5, size = 4.7, fontface = "bold",
              lineheight = 1.05, colour = IN_COL) +
    geom_text(data = last_row, aes(x = date + days(22), y = y_out,
                                   label = paste0(out_lab, "\n", comma(round(out3)), "k (3-mo avg)")),
              hjust = 0, vjust = 0.5, size = 4.7, fontface = "bold",
              lineheight = 1.05, colour = OUT_COL) +
    # park each 2019 reference label on the OUTSIDE of the pair so they never
    # collide, whichever of the two lines happens to sit higher
    annotate("text", x = min(dd$date), y = ref_in, label = " 2019 average, in",
             hjust = 0, vjust = if (ref_in >= ref_out) -0.7 else 1.6,
             size = 4, colour = IN_COL) +
    annotate("text", x = min(dd$date), y = ref_out, label = " 2019 average, out",
             hjust = 0, vjust = if (ref_out > ref_in) -0.7 else 1.6,
             size = 4, colour = OUT_COL) +
    scale_x_date(breaks = date_breaks_every(dd$date, 6), date_labels = "%b\n%Y",
                 expand = expansion(mult = c(0.02, 0.30))) +
    scale_y_continuous(labels = comma) +
    labs(title = ttl, subtitle = sub, x = NULL,
         y = "Thousands of people per month", caption = CAPTION) +
    coord_cartesian(clip = "off") +
    theme_esp(base_size = 16) +
    theme(
      plot.title    = element_text(size = 22, face = "bold"),
      plot.subtitle = element_text(size = 13, lineheight = 1.2),
      axis.title.y  = element_text(size = 12, colour = "grey35"),
      axis.text.x   = element_text(size = 11.5),
      panel.grid.major.x = element_blank(),
      plot.caption.position = "plot"
    )
}

START <- as.Date("2022-01-01")

p1 <- tubs %>%
  filter(date >= START) %>%
  transmute(date, inflow = u_in, outflow = u_out) %>%
  tub_flow_chart(
    in_lab  = "INTO unemployment",
    out_lab = "OUT of unemployment",
    ref_in = REF19$u_in, ref_out = REF19$u_out,
    ttl = "The Unemployment Tub: Nearly Half the Water Is Replaced Every Month",
    sub = paste0(
      "People entering and leaving unemployment each month. In goes laid-off workers plus people starting a job ",
      "search;\nout goes people who found work plus people who quit searching. Heavy lines and shading are three-month ",
      "averages,\nthin lines are the monthly readings. Orange shading means the tub was filling, teal means it was ",
      "draining. In ", latest_lab, ",\n", comma(round(TL$u_out)), ",000 left a pool of ", comma(round(TL$U)),
      ",000: the whole tub turns over in about ", sprintf("%.1f", TL$U/TL$u_out), " months."
    )
  )

ggsave("graphics/10c_1_tub_unemployment.png", p1, dpi = "retina",
       width = 13, height = 7.5, units = "in")

p2 <- tubs %>%
  filter(date >= START) %>%
  transmute(date, inflow = lf_in, outflow = lf_out) %>%
  tub_flow_chart(
    in_lab  = "INTO the labor force",
    out_lab = "OUT of the labor force",
    ref_in = REF19$lf_in, ref_out = REF19$lf_out,
    ttl = "The Labor Force Tub Is Draining",
    sub = paste0(
      "People entering and leaving the labor force each month, whether they land in a job or a job search. Heavy lines ",
      "and\nshading are three-month averages, thin lines are the monthly readings. Orange shading means the labor force ",
      "was\ngrowing, teal means it was shrinking. This is the drain that lets the unemployment rate fall while employment ",
      "falls:\nworkers can leave the unemployment tub through here instead of into a job."
    )
  )

ggsave("graphics/10c_2_tub_laborforce.png", p2, dpi = "retina",
       width = 13, height = 7.5, units = "in")

# ==============================================================================
# CHART 3: THE TUBS, DRAWN
# ==============================================================================
# Water level is the true share of the population age 16 and over in each state,
# so the two tubs sit on one honest vertical scale. Pipe thickness is on one
# common flow scale across both tubs. The unemployment tub is a shallow puddle
# with a firehose running through it; the labor force tub is deep and slow.

TUBS <- c("UNEMPLOYMENT TUB", "LABOR FORCE TUB")
g <- function(v) dat[[v]][dat$date == latest]

tub_geo <- tibble(
  tub     = factor(TUBS, levels = TUBS),
  level   = c(TL$U, TL$LF),
  inflow  = c(TL$u_in, TL$lf_in),
  outflow = c(TL$u_out, TL$lf_out),
  net     = c(TL$u_net, TL$lf_net),
  marg    = c(TL$u_marg, TL$lf_marg),
  chg     = c(TL$u_lvl_chg, TL$lf_lvl_chg),
  in_txt  = c(paste0("laid off ", comma(g("EU")), "k  +  started looking ", comma(g("NU")), "k"),
              paste0("took a job ", comma(g("NE")), "k  +  started looking ", comma(g("NU")), "k")),
  out_txt = c(paste0("found work ", comma(g("UE")), "k  +  quit looking ", comma(g("UN")), "k"),
              paste0("left work ", comma(g("EN")), "k  +  quit looking ", comma(g("UN")), "k"))
) %>%
  mutate(
    water  = 100 * level / TL$POP,
    lw_in  = rescale(inflow,  to = c(2, 15), from = c(0, max(outflow))),
    lw_out = rescale(outflow, to = c(2, 15), from = c(0, max(outflow)))
  )

TUB_TOP <- 72; TUB_L <- 1.2; TUB_R <- 11.8

walls <- tub_geo %>%
  transmute(tub, seg = list(tibble(
    x    = c(TUB_L, TUB_R, TUB_L),
    xend = c(TUB_L, TUB_R, TUB_R),
    y    = c(TUB_TOP, TUB_TOP, 0),
    yend = c(0, 0, 0)))) %>%
  unnest(seg)

p3 <- ggplot() +
  geom_rect(data = tub_geo,
            aes(xmin = TUB_L, xmax = TUB_R, ymin = 0, ymax = water),
            fill = "#8fb8c4", alpha = 0.85) +
  geom_segment(data = walls, aes(x, y, xend = xend, yend = yend),
               colour = NAVY, linewidth = 1.6, lineend = "round") +
  # turnover headline, above everything
  geom_text(data = tub_geo,
            aes(x = (TUB_L + TUB_R)/2, y = 118,
                label = paste0(sprintf("%.0f%%", 100*outflow/level),
                               " of this tub empties every month")),
            size = 5.2, fontface = "bold", colour = NAVY) +
  # inflow pipe and label
  geom_segment(data = tub_geo,
               aes(x = 3.2, xend = 3.2, y = 104, yend = TUB_TOP + 3,
                   linewidth = I(lw_in)),
               colour = IN_COL, lineend = "butt",
               arrow = arrow(length = unit(0.42, "cm"), type = "closed")) +
  geom_text(data = tub_geo,
            aes(x = 4.4, y = 100, label = paste0("IN   ", comma(round(inflow)), "k")),
            hjust = 0, vjust = 1, size = 5.2, fontface = "bold", colour = IN_COL) +
  geom_text(data = tub_geo, aes(x = 4.4, y = 88, label = in_txt),
            hjust = 0, vjust = 1, size = 4.3, colour = IN_COL) +
  # outflow pipe and label
  geom_segment(data = tub_geo,
               aes(x = 3.2, xend = 3.2, y = -4, yend = -26,
                   linewidth = I(lw_out)),
               colour = OUT_COL, lineend = "butt",
               arrow = arrow(length = unit(0.42, "cm"), type = "closed")) +
  geom_text(data = tub_geo,
            aes(x = 4.4, y = -8, label = paste0("OUT   ", comma(round(outflow)), "k")),
            hjust = 0, vjust = 1, size = 5.2, fontface = "bold", colour = OUT_COL) +
  geom_text(data = tub_geo, aes(x = 4.4, y = -20, label = out_txt),
            hjust = 0, vjust = 1, size = 4.3, colour = OUT_COL) +
  # level readout inside the tub
  geom_text(data = tub_geo,
            aes(x = (TUB_L + TUB_R)/2, y = pmax(water/2, 10),
                label = paste0(comma(round(level)), "k\n",
                               sprintf("%.1f%%", water), " of the population age 16+")),
            size = 5.2, lineheight = 1.15, fontface = "bold", colour = NAVY) +
  # net change readout
  geom_text(data = tub_geo,
            aes(x = (TUB_L + TUB_R)/2, y = -36,
                label = paste0("LEVEL ", if_else(chg >= 0, "+", ""), comma(round(chg)),
                               "k THIS MONTH\nnet flow ", if_else(net >= 0, "+", ""),
                               comma(round(net)), "k, population and sample margin ",
                               if_else(marg >= 0, "+", ""), comma(round(marg)), "k")),
            size = 4.6, lineheight = 1.2, fontface = "bold", colour = NAVY) +
  facet_wrap(~tub) +
  scale_x_continuous(limits = c(0.5, 13.0)) +
  scale_y_continuous(limits = c(-44, 124)) +
  labs(
    title    = paste0("Two Bathtubs, ", latest_lab),
    subtitle = paste0(
      "Water level is the true share of the population age 16 and over, so both tubs sit on one vertical scale. ",
      "Pipe\nthickness is on one common flow scale. The unemployment tub is a shallow puddle with a firehose running ",
      "through\nit. The labor force tub is deep and slow, and right now both of them are draining."
    ),
    caption = CAPTION
  ) +
  theme_esp(base_size = 16) +
  theme(
    plot.title       = element_text(size = 23, face = "bold"),
    plot.subtitle    = element_text(size = 13, lineheight = 1.2),
    axis.text        = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(size = 16, face = "bold"),
    panel.spacing    = unit(2.5, "lines"),
    plot.caption.position = "plot"
  )

ggsave("graphics/10c_3_tub_schematic.png", p3, dpi = "retina",
       width = 15, height = 9.5, units = "in")

# ==============================================================================
# CHART 4: WHERE THE WATER LINE IS HEADED
# ==============================================================================
# Treat the six transition rates as a three-state Markov chain and solve for the
# stationary distribution: the level each tub would settle at if the current
# flow rates persisted. The monthly estimate is noisy, badly so for
# participation, so the heavy line is built from six-month averages of the RATES
# (smoothing the inputs behaves far better than smoothing the output) and the
# shaded band is the range of the six monthly estimates inside each window. Read
# the band, not the line: for unemployment the gap is inside the noise.

steady <- function(f_UE, f_UN, s_EU, s_EN, e_NU, e_NE) {
  if (any(is.na(c(f_UE, f_UN, s_EU, s_EN, e_NU, e_NE)))) return(c(NA_real_, NA_real_))
  P <- matrix(c(1 - s_EU - s_EN, s_EU,            s_EN,
                f_UE,            1 - f_UE - f_UN, f_UN,
                e_NE,            e_NU,            1 - e_NE - e_NU),
              nrow = 3, byrow = TRUE)
  ev <- eigen(t(P))
  v  <- Re(ev$vectors[, which.min(abs(ev$values - 1))]); v <- v / sum(v)
  c(v[2] / (v[1] + v[2]), v[1] + v[2])
}

RK <- c("f_UE", "f_UN", "s_EU", "s_EN", "e_NU", "e_NE")

# Roll over the six most recent AVAILABLE readings rather than six calendar
# months, so the two missing survey months do not blank out half a year of the
# smoothed series. The only distortion is that one window straddles the gap.
ss <- rates %>%
  filter(!is.na(f_UE)) %>%
  mutate(across(all_of(RK), ~ rollk(.x, 6), .names = "s6_{.col}")) %>%
  rowwise() %>%
  mutate(m  = list(steady(f_UE, f_UN, s_EU, s_EN, e_NU, e_NE)),
         s  = list(steady(s6_f_UE, s6_f_UN, s6_s_EU, s6_s_EN, s6_e_NU, s6_e_NE))) %>%
  ungroup() %>%
  transmute(date,
            u_m    = map_dbl(m, 1), lfpr_m = map_dbl(m, 2),
            u_s    = map_dbl(s, 1), lfpr_s = map_dbl(s, 2)) %>%
  mutate(
    u_lo    = zoo::rollapplyr(u_m,    6, min, fill = NA),
    u_hi    = zoo::rollapplyr(u_m,    6, max, fill = NA),
    lfpr_lo = zoo::rollapplyr(lfpr_m, 6, min, fill = NA),
    lfpr_hi = zoo::rollapplyr(lfpr_m, 6, max, fill = NA)
  ) %>%
  left_join(dat %>% select(date, u, lfpr), by = "date")

water_line <- function(d, ttl, ylab, acc) {
  dd <- d %>%
    filter(!is.na(actual), !is.na(star), date >= as.Date("2015-01-01"),
           date < as.Date("2020-02-01") | date > as.Date("2021-12-01")) %>%
    add_gap_groups()
  lastd <- dd %>% filter(date == max(date))
  ggplot(dd, aes(x = date, group = grp)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = RED, alpha = 0.16) +
    geom_line(aes(y = actual), colour = NAVY, linewidth = 1.4) +
    geom_line(aes(y = star), colour = RED, linewidth = 1.4) +
    geom_point(data = lastd, aes(y = actual), colour = NAVY, size = 3) +
    geom_point(data = lastd, aes(y = star),  colour = RED,  size = 3) +
    geom_text(data = lastd, aes(x = date + days(45), y = actual,
                                label = paste0("actual ", percent(actual, acc))),
              hjust = 0, size = 4.5, fontface = "bold", colour = NAVY) +
    geom_text(data = lastd, aes(x = date + days(45), y = star,
                                label = paste0("implied ", percent(star, acc))),
              hjust = 0, size = 4.5, fontface = "bold", colour = RED) +
    scale_y_continuous(labels = percent_format(accuracy = 100*acc)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                 expand = expansion(mult = c(0.02, 0.14))) +
    labs(title = ttl, x = NULL, y = ylab) +
    coord_cartesian(clip = "off") +
    theme_esp(base_size = 15) +
    theme(plot.title   = element_text(size = 17, face = "bold"),
          axis.title.y = element_text(size = 11, colour = "grey35"))
}

p4a <- ss %>%
  transmute(date, actual = u, star = u_s, lo = u_lo, hi = u_hi) %>%
  water_line("Unemployment tub", "Unemployment rate", 0.001)

p4b <- ss %>%
  transmute(date, actual = lfpr, star = lfpr_s, lo = lfpr_lo, hi = lfpr_hi) %>%
  water_line("Labor force tub", "Participation rate, age 16+", 0.001)

p4 <- (p4a / p4b) +
  plot_annotation(
    title    = "Neither Tub Is Far From the Level Its Own Flows Point To",
    subtitle = paste0(
      "Red is the level each tub would settle at if the current transition rates persisted, from the stationary ",
      "distribution\nof the implied three-state Markov chain, built on six-month averages of the rates. The band is ",
      "the range of the six\nmonthly estimates inside each window, and it is wide: read the band, not the line. ",
      "February 2020 through December\n2021 omitted. Neither gap is big enough to call a turn."
    ),
    caption = CAPTION,
    theme = theme_esp(base_size = 16) +
      theme(plot.title    = element_text(size = 22, face = "bold"),
            plot.subtitle = element_text(size = 13, lineheight = 1.2),
            plot.caption.position = "plot")
  ) &
  theme(plot.background = element_rect(fill = "#f4f2e4", color = NA))

ggsave("graphics/10c_4_water_line.png", p4, dpi = "retina",
       width = 13, height = 10, units = "in")

ss_last <- ss %>% filter(date == latest)
cat(sprintf("\nFlow-implied levels (6m avg of rates): u* %.2f%% (band %.2f-%.2f) vs u %.2f%%\n",
            100*ss_last$u_s, 100*ss_last$u_lo, 100*ss_last$u_hi, 100*ss_last$u))
cat(sprintf("                                       lfpr* %.2f%% (band %.2f-%.2f) vs lfpr %.2f%%\n",
            100*ss_last$lfpr_s, 100*ss_last$lfpr_lo, 100*ss_last$lfpr_hi, 100*ss_last$lfpr))

# ==============================================================================
# CHART 5: THE WHOLE PLUMBING BILL SINCE 2024
# ==============================================================================
# Gross flows summed over every measured month since January 2024, against the
# net change they produced. This is the scale that gets lost in a monthly print:
# tens of millions of transitions on each side, netting to almost nothing.

CUM_START <- as.Date("2024-01-01")

cum_rows <- dat %>% filter(date >= CUM_START, contiguous, !is.na(EU))
n_months  <- nrow(cum_rows)
cum_parts <- cum_rows %>%
  summarize(across(c(EU, NU, UE, UN, NE, EN, r_U, r_LF, dU, dLF), sum))

stopifnot(!any(is.na(cum_parts)))

cum_df <- bind_rows(
  tibble(tub = TUBS[1],
         part = c("Laid off\n(E to U)", "Started looking\n(NLF to U)",
                  "Found work\n(U to E)", "Quit looking\n(U to NLF)",
                  "Population /\nsample margin"),
         value = c(cum_parts$EU, cum_parts$NU, -cum_parts$UE, -cum_parts$UN, cum_parts$r_U)),
  tibble(tub = TUBS[2],
         part = c("Took a job\n(NLF to E)", "Started looking\n(NLF to U)",
                  "Left work\n(E to NLF)", "Quit looking\n(U to NLF)",
                  "Population /\nsample margin"),
         value = c(cum_parts$NE, cum_parts$NU, -cum_parts$EN, -cum_parts$UN, cum_parts$r_LF))
) %>%
  mutate(tub  = factor(tub, levels = TUBS),
         dir  = if_else(value >= 0, "fills the tub", "drains the tub"),
         part = fct_inorder(part))

net_df <- tibble(tub = factor(TUBS, levels = TUBS),
                 value = c(cum_parts$dU, cum_parts$dLF)) %>%
  mutate(lab = paste0("NET CHANGE IN THE LEVEL:  ", if_else(value >= 0, "+", ""),
                      comma(round(value)), "k"))

p5 <- ggplot(cum_df, aes(value / 1000, fct_rev(part), fill = dir)) +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.7) +
  geom_col(width = 0.66) +
  geom_text(aes(label = paste0(if_else(value >= 0, "+", ""),
                               number(value/1000, accuracy = 0.1), "M"),
                hjust = if_else(value >= 0, -0.12, 1.12)),
            size = 4.7, fontface = "bold", colour = NAVY) +
  geom_text(data = net_df, aes(x = 0, y = 0.30, label = lab),
            inherit.aes = FALSE, hjust = 0.5, vjust = 0, size = 5,
            fontface = "bold", colour = NAVY) +
  facet_wrap(~tub, scales = "free_y") +
  scale_fill_manual(values = c(`fills the tub` = IN_COL, `drains the tub` = OUT_COL),
                    guide = "none") +
  scale_x_continuous(labels = label_number(suffix = "M", style_positive = "plus"),
                     expand = expansion(mult = 0.24)) +
  coord_cartesian(ylim = c(0.1, 5.6)) +
  labs(
    title    = "Tens of Millions of Transitions, Almost No Net Change",
    subtitle = paste0(
      "Every measured monthly flow summed from January 2024 through ", latest_lab, " (", n_months,
      " months). The two missing survey\nmonths are excluded, so these are sums of measured monthly changes rather ",
      "than level differences. Orange fills the\ntub, teal drains it. The unemployment tub took in ",
      number((cum_parts$EU + cum_parts$NU)/1000, accuracy = 0.1),
      " million people and let out ",
      number((cum_parts$UE + cum_parts$UN)/1000, accuracy = 0.1), " million."
    ),
    x = "Millions of transitions", y = NULL, caption = CAPTION
  ) +
  theme_esp(base_size = 16) +
  theme(
    plot.title      = element_text(size = 22, face = "bold"),
    plot.subtitle   = element_text(size = 13, lineheight = 1.2),
    axis.text.y     = element_text(size = 12, face = "bold", lineheight = 1.05),
    axis.title.x    = element_text(size = 12, colour = "grey35"),
    strip.text      = element_text(size = 15, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.spacing   = unit(2, "lines"),
    plot.caption.position = "plot"
  )

ggsave("graphics/10c_5_cumulative.png", p5, dpi = "retina",
       width = 14, height = 8.5, units = "in")

cat("\nWrote 5 charts to graphics/10c_*.png\n")
