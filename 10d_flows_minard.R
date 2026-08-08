# ==============================================================================
# 10d_flows_minard.R
#
# The CPS unemployment flows drawn in the manner of Charles Joseph Minard's 1869
# map of Napoleon's Russian campaign.
#
#   Minard                        Here
#   ---------------------------   ------------------------------------------
#   the army marching             the pool of unemployed people, month by month
#   band width = men remaining    band width = people unemployed
#   tan = the advance             tan = flows INTO unemployment
#   black = the retreat           black = flows OUT of unemployment
#   tributaries splitting off     the four gross flows, drawn to the same scale
#   town names                    month names
#   temperature panel below       job-finding rate: the weather of the market
#
# Everything drawn is observed data. No cohort simulation, no smoothing: each
# band width is a published level and each tributary is a published gross flow,
# all on one common scale in thousands of people.
#
# Data layer lives in scripts/flows_data.R. Requires BLS_KEY in .Renviron.
#
# Output: graphics/10d_minard_flows.png
# ==============================================================================

library(patchwork)
source("scripts/graphic_scripts.R")
source("scripts/flows_data.R")

PARCH <- "#f2ead6"   # parchment
INK   <- "#2b2620"   # sepia ink

FLOW_STYLE <- tribble(
  ~key,  ~label,                          ~fill,      ~side,
  "EU",  "LAID OFF",                      "#c08a3e",  "in",
  "NU",  "STARTED LOOKING FOR WORK",      "#e8cba0",  "in",
  "UE",  "FOUND WORK",                    "#2b2620",  "out",
  "UN",  "LEFT THE LABOR FORCE",          "#6f6a60",  "out"
)

# ---- the march: the longest run of consecutive months at the end of the data --
march <- dat %>%
  filter(!is.na(du_act)) %>%
  arrange(date) %>%
  # gaps have to be found from the dates themselves: `contiguous` is already
  # spent, having been used upstream to blank out the un-differenceable months
  mutate(run = cumsum(replace_na(round(as.numeric(date - lag(date)) / 30.4), 1) != 1)) %>%
  filter(run == max(run)) %>%
  slice_tail(n = 12) %>%
  transmute(date, U, EU, NU, UE, UN) %>%
  mutate(i = row_number(), h = U / 2)

N <- nrow(march)
cat(sprintf("Marching %d consecutive months: %s to %s\n", N,
            format(min(march$date), "%B %Y"), format(max(march$date), "%B %Y")))

LIFT <- 2300   # how far above/below the band the tributaries reach
RUN  <- 0.72   # how far along the time axis a tributary travels

# A tributary is a constant-thickness parallelogram: the lower edge runs from
# (x0, y0) to (x1, y1) and the band extends `thick` above it.
trib <- function(id, key, x0, y0, x1, y1, thick) {
  tibble(id = id, key = key,
         x = c(x0, x1, x1, x0),
         y = c(y0, y1, y1 + thick, y0 + thick))
}

polys <- pmap_dfr(march, function(date, U, EU, NU, UE, UN, i, h) {
  bind_rows(
    # inflows arrive from the upper left and land on the top edge of the band
    trib(paste0(i, "EU"), "EU", i - RUN, h + LIFT,           i, h,      EU),
    trib(paste0(i, "NU"), "NU", i - RUN, h + LIFT + EU,      i, h + EU, NU),
    # outflows peel off the bottom edge and run down to the right
    trib(paste0(i, "UE"), "UE", i, -h - UE,      i + RUN, -h - LIFT - UE,      UE),
    trib(paste0(i, "UN"), "UN", i, -h - UE - UN, i + RUN, -h - LIFT - UE - UN, UN)
  )
}) %>%
  left_join(FLOW_STYLE, by = "key") %>%
  mutate(key = factor(key, levels = FLOW_STYLE$key))

# Minard writes his numbers on the bands themselves. Same here: each figure sits
# at the midpoint of its own tributary, and only the four flow NAMES hang off
# the outer ends.
trib_num <- march %>%
  transmute(i,
            EU_x = i - RUN/2, EU_y = h + LIFT/2 + EU/2,                 EU_v = EU,
            NU_x = i - RUN/2, NU_y = h + EU + LIFT/2 + NU/2,            NU_v = NU,
            UE_x = i + RUN/2, UE_y = -h - LIFT/2 - UE/2,                UE_v = UE,
            UN_x = i + RUN/2, UN_y = -h - LIFT/2 - UE - UN/2,           UN_v = UN) %>%
  pivot_longer(-i, names_to = c("key", ".value"), names_sep = "_") %>%
  left_join(FLOW_STYLE, by = "key") %>%
  mutate(ink = if_else(key %in% c("UE", "UN"), PARCH, INK))

trib_name <- march %>%
  transmute(i, h, EU, NU, UE, UN) %>%
  filter(i %in% c(1L, N)) %>%
  transmute(i, h,
            EU_y = h + LIFT + EU/2,
            NU_y = h + LIFT + EU + NU/2,
            UE_y = -h - LIFT - UE/2,
            UN_y = -h - LIFT - UE - UN/2) %>%
  pivot_longer(-c(i, h), names_to = c("key", ".value"), names_sep = "_") %>%
  left_join(FLOW_STYLE, by = "key") %>%
  filter((side == "in" & i == 1L) | (side == "out" & i == N)) %>%
  mutate(x  = if_else(side == "in", i - RUN - 0.08, i + RUN + 0.08),
         hj = if_else(side == "in", 1, 0))

stopifnot(nrow(polys) == 16 * N)

# ---- the band ---------------------------------------------------------------
band <- march %>% select(i, h, U, date)

XLIM <- c(-2.15, N + 2.9)
# Derive the vertical extent from the drawing itself. A hard-coded limit silently
# dropped December's outflow polygon, whose deepest vertex fell just outside it.
YMAX <- max(march$h + LIFT + march$EU + march$NU) + 750
YMIN <- min(-march$h - LIFT - march$UE - march$UN) - 750
scale_bar <- tibble(x = N + 1.15, ymin = -1400, ymax = -1400 + 1000)

p_map <- ggplot() +
  # tributaries first so the band sits on top of them
  geom_polygon(data = polys, aes(x, y, group = id, fill = key),
               colour = INK, linewidth = 0.25) +
  # the pool itself
  geom_ribbon(data = band, aes(x = i, ymin = -h, ymax = h),
              fill = "#d9bd8c", colour = INK, linewidth = 0.5) +
  # level written inside the band
  geom_text(data = band, aes(i, 0, label = comma(U)),
            family = "serif", fontface = "bold", size = 4.6, colour = INK) +
  geom_text(data = band, aes(i, -h + 620, label = format(date, "%b %Y")),
            family = "serif", size = 3.9, colour = INK) +
  # tributary numbers and names
  geom_text(data = trib_num, aes(x, y, label = comma(v), colour = I(ink)),
            family = "serif", size = 3.7) +
  geom_text(data = trib_name, aes(x, y, label = label, hjust = hj),
            family = "serif", fontface = "bold", size = 4.0, colour = INK) +
  # scale reference, in Minard's spirit
  geom_rect(data = scale_bar,
            aes(xmin = x, xmax = x + 0.13, ymin = ymin, ymax = ymax),
            fill = "#d9bd8c", colour = INK, linewidth = 0.4) +
  geom_text(data = scale_bar, aes(x + 0.20, ymin + 500),
            label = "one million\npeople", hjust = 0, family = "serif",
            size = 3.6, lineheight = 1.05, colour = INK) +
  scale_fill_manual(values = setNames(FLOW_STYLE$fill, FLOW_STYLE$key), guide = "none") +
  scale_x_continuous(expand = expansion(0)) +
  scale_y_continuous(expand = expansion(0)) +
  coord_cartesian(xlim = XLIM, ylim = c(YMIN, YMAX)) +
  labs(
    title = paste0("Figurative Map of the Successive Movements of the Unemployed, ",
                   format(min(march$date), "%B %Y"), " to ", format(max(march$date), "%B %Y")),
    subtitle = paste0(
      "The width of every band is proportional to the number of people, on one common scale. ",
      "Tan bands are people entering unemployment;\nblack and grey bands are people leaving it. ",
      "Numbers are in thousands. Nearly half the pool is replaced every month, so the level barely\nmoves while ",
      "more than three million people cross in each direction. Drawn in the manner of Charles Joseph Minard, 1869, ",
      "from the\nBureau of Labor Statistics Current Population Survey labor force flows, seasonally adjusted, ",
      "by Mike Konczal.")
  ) +
  theme_void(base_family = "serif") +
  theme(
    plot.background  = element_rect(fill = PARCH, colour = NA),
    panel.background = element_rect(fill = PARCH, colour = NA),
    plot.title       = element_text(size = 20, face = "bold", colour = INK,
                                    hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 11.5, colour = INK, hjust = 0.5,
                                    lineheight = 1.2, margin = margin(b = 8)),
    plot.margin      = margin(12, 12, 2, 12)
  )

# ---- the temperature panel: the job-finding rate ----------------------------
# Minard hung a temperature curve under the retreat to explain the losses. The
# labor market equivalent is the share of the unemployed who find work each
# month: the colder it gets, the harder it is to get out of the pool.

temper <- rates %>%
  inner_join(march %>% select(date, i), by = "date") %>%
  transmute(i, date, f = 100 * f_UE)

p_temp <- ggplot(temper, aes(i, f)) +
  geom_segment(aes(xend = i, y = min(f) - 2.4, yend = f),
               colour = INK, linewidth = 0.3, linetype = "22") +
  geom_line(colour = INK, linewidth = 0.9) +
  geom_point(colour = INK, size = 2.1) +
  geom_text(aes(label = sprintf("%.1f%%", f)), vjust = -1.1,
            family = "serif", size = 3.7, colour = INK) +
  annotate("text", x = XLIM[1] + 0.15, y = max(temper$f) + 2.5, hjust = 0, vjust = 1,
           family = "serif", fontface = "italic", size = 4.2, colour = INK,
           label = "GRAPHIC TABLE of the share of the unemployed who found work, in percent") +
  scale_x_continuous(expand = expansion(0)) +
  scale_y_continuous(expand = expansion(0)) +
  coord_cartesian(xlim = XLIM,
                  ylim = c(min(temper$f) - 2.4, max(temper$f) + 2.7)) +
  theme_void(base_family = "serif") +
  theme(
    plot.background  = element_rect(fill = PARCH, colour = NA),
    panel.background = element_rect(fill = PARCH, colour = NA),
    plot.margin      = margin(0, 12, 10, 12)
  )

minard <- (p_map / p_temp) +
  plot_layout(heights = c(3.4, 1)) &
  theme(plot.background = element_rect(fill = PARCH, colour = NA))

ggsave("graphics/10d_minard_flows.png", minard, dpi = "retina",
       width = 17, height = 9, units = "in")

cat("Wrote graphics/10d_minard_flows.png\n")
