# ==============================================================================
# 10e_markov_matrix.R
#
# The CPS labor market as a three-state Markov chain, drawn three ways.
#
# The transition matrix P has rows = state last month, columns = state this
# month, and every row sums to one. The six off-diagonal entries are the
# published gross flows divided by their origin stock; the three diagonal
# entries are whatever is left over, i.e. the probability of staying put.
#
#   10e_1_matrix_heatmap.png     the matrix itself, today against 2019
#   10e_2_matrix_history.png     the same nine cells as time series
#   10e_3_chain_diagram.png      the chain as states and arrows
#
# Data layer lives in scripts/flows_data.R. Requires BLS_KEY in .Renviron.
# ==============================================================================

source("scripts/graphic_scripts.R")
source("scripts/flows_data.R")

STATES <- c("Employed", "Unemployed", "Not in\nlabor force")
SHORT  <- c("Employed", "Unemployed", "Not in labor force")

# ---- build the nine cells ----------------------------------------------------
P_long <- rates %>%
  filter(!is.na(f_UE)) %>%
  transmute(
    date,
    E_E = 1 - s_EU - s_EN, E_U = s_EU,             E_N = s_EN,
    U_E = f_UE,            U_U = 1 - f_UE - f_UN,  U_N = f_UN,
    N_E = e_NE,            N_U = e_NU,             N_N = 1 - e_NE - e_NU
  ) %>%
  pivot_longer(-date, names_to = c("from", "to"), names_sep = "_", values_to = "p") %>%
  mutate(
    from = factor(from, levels = c("E", "U", "N"), labels = STATES),
    to   = factor(to,   levels = c("E", "U", "N"), labels = STATES),
    stay = as.integer(from) == as.integer(to)
  )

# every row of the matrix must sum to one, every month
row_check <- P_long %>%
  group_by(date, from) %>%
  summarize(s = sum(p), .groups = "drop") %>%
  summarize(max_err = max(abs(s - 1)))
cat("Max row-sum error (should be 0):", signif(row_check$max_err, 3), "\n")
stopifnot(row_check$max_err < 1e-12)

# headline matrix: three-month average, which is what the flows will support.
# A single month moves these cells by more than the change we are describing.
NOW3 <- P_long %>%
  filter(date >= latest %m-% months(2)) %>%
  group_by(from, to, stay) %>%
  summarize(p = mean(p), .groups = "drop")

REF19 <- P_long %>%
  filter(year(date) == 2019) %>%
  group_by(from, to) %>%
  summarize(p19 = mean(p), .groups = "drop")

cells <- NOW3 %>%
  inner_join(REF19, by = c("from", "to")) %>%
  mutate(
    chg  = p - p19,
    # Colour is assigned by hand rather than by a scale: the diagonal runs
    # 52-96 percent and the off-diagonal 0.9-24, so one shared ramp would flatten
    # everything that matters. Movers get their own ramp, stayers a flat tan.
    fill = if_else(
      stay, "#e0d3b4",
      scales::col_numeric(c("#dceae9", "#2a7f8f", "#123f4a"),
                          domain = c(0, 25))(pmin(100 * p, 25))
    ),
    ink  = if_else(!stay & 100 * p > 11, "#f4f2e4", "#2c3254"),
    lab_p   = paste0(sprintf("%.2f", 100 * p), "%"),
    lab_ref = paste0("2019: ", sprintf("%.2f", 100 * p19), "%   ",
                     sprintf("%+.2f", 100 * chg), " pp")
  )

cat("\nTransition matrix, three months through", latest_lab, "(percent):\n")
print(cells %>%
        transmute(from, to, now = round(100*p, 2), y2019 = round(100*p19, 2),
                  change = round(100*chg, 2)) %>% as.data.frame())

# ==============================================================================
# 1. THE MATRIX AS A HEATMAP
# ==============================================================================

p1 <- ggplot(cells, aes(to, fct_rev(from))) +
  geom_tile(aes(fill = I(fill)), colour = "#f4f2e4", linewidth = 3) +
  geom_text(aes(label = lab_p, colour = I(ink)), vjust = -0.15,
            size = 9.5, fontface = "bold") +
  geom_text(aes(label = lab_ref, colour = I(ink)), vjust = 2.4,
            size = 4.4) +
  annotate("segment", x = 0.45, xend = 3.55, y = 3.62, yend = 3.62,
           colour = NAVY, linewidth = 0.6,
           arrow = arrow(length = unit(0.25, "cm"), type = "closed")) +
  annotate("text", x = 0.45, y = 3.70, hjust = 0, vjust = 0,
           label = "WHERE THEY ARE THIS MONTH", size = 4.6,
           fontface = "bold", colour = NAVY) +
  scale_x_discrete(position = "top", expand = expansion(0)) +
  scale_y_discrete(expand = expansion(0)) +
  coord_cartesian(clip = "off", ylim = c(0.5, 3.9)) +
  labs(
    title    = "The Labor Market as a Three-State Transition Matrix",
    subtitle = paste0(
      "Probability that a person in the row state last month is in the column state this month. Every row sums to 100%. ",
      "Three-\nmonth average through ", latest_lab,
      ", because a single month moves these cells by more than the changes worth describing.\nShaded cells are people who ",
      "moved; the tan diagonal is people who stayed put."
    ),
    x = NULL, y = "WHERE THEY WERE LAST MONTH", caption = CAPTION
  ) +
  theme_esp(base_size = 16) +
  theme(
    plot.title       = element_text(size = 23, face = "bold"),
    plot.subtitle    = element_text(size = 13, lineheight = 1.2),
    axis.title.y     = element_text(size = 13, face = "bold", colour = NAVY,
                                    angle = 90),
    axis.text        = element_text(size = 15, face = "bold", colour = NAVY),
    axis.text.y      = element_text(lineheight = 1.05),
    panel.grid       = element_blank(),
    plot.caption.position = "plot"
  )

ggsave("graphics/10e_1_matrix_heatmap.png", p1, dpi = "retina",
       width = 13.5, height = 9, units = "in")

# ==============================================================================
# 2. THE SAME NINE CELLS, AS HISTORY
# ==============================================================================
# Laid out in the same 3x3 grid so the matrix and the history read as one
# object. Each panel gets its own y scale: the diagonal and the off-diagonal
# differ by two orders of magnitude.

# facet_grid(scales = "free_y") frees the scale per ROW, which puts the 96% and
# the 0.8% cells on one axis and flattens both. facet_wrap over a row-major cell
# factor gives every panel its own scale while keeping the 3x3 reading order.
# strips have to fit the panel width, so the long state name gets abbreviated
BRIEF <- c("Employed", "Unemployed", "Not in LF")
cell_lab <- function(f, t) paste0(toupper(f), "   to   ", t)
LEV <- as.vector(t(outer(BRIEF, BRIEF, cell_lab)))

hist_df <- P_long %>%
  filter(date >= as.Date("1996-01-01"), !(date %within% COVID)) %>%
  inner_join(REF19, by = c("from", "to")) %>%
  mutate(cell = factor(cell_lab(BRIEF[as.integer(from)], BRIEF[as.integer(to)]),
                       levels = LEV))

now_pt <- hist_df %>% filter(date == latest)

p2 <- ggplot(hist_df, aes(date, 100 * p)) +
  geom_hline(aes(yintercept = 100 * p19), colour = RED,
             linetype = "22", linewidth = 0.7) +
  geom_line(aes(colour = I(if_else(stay, GREY, NAVY))), linewidth = 0.55) +
  geom_point(data = now_pt, colour = RED, size = 2.8) +
  geom_text(data = now_pt, aes(label = paste0(sprintf("%.1f", 100 * p), "%")),
            hjust = -0.22, vjust = 0.4, size = 4.6, fontface = "bold",
            colour = RED) +
  facet_wrap(~cell, ncol = 3, scales = "free_y") +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y",
               expand = expansion(mult = c(0.02, 0.26))) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "Every Cell of the Transition Matrix Since 1996",
    subtitle = paste0(
      "Same 3x3 reading order as the matrix: each row of panels is one origin state, each column one destination. ",
      "Dashed\nred line is the 2019 average, red dot is ", latest_lab,
      ". Grey lines are the stayers on the diagonal. Every panel has its own\nscale, because the diagonal and the ",
      "off-diagonal differ by two orders of magnitude. March through December 2020 omitted."
    ),
    x = NULL, y = NULL, caption = CAPTION
  ) +
  theme_esp(base_size = 15) +
  theme(
    plot.title      = element_text(size = 22, face = "bold"),
    plot.subtitle   = element_text(size = 13, lineheight = 1.2),
    strip.text      = element_text(size = 12.5, face = "bold"),
    axis.text       = element_text(size = 10.5),
    panel.spacing   = unit(1.2, "lines"),
    plot.caption.position = "plot"
  )

ggsave("graphics/10e_2_matrix_history.png", p2, dpi = "retina",
       width = 13.5, height = 9, units = "in")

# ==============================================================================
# 3. THE CHAIN AS STATES AND ARROWS
# ==============================================================================
# The textbook picture. Arrow width is proportional to the transition
# probability; the probability of staying put is written inside the state
# instead, because at 52 to 96 percent those self-loops would swamp everything.

node <- tibble(
  st = c("E", "U", "N"),
  x  = c(1.0, 5.4, 3.2),
  y  = c(3.5, 3.5, 0.9),
  nm = c("EMPLOYED", "UNEMPLOYED", "NOT IN THE\nLABOR FORCE")
) %>%
  left_join(
    cells %>% filter(stay) %>%
      transmute(st = c("E", "U", "N")[as.integer(from)], stay_p = p),
    by = "st"
  ) %>%
  mutate(lab = paste0(nm, "\n", sprintf("%.0f%%", 100 * stay_p), " stay"))

mv <- cells %>%
  filter(!stay) %>%
  transmute(f = c("E", "U", "N")[as.integer(from)],
            t = c("E", "U", "N")[as.integer(to)], p)

# two lanes per pair of states so the arrows in opposite directions do not sit
# on top of each other
arc <- tribble(
  ~f,  ~t,  ~x,   ~y,   ~xend, ~yend, ~lx,  ~ly,  ~hj,
  "E", "U", 1.75, 3.78, 4.70,  3.78,  3.22, 4.02, 0.5,
  "U", "E", 4.70, 3.22, 1.75,  3.22,  3.22, 2.98, 0.5,
  "E", "N", 1.42, 3.02, 2.68,  1.42,  1.95, 2.62, 0.0,
  "N", "E", 2.36, 1.28, 1.10,  2.88,  1.55, 1.92, 1.0,
  "U", "N", 4.82, 3.02, 3.72,  1.42,  4.35, 2.62, 1.0,
  "N", "U", 4.06, 1.28, 5.16,  2.88,  5.05, 1.92, 0.0
) %>%
  inner_join(mv, by = c("f", "t")) %>%
  mutate(
    lw  = rescale(p, to = c(0.8, 6.2), from = c(0, max(mv$p))),
    txt = paste0(sprintf("%.1f", 100 * p), "%")
  )

p3 <- ggplot() +
  geom_segment(data = arc,
               aes(x, y, xend = xend, yend = yend, linewidth = I(lw)),
               colour = "#2a7f8f", alpha = 0.9, lineend = "butt",
               arrow = arrow(length = unit(0.55, "cm"), type = "closed")) +
  geom_text(data = arc, aes(lx, ly, label = txt, hjust = hj),
            size = 5.4, fontface = "bold", colour = "#1b5966") +
  geom_label(data = node, aes(x, y, label = lab),
             size = 5.2, lineheight = 1.2, fontface = "bold",
             colour = "#f4f2e4", fill = NAVY,
             label.padding = unit(0.6, "lines"), label.r = unit(0.35, "lines")) +
  scale_x_continuous(limits = c(-0.1, 6.6)) +
  scale_y_continuous(limits = c(0.1, 4.6)) +
  labs(
    title    = "One Month in the Life of the Labor Market",
    subtitle = paste0(
      "Monthly transition probabilities, three-month average through ", latest_lab,
      ". Arrow thickness is proportional to the\nprobability. The chance of staying put is written inside each state ",
      "rather than drawn, because at 52 to 96 percent\nthose loops would swamp the arrows. The three probabilities ",
      "leaving each state sum to 100%."
    ),
    caption  = CAPTION
  ) +
  theme_esp(base_size = 16) +
  theme(
    plot.title       = element_text(size = 23, face = "bold"),
    plot.subtitle    = element_text(size = 13, lineheight = 1.2),
    axis.text        = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption.position = "plot"
  )

ggsave("graphics/10e_3_chain_diagram.png", p3, dpi = "retina",
       width = 13, height = 8.5, units = "in")

cat("\nWrote 3 charts to graphics/10e_*.png\n")
