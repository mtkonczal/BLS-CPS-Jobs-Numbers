library(tidyverse)
library(lubridate)
library(janitor)
library(govMacroTools)
library(scales)

# Data: revisions + PAYEMS level
x <- read_csv("data/job_revisions_full.csv", show_col_types = FALSE) |>
  clean_names() |>
  mutate(date = as.Date(date)) |>
  select(date, sa_rev_3rd_1st) |>
  left_join(
    getFRED("PAYEMS") |> select(date, payems),
    by = "date"
  ) |>
  arrange(date) |>
  mutate(ratio = sa_rev_3rd_1st / payems,
         mon_index = year(date) * 12 + month(date))

# Recession start months (0 = start), tag era by start date
rec_starts <- getFRED("USREC") |>
  arrange(date) |>
  mutate(on = usrec == 1,
         start = on & !lag(on, default = FALSE)) |>
  filter(start) |>
  transmute(
    start_date = date,
    start_index = year(start_date) * 12 + month(start_date),
    era = "all"
  )

# Event-window data: keep each recession episode separately
ev <- rec_starts |>
  crossing(x) |>
  mutate(relative = mon_index - start_index) |>
  filter(relative >= -18, relative <= 18) |>
  select(start_date, era, relative, ratio)

ev %>%
  filter(era != "Pre-2003") %>%
# Boxplots by relative month, faceted by era
ggplot(aes(x = relative, y = ratio, group = relative)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_boxplot(width = 0.7, outlier.alpha = 0.4) +
  facet_wrap(~ era, ncol = 1) +
  scale_x_continuous(breaks = seq(-18, 18, 6)) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "CES 3rd-to-1st Revisions Around Recession Starts",
    subtitle = "Boxplots show dispersion across recession episodes by month relative to recession start (0). Faceted by start-era (pre-2003 vs 2003–present).",
    x = "Months relative to recession start",
    y = "Revision ÷ PAYEMS",
    caption = "BLS CES; PAYEMS from FRED; NBER recessions = USREC. Mike Konczal."
  ) +
  theme_classic(base_size = 18) +
  theme(plot.title.position = "plot")


# - annual

x <- read_csv("data/job_revisions_full.csv", show_col_types = FALSE) 

x %>%
  filter(year != 2020) %>%
  ggplot(aes(sa_1st, sa_rev_3rd_1st)) +
  geom_point() +
  geom_smooth(method = "lm")

summary(lm(sa_rev_3rd_1st ~sa_1st, data = x %>% filter(year != 2020)))

