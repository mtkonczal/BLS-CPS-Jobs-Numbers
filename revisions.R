library(janitor)
library(tidyverse)
library(ggtext)
library(blsR)
library(govMacroTools)
library(lubridate)
library(scales)

df <- read_csv("data/job_revisions_full.csv")

df %>%
  mutate(per_off = (sa_rev_3rd_1st)/sa_1st) %>%
  group_by(year) %>%
  reframe(per_off = mean(per_off, rm.na = TRUE)) %>%
ggplot(aes(year, per_off)) + geom_line() +
  theme_minimal() +
  labs(title = "3rd-to-1st Month Revision as a Percent of Initial Jobs Estimates.",
       caption = "Mike Konczal.") +
  theme(plot.title.position = "plot") +
  scale_y_continuous(label = percent)




library(janitor)
library(tidyverse)
library(lubridate)
library(slider)
library(govMacroTools)

df <- read_csv("data/job_revisions_full.csv") |> 
  clean_names() |> 
  arrange(date) |> 
  mutate(ma6_sa_rev_3rd_1st = slide_dbl(sa_rev_3rd_1st/payems, mean, .before = 5, .complete = TRUE))

# Recession shading (NBER via USREC)
recessions <- getFRED("USREC") |>
  filter(date >= min(df$date)) |>
  mutate(rec = usrec == 1,
         grp = cumsum(rec != lag(rec, default = FALSE))) |>
  group_by(grp) |>
  filter(any(rec)) |>
  summarise(start = min(date), end = max(date) + months(1), .groups = "drop")

# Plot raw series + 6-month MA with recession bars
df |>
  ggplot(aes(date, ma6_sa_rev_3rd_1st)) +
  geom_rect(data = recessions,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, alpha = 0.2) +
  geom_line() +
  labs(title = "Six-month Moving Average of SA Revision (3rd - 1st) as a Percent of Non-Farm Payroll Employment",
       subtitle = "Nonfarm payroll over-the-month change revisions. Shaded bars are recessions.",
       y = "Thousands", x = NULL,
       caption = "BLS CES; shading = NBER recessions (USREC). Mike Konczal.") +
  theme_classic(base_size = 18) +
  theme(plot.title.position = "plot") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(label = percent)
  


df <- read_csv("data/job_revisions_full.csv")


df %>%
  mutate(sa_1st_diff = sa_1st - lag(sa_1st)) %>%
  filter(year != 2020, year >= 2000) %>%
  ggplot(aes(sa_1st_diff, sa_rev_3rd_1st)) +
  geom_point() +
  geom_smooth(method = "lm")



summary(lm(sa_rev_3rd_1st ~ sa_1st_diff, data = df %>%
            mutate(sa_1st_diff = sa_1st - lag(sa_1st)) %>%
            filter(year >= 2000, year != 2020)))


df %>% 
  filter(!is.na(lag(sa_rev_3rd_2nd,1))) %>%
  mutate(add = lag(sa_rev_3rd_2nd, 1) + sa_rev_2nd_1st,
         addP = add/payems,
         sa_1st_diff = sa_1st - lag(sa_1st),
         max_date = date == max(date)) %>%
  filter(year != 2020) %>%
  ggplot(aes(sa_1st_diff, add, color = max_date)) +
  geom_point() +
  geom_smooth(method = "lm")
  


df %>%
  reframe(date = date,
          per = sa_1st/payems,
          per = if_else(year(date) == 2020, NA, per)) %>%
  ggplot(aes(date, per)) + geom_line() +
  theme_minimal() +
  scale_y_continuous(label = percent)
  


df %>%
  filter(!is.na(sa_1st))
  reframe(date = date,
          per = sa_1st/payems,
          perA = abs(per),
          after_2003 = year(date) >= 2003) %>%
  group_by(after_2003) %>%
  reframe(mean = mean(per),
          mean_abs = mean(perA))