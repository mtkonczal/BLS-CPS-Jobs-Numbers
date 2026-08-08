# ==============================================================================
# scripts/flows_data.R
#
# Shared data layer for the CPS gross-flows charts (10b_ and 10c_).
# Pulls all twelve labor force flow series plus the E / U / LF / NLF stocks,
# runs the reconciliation diagnostics, and builds:
#
#   dat     one row per month: raw flows, stocks, and the EXACT decomposition
#           of the monthly change in the unemployment rate
#   rates   the six monthly transition probabilities (flow / lagged origin stock)
#   latest, latest_lab   most recent month with a usable month-over-month change
#
# Source this after scripts/graphic_scripts.R. Requires BLS_KEY in .Renviron.
# ==============================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(blsR)

bls_set_key(Sys.getenv("BLS_KEY"))

CAPTION <- "BLS CPS labor force flows, seasonally adjusted. October and November 2025 flows unavailable (no October household survey). Mike Konczal"

# The pandemic transition rates are two orders of magnitude off everything else
# and would set the scale on every historical chart. Excluded from plots where
# noted, never from percentile or baseline calculations.
COVID <- interval(as.Date("2020-03-01"), as.Date("2020-12-01"))

NAVY  <- "#2c3254"
RED   <- "#a4322c"
GREEN <- "#20705a"
ORANGE <- "#e08b3c"
BLUE  <- "#4a7fb5"
GREY  <- "#8d8b7f"

# One rule, used in every chart: warm colors push the unemployment rate UP,
# cool colors push it DOWN. Pale versions are the flows the rate barely feels.
FLOW_COLS <- c(
  "Employed to Unemployed"      = RED,
  "Not in LF to Unemployed"     = ORANGE,
  "Employed to Not in LF"       = "#d8a39d",
  "Unemployed to Employed"      = GREEN,
  "Unemployed to Not in LF"     = BLUE,
  "Not in LF to Employed"       = "#9dc3b3",
  "Population / sample margin"  = GREY
)
FLOW_LEVELS <- names(FLOW_COLS)

date_breaks_every <- function(x, n) {
  d <- sort(unique(x[!is.na(x)]), decreasing = TRUE)
  rev(d[seq(1, length(d), n)])
}

# Rank language that survives a value at the very edge of the distribution
# ("0th percentile" is both ugly and misleading).
rank_phrase <- function(x, latest_v, since_lab) {
  n <- length(x)
  k_low <- sum(x <= latest_v)
  k   <- if (k_low <= n / 2) k_low else n - k_low + 1
  dir <- if (k_low <= n / 2) "lowest" else "highest"
  if (k == 1) {
    paste0(dir, " of ", comma(n), " months since ", since_lab)
  } else {
    paste0(ordinal(k), " ", dir, " of ", comma(n), " months since ", since_lab)
  }
}

# ---- pull --------------------------------------------------------------------

flow_ids <- tribble(
  ~series_id,     ~flow,
  "LNS17000000",  "EE",
  "LNS17100000",  "UE",
  "LNS17200000",  "NE",
  "LNS17300000",  "MI_E",
  "LNS17400000",  "EU",
  "LNS17500000",  "UU",
  "LNS17600000",  "NU",
  "LNS17700000",  "MI_U",
  "LNS17800000",  "EN",
  "LNS17900000",  "UN",
  "LNS18000000",  "NN",
  "LNS18100000",  "MI_N"
)

stock_ids <- c(E = "LNS12000000", U = "LNS13000000",
               LF = "LNS11000000", N = "LNS15000000")

# BLS v2 caps a request at 20 years, so pull in two chunks.
pull_bls <- function(ids, sy, ey) {
  get_n_series_table(ids, api_key = bls_get_key(),
                     start_year = sy, end_year = ey, tidy = TRUE) %>%
    mutate(across(everything(), as.character))
}
pull_long <- function(ids) {
  bind_rows(pull_bls(ids, 1990, 2008), pull_bls(ids, 2009, year(Sys.Date()))) %>%
    mutate(date = as.Date(paste0(year, "/", month, "/01"))) %>%
    select(-year, -month) %>%
    pivot_longer(-date, names_to = "series_id", values_to = "value") %>%
    mutate(value = suppressWarnings(as.numeric(value)))
}

flows_raw <- pull_long(flow_ids$series_id) %>%
  inner_join(flow_ids, by = "series_id") %>%
  select(date, flow, value) %>%
  pivot_wider(names_from = flow, values_from = value)

stocks_raw <- pull_long(unname(stock_ids)) %>%
  inner_join(tibble(series_id = unname(stock_ids), nm = names(stock_ids)),
             by = "series_id") %>%
  select(date, nm, value) %>%
  pivot_wider(names_from = nm, values_from = value)

# ---- diagnostic: the published stocks should be the sum of the flows ---------
recon <- flows_raw %>%
  transmute(date,
            E_f = EE + UE + NE + MI_E,
            U_f = EU + UU + NU + MI_U) %>%
  inner_join(stocks_raw, by = "date") %>%
  filter(!is.na(E_f), !is.na(E)) %>%
  summarize(max_abs_E_gap = max(abs(E_f - E)), max_abs_U_gap = max(abs(U_f - U)))
cat("Flow/stock reconciliation (thousands, ~1 expected from rounding):\n")
print(recon)

# ==============================================================================
# THE DECOMPOSITION
# ==============================================================================
#
# The unemployment rate is u = U / (U + E), so it moves with the numerator AND
# the denominator. Exactly:
#
#     u_t - u_{t-1} = [ (1 - u_{t-1}) * dU  -  u_{t-1} * dE ] / LF_t
#
# Substituting the gross flows for dU and dE (dU = EU + NU - UE - UN + r_U,
# dE = UE + NE - EU - EN + r_E) and collecting terms gives an EXACT additive
# decomposition of the monthly change in the unemployment rate:
#
#     du * LF_t =  EU  -  UE                (employment <-> unemployment)
#               + (1-u) * (NU - UN)         (non-participation <-> unemployment)
#               +  u    * (EN - NE)         (employment <-> non-participation)
#               + residual
#
# The u-weight on the third channel is ~0.04, which is the point: the
# unemployment rate is almost blind to people moving between employment and
# non-participation. Employment can fall through that channel with essentially
# no effect on the rate. r_U and r_E are the CPS margin adjustments (population
# growth and unmatched records); they are reported explicitly rather than
# quietly dropped.

dat <- flows_raw %>%
  left_join(stocks_raw, by = "date") %>%
  arrange(date) %>%
  mutate(
    # never difference across the missing-survey gap
    contiguous = !is.na(lag(date)) &
      round(as.numeric(date - lag(date)) / 30.4) == 1,
    # BLS rounds E, U and LF to thousands independently, so published LF can
    # differ from E + U by 1k. Rebuild it so the identity above holds exactly.
    LF       = E + U,
    POP      = E + U + N,
    u        = U / LF,
    lfpr     = LF / POP,
    u_lag    = lag(U) / lag(LF),
    dU       = U - lag(U),
    dE       = E - lag(E),
    dN       = N - lag(N),
    dLF      = LF - lag(LF),
    r_U      = dU  - (EU + NU - UE - UN),
    r_E      = dE  - (UE + NE - EU - EN),
    r_N      = dN  - (EN + UN - NE - NU),
    r_LF     = dLF - (NE + NU - EN - UN),
    # six-way (gross) version
    `Employed to Unemployed`     =  EU             / LF,
    `Unemployed to Employed`     = -UE             / LF,
    `Not in LF to Unemployed`    =  (1 - u_lag)*NU / LF,
    `Unemployed to Not in LF`    = -(1 - u_lag)*UN / LF,
    `Employed to Not in LF`      =  u_lag*EN       / LF,
    `Not in LF to Employed`      = -u_lag*NE       / LF,
    `Population / sample margin` = ((1 - u_lag)*r_U - u_lag*r_E) / LF,
    du_act   = u - u_lag
  ) %>%
  mutate(across(c(all_of(FLOW_LEVELS), du_act,
                  dU, dE, dN, dLF, r_U, r_E, r_N, r_LF),
                ~ if_else(contiguous, .x, NA_real_)))

# hard check: the decomposition must reproduce the actual change exactly
chk <- dat %>%
  filter(!is.na(du_act)) %>%
  mutate(du_hat = rowSums(across(all_of(FLOW_LEVELS)))) %>%
  summarize(max_err_pp = max(abs(du_hat - du_act)) * 100)
cat("Max decomposition error (pp, should be ~0):", signif(chk$max_err_pp, 3), "\n")
stopifnot(chk$max_err_pp < 1e-9)

# ---- transition (hazard) rates ----------------------------------------------
# Levels of flows scale with the size of the origin stock, which makes them a
# poor read on behavior: the unemployed-to-employed flow can fall simply because
# there are fewer unemployed people.

rates <- dat %>%
  transmute(
    date, contiguous,
    f_UE = UE / lag(U), f_UN = UN / lag(U),
    s_EU = EU / lag(E), s_EN = EN / lag(E),
    e_NU = NU / lag(N), e_NE = NE / lag(N)
  ) %>%
  mutate(across(-c(date, contiguous), ~ if_else(contiguous, .x, NA_real_)))

latest     <- max(dat$date[!is.na(dat$du_act)])
latest_lab <- format(latest, "%B %Y")
L          <- dat %>% filter(date == latest)

cat("\n---", latest_lab, "---\n")
cat(sprintf("u: %.2f%% -> %.2f%% (%+.2f pp)\n", 100*L$u_lag, 100*L$u, 100*L$du_act))
cat(sprintf("Employment: %+.0fk   Unemployment: %+.0fk   Not in LF: %+.0fk   Labor force: %+.0fk\n",
            L$dE, L$dU, L$dN, L$dLF))
