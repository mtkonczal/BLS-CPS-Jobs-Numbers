library(tidyverse)
library(lubridate)
library(scales)

# --- (1) Lookup table: Ranks for Age/Edu, NA for others -------------------------
unrate_lookup <- tribble(
  ~series_id    , ~meta             , ~name                     , ~rank ,
  "LNS14000000" , "Overall"         , "Total"                   , NA    ,
  "LNS14000001" , "Gender"          , "Men"                     , NA    ,
  "LNS14000002" , "Gender"          , "Women"                   , NA    ,
  "LNS14000003" , "Race/Ethnicity"  , "White"                   , NA    ,
  "LNS14000006" , "Race/Ethnicity"  , "Black"                   , NA    ,
  "LNS14000009" , "Race/Ethnicity"  , "Hispanic or Latino"      , NA    ,
  "LNS14032183" , "Race/Ethnicity"  , "Asian"                   , NA    ,
  "LNS14000012" , "Age"             , "16–19"                 ,     1 ,
  "LNS14000036" , "Age"             , "20–24"                 ,     2 ,
  "LNS14000089" , "Age"             , "25–34"                 ,     3 ,
  "LNS14000091" , "Age"             , "35–44"                 ,     4 ,
  "LNS14000093" , "Age"             , "45–54"                 ,     5 ,
  "LNS14024230" , "Age"             , "55+"                     ,     6 ,
  "LNS14027659" , "Education (25+)" , "< High school"           ,     1 ,
  "LNS14027660" , "Education (25+)" , "High school, no college" ,     2 ,
  "LNS14027689" , "Education (25+)" , "Some college / AA"       ,     3 ,
  "LNS14027662" , "Education (25+)" , "BA+"                     ,     4
)

# --- (2) Helpers ----------------------------------------------------------------
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}
scale_x_reordered <- function(..., sep = "___") {
  scale_x_discrete(labels = function(x) sub(paste0(sep, ".*$"), "", x), ...)
}

# --- (3) Your pull (Assumed loaded) ---------------------------------------------
unrate_across <- get_n_series_table(
  unrate_lookup$series_id,
  api_key = bls_get_key(),
  start_year = 2024,
  end_year = 2025,
  tidy = TRUE
) %>%
  mutate(date = as.Date(paste0(year, "/", month, "/", 1))) %>%
  select(-year, -month)

unrate_across <- unrate_across %>%
  mutate(
    across(
      -c(date),
      as.numeric
    )
  )

# --- (4) Compute latest vs Dec 2024, MIXED SORTING ------------------------------
dec_2024 <- as.Date("2024-12-01")

unrate_change <- unrate_across %>%
  pivot_longer(
    cols = -c(date),
    names_to = "series_id",
    values_to = "value"
  ) %>%
  left_join(unrate_lookup, by = "series_id") %>%
  filter(!is.na(meta)) %>%
  group_by(series_id, meta, name, rank) %>%
  summarize(
    latest_date = max(date, na.rm = TRUE),
    latest = value[date == latest_date][1],
    dec2024 = value[date == dec_2024][1],
    change_pp = latest - dec2024,
    .groups = "drop"
  ) %>%
  mutate(
    # Conditional sorting: Age/Edu by rank, others by value
    sorting_var = case_when(
      meta %in% c("Age", "Education (25+)") ~ -rank,
      TRUE ~ change_pp
    ),
    name_reord = reorder_within(name, sorting_var, meta)
  )

# --- (5) Get Month Name for Subtitle --------------------------------------------
# Extracts the latest date from the summary and formats it (e.g., "February")
latest_month_name <- format(max(unrate_change$latest_date), "%B, %Y")

# --- (6) Plot -------------------------------------------------------------------
ggplot(unrate_change, aes(x = name_reord, y = change_pp)) +
  geom_hline(yintercept = 0) +

  # Color: Deep Steel Blue (#28587B) matches beige backgrounds well
  geom_col(fill = "#28587B") +

  geom_text(
    aes(
      label = sprintf("%.1f", change_pp),
      hjust = ifelse(change_pp >= 0, 1.2, -0.2)
    ),
    color = "white",
    size = 3,
    fontface = "bold"
  ) +

  coord_flip() +
  facet_wrap(~meta, scales = "free_y") +
  scale_x_reordered() +
  scale_y_continuous(labels = function(x) paste0(sprintf("%.1f", x), " pp")) +

  labs(
    title = "Unemployment Rate Change Since December 2024",
    # Dynamic subtitle inserting the month name
    subtitle = paste0(
      latest_month_name,
      " unemployment rate minus December, 2024 (percentage points). Seasonally Adjusted."
    ),
    caption = "Source: BLS, CPS. Mike Konczal, Economic Security Project.",
    x = NULL,
    y = "Change (pp)"
  ) +
  theme_esp()
# theme_minimal() # Removed minimal so your ESP theme takes precedence

ggsave(
  "graphics/12_where_unrate_increased.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)
