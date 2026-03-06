# Next Month Notes

## What Changed This Month

This repo was reorganized so the monthly run is more explicit and less dependent on fragile ordering.

- `00_run_monthly.R` is now the main entry point.
- It refreshes revisions first with `99_download_jobs_revisions.py`.
- It preloads the shared headline objects needed by the front-end scripts:
  - `revisions_df`
  - `unrate`
- It now runs:
  - `01_initial_tweet.R`
  - `02_unrate_jobs.R`
  - `03_lfp_epop.R`
  - then the rest of the charts
  - `98_revisions_estimate.R` at the end

Older file structure changes:

- removed `03_initial_tweet.R`
- moved `02a_initial_tweet.R` to `01_initial_tweet.R`
- moved `01_revisions_estimate.R` to `98_revisions_estimate.R`
- moved `00_download_jobs_revisions.py` to `99_download_jobs_revisions.py`

## Front Page / Headline Changes

`01_initial_tweet.R`

- removed the consensus jobs and consensus unemployment analysis
- simplified the prepared tweet so it just focuses on payroll growth and revisions
- this script now assumes the runner already loaded the needed data

`02_unrate_jobs.R`

- now reuses the preloaded `unrate` object when available
- removed `02c_federal_vs_rest.png`
- removed `02e_gender.png` generation from here
- removed `02g_jobs_gained_fixed.png`

## Health Care / Gender Graphic

`04_health_care.R` now owns the health care and gender composition graphics.

- moved the CES pull for the gender chart into this file
- rewrote the right-hand panel to be private-sector men vs women
- added a combined graphic:
  - `graphics/04_health_care_gender_combined.png`
- both panels now use the same private-sector framing since Liberation Day
- both start in January 2025
- the combined title block was made larger for social media
- the subgraphics no longer have subtitles/captions
- dotted red Liberation Day lines remain

Important series used for the private-sector gender chart:

- `CES0500000001`: total private employment
- `CES0500000010`: private women employment
- private men is calculated as the difference

## 2025 Lock / API Cleanup

Hard-coded `end_year = 2025` calls were replaced with dynamic current-year pulls in:

- `05_goods_loglines.R`
- `07_young_unrate.R`
- `08_immigration_unrate.R`
- `09_unrate_by_type_ahe.R`
- `10_flows_4_types.R`
- `11_unemployment_durations.R`
- `12_where_unrate_increased.R`

Also:

- `09_unrate_by_type_ahe.R` no longer exposes a BLS API key
- it now uses `Sys.getenv("BLS_KEY")`

## 08 / 09 Graphics

`08_immigration_unrate.R`

- brought 2023 back into `08_native_by_month_manual.png`
- fixed the year-selection logic so it does not truncate the display

`09_unrate_by_type_ahe.R`

- `09a_u_by_type.png` now:
  - drops the legend
  - shows the dotted 2024-average endpoint value
  - colors that endpoint label to match the facet line
  - aligns the dotted-line and actual endpoint labels at the same earlier label date
  - uses a larger title for Twitter

## Goods / Blue-Collar Work

`05_goods_loglines.R` was rewritten around the blue-collar/log-linear idea.

- removed the old `05_goods_loglinear_projection.png` output
- built a new faceted chart:
  - `graphics/05_blue_collar_components_loglinear.png`
- sectors included:
  - Mining and Logging `CES1000000001`
  - Construction `CES2000000001`
  - Manufacturing `CES3000000001`
  - Transportation and Warehousing `CES4300000001`
  - Utilities `CES4422000001`
- each facet now shows:
  - latest actual value
  - latest projected value
- the title includes the total shortfall versus trend, rounded to the nearest thousand jobs
- the title and sizing were increased for social media
- subtitle now says `Current Employment Statistics` explicitly and refers to employment levels

Blue-collar aggregate definition used this month:

- `CES0600000001`
- `CES4422000001`
- `CES4300000001`

## Flows / Bathtub

`10_flows_4_types.R`

- left the original `graphics/10_flows_unrate.png` in place
- added a second cleaner version:
  - `graphics/10_flows_unrate_clean.png`
- the cleaner version uses:
  - four small multiples
  - direct end labels
  - free `y` scales
  - no in-panel legend clutter
  - no unemployment-change bars

This is the current starting point for the bathtub-style cleanup, but it still needs another pass.

## EPOP / LFPR

Added a new script:

- `03_lfp_epop.R`

It creates:

- `graphics/03a_lfp_epop_levels.png`
- `graphics/03b_lfp_epop_change.png`

Current design:

- a levels chart for overall and prime-age `LFPR` and `EPOP`
- dotted 2019 baselines
- a 3-month change chart for:
  - `LFPR`
  - `EPOP`
  - `LFPR - EPOP`

Why this framing:

- prime-age `EPOP` is one of the cleaner slack measures macro labor people watch
- `LFPR - EPOP` is the nonemployment share of the population, so it helps distinguish helpful participation gains from outright weakening

## Jobs Table / CES Summary

`make_jobs_chart(ces_data)` in `scripts/graphic_scripts.R`

- fixed the stale grouping/filter logic to use `industry_display_level`
- added wage columns using `data_type_code == 3`
- current table now includes:
  - last month jobs change
  - prior 3-month average jobs change
  - 2024 annual average jobs change
  - 3-month annualized wage growth
  - year-over-year wage growth
- grouped the two wage columns under a `Wage Growth` spanner

This still relies on the slower CES flat-file load:

- `ces_data <- getBLSFiles("ces", ...)`

That should be replaced next month with targeted API pulls if possible.

## Cyclical Industries

`13_cyclical_industries.R`

- fixed the stale `display_level` reference at the end
- changed that grouped summary to use `industry_display_level`
- added an export:
  - `data/13_cyclical_industry_coefs_latest_jobs.csv`

That CSV includes the cyclical sensitivity data plus the latest 3-month average job gain for each industry.

## Shared Styling / Technical Fixes

- added an in-repo `theme_esp()` definition to `scripts/graphic_scripts.R`
- this fixed runtime issues where the theme was assumed to exist but did not

## Notes On How This Was Done

The key pattern this month was:

1. move data loading up into the runner when multiple scripts need the same object
2. remove hard-coded year logic
3. remove orphaned or duplicated graphics
4. move related graphics into the same script when they use the same pulls
5. favor explicit chart-specific code over clever abstraction for jobs day

For the chart work, the cleanup usually meant:

- removing legends where direct labels worked better
- making titles bigger for Twitter
- aligning endpoint labels so the final value is easy to read
- simplifying multi-message charts into either small multiples or a combined graphic with one clear framing

## Next Month Priorities

1. Keep working on the `EPOP` / `LFPR` charts.
   - current `03_lfp_epop.R` is only a first pass
   - likely next step is a better "helpful vs worrisome" visual, possibly a quadrant or a more compact decomposition

2. Move `make_jobs_chart()` away from CES flat-file access and onto targeted API calls.
   - goal is speed on jobs day
   - keep the current table structure, just replace the data source

3. Keep working on the bathtub graphic in `10_flows_4_types.R`.
   - the new clean version is a useful second try
   - but the conceptual presentation is still not there yet

4. Clean up the jobs-number displays in `01_initial_tweet.R`.
   - the current numbers/wording are still rough
   - those edits should preserve your voice

5. Consider making `10_flows_4_types.R` run standalone.
   - right now it still depends on `unrate` already existing in memory

6. Clean up remaining warnings.
   - `ggplot2` `size` vs `linewidth`
   - `tidyusmacro::logLinearProjection()` deprecation warning
   - repeated numeric coercion warnings in BLS pulls

7. Audit chart text for stale fixed-year references.
   - the API calls were fixed
   - some chart titles/subtitles may still literally mention `2025`

8. Consider consolidating repeated helper logic.
   - there are now several scripts with local `pull_month_value` / label-date patterns
   - probably not worth abstracting too aggressively, but worth checking for duplication
