# EXERCISE: January 2026 Employment Situation — Benchmark Revision Graphics

## Context

The January 2026 Employment Situation (released February 11, 2026) includes the final annual benchmark revision to establishment survey data. The preliminary estimate (September 9, 2025) showed -911,000 jobs (-0.6%) for the 12 months ending March 2025. Tomorrow's release incorporates the final revision, re-estimates seasonal factors back to January 2021, and changes the birth-death model methodology. Population controls for the household survey are delayed to the March release.

You are building three publication-ready graphics for economic policy analysis.

## Setup

- Look at existing R scripts in this project folder for patterns on:
  - How `blsR::get_n_series_table()` is called (API key handling, series ID formatting)
  - ggplot2 theme and styling conventions (fonts, colors, sizing)
  - `ggsave()` output conventions (dimensions, dpi, output path to `graphics/` folder)
- Adopt those conventions. If no prior scripts exist, use a clean minimal theme with these defaults:
  - `theme_minimal(base_size = 14)`
  - Title in bold, subtitle in gray, caption with source attribution
  - `ggsave()` to `graphics/` at 10x6 inches, 300 dpi, PNG
  - Colors: use a two-tone palette (e.g., steel blue for revised, light gray for old) for comparisons

## Data Pull

Use `blsR` to pull the following CES series. You need TWO vintages of the same series:

1. **Pre-revision data**: This must be saved/cached BEFORE the new release drops, or reconstructed from the BLS archives. Since the release happens tomorrow (Feb 11, 2026), the current API pull as of today (Feb 10) should still return the OLD (pre-benchmark) estimates. **Pull and cache these today.**
2. **Post-revision data**: After the release, pull the same series again to get the revised estimates. The script should be structured to handle both steps.

### Series IDs needed

**Total nonfarm and major supersectors (SA, monthly, in thousands):**

| Series ID       | Description                        |
|-----------------|------------------------------------|
| CES0000000001   | Total nonfarm                      |
| CES0500000001   | Total private                      |
| CES1000000001   | Mining and logging                 |
| CES2000000001   | Construction                       |
| CES3000000001   | Manufacturing                      |
| CES4000000001   | Trade, transportation, utilities   |
| CES4142000001   | Retail trade                       |
| CES5000000001   | Information                        |
| CES5500000001   | Financial activities               |
| CES6000000001   | Professional and business services |
| CES6500000001   | Education and health services      |
| CES7000000001   | Leisure and hospitality            |
| CES8000000001   | Other services                     |
| CES9000000001   | Government                         |

Pull all of these for at least 2023M01 through the latest available month. This gives enough history for seasonal context and the revision window.

### Script structure

Create a single R script: `benchmark_revision_analysis.R`

The script should:
1. Pull and save pre-revision data to `data/pre_revision_ces.rds` (run this part TODAY, Feb 10)
2. Pull and save post-revision data to `data/post_revision_ces.rds` (run this part AFTER release, Feb 11)
3. Load both cached files to produce the three graphics below

Include a flag or step indicator at the top of the script so the user can run Step 1 today and Steps 2-3 tomorrow. Something like:

```r
# Set to 1 today (Feb 10), set to 2 after release (Feb 11), set to 3 to build graphics
STEP <- 1
```

## Graphic 1: "The Revision Story" — Monthly NFP Change, Old vs. Revised

**File:** `graphics/01_revision_comparison.png`

**Design:** Grouped (dodged) bar chart of month-over-month change in total nonfarm payrolls (SA, in thousands).

- X-axis: months, April 2024 through January 2026 (or latest available)
- Y-axis: monthly change in thousands
- Two bars per month: OLD estimate (light gray, slightly transparent) and REVISED estimate (steel blue or similar bold color)
- Horizontal line at zero
- Annotate the cumulative revision (sum of old monthly changes minus sum of revised monthly changes over the benchmark window Apr 2024–Mar 2025) in the top-left or top-right corner as a text callout, e.g., "Cumulative revision: -XXXk"
- Title: "Monthly Job Gains: Before and After Benchmark Revision"
- Subtitle: "Seasonally adjusted monthly change in total nonfarm payrolls, thousands"
- Caption: "Source: Bureau of Labor Statistics, Current Employment Statistics. Pre-revision data cached Feb 10, 2026."

**Computation notes:**
- Monthly change = current month level minus prior month level
- Compute this separately for the old and revised series
- For months after March 2025 (outside the benchmark window), the revised series will still differ due to seasonal re-estimation and the new birth-death methodology

## Graphic 2: "Where Were the Phantom Jobs?" — Industry Revision Decomposition

**File:** `graphics/02_industry_revisions.png`

**Design:** Horizontal bar chart showing the revision to the LEVEL of employment at March 2025 by supersector.

- Y-axis: industry supersectors, sorted by magnitude of revision (largest negative at top)
- X-axis: revision in thousands (revised level minus old level at March 2025)
- Bars colored by sign: negative revisions in red/coral, positive in teal/green
- Label each bar with the revision amount in thousands
- Vertical line at zero
- Title: "Benchmark Revisions by Industry, March 2025"
- Subtitle: "Difference between revised and pre-revision employment levels, thousands"
- Caption: "Source: Bureau of Labor Statistics, CES benchmark revision."

**Computation notes:**
- For each supersector, take the March 2025 level from the revised data minus the March 2025 level from the old data
- Use the supersectors listed in the series table above (not retail separately — it's a subset of TTU)
- If you want, include both Total Private and the supersector detail, but don't double-count. Safest: show the ~10 supersectors and annotate total nonfarm revision as a reference line or text note.

## Graphic 3: "January in Context" — Monthly NFP Change on Revised Series

**File:** `graphics/03_january_in_context.png`

**Design:** Single-color bar chart of monthly NFP change using the REVISED series only.

- X-axis: months, January 2025 through January 2026 (13 months)
- Y-axis: monthly change in thousands
- All bars in a muted blue/gray EXCEPT January 2026, which is highlighted in a bold accent color (e.g., dark blue or orange)
- Horizontal dashed line at the trailing 6-month average (July 2025–December 2025) to show the recent trend
- Annotate the trailing average value near the line
- Title: "Monthly Job Gains on the Revised Series"
- Subtitle: "Seasonally adjusted monthly change in total nonfarm payrolls, thousands"
- Caption: "Source: Bureau of Labor Statistics, CES (post-benchmark revision)."

**Computation notes:**
- Use the REVISED series only
- Monthly change = current month level minus prior month level
- The trailing average line provides context for whether January is an acceleration, deceleration, or continuation

## Output Checklist

When complete, the `graphics/` folder should contain:
- `01_revision_comparison.png`
- `02_industry_revisions.png`
- `03_january_in_context.png`

And the `data/` folder should contain:
- `pre_revision_ces.rds`
- `post_revision_ces.rds`

## Notes

- If the BLS API is rate-limited or down on release day, the script should fail gracefully with an informative message.
- All series are seasonally adjusted. Do NOT use NSA series.
- The benchmark revision affects NSA data from April 2024 forward and SA data from January 2021 forward, but the graphics focus on the April 2024+ window where the substantive revisions occur.
- The script should create `data/` and `graphics/` directories if they don't exist.
