# Design Doc: Step 7 — Posterior Export to JSON

**Author:** Claude / Merlin
**Date:** 2026-04-06
**Status:** Accepted — decisions finalized 2026-04-06

---

## 1. Goal

Write `estimation/export.R` that reads `data/spline_fit.rds` and produces three JSON files consumable by the D3 frontend:

| File | Purpose | Consumer |
|---|---|---|
| `forecast_timeseries.json` | Full posterior trajectory (all time periods × parties × quantiles) | Time series chart (Step 9) |
| `current_forecast.json` | Latest-period snapshot per party | Summary panel (Step 10) |
| `polls.json` | Canonical poll data for scatter overlay | Time series chart (Step 9) |

Output location: `web/public/data/`.

---

## 2. Context

### What exists today

- `estimation/main.R` fits the Stan model and saves `data/spline_fit.rds` containing the cmdstan fit object, a pre-summarized `trend` tibble (quantiles of `pi[t,p]`), the date grid, party labels, and election metadata.
- `estimation/utils.R` has `save_daily_vote_shares()` which exports quantiles to CSV, but targets a different variable name (`trend_shares`) and writes CSV, not JSON.
- `estimation/retrodictive_checks.R` already extracts `pi[t,p]` draws from the fit and serializes benchmark data to JSON via `jsonlite::write_json`.
- The frontend (`visualization.js`) currently loads per-pollster CSVs via `d3.csv()` and only renders 3 parties with rolling averages. No JSON loading, no credible intervals.

### What the Stan model provides

The generated quantities block in `spline.stan` produces:

- **`pi[T, P]`** — posterior vote share (simplex) for each of T=208 biweekly periods and P=7 parties. This is the core quantity we export.
- **`alpha[T, P]`** — log-odds (intermediate, not needed for frontend).
- **`log_lik[N]`** — pointwise log-likelihood (not needed for frontend).

The fit object also carries sampler diagnostics (divergences, treedepth, ESS, Rhat).

---

## 3. Output schemas

### 3.1 `forecast_timeseries.json`

**Design choice: wide-by-party, one object per time period.**

This is the most D3-friendly layout — each datum maps directly to a chart row, and party fields are accessed as `d.CDU_CSU.median` without filtering.

```jsonc
{
  "metadata": {
    "model": "spline",
    "generated_at": "2026-04-06T14:30:00+02:00",
    "n_draws": 2000,
    "n_days": 10322,
    "date_range": ["1998-01-01", "2026-04-06"],
    "source_period_days": 14,
    "parties": ["CDU_CSU", "SPD", "GRUENE", "FDP", "AfD", "LINKE", "Sonstige"],
    "bsw_in_sonstige": true,
    "quantiles": [0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95],
    "diagnostics": {
      "n_divergent": 0,
      "max_treedepth_hits": 2,
      "min_ess_bulk": 812,
      "max_rhat": 1.003
    }
  },
  "parties": {
    "CDU_CSU": { "display": "CDU/CSU", "color": "#000000" },
    "SPD":     { "display": "SPD",     "color": "#E3000F" },
    "GRUENE":  { "display": "GRÜNE",   "color": "#1AA037" },
    "FDP":     { "display": "FDP",     "color": "#FFCC00" },
    "AfD":     { "display": "AfD",     "color": "#009EE0" },
    "LINKE":   { "display": "LINKE",   "color": "#BE3075" },
    "Sonstige":{ "display": "Sonstige","color": "#AAAAAA" }
  },
  "elections": [
    { "date": "2021-09-26", "results": { "CDU_CSU": 0.242, "SPD": 0.257, ... } },
    ...
  ],
  "timeseries": [
    {
      "date": "1998-01-01",
      "CDU_CSU": { "q05": 0.340, "q10": 0.345, "q25": 0.350, "median": 0.360, "q75": 0.370, "q90": 0.375, "q95": 0.380 },
      "SPD":     { "q05": 0.300, ... },
      ...
    },
    {
      "date": "1998-01-02",
      ...
    },
    ...
  ]
}
```

**Why this layout over alternatives:**

| Layout | Pros | Cons |
|---|---|---|
| **Wide-by-party (chosen)** | Natural D3 access pattern: `d.SPD.median`. One pass to draw chart. Compact. | Slightly harder to iterate "all parties for one field". |
| Long format (one row per date×party) | Easy to group/filter in JS. | 7× more objects. Frontend must nest/group before drawing lines. |
| Array-of-arrays (columnar) | Smallest on disk. | Requires index lookups, unreadable, error-prone. |

**Quantile set:** 7 quantiles (5/10/25/50/75/90/95) instead of the minimal 5 (5/25/50/75/95). The extra 10/90 quantiles cost ~15% more JSON size but enable a nested fan chart (dark inner band = 50% CI, medium = 80% CI, light outer = 90% CI) which is the standard in forecast visualization (cf. FiveThirtyEight, Economist).

**Vote shares are proportions (0–1), not percentages (0–100).** The frontend multiplies by 100 for display. This avoids ambiguity and matches the Stan model output.

**Precision: 3 decimal places (0.1 percentage point).** Values are rounded to the nearest 0.001 (e.g., 0.295 = 29.5%). This is more than sufficient for chart rendering and tooltip display. `jsonlite::toJSON(digits = 3)`.

### Daily broadcasting

The Stan model estimates `pi[t,p]` on a biweekly (14-day) grid, producing ~208 time periods. The exported JSON broadcasts these to **daily resolution** — every day within a 14-day period receives the same quantile values as its parent period.

**Why:** Daily dates give the frontend a uniform x-axis. D3's time scale maps dates naturally without needing to reason about period boundaries. Tooltips show a specific date rather than a period range. Brush/zoom works at day granularity.

**How it works in export.R:**

```r
# For each biweekly period [start_date, end_date):
#   expand to a row per day in [start_date, end_date)
#   all days inherit the same quantile values
daily <- period_data %>%
  rowwise() %>%
  mutate(date = list(seq(period_start, period_end - 1, by = "day"))) %>%
  unnest(date)
```

The last period extends to the current date (or the date of the last poll, whichever is later).

**Size impact:** ~208 periods × 14 days ≈ ~2,900 daily rows (1998–2026) instead of 208. At 7 parties × 7 quantiles × 3 digits per value, this is roughly ~800 KB uncompressed, ~60 KB gzipped. Still well within budget.

**Note:** This is a presentation-layer expansion, not a modeling claim. The model's temporal resolution is biweekly. The metadata field `source_period_days: 14` makes this explicit.

### 3.2 `current_forecast.json`

Snapshot of the last time period. Lightweight file for the summary panel.

```jsonc
{
  "date": "2026-04-06",
  "model": "spline",
  "generated_at": "2026-04-06T14:30:00+02:00",
  "parties": [
    {
      "id": "CDU_CSU",
      "display": "CDU/CSU",
      "color": "#000000",
      "median": 0.295,
      "q05": 0.270,
      "q95": 0.320,
      "q25": 0.285,
      "q75": 0.305
    },
    ...
  ],
  "meta": {
    "n_polls_total": 5605,
    "n_polls_last_30d": 24,
    "last_poll_date": "2026-04-04",
    "last_poll_pollster": "Infratest dimap"
  }
}
```

**Array of party objects** (not keyed object) because the summary panel renders parties in a fixed order (by median, descending). The frontend can sort the array directly.

### 3.3 `polls.json`

Canonical poll data for scatter overlay on the time series chart. Derived from `data/polls.csv`.

```jsonc
{
  "generated_at": "2026-04-06T14:30:00+02:00",
  "n_polls": 5605,
  "columns": ["date", "pollster", "sample_size", "CDU_CSU", "SPD", "GRUENE", "FDP", "AfD", "LINKE", "BSW", "Sonstige"],
  "polls": [
    {
      "date": "2026-04-03",
      "pollster": "Infratest dimap",
      "sample_size": 1500,
      "CDU_CSU": 0.295,
      "SPD": 0.155,
      "GRUENE": 0.135,
      "FDP": 0.045,
      "AfD": 0.215,
      "LINKE": 0.035,
      "BSW": 0.060,
      "Sonstige": 0.060
    },
    ...
  ]
}
```

**Vote shares as proportions** (same convention as timeseries). Values are divided by 100 during export if the source CSV stores percentages.

**BSW column:** Present for polls from Jan 2024 onward; `null` for earlier polls. The model trajectory in `forecast_timeseries.json` does not include BSW as a separate party (it's folded into Sonstige), but the frontend can show BSW poll dots as an unmodeled overlay.

**No dedup hashes or rejected polls** — this is the frontend view, not the data pipeline artifact. Only validated polls from `polls.csv` are included.

---

## 4. Implementation plan

### 4.1 File: `estimation/export.R`

Three exported functions + one orchestrator:

```
export_timeseries(fit_data, out_dir)
export_current(fit_data, polls_df, out_dir)
export_polls(polls_path, out_dir)
export_all(fit_path, polls_path, out_dir)   # orchestrator
```

**`export_timeseries`** — Reads the pre-summarized `trend` tibble (which now contains all 7 quantiles per D3). Steps:

1. Parse variable names `pi[t,p]` → (time_idx, party_idx), join with date grid and party labels.
2. **Daily broadcast:** For each biweekly period, expand to one row per day. All days in the period inherit the same quantile values.
3. Round all vote-share values to 3 decimal places.
4. Pivot to wide-by-party nested structure.
5. Attach metadata (including `bsw_in_sonstige: true` flag), party definitions, and election results.
6. Serialize with `jsonlite::toJSON(auto_unbox = TRUE, digits = 3)`.

**`export_current`** — Subset the last time period from the timeseries summary. Enrich with poll-count metadata by filtering `polls.csv` on the last 30 days.

**`export_polls`** — Read `data/polls.csv`, select relevant columns, convert percentages to proportions, rename party columns to JS-safe keys (e.g., `cdu_csu` → `CDU_CSU`). Include `BSW` as a separate column (NULL/absent for polls before Jan 2024). This enables the frontend to render BSW poll dots even though the model trajectory lumps BSW into Sonstige. Serialize with `digits = 3`.

### 4.2 Party key convention

The canonical R columns use lowercase (`cdu_csu`, `gruene`), but JSON keys should be JS-identifier-safe and consistent with the `parties` metadata block. Convention:

| R column | JSON key | Display |
|---|---|---|
| `cdu_csu` | `CDU_CSU` | CDU/CSU |
| `spd` | `SPD` | SPD |
| `gruene` | `GRUENE` | GRÜNE |
| `fdp` | `FDP` | FDP |
| `afd` | `AfD` | AfD |
| `linke` | `LINKE` | LINKE |
| `bsw` | `BSW` | BSW |
| `sonstige` | `Sonstige` | Sonstige |

### 4.3 Dependencies

- `jsonlite` (already used in retrodictive_checks.R)
- `dplyr`, `tidyr` (already in the project)
- No new packages required.

### 4.4 Integration with main.R

Add a call at the end of `main.R`:

```r
source("export.R")
export_all(
  fit_path  = "data/spline_fit.rds",
  polls_path = "data/polls.csv",
  out_dir   = "../web/public/data/"
)
```

Also callable standalone: `Rscript estimation/export.R` (with a `if (sys.nframe() == 0)` guard).

---

## 5. Size estimates

| File | Rows/objects | Est. size (uncompressed) | Gzipped |
|---|---|---|---|
| `forecast_timeseries.json` | ~2,900 days × 7 parties × 7 quantiles | ~800 KB | ~60 KB |
| `current_forecast.json` | 7 parties | ~1 KB | <1 KB |
| `polls.json` | ~5,600 polls × 9 fields | ~700 KB | ~80 KB |

Total: ~1.5 MB uncompressed, ~140 KB gzipped. Well within acceptable limits for a static site. Vercel serves gzipped by default.

Daily broadcasting inflates `forecast_timeseries.json` by ~14× over the biweekly version, but gzip compresses repeated values extremely well (consecutive days within a period are identical), so the gzipped cost is only ~4× larger.

---

## 6. Resolved decisions

### D1: BSW handling → Sonstige with breakout switch

BSW stays inside Sonstige in the model output (P=7). The `parties` metadata block includes a `bsw_in_sonstige: true` flag. `polls.json` exports BSW as a separate column when available in the source data, enabling the frontend to render raw BSW poll dots without a model trend line. When BSW is later added to the Stan model as an 8th party, flipping the flag to `false` and adding `BSW` to the parties block is all the frontend needs.

### D2: Poll time range → Export everything

All ~5,600 polls back to 1998. At ~80 KB gzipped this is negligible. The frontend filters by the chart's visible date range client-side.

### D3: Quantile extraction → Modify main.R to compute all 7 upfront

`main.R`'s trend computation will be updated to produce 7 quantiles (5/10/25/50/75/90/95) and store them in the `trend` tibble saved to `spline_fit.rds`. `export.R` then just reshapes and serializes — no re-extraction from the fit object needed.

### D4: Decimal precision → 3 digits (0.1 pp)

`jsonlite::toJSON(digits = 3)`. Values rounded to nearest 0.001 (0.1 percentage point). Sufficient for all display purposes. Standard rounding (round half to even).

### D5: Temporal resolution → Daily broadcasting

The biweekly model estimates are broadcast to daily resolution in the JSON (see §3.1 "Daily broadcasting" above). ~2,900 daily rows instead of 208 biweekly periods. Each day inherits the quantile values of its parent 14-day period. The metadata field `source_period_days: 14` documents the model's actual resolution.

---

## 7. Non-goals

- **Forecast forward projection.** Step 7 exports the model's smoothed trajectory up to the last observed poll. Forward extrapolation (e.g., election-day prediction) is a separate model feature.
- **Seat projection.** Converting vote shares to Bundestag seats (Sainte-Laguë) is out of scope.
- **API endpoint.** These are static JSON files served by Vercel, not a REST API.
- **Incremental export.** `export.R` regenerates all three files on every run. Incremental diffing adds complexity for minimal gain given the small file sizes.

---

## 8. Testing

- **Round-trip test:** Export → load in R via `jsonlite::fromJSON` → verify quantile values match `fit$summary` output within floating-point tolerance.
- **Schema validation:** Check that every time period has exactly 7 parties, every party has exactly 7 quantiles, and quantile ordering is monotonic (q05 ≤ q10 ≤ ... ≤ q95).
- **Simplex constraint:** For each time period, verify that party medians sum to approximately 1.0 (within ±0.01, since quantile medians don't perfectly sum to 1).
- **Date continuity:** Verify no gaps in the biweekly date grid.
- **Frontend smoke test:** `d3.json("data/forecast_timeseries.json")` parses without error and `data.timeseries.length === 208`.
