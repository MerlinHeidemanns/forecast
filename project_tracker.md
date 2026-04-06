# Project Tracker

## Summary

| Metric | Count | % of Total |
|---|---|---|
| Total tasks | 18 | |
| Done | 13 | 72% |
| Partial | 1 | 6% |
| Not started | 4 | 22% |

---

## Phase 1 — Data Foundation

### Step 1: Historical poll data
**Status:** Done
**Owner:** —

Download Sonntagsfrage tables from Wahlrecht.de. Parse into raw dataframe. Save to disk.

**What exists today:** Scraper in `scraping/scrapers.py` fetches all institutes. CSVs for 8 pollsters already in `web/public/polling_data/`.

**Action needed:** None — complete.

---

### Step 2: Clean into canonical schema
**Status:** Done
**Owner:** Claude / Merlin

Standardize columns, parse dates, normalize pollster names, generate dedup hashes. Output as `polls.csv`.

**What exists today:** `estimation/canonicalize.R` implements the full 7-step pipeline: load all pollster CSVs, harmonize party names, pivot to wide format, compute SHA-256 hashes, deduplicate, validate (vote-share-sum, sample-size range, date validity, null checks), and write `data/polls.csv` + `data/rejected_polls.csv`. 5,605 valid polls from 8 pollsters. Historical polls with missing sample sizes or party columns (e.g. BSW, AfD pre-existence) are retained with NAs.

**Action needed:** None — complete.

---

### Step 3: Election results & pollster registry
**Status:** Done
**Owner:** Claude / Merlin

Compile `results.csv` (Zweitstimme 1990–2025) and `pollsters.csv`. Cross-check coverage.

**What exists today:** `estimation/build_references.R` implements three functions: `build_results()` produces `data/results.csv` with 10 elections (1990–2025) in canonical schema matching `polls.csv`, including 2025 Bundestag results. `build_pollsters()` produces `data/pollsters.csv` with metadata and coverage stats for all 8 pollsters (7 active, 1 inactive). `verify_coverage()` cross-checks pollster registry completeness, party column alignment, and election-to-poll coverage. `main.R` auto-generates both files if missing.

**Action needed:** None — complete.

---

## Phase 2 — Model (Prototype)

### Step 4: Baseline Stan model
**Status:** Done
**Owner:** Claude / Merlin

Minimal Stan model: latent support trajectory, noisy poll observations, election anchors. Compile with cmdstanr.

**What exists today:** `estimation/stan/spline.stan` — B-spline model on log-odds with: cubic B-splines (knots at election dates, ~10 basis functions), Dirichlet-multinomial likelihood with overdispersion parameter `phi`, non-centered RW1 smoothing prior on spline coefficients with per-party precision `tau`, soft Dirichlet election anchoring, softmax only computed at observed time indices during sampling (full trajectory in generated quantities). Profiled with Stan `profile()` blocks. Previous models (`simple_rw.stan`, `benchmark_forecast.stan`, `gp_model.stan`) retained for reference.

**Action needed:** Model tuning — election coverage at 57% (target ~90%). Consider: softening election anchor, per-party `phi`, additional knots between elections.

---

### Step 5: Full fitting script
**Status:** Done
**Owner:** Claude / Merlin

Write `fit.R`: reads CSVs, constructs Stan data list, runs sampler, saves fit. Handle date-to-index mapping and compositional constraint.

**What exists today:** `estimation/main.R` — complete pipeline: loads canonical data via `canonicalize.R` and `build_references.R`, auto-generates CSVs if missing. Biweekly time grid (14-day periods). Requires major parties (CDU/CSU, SPD, GRÜNE, FDP), fills missing minor parties with 0. Pre-aggregates polls by time period. Constructs B-spline basis with knots at election dates. Builds observed-time lookup tables for optimized softmax. Compiles and fits `spline.stan` (4 chains, 500+500). Runs profiling diagnostics. Extracts posterior trends and plots with `theme.R`. Saves fit as RDS. Shared infrastructure: `estimation/theme.R` (party colors, labels, `theme_forecast()`, scale helpers).

**Action needed:** None — complete.

---

### Step 6: Retrodictive check
**Status:** Done
**Owner:** Claude / Merlin

Assess model fit against training data. Save persistent benchmarks for model comparison over time.

**What exists today:** `estimation/retrodictive_checks.R` — standalone script that loads `data/spline_fit.rds` and runs 3 checks: (1) Election result recovery: posterior vs actual Zweitstimme per election per party, with 90%/50% coverage and MAE. (2) Poll residuals over time: observed minus fitted per party with LOESS smoother for systematic misfit. (3) Posterior predictive check: simulated vs observed poll shares via Dirichlet-multinomial, with 90% calibration coverage per party. Produces 3 plots in `estimation/plt/spline/`. Saves timestamped benchmark JSON to `data/benchmarks/spline_YYYYMMDD_HHMMSS.json` containing: full Stan code, data/sampler config, R/cmdstan versions, sampler diagnostics, parameter summaries, and all check results by party and election.

**Latest results:** 90% election coverage: 57%, MAE: 0.015. PPC 90% coverage: 80–99% by party (major parties slightly underdispersed, small parties overdispersed). Residuals centered at zero, no systematic bias.

**Action needed:** Re-run after zero-fill fix to get pre-2013 polls. Improve election coverage through model tuning.

---

### Step 7: Posterior export to JSON
**Status:** Done
**Owner:** Claude / Merlin

Write `export.R`: outputs `current_forecast.json`, `forecast_timeseries.json`, and `polls.json` for the frontend.

**What exists today:** `estimation/export.R` implements the full export pipeline: loads `spline_fit.rds`, broadcasts biweekly posteriors to daily resolution, writes three JSON files to `web/public/data/`. Handles backward-compatible re-extraction if fit was generated with 5 quantiles. `main.R` updated to compute 7 quantiles (5/10/25/50/75/90/95) and call export automatically. `test_export.R` validates all output schemas. Design doc in `docs/design/step7_json_export.md`.

**Action needed:** None — complete.

---

## Phase 3 — Frontend

### Step 8: Static site skeleton
**Status:** Done
**Owner:** —

`index.html` with CSS, party color palette, responsive layout, navigation. Deploy to hosting.

**What exists today:** `web/public/` has `index.html`, `about.html`, `forecasts.html`, `docs/`, CSS with party colors, responsive nav, fixed header. Vercel config present.

**Action needed:** None — structure is in place.

---

### Step 9: Time series chart
**Status:** Done
**Owner:** Claude / Merlin

D3 chart: one line per party with shaded 90% credible interval bands. Poll dots as scatter overlay.

**What exists today:** `visualization.js` fully rewritten. Loads `forecast_timeseries.json` and `polls.json`. All 7 parties rendered with nested fan chart bands (50%/80%/90% CI), median trend lines, raw poll dot overlay, election date markers. Brush zoom via context chart. Hover tooltip shows all parties sorted by median with 90% interval. Clip path prevents overflow. `forecasts.html` updated with chart CSS.

**Action needed:** None — complete.

---

### Step 10: Current estimate display
**Status:** Done
**Owner:** Claude / Merlin

Summary panel: each party median and 90% interval. Last updated timestamp and poll count.

**What exists today:** `index.html` has a "Germany" forecast panel that loads `current_forecast.json` and renders horizontal bar chart with party colors, median percentage, 90% CI range, and a lighter CI band behind each bar. Sorted by median descending. Footer shows total poll count, last pollster, and links to full forecast page. Responsive (CI column hides on mobile). Falls back gracefully if data unavailable.

**Action needed:** None — complete.

---

### Step 11: Methodology page
**Status:** Not started
**Owner:** Merlin / Claude

~800 words explaining model, uncertainty intervals, data sources, planned improvements. Link to repo.

**What exists today:** `web/docs/` has an R Markdown skeleton and bibliography but no actual methodology content.

**Action needed:** Write methodology page content (~800 words, non-statistician readable).

---

## Phase 4 — Automation

### Step 12: Wahlrecht.de scraper
**Status:** Done
**Owner:** Claude / Merlin

Python script: fetch page, parse table, compare hashes, append new rows. Handle errors gracefully.

**What exists today:** `scraping/scrapers.py` rewritten with two modes: `scrape_incremental` (default, current pages only) and `scrape_full` (all pages + historical, behind `--full` flag). Deterministic SHA-256 content hashing in `utils.py` replaces random `uuid4` — hash computed on wide-format rows before melt. Existing CSVs with old uuid4 IDs are auto-migrated on first incremental run. HTTP requests use `fetch_with_retry` with exponential backoff (3 attempts). Per-pollster error isolation. Structured logging via Python `logging`. Both functions return a summary dict consumable by the future pipeline CLI. `main.py` updated with `argparse` (`--full`, `--data-dir`, `--verbose`). Backward-compatible `scrape_all_polling_houses` alias retained. Design doc in `docs/design/step12_incremental_scraper.md`.

**Action needed:** None — complete.

---

### Step 13: Validation logic
**Status:** Partial
**Owner:** Claude / Merlin

`validate.py` with structural checks, dedup, anomaly flagging. Unit tests.

**What exists today:** Validation is implemented in R within `estimation/canonicalize.R` (vote-share-sum check, sample-size range, date validity, null checks, SHA-256 dedup). No Python `validate.py`. No unit tests. No 3-sigma anomaly flagging.

**Action needed:** Decide whether to keep validation in R (canonicalize.R) or port to Python. Add 3-sigma anomaly flagging. Write unit tests.

---

### Step 14: Pipeline CLI
**Status:** Done
**Owner:** Claude / Merlin

`pipeline.py` with subcommands: scrape, canonicalize, fit, export, run. Early exit if no new polls.

**What exists today:** `pipeline.py` — full orchestrator with 5 subcommands (`run`, `scrape`, `canonicalize`, `fit`, `export`). `run` executes the full pipeline with two early-exit gates: (1) scraper reports 0 new rows → skip, (2) `polls.csv` hash unchanged after canonicalize → skip. `--force` bypasses both. R stages invoked via `subprocess.run(["Rscript", ...])`. Each stage returns structured `StageResult`. `pipeline_result.json` written on completion with per-stage success/skipped flags and `data_changed` boolean for CI consumption. `main.py` retained as scraper-only entry point. Design doc in `docs/design/step14_pipeline_cli.md`.

**Action needed:** None — complete.

---

### Step 15: GitHub Actions workflow
**Status:** Done
**Owner:** Claude / Merlin

Cron workflow every 6 hours. Scrape + canonicalize only (no Stan fitting in CI). Commit and push if data changed.

**What exists today:** `.github/workflows/update.yml` — two-tier design. Tier 1 (CI, automated): runs `pipeline.py update-data` every 6h to scrape Wahlrecht.de and re-canonicalize. Commits `data/polls.csv`, `data/rejected_polls.csv`, and `web/public/polling_data/` if changed. Python 3.12 + R 4.4 with cached R packages. Tier 2 (local, manual): `pipeline.py run` for model fitting + JSON export. `pipeline.py` updated with `update-data` subcommand (scrape + canonicalize, no Stan). Concurrency guard prevents overlapping runs. `pipeline_result.json` uploaded as artifact on failure. Design doc in `docs/design/step15_github_actions.md`.

**Action needed:** None — complete. Push to main and verify first Action run succeeds.

---

### Step 16: Anomaly notifications
**Status:** Not started
**Owner:** Merlin

Telegram bot or email notification on flagged polls. Manual approval via JSON allowlist.

**What exists today:** Nothing exists for this.

**Action needed:** Choose notification channel (Telegram/email). Implement flagging + approval flow.

---

## Phase 5 — Launch

### Step 17: Domain and polish
**Status:** Not started
**Owner:** Merlin

Register domain, configure DNS, add Open Graph tags, test mobile, proof methodology page.

**What exists today:** Vercel config exists (`web/vercel.json`) but no custom domain or OG tags.

**Action needed:** Register domain. Add OG meta tags. Verify mobile layout. Confirm pipeline stability.

---

### Step 18: Soft launch
**Status:** Not started
**Owner:** Merlin

Write ~500 word launch post. Share to polisci and forecasting communities.

**What exists today:** Nothing exists for this.

**Action needed:** Write launch post. Identify target communities (polisci Twitter/Bluesky, forecasting forums).
