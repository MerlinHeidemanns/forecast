# German Election Poll Tracker

Bayesian forecast of German federal election polling. Scrapes Sonntagsfrage data from [Wahlrecht.de](https://www.wahlrecht.de/umfragen/), fits a B-spline latent-support model in Stan, and publishes daily-resolution forecasts with credible intervals to a static site.

## Quickstart

### Prerequisites

- **Python 3.10+** with `requests`, `beautifulsoup4`, `pandas`
- **R 4.3+** with `tidyverse`, `cmdstanr`, `lubridate`, `splines`, `jsonlite`
- [CmdStan](https://mc-stan.org/cmdstanr/) installed and configured

### Run the pipeline

```bash
# Full pipeline: scrape → canonicalize → fit → export
python pipeline.py run

# Re-fit even if no new polls (e.g. after model changes)
python pipeline.py run --force

# Data update only (what CI runs): scrape → canonicalize, no Stan
python pipeline.py update-data
```

The pipeline exits early if no new polls are detected, making it safe to run on a cron.

### Individual stages

```bash
python pipeline.py scrape              # incremental scrape (current pages only)
python pipeline.py scrape --full       # full backfill including historical archives
python pipeline.py canonicalize        # re-run R canonicalization only
python pipeline.py fit                 # canonicalize + fit + export
python pipeline.py export              # re-export JSON from existing model fit
```

### CI/CD

A GitHub Actions workflow (`.github/workflows/update.yml`) runs `pipeline.py update-data` every 6 hours to scrape and canonicalize new polls. Model fitting is done locally via `pipeline.py run`. Push to `main` triggers Vercel redeploy.

### Scraper standalone

```bash
python main.py                         # incremental scrape
python main.py --full                  # full historical scrape
python main.py --data-dir PATH         # custom output directory
```

### Serve the site locally

```bash
npx serve web/public
```

## Project Structure

```
pipeline.py                     # Pipeline orchestrator (scrape → fit → export)
main.py                         # Scraper-only entry point

scraping/
  scrapers.py                   # Wahlrecht.de scraper (incremental + full modes)
  utils.py                      # Hashing, retry, parsing helpers

estimation/
  main.R                        # Full fitting pipeline (canonicalize → fit → export)
  canonicalize.R                # Merge pollster CSVs → data/polls.csv
  build_references.R            # Election results + pollster registry
  export.R                      # Posterior → JSON for frontend
  retrodictive_checks.R         # Model validation + benchmarks
  theme.R                       # Shared party colors, labels, ggplot theme
  stan/
    spline.stan                 # B-spline latent-support model

web/public/
  index.html                    # Home page with forecast summary panel
  forecasts.html                # Full time series chart
  about.html                    # About page
  js/visualization.js           # D3 fan chart + brush zoom
  data/                         # JSON output consumed by frontend
    forecast_timeseries.json    # Daily posterior (7 quantiles × 7 parties)
    current_forecast.json       # Latest snapshot per party
    polls.json                  # Raw poll data for scatter overlay

data/
  polls.csv                     # Canonical validated polls (wide format)
  results.csv                   # Zweitstimme 1990–2025
  spline_fit.rds                # Saved model fit
  benchmarks/                   # Timestamped retrodictive check results

docs/design/                    # Design documents for major steps
```

## Model

Cubic B-spline on log-odds with knots at election dates. Dirichlet-multinomial likelihood with overdispersion. RW1 smoothing prior on spline coefficients with per-party precision. Soft Dirichlet election anchoring. Fit with CmdStan (4 chains, 500 warmup + 500 sampling). Posterior exported at 7 quantiles (5/10/25/50/75/90/95) and broadcast to daily resolution.

See `docs/design/` for detailed design documents on the JSON export schema and pipeline architecture.

## Pipeline Architecture

```
python pipeline.py run
  │
  ├─ scrape        Python: fetch Wahlrecht.de, hash-based incremental dedup
  ├─ canonicalize  R: merge CSVs, harmonize parties, validate, deduplicate
  ├─ fit           R: B-spline Stan model, ~11 min on M-series Mac
  └─ export        R: posterior → 3 JSON files → web/public/data/
```

Early-exit gates skip the expensive fit stage when no new data exists. `--force` bypasses all gates. `pipeline_result.json` is written on completion with per-stage status for CI consumption.

## License

© 2024–2026. All rights reserved.

This software is made available for academic and research purposes only. Commercial use is strictly prohibited.
