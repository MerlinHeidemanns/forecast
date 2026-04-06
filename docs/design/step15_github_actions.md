# Step 15: GitHub Actions Workflow

**Status:** Accepted — Implemented
**Author:** Claude / Merlin
**Date:** 2026-04-06

---

## Problem

The forecast pipeline (`pipeline.py run`) runs locally. To keep the site fresh, someone must manually scrape, fit, and push. The goal is to automate at least the data collection so new polls are never missed.

## Constraints

CmdStan model fitting takes ~11 minutes on an M4 MacBook Pro and would take 30–45 minutes on a GitHub Actions runner (2-core, 7GB). This is too slow and fragile for CI. Model fitting stays local.

## Design: Two-Tier Update

**Tier 1 — Automated (GitHub Actions, every 6h):**
Scrape → canonicalize → commit `data/polls.csv` if changed. Python + lightweight R, no Stan. Runs in ~2 minutes. New polls accumulate automatically.

**Tier 2 — Manual (local machine):**
`python pipeline.py run` or `python pipeline.py run --force`. Fits the model, exports JSON, push to main. Vercel redeploys. Run as needed (weekly, or after significant new polls).

This split means polling data is always current, and the forecast is updated at whatever cadence Merlin chooses. Most election forecast sites update daily or weekly, so a manual refit cadence is standard.

---

## Design Decisions

| Decision | Choice | Rationale |
|---|---|---|
| Schedule | Every 6 hours (`0 */6 * * *`) | German pollsters publish 1–3 times per week. 6h catches same-day. |
| Branch | `main` | Vercel deploys from main. |
| Runner | `ubuntu-latest` | Python + R available. |
| R setup | `r-lib/actions/setup-r@v2` | Needed for `canonicalize.R`. |
| R packages | `tidyverse`, `digest` | Only what canonicalize needs. No cmdstanr. |
| Python deps | `requests`, `beautifulsoup4`, `pandas` | Scraper only. |
| Commit scope | `data/polls.csv`, `data/rejected_polls.csv`, `web/public/polling_data/` | Raw + canonical data. Not JSON (unchanged without refit). |
| Concurrency | `concurrency: forecast-update` | Prevent overlapping runs. |
| Manual trigger | `workflow_dispatch` | Manual runs from Actions tab. |
| Permissions | `contents: write` | Needed for git push. |

---

## Implementation Tasks

### Task 1: Pipeline support for scrape+canonicalize mode

**What:** Add a `pipeline.py update-data` subcommand that runs only scrape → canonicalize (no fit, no export). This is what CI calls.

Returns exit 0 and sets `data_changed: true` in `pipeline_result.json` if `polls.csv` changed.

**Files touched:** `pipeline.py`

#### Verify Task 1

1. `python pipeline.py update-data` runs scrape + canonicalize, skips fit/export
2. Exit code 0 on success
3. `pipeline_result.json` written with correct stage statuses

---

### Task 2: Create the workflow file

**What:** Create `.github/workflows/update.yml`:

```
Steps:
  1. Checkout (fetch-depth: 0)
  2. Setup Python 3.12 + pip install deps
  3. Setup R 4.4 + install tidyverse, digest
  4. Run: python pipeline.py update-data
  5. Check git diff on data files
  6. If changed: git add, commit, push
  7. If pipeline fails: upload pipeline_result.json as artifact
```

**R package caching:** Use `actions/cache` keyed on R version + package list hash. Avoids ~90s tidyverse install on each run.

**Files created:** `.github/workflows/update.yml`

#### Verify Task 2

1. Valid YAML (lint passes)
2. Triggers on `schedule` and `workflow_dispatch`
3. All referenced actions have correct version tags

---

### Task 3: Conditional commit logic

**What:** After `pipeline.py update-data`, check if data files changed:

```bash
git diff --quiet data/polls.csv data/rejected_polls.csv web/public/polling_data/ || {
    git config user.name "github-actions[bot]"
    git config user.email "github-actions[bot]@users.noreply.github.com"
    git add data/polls.csv data/rejected_polls.csv web/public/polling_data/
    git commit -m "Update polling data [automated]"
    git push
}
```

The `|| { ... }` idiom means: if `git diff --quiet` exits non-zero (files changed), run the commit block.

**Files touched:** `.github/workflows/update.yml`

#### Verify Task 3

1. No new polls → no commit, workflow succeeds (green)
2. New polls → commit with `[automated]` tag, pushed to main
3. Vercel picks up the push and redeploys (if connected)

---

### Task 4: Error handling

**What:** If the pipeline fails:
- The step fails, workflow shows red
- An `if: failure()` step uploads `pipeline_result.json` as an artifact for debugging
- GitHub's default email notifications alert on failure

**Files touched:** `.github/workflows/update.yml`

#### Verify Task 4

1. Simulate failure (e.g. bad URL) → workflow red, artifact uploaded
2. Success → green, no artifact (or artifact with success result)

---

## Execution Order

```
Task 1  (update-data subcommand in pipeline.py)
  ↓
Task 2  (base workflow file)
  ↓
Task 3  (conditional commit logic, in workflow)
  ↓
Task 4  (error handling, in workflow)
```

---

## Files Changed

| File | Change |
|---|---|
| `pipeline.py` | Add `update-data` subcommand |
| `.github/workflows/update.yml` | New — CI workflow |

## Files NOT Changed

| File | Reason |
|---|---|
| `estimation/main.R` | Not called in CI |
| `estimation/export.R` | Not called in CI |
| `main.py` | Scraper entry point unchanged |

---

## Open Questions

1. **Should `data/spline_fit.rds` be gitignored?** 61MB, only needed locally. Recommend adding to `.gitignore`.
2. **Vercel auto-deploy:** Confirm Vercel deploys on push to `main`. If yes, the JSON files from a local `pipeline.py run --force` + push trigger a redeploy automatically.
