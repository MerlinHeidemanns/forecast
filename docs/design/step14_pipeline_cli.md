# Step 14: Pipeline CLI

**Status:** Accepted — Implemented
**Author:** Claude / Merlin
**Date:** 2026-04-06

---

## Problem

The forecast pipeline has four stages — scrape, canonicalize, fit, export — each invoked separately: `python main.py` for scraping, then `Rscript estimation/main.R` (which calls canonicalize, fit, and export internally). There's no single command to run the full pipeline, no early-exit logic when nothing has changed, and no way to run individual stages in isolation for debugging.

## Goal

A single `pipeline.py` CLI that orchestrates all stages with subcommands, skips expensive stages when no new data exists, and produces structured output suitable for GitHub Actions (Step 15).

---

## Design Decisions

| Decision | Choice | Rationale |
|---|---|---|
| CLI framework | `argparse` with subcommands | Standard library, no deps. Consistent with current `main.py`. |
| Entry point | `pipeline.py` (new file) | `main.py` stays as scraper-only entry point for backward compat. `pipeline.py` is the orchestrator. |
| R invocation | `subprocess.run(["Rscript", ...])` | R scripts are already structured as standalone source-able files. Subprocess is the cleanest cross-language boundary. |
| Early exit | Hash `data/polls.csv` before and after canonicalize | If the polls CSV is byte-identical after re-canonicalization, no new data entered the model. Skip fit+export. |
| Stage isolation | Each stage is a function returning `(success: bool, detail: dict)` | Composable: `run` calls all in sequence, individual subcommands call one. |
| Force flag | `--force` on `run` | Bypass the early-exit check and re-fit even if no new polls. Useful after model changes. |
| Fit stage | Calls `Rscript estimation/main.R` | `main.R` already handles canonicalize → fit → export internally. But the pipeline needs canonicalize separately to do the early-exit check. |
| Canonicalize independently | `Rscript -e 'source("estimation/canonicalize.R"); canonicalize_polls()'` | Separates the cheap canonicalize from the expensive fit, enabling the early-exit optimization. |
| Export independently | `Rscript -e 'source("estimation/export.R"); export_all()'` | Allows re-exporting without re-fitting (e.g. after frontend schema changes). |

---

## Architecture

```
pipeline.py run [--force]
  │
  ├── 1. scrape    →  python: scrape_incremental()
  │                    Returns: {new_rows: N}
  │                    Early exit if new_rows == 0 and not --force
  │
  ├── 2. canonicalize  →  Rscript: canonicalize_polls()
  │                        Produces: data/polls.csv
  │                        Hash before/after for change detection
  │
  ├── 3. fit       →  Rscript: estimation/main.R
  │                    (includes canonicalize + fit + export internally,
  │                     but we already canonicalized in step 2)
  │                    Produces: data/spline_fit.rds
  │                    Skipped if polls.csv unchanged and not --force
  │
  └── 4. export    →  Rscript: export_all()
                       Produces: web/public/data/*.json
                       Runs after fit, or standalone via subcommand
```

---

## Subcommands

| Command | What it does |
|---|---|
| `pipeline.py run` | Full pipeline: scrape → canonicalize → fit → export. Skips fit if no new polls (unless `--force`). |
| `pipeline.py scrape [--full]` | Scrape only. Delegates to `scrape_incremental` or `scrape_full`. |
| `pipeline.py canonicalize` | Re-run R canonicalization only. |
| `pipeline.py fit` | Canonicalize + fit + export (full `main.R`). |
| `pipeline.py export` | Re-export JSON from existing `spline_fit.rds` without refitting. |

All subcommands accept `--verbose` and return exit code 0 on success, 1 on failure.

---

## Implementation Tasks

### Task 1: Stage runner helpers

**What:** Create `pipeline.py` with helper functions for running each stage as a subprocess or function call. Each returns a structured result.

**Functions:**
- `run_scrape(full=False, data_dir="web/public/polling_data") -> StageResult`
- `run_canonicalize() -> StageResult`
- `run_fit() -> StageResult`
- `run_export() -> StageResult`
- `hash_file(path) -> str` — SHA-256 of file contents for change detection

`StageResult` is a `namedtuple` or dataclass: `(success: bool, skipped: bool, detail: dict)`.

Subprocess calls capture stdout/stderr, log them, and detect non-zero exit codes.

**Files touched:** `pipeline.py` (new)

#### Verify Task 1

1. `python pipeline.py scrape` runs the incremental scraper and reports 0 new rows
2. `python pipeline.py canonicalize` invokes R canonicalization (requires R on the machine)
3. `python pipeline.py export` invokes R export (requires `spline_fit.rds` to exist)
4. Each returns exit code 0 on success

---

### Task 2: Early-exit logic

**What:** Implement the `run` subcommand with the skip-if-unchanged optimization:

1. Run scrape. If scraper reports 0 new rows AND `--force` is not set → log "No new polls, skipping fit" and exit 0.
2. If new rows > 0 OR `--force`: hash `data/polls.csv`, run canonicalize, hash again.
3. If hash unchanged AND `--force` not set → log "Canonical data unchanged, skipping fit" and exit 0. (This catches edge cases where new scraped rows were all duplicates that canonicalize deduped.)
4. Otherwise: run fit, then export.

**Files touched:** `pipeline.py`

#### Verify Task 2

1. Run `python pipeline.py run` twice with no new polls → second run skips fit, logs reason, exit 0
2. Run `python pipeline.py run --force` → fit runs even with no new polls
3. Manually add a fake poll row to one pollster CSV, run `python pipeline.py run` → fit runs

---

### Task 3: Structured output for CI

**What:** The `run` subcommand writes a `pipeline_result.json` to the project root on completion. This is what GitHub Actions (Step 15) will read to decide whether to commit.

```json
{
  "timestamp": "2026-04-06T16:00:00Z",
  "stages": {
    "scrape":       {"success": true, "skipped": false, "new_rows": 5},
    "canonicalize": {"success": true, "skipped": false, "polls_changed": true},
    "fit":          {"success": true, "skipped": false},
    "export":       {"success": true, "skipped": false}
  },
  "data_changed": true,
  "exit_code": 0
}
```

`data_changed` is `true` if the export stage ran (new JSON files produced). GitHub Actions will `git diff --quiet web/public/data/` and only commit if files changed, but this flag provides a fast hint.

**Files touched:** `pipeline.py`

#### Verify Task 3

1. Run `python pipeline.py run` → `pipeline_result.json` exists and is valid JSON
2. Fields match the schema above
3. After a no-change run, `data_changed` is `false` and fit/export are `skipped: true`

---

### Task 4: Logging and error handling

**What:** Use Python `logging` throughout, consistent with the scraper's style. Each stage logs its start/end, duration, and outcome. Subprocess stderr is captured and logged at WARNING level. Any stage failure logs the error and aborts the pipeline (no partial state).

Additionally, the `run` subcommand prints a one-line summary at the end:

```
Pipeline complete: scrape(5 new) → canonicalize(changed) → fit(ok) → export(ok) [42s]
Pipeline complete: scrape(0 new) → skipped [2s]
```

**Files touched:** `pipeline.py`

#### Verify Task 4

1. Run with `-v` flag → see DEBUG-level subprocess output
2. Simulate R not found (e.g. bad PATH) → clear error message, exit 1
3. One-line summary printed to stdout at end

---

## Execution Order

```
Task 1  (stage runners)
  ↓
Task 2  (early-exit logic, depends on 1)
  ↓
Task 3  (structured output, depends on 2)
  ↓
Task 4  (logging polish, depends on 1-3)
```

---

## Files Changed

| File | Change |
|---|---|
| `pipeline.py` | New file — full pipeline orchestrator |

## Files NOT Changed

| File | Reason |
|---|---|
| `main.py` | Kept as scraper-only entry point for backward compat |
| `estimation/main.R` | Already a complete fit pipeline; called via subprocess |
| `estimation/canonicalize.R` | Called via subprocess; no code changes needed |
| `estimation/export.R` | Called via subprocess; no code changes needed |

---

## Open Questions

None — all decisions resolved.
