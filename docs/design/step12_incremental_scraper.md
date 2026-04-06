# Step 12: Incremental Wahlrecht.de Scraper

**Status:** Accepted — Implemented
**Author:** Claude / Merlin
**Date:** 2026-04-06

---

## Problem

`scraping/scrapers.py` re-downloads every pollster page (current + all historical archives) on every run. This is ~50 HTTP requests, wasteful of Wahlrecht.de's bandwidth, slow, and fragile — a single network blip can abort the whole scrape. Rows get fresh `uuid4` identifiers each run so there's no way to detect "already seen this poll."

## Goal

Make the scraper incremental: on a routine run, fetch only each pollster's current page (~8 requests), detect which rows are new via content hashing, and append only those. Historical backfill remains available behind a flag. Error handling isolates failures per-pollster. Output format stays identical so `canonicalize.R` requires zero changes.

---

## Design Decisions

| Decision | Choice | Rationale |
|---|---|---|
| Hash scope | Wide-format row (pre-melt) | One poll = one row. Hashing after melt would produce 7+ hashes per poll. |
| Hash algorithm | SHA-256 | Matches `canonicalize.R`'s `digest(..., algo="sha256")` |
| Hash input | `pollster\|date\|party=value` pairs (sorted) | Mirror `canonicalize.R`'s `compute_poll_hash` so hashes are comparable across Python and R |
| Incremental scope | Current page only | Historical pages are static archives on Wahlrecht.de; they don't change |
| Historical mode | `--full` flag | Available for backfill or repair; skipped in routine runs |
| File format | Long-format CSV (unchanged) | `canonicalize.R` reads these directly; no downstream changes |
| UUID column | Replaced by deterministic hash | `uuid` column now contains the SHA-256 hash instead of random `uuid4` |
| Retry policy | 3 attempts, exponential backoff (2s, 4s, 8s) | Resilient to transient failures without hammering the server |

---

## Implementation Tasks

### Task 1: Deterministic row hashing in `utils.py`

**What:** Add a function `compute_row_hash(row_dict, pollster)` that produces a SHA-256 hash identical to what `canonicalize.R` generates.

**Hash input format** (must match R's `compute_poll_hash`):
```
{pollster}|{publishing_date}|{sampling_start_day}|{sampling_end_day}|{survey_count}|{party1}={value1}|{party2}={value2}|...
```
Party key-value pairs are sorted alphabetically. `NA`/missing values render as `"NA"`.

**Changes:**
- `utils.py`: new function `compute_row_hash(row_dict: dict, pollster: str) -> str`
- `utils.py`: replace `add_uuid` with `add_content_hash(df, pollster)` that applies `compute_row_hash` to each wide-format row and writes the result into the `uuid` column

**Files touched:** `scraping/utils.py`

#### Verify Task 1

Run a standalone test that:
1. Constructs a sample wide-format row with known values
2. Calls `compute_row_hash` and checks the output is a 64-char hex string
3. Calls it twice with the same input → same hash
4. Calls it with a different vote share → different hash
5. (If R is available) Generates the same row's hash via `canonicalize.R`'s `compute_poll_hash` and confirms they match

---

### Task 2: Replace `add_uuid` call site in `scrapers.py`

**What:** Update `scrape_all_polling_houses` (and the new incremental function) to call `add_content_hash(df, pollster)` instead of `add_uuid(df)`. The hash must be computed on the **wide-format** DataFrame, before `convert_to_long_format`.

**Current call order:**
```python
current_df = scrape_table(url)          # returns wide-format
current_df = add_uuid(current_df)       # uuid4 per row
current_df = convert_to_long_format(current_df)  # melt to long
```

**New call order:**
```python
current_df = scrape_table(url)                    # wide-format
current_df = add_content_hash(current_df, pollster)  # sha256 per row
current_df = convert_to_long_format(current_df)      # melt to long
```

**Files touched:** `scraping/scrapers.py`

#### Verify Task 2

1. Run `scrape_table` on a single pollster URL (e.g. Forsa current page)
2. Confirm the returned DataFrame has a `uuid` column containing 64-char hex strings
3. Run it again → identical hashes for the same rows
4. Confirm the long-format CSV has the same hash repeated across all party rows for a given poll

---

### Task 3: HTTP retry with exponential backoff

**What:** Wrap `requests.get` in a retry helper that attempts up to 3 times with exponential backoff (2s, 4s, 8s). Raise after final failure.

**Changes:**
- `scraping/utils.py`: new function `fetch_with_retry(url, max_retries=3, base_delay=2.0) -> requests.Response`
- `scraping/scrapers.py`: replace all `requests.get(url)` calls with `fetch_with_retry(url)`

**Files touched:** `scraping/utils.py`, `scraping/scrapers.py`

#### Verify Task 3

1. Mock a URL that returns 500 on first two attempts, 200 on third → succeeds
2. Mock a URL that returns 500 on all three attempts → raises with clear error message
3. Confirm delay between attempts (log timestamps or mock `time.sleep`)

---

### Task 4: Incremental scrape function

**What:** New function `scrape_incremental(data_dir, polling_structure)` that, for each pollster:

1. Fetches only the **current** page
2. Parses into a wide-format DataFrame via `scrape_table`
3. Computes content hashes via `add_content_hash`
4. Loads the existing `current.csv` (if any) and collects the set of known hashes
5. Filters the scraped DataFrame to only rows with hashes not in the known set
6. Converts new rows to long format and appends to `current.csv`
7. Returns a summary dict: `{pollster: {new_rows: int, error: str|None}}`

**Key detail on dedup:** The existing `current.csv` was written with `uuid4` hashes. On the first incremental run, all existing hashes will be random and won't match the new content hashes, so all scraped rows would appear "new." Two options:

- **Option A (recommended):** On first incremental run, recompute hashes for the existing CSV in-place before comparing. This is a one-time migration that converts old random UUIDs to deterministic hashes.
- **Option B:** Accept that the first run will produce duplicates and let `canonicalize.R`'s dedup handle it.

We go with **Option A**: if the existing CSV's `uuid` column doesn't look like SHA-256 hashes (check length ≠ 64 or contains hyphens), recompute all hashes in-place and overwrite the file, then proceed with the diff.

**Per-pollster error isolation:** Each pollster is wrapped in try/except. If one fails, the error is logged and recorded in the summary, but the loop continues to the next pollster.

**Files touched:** `scraping/scrapers.py`

#### Verify Task 4

1. Run `scrape_incremental` with existing CSVs in place → reports 0 new rows for each pollster (data hasn't changed since last full scrape)
2. Manually delete the last 5 rows from one pollster's `current.csv`, re-run → reports exactly 5 new rows for that pollster
3. Simulate a network failure for one pollster (e.g. bad URL) → that pollster shows an error, all others succeed
4. Check that `current.csv` still has valid long-format structure after append

---

### Task 5: Refactor `scrape_all_polling_houses` and add CLI flags

**What:** Restructure the scraper entry points:

- `scrape_incremental(data_dir, polling_structure)` — default mode, current pages only
- `scrape_full(data_dir, polling_structure)` — full re-scrape of all pages (current + historical), existing behavior preserved
- `scraping/scrapers.py` remains importable; both functions are public

Update `main.py` to accept a `--full` flag:

```
python main.py              # incremental (current pages only)
python main.py --full       # full re-scrape (all pages, all history)
```

**Backward compatibility:** `scrape_all_polling_houses` is kept as an alias for `scrape_full` so any existing callers don't break.

**Files touched:** `scraping/scrapers.py`, `main.py`

#### Verify Task 5

1. `python main.py` → runs incremental, only hits ~8 URLs (one per pollster)
2. `python main.py --full` → runs full scrape, hits all current + historical pages
3. Importing `from scraping.scrapers import scrape_all_polling_houses` still works (backward compat)

---

### Task 6: Structured logging and return value

**What:** Replace bare `print()` calls with Python `logging` at appropriate levels:

| Level | Usage |
|---|---|
| `INFO` | "Scraping Forsa current page", "3 new rows appended" |
| `WARNING` | "Forsa: retrying after 500 (attempt 2/3)" |
| `ERROR` | "INSA: failed after 3 attempts: ConnectionError" |

Both `scrape_incremental` and `scrape_full` return a summary dict:

```python
{
    "mode": "incremental" | "full",
    "timestamp": "2026-04-06T14:30:00",
    "pollsters": {
        "Forsa": {"new_rows": 7, "total_rows": 1842, "error": None},
        "INSA":  {"new_rows": 0, "total_rows": 0,    "error": "ConnectionError: ..."},
        ...
    }
}
```

This summary is what Step 14 (pipeline CLI) will consume to decide whether to proceed with fitting.

**Files touched:** `scraping/scrapers.py`, `scraping/utils.py`

#### Verify Task 6

1. Run with `logging.basicConfig(level=logging.INFO)` → see structured log lines
2. Confirm return dict has the expected shape
3. Simulate one pollster failure → `error` field is populated, others show `None`

---

## Execution Order

```
Task 1  (hash function)
  ↓
Task 2  (wire into scraper)   ←  Task 3  (retry helper, independent)
  ↓                                ↓
Task 4  (incremental logic, depends on 1+2+3)
  ↓
Task 5  (CLI flags, depends on 4)
  ↓
Task 6  (logging, can be done alongside 5)
```

Tasks 1–3 can be developed and verified independently. Task 4 is the core and depends on all three. Tasks 5–6 are wiring and polish.

---

## Files Changed (summary)

| File | Change |
|---|---|
| `scraping/utils.py` | Add `compute_row_hash`, `add_content_hash`, `fetch_with_retry`. Deprecate `add_uuid`. |
| `scraping/scrapers.py` | Add `scrape_incremental`, refactor `scrape_all_polling_houses` → `scrape_full`. Replace `add_uuid` calls. Use `fetch_with_retry`. Add `logging`. |
| `main.py` | Add `argparse` with `--full` flag. Default to incremental. |

## Files NOT changed

| File | Reason |
|---|---|
| `estimation/canonicalize.R` | Long-format CSV schema is unchanged. R-side hash still works. |
| `web/public/polling_data/*/current.csv` | Format unchanged; UUID column values will migrate from random to deterministic on first incremental run. |

---

## Open Questions

None — all decisions resolved.
