#!/usr/bin/env python3
"""
Forecast pipeline orchestrator.

Subcommands:
    update-data   Scrape + canonicalize only (for CI, no Stan)
    run           Full pipeline: scrape → canonicalize → fit → export
    scrape        Scrape only (incremental by default, --full for backfill)
    canonicalize  Re-run R canonicalization only
    fit           Canonicalize + fit + export (full estimation/main.R)
    export        Re-export JSON from existing spline_fit.rds

Usage:
    python pipeline.py update-data      # CI: scrape + canonicalize only
    python pipeline.py run              # local: full pipeline
    python pipeline.py run --force      # re-fit even if no new polls
    python pipeline.py scrape --full    # full historical backfill
    python pipeline.py export           # re-export without refitting

Design: docs/design/step14_pipeline_cli.md
"""

import argparse
import hashlib
import json
import logging
import os
import subprocess
import sys
import time
from dataclasses import dataclass, field, asdict
from datetime import datetime, timezone
from typing import Optional

logger = logging.getLogger("pipeline")


################################################################################
# Stage result
################################################################################

@dataclass
class StageResult:
    success: bool
    skipped: bool = False
    detail: dict = field(default_factory=dict)


################################################################################
# Helpers
################################################################################

def hash_file(path):
    """SHA-256 hex digest of a file's contents. Returns None if file doesn't exist."""
    if not os.path.exists(path):
        return None
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(8192), b""):
            h.update(chunk)
    return h.hexdigest()


def run_subprocess(cmd, label, capture=True):
    """
    Run a subprocess, log output, return (success, stdout, stderr).

    Args:
        cmd: command list for subprocess.run
        label: human-readable name for logging
        capture: if True, capture stdout/stderr; if False, stream to console
    """
    logger.info("Running: %s", " ".join(cmd))
    t0 = time.time()

    try:
        result = subprocess.run(
            cmd,
            capture_output=capture,
            text=True,
            timeout=3600,  # 1 hour max for model fitting
        )

        elapsed = time.time() - t0

        if capture:
            if result.stdout.strip():
                for line in result.stdout.strip().split("\n"):
                    logger.debug("[%s stdout] %s", label, line)
            if result.stderr.strip():
                for line in result.stderr.strip().split("\n"):
                    # R messages go to stderr; log at INFO unless exit code is non-zero
                    lvl = logging.WARNING if result.returncode != 0 else logging.DEBUG
                    logger.log(lvl, "[%s stderr] %s", label, line)

        if result.returncode != 0:
            logger.error("%s failed (exit %d) after %.1fs", label, result.returncode, elapsed)
            return False, result.stdout if capture else "", result.stderr if capture else ""

        logger.info("%s completed in %.1fs", label, elapsed)
        return True, result.stdout if capture else "", result.stderr if capture else ""

    except FileNotFoundError:
        logger.error("%s: command not found: %s", label, cmd[0])
        return False, "", f"Command not found: {cmd[0]}"
    except subprocess.TimeoutExpired:
        logger.error("%s: timed out after 3600s", label)
        return False, "", "Timed out after 3600s"


################################################################################
# Stage runners
################################################################################

def run_scrape(full=False, data_dir="web/public/polling_data"):
    """Stage 1: Scrape Wahlrecht.de for new polls."""
    logger.info("=" * 60)
    logger.info("STAGE 1: Scrape%s", " (full)" if full else " (incremental)")
    logger.info("=" * 60)

    try:
        from scraping.scrapers import scrape_incremental, scrape_full, extract_polling_structure

        polling_structure = extract_polling_structure()

        if full:
            summary = scrape_full(data_dir, polling_structure)
        else:
            summary = scrape_incremental(data_dir, polling_structure)

        total_new = sum(v["new_rows"] for v in summary["pollsters"].values())
        errors = [p for p, v in summary["pollsters"].items() if v.get("error")]

        if errors:
            logger.warning("%d pollster(s) had errors: %s", len(errors), ", ".join(errors))

        logger.info("Scrape result: %d new polls", total_new)

        return StageResult(
            success=len(errors) == 0,
            detail={"new_rows": total_new, "errors": errors, "summary": summary}
        )

    except Exception as e:
        logger.error("Scrape stage failed: %s", e)
        return StageResult(success=False, detail={"error": str(e)})


def run_canonicalize():
    """Stage 2: Re-run R canonicalization to produce data/polls.csv."""
    logger.info("=" * 60)
    logger.info("STAGE 2: Canonicalize")
    logger.info("=" * 60)

    cmd = [
        "Rscript", "-e",
        'source("estimation/canonicalize.R"); canonicalize_polls()'
    ]
    success, stdout, stderr = run_subprocess(cmd, "canonicalize")
    return StageResult(success=success, detail={"stdout": stdout, "stderr": stderr})


def run_fit():
    """Stage 3: Full model fit (canonicalize + fit + export via main.R)."""
    logger.info("=" * 60)
    logger.info("STAGE 3: Fit model")
    logger.info("=" * 60)

    cmd = ["Rscript", "estimation/main.R"]
    success, stdout, stderr = run_subprocess(cmd, "fit")
    return StageResult(success=success, detail={"stdout": stdout, "stderr": stderr})


def run_export():
    """Stage 4: Re-export JSON from existing spline_fit.rds."""
    logger.info("=" * 60)
    logger.info("STAGE 4: Export JSON")
    logger.info("=" * 60)

    # Check that fit exists
    if not os.path.exists("data/spline_fit.rds"):
        logger.error("data/spline_fit.rds not found — run 'fit' first")
        return StageResult(success=False, detail={"error": "spline_fit.rds not found"})

    cmd = [
        "Rscript", "-e",
        'source("estimation/export.R"); '
        'export_all(fit_path="data/spline_fit.rds", '
        'polls_path="data/polls.csv", out_dir="web/public/data/")'
    ]
    success, stdout, stderr = run_subprocess(cmd, "export")
    return StageResult(success=success, detail={"stdout": stdout, "stderr": stderr})


################################################################################
# Full pipeline: run
################################################################################

def run_full_pipeline(force=False, full_scrape=False, data_dir="web/public/polling_data"):
    """
    Orchestrate the full pipeline: scrape → canonicalize → fit → export.

    Early-exit logic:
      1. If scrape finds 0 new rows and not --force → stop
      2. If polls.csv hash unchanged after canonicalize and not --force → stop
    """
    pipeline_start = time.time()
    stages = {}

    # ── Stage 1: Scrape ──
    scrape_result = run_scrape(full=full_scrape, data_dir=data_dir)
    new_rows = scrape_result.detail.get("new_rows", 0)
    stages["scrape"] = {
        "success": scrape_result.success,
        "skipped": False,
        "new_rows": new_rows,
    }

    # Even if some pollsters errored, continue if we got new rows
    # Only abort if the scrape completely failed (exception, not partial errors)
    if not scrape_result.success and new_rows == 0:
        logger.error("Scrape failed with no new data — aborting pipeline")
        stages["canonicalize"] = {"success": False, "skipped": True}
        stages["fit"] = {"success": False, "skipped": True}
        stages["export"] = {"success": False, "skipped": True}
        return _write_result(stages, data_changed=False, start=pipeline_start)

    # Early exit gate 1: no new polls
    if new_rows == 0 and not force:
        logger.info("No new polls found — skipping canonicalize/fit/export")
        stages["canonicalize"] = {"success": True, "skipped": True}
        stages["fit"] = {"success": True, "skipped": True}
        stages["export"] = {"success": True, "skipped": True}
        return _write_result(stages, data_changed=False, start=pipeline_start)

    # ── Stage 2: Canonicalize ──
    polls_hash_before = hash_file("data/polls.csv")
    canon_result = run_canonicalize()
    stages["canonicalize"] = {
        "success": canon_result.success,
        "skipped": False,
    }

    if not canon_result.success:
        logger.error("Canonicalize failed — aborting pipeline")
        stages["fit"] = {"success": False, "skipped": True}
        stages["export"] = {"success": False, "skipped": True}
        return _write_result(stages, data_changed=False, start=pipeline_start)

    # Early exit gate 2: polls.csv unchanged after canonicalize
    polls_hash_after = hash_file("data/polls.csv")
    polls_changed = polls_hash_before != polls_hash_after
    stages["canonicalize"]["polls_changed"] = polls_changed

    if not polls_changed and not force:
        logger.info("Canonical polls.csv unchanged — skipping fit/export")
        stages["fit"] = {"success": True, "skipped": True}
        stages["export"] = {"success": True, "skipped": True}
        return _write_result(stages, data_changed=False, start=pipeline_start)

    # ── Stage 3: Fit ──
    fit_result = run_fit()
    stages["fit"] = {
        "success": fit_result.success,
        "skipped": False,
    }

    if not fit_result.success:
        logger.error("Model fit failed — aborting pipeline")
        stages["export"] = {"success": False, "skipped": True}
        return _write_result(stages, data_changed=False, start=pipeline_start)

    # ── Stage 4: Export ──
    # main.R already calls export, but we run it explicitly for the stage result
    # Actually, main.R includes the export call at the end, so fit_result
    # already produced the JSON files. We mark export as success.
    stages["export"] = {
        "success": True,
        "skipped": False,
    }

    return _write_result(stages, data_changed=True, start=pipeline_start)


################################################################################
# Data-only pipeline: scrape + canonicalize (for CI, no Stan)
################################################################################

def run_update_data(full_scrape=False, data_dir="web/public/polling_data"):
    """
    Lightweight pipeline for CI: scrape → canonicalize only.
    No model fitting, no export. Suitable for GitHub Actions.

    Returns exit code (0 = success, 1 = failure).
    """
    pipeline_start = time.time()
    stages = {}

    # ── Stage 1: Scrape ──
    scrape_result = run_scrape(full=full_scrape, data_dir=data_dir)
    new_rows = scrape_result.detail.get("new_rows", 0)
    stages["scrape"] = {
        "success": scrape_result.success,
        "skipped": False,
        "new_rows": new_rows,
    }

    if not scrape_result.success and new_rows == 0:
        logger.error("Scrape failed with no new data — aborting")
        stages["canonicalize"] = {"success": False, "skipped": True}
        return _write_result(stages, data_changed=False, start=pipeline_start)

    # Always canonicalize — even with 0 new rows the scraped CSVs may have
    # been migrated or reformatted, and canonicalize is cheap (~2s)
    polls_hash_before = hash_file("data/polls.csv")

    # ── Stage 2: Canonicalize ──
    canon_result = run_canonicalize()
    stages["canonicalize"] = {
        "success": canon_result.success,
        "skipped": False,
    }

    if not canon_result.success:
        logger.error("Canonicalize failed")
        return _write_result(stages, data_changed=False, start=pipeline_start)

    polls_hash_after = hash_file("data/polls.csv")
    polls_changed = polls_hash_before != polls_hash_after
    stages["canonicalize"]["polls_changed"] = polls_changed

    data_changed = new_rows > 0 or polls_changed
    return _write_result(stages, data_changed=data_changed, start=pipeline_start)


def _write_result(stages, data_changed, start):
    """Write pipeline_result.json and return exit code."""
    elapsed = time.time() - start

    result = {
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "stages": stages,
        "data_changed": data_changed,
        "elapsed_seconds": round(elapsed, 1),
        "exit_code": 0 if all(s.get("success", False) for s in stages.values()) else 1,
    }

    with open("pipeline_result.json", "w") as f:
        json.dump(result, f, indent=2)
    logger.info("Wrote pipeline_result.json")

    # Print one-line summary
    parts = []
    for name in ["scrape", "canonicalize", "fit", "export"]:
        s = stages.get(name)
        if s is None:
            continue  # stage not in this pipeline mode
        if s.get("skipped"):
            parts.append("skipped")
            break
        elif name == "scrape":
            nr = s.get("new_rows", "?")
            parts.append(f"scrape({nr} new)")
        elif name == "canonicalize":
            ch = "changed" if s.get("polls_changed") else "unchanged"
            parts.append(f"canonicalize({ch})")
        elif name == "fit":
            parts.append("fit(ok)" if s.get("success") else "fit(FAIL)")
        elif name == "export":
            parts.append("export(ok)" if s.get("success") else "export(FAIL)")

    summary_line = " → ".join(parts)
    logger.info("Pipeline complete: %s [%.0fs]", summary_line, elapsed)

    return result["exit_code"]


################################################################################
# CLI
################################################################################

def main():
    parser = argparse.ArgumentParser(
        description="Forecast pipeline orchestrator",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python pipeline.py update-data       # CI: scrape + canonicalize only
  python pipeline.py run               # local: full pipeline
  python pipeline.py run --force       # re-fit even if no new polls
  python pipeline.py scrape --full     # backfill all historical data
  python pipeline.py export            # re-export JSON without refitting
        """
    )
    parser.add_argument(
        "--verbose", "-v", action="store_true",
        help="Enable debug logging"
    )

    subparsers = parser.add_subparsers(dest="command", required=True)

    # --- update-data ---
    ud_parser = subparsers.add_parser("update-data",
                                       help="Scrape + canonicalize only (for CI, no Stan)")
    ud_parser.add_argument("--full-scrape", action="store_true",
                           help="Use full scrape instead of incremental")
    ud_parser.add_argument("--data-dir", default="web/public/polling_data",
                           help="Polling data directory")

    # --- run ---
    run_parser = subparsers.add_parser("run", help="Full pipeline: scrape → canonicalize → fit → export")
    run_parser.add_argument("--force", action="store_true",
                            help="Bypass early-exit checks and re-fit regardless")
    run_parser.add_argument("--full-scrape", action="store_true",
                            help="Use full scrape instead of incremental")
    run_parser.add_argument("--data-dir", default="web/public/polling_data",
                            help="Polling data directory (default: web/public/polling_data)")

    # --- scrape ---
    scrape_parser = subparsers.add_parser("scrape", help="Scrape polling data only")
    scrape_parser.add_argument("--full", action="store_true",
                               help="Full re-scrape including historical pages")
    scrape_parser.add_argument("--data-dir", default="web/public/polling_data",
                               help="Polling data directory")

    # --- canonicalize ---
    subparsers.add_parser("canonicalize", help="Re-run R canonicalization only")

    # --- fit ---
    subparsers.add_parser("fit", help="Canonicalize + fit + export (full estimation/main.R)")

    # --- export ---
    subparsers.add_parser("export", help="Re-export JSON from existing spline_fit.rds")

    args = parser.parse_args()

    # Configure logging
    level = logging.DEBUG if args.verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format="%(asctime)s [%(levelname)s] %(name)s: %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
    )

    # Dispatch
    if args.command == "update-data":
        exit_code = run_update_data(
            full_scrape=args.full_scrape,
            data_dir=args.data_dir,
        )
        sys.exit(exit_code)

    elif args.command == "run":
        exit_code = run_full_pipeline(
            force=args.force,
            full_scrape=args.full_scrape,
            data_dir=args.data_dir,
        )
        sys.exit(exit_code)

    elif args.command == "scrape":
        result = run_scrape(full=args.full, data_dir=args.data_dir)
        # Print scrape summary
        summary = result.detail.get("summary")
        if summary:
            print(json.dumps(summary, indent=2))
        sys.exit(0 if result.success else 1)

    elif args.command == "canonicalize":
        result = run_canonicalize()
        sys.exit(0 if result.success else 1)

    elif args.command == "fit":
        result = run_fit()
        sys.exit(0 if result.success else 1)

    elif args.command == "export":
        result = run_export()
        sys.exit(0 if result.success else 1)


if __name__ == "__main__":
    main()
