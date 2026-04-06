#!/usr/bin/env python3
"""
Pipeline entry point for polling data scraping.

Usage:
    python main.py                  # incremental scrape (current pages only)
    python main.py --full           # full re-scrape (all pages + historical)
    python main.py --data-dir PATH  # custom output directory
"""

import argparse
import json
import logging
import sys

from scraping.scrapers import scrape_incremental, scrape_full, extract_polling_structure


def main():
    parser = argparse.ArgumentParser(
        description="Wahlrecht.de polling data scraper"
    )
    parser.add_argument(
        '--full', action='store_true',
        help='Full re-scrape of all pages including historical archives. '
             'Default is incremental (current pages only).'
    )
    parser.add_argument(
        '--data-dir', default='web/public/polling_data',
        help='Directory for per-pollster CSV output (default: web/public/polling_data)'
    )
    parser.add_argument(
        '--verbose', '-v', action='store_true',
        help='Enable debug logging'
    )
    args = parser.parse_args()

    # Configure logging
    level = logging.DEBUG if args.verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
        datefmt='%Y-%m-%d %H:%M:%S'
    )

    logger = logging.getLogger('scraper')

    # Discover pollster URLs
    logger.info("Discovering pollster pages...")
    polling_structure = extract_polling_structure()
    logger.info("Found %d pollsters", len(polling_structure))

    # Run scraper
    if args.full:
        logger.info("Running FULL scrape...")
        summary = scrape_full(args.data_dir, polling_structure)
    else:
        logger.info("Running INCREMENTAL scrape...")
        summary = scrape_incremental(args.data_dir, polling_structure)

    # Print summary
    print(json.dumps(summary, indent=2))

    # Exit code: 0 if no errors, 1 if any pollster failed
    errors = [p for p, v in summary["pollsters"].items() if v.get("error")]
    if errors:
        logger.warning("%d pollster(s) had errors: %s", len(errors), ", ".join(errors))
        sys.exit(1)

    total_new = sum(v["new_rows"] for v in summary["pollsters"].values())
    logger.info("Done. %d new polls across %d pollsters.",
                total_new, len(summary["pollsters"]))


if __name__ == "__main__":
    main()
