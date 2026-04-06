"""
Wahlrecht.de polling data scraper.

Modes:
    - Incremental (default): fetch only each pollster's current page, append new rows.
    - Full (--full): re-scrape all pages including historical archives.

Usage:
    from scraping.scrapers import scrape_incremental, scrape_full
"""

import logging
import os
from datetime import datetime, timezone
from urllib.parse import urljoin

import pandas as pd
from bs4 import BeautifulSoup

from .utils import (
    add_content_hash,
    clean_column_names,
    clean_percentage,
    convert_to_long_format,
    fetch_with_retry,
    get_numeric_columns,
    get_party_columns,
    is_content_hash,
    adjust_date,
)

logger = logging.getLogger(__name__)


################################################################################
# Low-level: scrape a single Wahlrecht.de table
################################################################################

def scrape_table(url):
    """
    Scrape a single Wahlrecht.de Sonntagsfrage table.

    Returns a wide-format DataFrame (one row per poll, party columns as values)
    or None on failure. Does NOT add uuid/hash — caller is responsible.
    """
    response = fetch_with_retry(url)
    soup = BeautifulSoup(response.content, 'html.parser')
    table = soup.find("table", {"class": "wilko"})

    if not table:
        raise ValueError(f"No table with class 'wilko' at {url}")

    thead = table.find("thead")
    if not thead:
        raise ValueError(f"No thead found in table at {url}")

    headers = [th.get_text(strip=True) for th in thead.find_all(["th", "td"])]
    clean_headers = clean_column_names(headers)
    clean_headers[0] = 'publishing_date'

    rows = []
    tbody = table.find("tbody")
    if tbody:
        for row in tbody.find_all("tr"):
            cells = row.find_all(["td", "th"])

            skip_row = False
            for cell in cells:
                text = cell.get_text(strip=True)
                if 'Wahl' in text or 'Bundestagswahl' in text:
                    skip_row = True
                    break

            if skip_row:
                continue

            row_data = []
            for cell in cells:
                text = cell.get_text(strip=True)
                text = pd.NA if text == '' or text == '–' else text
                row_data.append(text)

            while len(row_data) < len(clean_headers):
                row_data.append(pd.NA)

            rows.append(row_data[:len(clean_headers)])

    df = pd.DataFrame(rows, columns=clean_headers)
    df['publishing_date'] = pd.to_datetime(df['publishing_date'], format='%d.%m.%Y', errors='coerce')

    if 'Zeitraum' in df.columns:
        df[['sampling_start_day', 'sampling_end_day']] = df.apply(
            lambda row: pd.Series(adjust_date(row['Zeitraum'], row['publishing_date'])),
            axis=1
        )

    if 'Befragte' in df.columns:
        try:
            df['mode'] = df['Befragte'].str.extract(r'([A-Za-z]+)', expand=False).fillna('')
            df['survey_count'] = (
                df['Befragte'].astype(str)
                .str.extract(r'([\d\.\?]+)', expand=False)
                .str.replace("?.", "1", regex=False)
                .str.replace("?", "0", regex=False)
                .str.replace('.', '', regex=False)
                .astype(int)
            )
        except Exception as e:
            logger.warning("Error processing 'Befragte' column: %s", e)

    numeric_cols = get_numeric_columns(df)
    for col in numeric_cols:
        df[col] = df[col].apply(clean_percentage)

    df = df.dropna(axis=1, how='all')

    return df


################################################################################
# Discovery: find pollster URLs
################################################################################

def extract_institute_links():
    """Fetch the Wahlrecht.de index and return {institute_name: url}."""
    url = "https://www.wahlrecht.de/umfragen/index.htm"
    response = fetch_with_retry(url)
    soup = BeautifulSoup(response.text, 'html.parser')

    institutes = {}
    table = soup.find('table', class_='wilko')
    header_row = table.find('thead').find('tr')

    for cell in header_row.find_all('th', class_='in'):
        link = cell.find('a')
        if link:
            name = link.text.strip()
            href = link.get('href')
            full_url = urljoin(url, href)
            institutes[name] = full_url

    return institutes


def extract_polling_structure():
    """
    Build the full polling structure: for each institute, its current URL
    and dict of historical archive URLs.

    Returns: {institute: {"current": url, "historical": {period: url, ...}}}
    """
    institutes = extract_institute_links()
    structure = {}

    for institute, main_url in institutes.items():
        try:
            response = fetch_with_retry(main_url)
            soup = BeautifulSoup(response.text, 'html.parser')

            data_ranges = {
                "current": main_url,
                "historical": {}
            }

            nav = soup.find('p', class_='navi')
            if nav:
                for link in nav.find_all('a'):
                    href = link.get('href')
                    text = link.text.strip()
                    if href:
                        full_url = urljoin(main_url, href)
                        data_ranges["historical"][text] = full_url

            structure[institute] = data_ranges
            logger.info("Discovered %s: current + %d historical pages",
                        institute, len(data_ranges["historical"]))

        except Exception as e:
            logger.error("Failed to discover %s at %s: %s", institute, main_url, e)
            continue

    return structure


################################################################################
# Hash migration: convert old uuid4 values to content hashes
################################################################################

def _migrate_hashes_if_needed(csv_path, pollster):
    """
    If an existing CSV has uuid4-style IDs (contains hyphens, not 64 hex chars),
    recompute content hashes in-place and overwrite the file.

    Returns the (possibly updated) DataFrame in long format.
    """
    df_long = pd.read_csv(csv_path, parse_dates=['publishing_date', 'sampling_start_day', 'sampling_end_day'])

    if df_long.empty:
        return df_long

    # Check first non-null uuid value
    sample_uuid = df_long['uuid'].dropna().iloc[0] if not df_long['uuid'].dropna().empty else None
    if sample_uuid is not None and is_content_hash(sample_uuid):
        return df_long  # Already migrated

    logger.info("Migrating %s from uuid4 to content hashes...", csv_path)

    # Pivot back to wide format to compute hashes
    id_cols = [c for c in df_long.columns if c not in ('party', 'vote_share')]
    id_cols_no_uuid = [c for c in id_cols if c != 'uuid']

    df_wide = df_long.pivot_table(
        index=id_cols_no_uuid,
        columns='party',
        values='vote_share',
        aggfunc='first'
    ).reset_index()

    # Compute content hashes
    df_wide = add_content_hash(df_wide, pollster)

    # Melt back to long format
    df_long_new = convert_to_long_format(df_wide)
    df_long_new.to_csv(csv_path, index=False, encoding='utf-8')
    logger.info("Migrated %d rows in %s", len(df_long_new), csv_path)

    return df_long_new


################################################################################
# Incremental scrape (default mode)
################################################################################

def scrape_incremental(data_dir, polling_structure=None):
    """
    Incremental scrape: for each pollster, fetch only the current page,
    detect new rows via content hash, and append them.

    Args:
        data_dir: directory containing per-pollster subdirectories with CSVs
        polling_structure: dict from extract_polling_structure(). If None,
                          will be fetched automatically.

    Returns:
        Summary dict with per-pollster results.
    """
    if polling_structure is None:
        polling_structure = extract_polling_structure()

    os.makedirs(data_dir, exist_ok=True)

    summary = {
        "mode": "incremental",
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "pollsters": {}
    }

    for institute, data in polling_structure.items():
        institute_dir = os.path.join(data_dir, institute.replace('/', '_'))
        os.makedirs(institute_dir, exist_ok=True)
        current_csv = os.path.join(institute_dir, 'current.csv')

        try:
            # 1. Scrape current page
            logger.info("Scraping %s current page...", institute)
            scraped_df = scrape_table(data['current'])
            if scraped_df is None:
                summary["pollsters"][institute] = {
                    "new_rows": 0, "total_rows": 0,
                    "error": "scrape_table returned None"
                }
                continue

            # 2. Hash on wide-format
            scraped_df = add_content_hash(scraped_df, institute)

            # 3. Load existing hashes (if any)
            known_hashes = set()
            if os.path.exists(current_csv):
                existing_df = _migrate_hashes_if_needed(current_csv, institute)
                known_hashes = set(existing_df['uuid'].dropna().unique())

            # 4. Filter to new rows (wide-format, before melt)
            scraped_hashes = set(scraped_df['uuid'].unique())
            new_hashes = scraped_hashes - known_hashes
            new_wide = scraped_df[scraped_df['uuid'].isin(new_hashes)]

            if new_wide.empty:
                total = len(known_hashes)
                logger.info("%s: no new polls (total: %d unique polls)", institute, total)
                summary["pollsters"][institute] = {
                    "new_rows": 0, "total_rows": total, "error": None
                }
                continue

            # 5. Convert new rows to long format and append
            new_long = convert_to_long_format(new_wide)

            if os.path.exists(current_csv):
                new_long.to_csv(current_csv, mode='a', header=False, index=False, encoding='utf-8')
            else:
                new_long.to_csv(current_csv, index=False, encoding='utf-8')

            n_new_polls = len(new_wide)
            total = len(known_hashes) + n_new_polls
            logger.info("%s: %d new polls appended (total: %d)", institute, n_new_polls, total)
            summary["pollsters"][institute] = {
                "new_rows": n_new_polls, "total_rows": total, "error": None
            }

        except Exception as e:
            logger.error("%s: failed — %s", institute, e)
            summary["pollsters"][institute] = {
                "new_rows": 0, "total_rows": 0, "error": str(e)
            }

    return summary


################################################################################
# Full scrape (--full flag)
################################################################################

def scrape_full(data_dir, polling_structure=None):
    """
    Full scrape: fetch all pages (current + historical) for every pollster.
    Overwrites existing CSVs. Uses content hashes instead of random UUIDs.

    Args:
        data_dir: directory to write per-pollster CSVs
        polling_structure: dict from extract_polling_structure(). If None,
                          will be fetched automatically.

    Returns:
        Summary dict with per-pollster results.
    """
    if polling_structure is None:
        polling_structure = extract_polling_structure()

    os.makedirs(data_dir, exist_ok=True)

    summary = {
        "mode": "full",
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "pollsters": {}
    }

    for institute, data in polling_structure.items():
        institute_dir = os.path.join(data_dir, institute.replace('/', '_'))
        os.makedirs(institute_dir, exist_ok=True)

        try:
            # Current page
            logger.info("Scraping %s current page...", institute)
            current_df = scrape_table(data['current'])
            if current_df is not None:
                current_df = add_content_hash(current_df, institute)
                current_long = convert_to_long_format(current_df)
                current_output = os.path.join(institute_dir, 'current.csv')
                current_long.to_csv(current_output, index=False, encoding='utf-8')
                logger.info("Scraped current data for %s (%d polls)", institute, len(current_df))
        except Exception as e:
            logger.error("Error scraping current data for %s: %s", institute, e)

        # Historical pages
        n_hist_polls = 0
        if 'historical' in data:
            all_historical_dfs = []
            for period, url in data['historical'].items():
                try:
                    df = scrape_table(url)
                    if df is not None:
                        all_historical_dfs.append(df)
                        logger.info("Scraped %s/%s (%d polls)", institute, period, len(df))
                except Exception as e:
                    logger.error("Error scraping %s/%s: %s", institute, period, e)

            if all_historical_dfs:
                historical_df = pd.concat(all_historical_dfs, ignore_index=True)
                historical_df = add_content_hash(historical_df, institute)
                historical_long = convert_to_long_format(historical_df)
                historical_output = os.path.join(institute_dir, 'historical.csv')
                historical_long.to_csv(historical_output, index=False, encoding='utf-8')
                n_hist_polls = len(historical_df)

        total = (len(current_df) if current_df is not None else 0) + n_hist_polls
        summary["pollsters"][institute] = {
            "new_rows": total, "total_rows": total, "error": None
        }

    return summary


# Backward compatibility alias
def scrape_all_polling_houses():
    """Deprecated alias for scrape_full with legacy paths."""
    import json
    data_dir = 'polling_data'
    os.makedirs(data_dir, exist_ok=True)

    with open('polling_data_structure.json', 'r', encoding='utf-8') as f:
        polling_structure = json.load(f)

    return scrape_full(data_dir, polling_structure)


# Keep legacy discovery functions available
def extract_all_polling_data():
    """Legacy wrapper around extract_polling_structure that also saves JSON."""
    import json
    structure = extract_polling_structure()
    with open('polling_data_structure.json', 'w', encoding='utf-8') as f:
        json.dump(structure, f, indent=4, ensure_ascii=False)
    return structure


def extract_date_range_links(url):
    """Legacy function for Allensbach date range links."""
    response = fetch_with_retry(url)
    soup = BeautifulSoup(response.text, 'html.parser')

    nav = soup.find('p', class_='navi')
    if not nav:
        return {}

    links = {}
    for link in nav.find_all('a'):
        href = link.get('href')
        text = link.text.strip()
        if href and 'allensbach' in href:
            full_url = urljoin(url, href)
            links[text] = full_url

    return links
