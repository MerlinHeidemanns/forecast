import hashlib
import logging
import time

import pandas as pd
import re

logger = logging.getLogger(__name__)

# Columns that are NOT party vote-share columns (used to identify party cols)
NON_PARTY_COLS = frozenset([
    'publishing_date', 'Zeitraum', 'Befragte', 'mode', 'survey_count',
    'sampling_start_day', 'sampling_end_day', 'period', 'uuid', 'pollster',
    'Empty',
])


################################################################################
# Hashing
################################################################################

def get_party_columns(df):
    """Return sorted list of columns that represent party vote shares."""
    return sorted([c for c in df.columns if c not in NON_PARTY_COLS])


def compute_row_hash(row_dict, pollster, party_cols):
    """
    Compute a deterministic SHA-256 hash for a single wide-format poll row.

    Hash input format:
        {pollster}|{publishing_date}|{sampling_start_day}|{sampling_end_day}|{survey_count}|{party1}={value1}|...

    Party key-value pairs are sorted alphabetically by party name.
    Missing/NA values render as "none".

    Note: This hash is for Python-side incremental dedup. It uses raw column
    names from scrape_table (e.g. "CDU/CSU", "publishing_date"). The R-side
    hash in canonicalize.R uses canonicalized names (e.g. "cdu_csu",
    "date_published") and serves a separate dedup purpose.
    """
    def _fmt(val):
        if val is None or (isinstance(val, float) and pd.isna(val)):
            return "none"
        if hasattr(val, 'isoformat'):
            return str(val.date()) if hasattr(val, 'date') else str(val)
        s = str(val)
        return "none" if s in ('', 'NaT', '<NA>', 'nan') else s

    party_parts = []
    for col in sorted(party_cols):
        party_parts.append(f"{col}={_fmt(row_dict.get(col))}")

    hash_input = "|".join([
        str(pollster),
        _fmt(row_dict.get('publishing_date')),
        _fmt(row_dict.get('sampling_start_day')),
        _fmt(row_dict.get('sampling_end_day')),
        _fmt(row_dict.get('survey_count')),
    ] + party_parts)

    return hashlib.sha256(hash_input.encode('utf-8')).hexdigest()


def add_content_hash(df, pollster):
    """
    Replace random UUIDs with deterministic content hashes.

    Operates on wide-format DataFrame (before melt to long format).
    Writes the SHA-256 hash into the 'uuid' column for backward compatibility.
    """
    party_cols = get_party_columns(df)
    hashes = []
    for _, row in df.iterrows():
        hashes.append(compute_row_hash(row.to_dict(), pollster, party_cols))
    df['uuid'] = hashes
    return df


def is_content_hash(uuid_val):
    """Check if a uuid value looks like a SHA-256 hex digest (64 hex chars, no hyphens)."""
    if not isinstance(uuid_val, str):
        return False
    return bool(re.match(r'^[0-9a-f]{64}$', uuid_val))


# Keep old function for backward compatibility but mark deprecated
def add_uuid(df):
    """Deprecated: use add_content_hash instead."""
    import uuid as _uuid
    logger.warning("add_uuid is deprecated; use add_content_hash(df, pollster) instead")
    df['uuid'] = df.apply(lambda _: str(_uuid.uuid4()), axis=1)
    return df


################################################################################
# HTTP helpers
################################################################################

def fetch_with_retry(url, max_retries=3, base_delay=2.0):
    """
    GET a URL with exponential backoff retry.

    Retries on HTTP 5xx, ConnectionError, and Timeout.
    Raises after final failure.
    """
    import requests

    last_exc = None
    for attempt in range(1, max_retries + 1):
        try:
            response = requests.get(url, timeout=30)
            if response.status_code >= 500 and attempt < max_retries:
                delay = base_delay * (2 ** (attempt - 1))
                logger.warning(
                    "%s returned %d (attempt %d/%d), retrying in %.0fs",
                    url, response.status_code, attempt, max_retries, delay
                )
                time.sleep(delay)
                continue
            response.raise_for_status()
            return response
        except (requests.ConnectionError, requests.Timeout) as exc:
            last_exc = exc
            if attempt < max_retries:
                delay = base_delay * (2 ** (attempt - 1))
                logger.warning(
                    "%s: %s (attempt %d/%d), retrying in %.0fs",
                    url, type(exc).__name__, attempt, max_retries, delay
                )
                time.sleep(delay)
            else:
                raise
        except requests.HTTPError:
            raise

    # Should not reach here, but just in case
    raise last_exc  # type: ignore


################################################################################
# Data format helpers (unchanged)
################################################################################

def convert_to_long_format(df):
    id_vars = ['uuid', 'publishing_date', 'Befragte', 'Zeitraum', 'mode', 'survey_count', 'sampling_start_day',
               'sampling_end_day']
    # Only include id_vars that actually exist in the dataframe
    id_vars = [c for c in id_vars if c in df.columns]
    value_vars = [col for col in df.columns if col not in id_vars]

    long_df = pd.melt(df, id_vars=id_vars, value_vars=value_vars, var_name='party', value_name='vote_share')
    return long_df


def clean_percentage(value):
    if pd.isna(value):
        return value
    try:
        cleaned = re.sub(r'\s+%|%', '', value).replace(',', '.').strip()
        return float(cleaned)
    except:
        return pd.NA


def get_numeric_columns(df):
    pattern = r'\d+(?:,\d+)?\s*%'
    numeric_cols = []
    for col in df.columns:
        if col not in ['publishing_date', 'Zeitraum', 'Befragte', 'mode', 'survey_count', 'period']:
            sample_values = df[col].dropna()
            if not sample_values.empty and isinstance(sample_values.iloc[0], str) and re.search(pattern,
                                                                                                sample_values.iloc[0]):
                numeric_cols.append(col)
    return numeric_cols


def clean_column_names(headers):
    clean_headers = []
    for header in headers:
        header = ' '.join(header.split())
        if 'CDU' in header and 'CSU' in header:
            header = 'CDU/CSU'
        elif header.strip() == '':
            header = 'Empty'
        header = re.sub(r'[^\w\s/]', '', header)
        clean_headers.append(header)
    return clean_headers


def adjust_date(x, release_date):
    try:
        start_match = re.search(r'(\d+)\.(\d+)[^0-9]+\d+\.\d+', x)
        end_match = re.search(r'(?:\?\?\.\?\?\.[^\d]+|bis\s*|(?:\d+\.\d+[^0-9]+))(\d+)\.(\d+)\.', x)
        release_date = pd.to_datetime(release_date)

        if not start_match:
            start_date = pd.NaT
        else:
            start_day, start_month = map(int, start_match.groups())
            start_year = release_date.year - 1 if start_month > release_date.month else release_date.year
            start_date = pd.to_datetime(f"{start_year}-{start_month:02d}-{start_day:02d}")

        if not end_match:
            end_date = pd.NaT
        else:
            end_day, end_month = map(int, end_match.groups())
            end_year = release_date.year - 1 if end_month > release_date.month else release_date.year
            end_date = pd.to_datetime(f"{end_year}-{end_month:02d}-{end_day:02d}")
        return start_date, end_date
    except:
        return pd.NaT, pd.NaT
