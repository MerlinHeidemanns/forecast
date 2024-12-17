import pandas as pd
import re
import uuid


def convert_to_long_format(df):
    id_vars = ['uuid', 'publishing_date', 'Befragte', 'Zeitraum', 'mode', 'survey_count', 'sampling_start_day',
               'sampling_end_day']
    value_vars = [col for col in df.columns if col not in id_vars]

    long_df = pd.melt(df, id_vars=id_vars, value_vars=value_vars, var_name='party', value_name='vote_share')
    return long_df


def add_uuid(df):
    df['uuid'] = df.apply(lambda _: str(uuid.uuid4()), axis=1)
    return df


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
