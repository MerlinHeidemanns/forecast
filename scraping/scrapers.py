import requests
from bs4 import BeautifulSoup
import pandas as pd
from urllib.parse import urljoin
import json
import os

from .utils import clean_percentage, get_numeric_columns, clean_column_names, adjust_date, add_uuid, \
    convert_to_long_format


def scrape_table(url):
    try:
        response = requests.get(url)
        response.raise_for_status()

        soup = BeautifulSoup(response.content, 'html.parser')
        table = soup.find("table", {"class": "wilko"})

        if not table:
            raise ValueError("No table found with class 'wilko'")

        thead = table.find("thead")
        if not thead:
            raise ValueError("No thead found in table")

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
            df[['sampling_start_day', 'sampling_end_day']] = (df.apply(
                lambda row: pd.Series(adjust_date(row['Zeitraum'], row['publishing_date'])),
                axis=1
            ))

        if 'Befragte' in df.columns:
            try:
                df['mode'] = df['Befragte'].str.extract(r'([A-Za-z]+)', expand=False).fillna('')
                df['survey_count'] = df['Befragte'].astype(str).str.extract(r'([\d\.\?]+)', expand=False).str.replace(
                    "?.", "1").str.replace("?", "0").str.replace('.', '').astype(int)
            except Exception as e:
                print(f"Warning: Error processing 'Befragte' column: {e}")

        numeric_cols = get_numeric_columns(df)
        for col in numeric_cols:
            df[col] = df[col].apply(clean_percentage)

        df = df.dropna(axis=1, how='all')

        return df

    except Exception as e:
        print(f"Error scraping URL {url}: {e}")
        return None


def extract_institute_links():
    url = "https://www.wahlrecht.de/umfragen/index.htm"
    try:
        response = requests.get(url)
        response.raise_for_status()
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

        with open('institute_links.json', 'w', encoding='utf-8') as f:
            json.dump(institutes, f, indent=4, ensure_ascii=False)

        return institutes

    except requests.RequestException as e:
        print(f"Error fetching URL: {e}")
        return {}


def extract_date_range_links(url):
    try:
        response = requests.get(url)
        response.raise_for_status()
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

        with open('date_range_links.json', 'w', encoding='utf-8') as f:
            json.dump(links, f, indent=4, ensure_ascii=False)

        return links

    except requests.RequestException as e:
        print(f"Error fetching URL: {e}")
        return {}


def extract_all_polling_data():
    institutes = extract_institute_links()
    nested_data = {}

    for institute, main_url in institutes.items():
        try:
            response = requests.get(main_url)
            response.raise_for_status()
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

            nested_data[institute] = data_ranges

        except requests.RequestException as e:
            print(f"Error fetching {institute} at {main_url}: {e}")
            continue

    with open('polling_data_structure.json', 'w', encoding='utf-8') as f:
        json.dump(nested_data, f, indent=4, ensure_ascii=False)

    return nested_data


def scrape_all_polling_houses():
    if not os.path.exists('polling_data'):
        os.makedirs('polling_data')

    with open('polling_data_structure.json', 'r', encoding='utf-8') as f:
        polling_structure = json.load(f)

    for institute, data in polling_structure.items():
        institute_dir = os.path.join('polling_data', institute.replace('/', '_'))
        if not os.path.exists(institute_dir):
            os.makedirs(institute_dir)

        try:
            current_df = scrape_table(data['current'])
            if current_df is not None:
                current_df = add_uuid(current_df)
                current_df = convert_to_long_format(current_df)
                current_output = os.path.join(institute_dir, 'current.csv')
                current_df.to_csv(current_output, index=False, encoding='utf-8')
                print(f"Scraped current data for {institute}")
        except Exception as e:
            print(f"Error scraping current data for {institute}: {e}")

        if 'historical' in data:
            all_historical_dfs = []
            for period, url in data['historical'].items():
                try:
                    df = scrape_table(url)
                    if df is not None:
                        all_historical_dfs.append(df)
                        print(f"Scraped {period} data for {institute}")
                except Exception as e:
                    print(f"Error scraping {period} data for {institute}: {e}")

            if all_historical_dfs:
                historical_df = pd.concat(all_historical_dfs, ignore_index=True)
                historical_df = add_uuid(historical_df)
                historical_df = convert_to_long_format(historical_df)
                historical_output = os.path.join(institute_dir, 'historical.csv')
                historical_df.to_csv(historical_output, index=False, encoding='utf-8')
