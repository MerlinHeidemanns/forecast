import pandas as pd
import re

if __name__ == "__main__":
    df = pd.read_csv('web/public/polling_data/Allensbach/current.csv')
    # print(list(df['Befragte']))
    print(df['Befragte'].astype(str).str.extract(r'([\d\.\?]+)', expand=False).str.replace("?.", "1").str.replace("?",
                                                                                                                  "0").str.replace(
        '.', '').astype(int))
    # str.replace(r"?", "0").str.replace(r"[^0-9]", ""))
