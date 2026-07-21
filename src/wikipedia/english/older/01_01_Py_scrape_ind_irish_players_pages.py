# -*- coding: utf-8 -*-
"""
Created on Sat Mar 30 22:30:09 2024

@author: David.OSullivan
"""

from bs4 import BeautifulSoup
import pandas as pd
import numpy as ny
import requests


def scrapp(url):
    response = requests.get(url)
    soup = BeautifulSoup(response.text, 'html.parser')
    tables = soup.find_all('table', class_='infobox')
    heading = soup.find(id="firstHeading")
    data = {}
    data["name"] = heading.text.strip()
    for table in tables:
        rows = table.find_all('tr')
        stage_type = None  # Initialize stage_type for each table
        for row in rows:
            header = row.find('th')
            if header:
                key = header.text.strip()
                values = []
                cells = row.find_all('td')
                for cell in cells:
                    value = cell.text.strip()
                    if value:
                        values.append(value)
                if len(cells) == 0:  # If no values, update stage_type with the header
                    stage_type = key
                if values:
                    if len(values) == 1:
                        data[key] = values[0]
                    else:
                        values.append(stage_type)
                        data[key] = values
    return data



scrapp(url)
# Assuming df is your DataFrame and 'Names' is the column with the names
# Define your scrapp function here
df = pd.read_csv('./data/IP.csv')
df

# Initialize an empty list to store the results
results = []
i = 1
# Iterate over each name in the DataFrame
for name in df['url']:
    # Apply the scrapp function and store the result
    result = scrapp(name)
    results.append(result)
    if (i % 1) == 0: print(f"Currently Finished {i} of {df.shape[0]}.")
    i = i + 1
    if i == 20: break

# You can then assign these results to a new column in the DataFrame
df['scrapped_data'] = results
df.to_pickle("./data/IP_scapped.pkl")



# =============================================================================
# 
# =============================================================================

# df = pd.read_pickle("./data/IP_scapped.pkl")


# def have_all_bits(test):
#     testnames = ["Date of birth", "Height", "Weight", "Position(s)"]
#     test1 = all(name in test for name in testnames)
#     testvalues = ["Senior career"]
#     test2 = any(value in test.values() for value in testvalues)
#     is_all_bits = test1 and test2
#     return is_all_bits


# df.iloc[1153] 


# # Applying the have_all_bits function and length of scrapped_data dictionary
# df['len'] = df['scrapped_data'].map(len)
# df['all_bits'] = df['scrapped_data'].map(have_all_bits)

# sum(df["all_bits"])
