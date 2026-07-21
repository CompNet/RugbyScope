# -*- coding: utf-8 -*-
"""
Created on Mon Aug 19 17:57:10 2024

@author: David.OSullivan
"""

# =============================================================================
# what packages to use
# =============================================================================

from bs4 import BeautifulSoup
import pandas as pd
import requests
import ollama
from io import StringIO

# import json
# import numpy as ny

# =============================================================================
# define the functions, and the system prompt
# =============================================================================


def get_infobox_tables(table):
    """
    Returns the content of a table as a string, handling nested tables and formatting.
    """
    output = StringIO()
    rows = table.find_all('tr')
    for row in rows:
        # Handle nested tables
        nested_tables = row.find_all('table')

        if nested_tables:
            for nested_table in nested_tables:
                output.write("\n")
                output.write(get_infobox_tables(nested_table))  # Recursively handle nested tables
            continue

        cols = row.find_all(['td', 'th'])
        col_texts = [col.get_text(strip=True) for col in cols]
        output.write(' | '.join(col_texts) + '\n')
    
    output.write("\n")
    return remove_duplicate_lines(output.getvalue())

# def get_infobox_tables(tables):
#     """
#     Returns each table in the infobox as a string, handling nested tables.
#     """
#     output = StringIO()
#     for i, table in enumerate(tables, start=1):
#         output.write(get_table(table))
    
#     return  

def remove_duplicate_lines(text):
    """
    Removes duplicate lines from the given text while maintaining the order of first occurrences.
    """
    lines = text.splitlines()
    unique_lines = list(dict.fromkeys(lines))  # Remove duplicates while maintaining order
    return "\n".join(unique_lines)

def scrape_rough(soup):
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

def scrap_rugby_wiki_wllama(url, name):
    response = requests.get(url)
    soup = BeautifulSoup(response.text, 'html.parser')
    tables = soup.find_all('table', class_='infobox')
    # st = ""
    if tables:
        infobox_table = tables[0]
        # Extract all text while preserving the table structure
        # table_text = infobox_table.get_text(separator="\n", strip=True)
        # st = table_text + ""
        st = get_infobox_tables(infobox_table)
        # Output the unstructured text
        rugbybio_json = get_llama_cleaned(st)
    else:
        print("No 'infobox' tables found.")
        rugbybio_json = if_missing_infobox(name)
    
    return rugbybio_json

def get_llama_cleaned(text):
    promt_text = "here is the text: " + str(text)
    # promt_text = "here is the text: " + str(text) + "; Here is the rough scraped table: " + str(sr)
    response = ollama.chat(
        model="llama3",
        messages=[
            {
                'role': 'system', 
                'content': system_promt
            },
            {
                "role": "user",
                "content": promt_text,
            },
        ],
    )
    return response['message']['content']

def if_missing_infobox(name):
    text = """{
  "name": \"""" + str(name) + """\",
  "successfully_scrape": "No",
  "date_of_birth": {
    "full": "",
    "date": "",
    "age": null
  },
  "place_of_birth": "",
  "height": {
    "meters": null,
    "feet_inches": ""
  },
  "weight": {
    "kg": null,
    "lbs": null,
    "stones_lbs": ""
  },
  "education": {
    "school": "",
    "university": ""
  },
  "position": [
    ""
  ],
  "current_team": "",
  "career": {
    "amateur": [
      {
        "years": [""],
        "team": [""],
        "apps": [null],
        "points": [null]
      }
    ],
    "senior_club": [
      {
        "years": [""],
        "team": [""],
        "apps": [null],
        "points": [null]
      }
    ],
    "international": [
      {
        "years": [""],
        "team": [""],
        "apps": [null],
        "points": [null]
      }
    ]
  }
}
    """
    return text


system_promt = """"
You are an advanced AI assistant designed to extract and structure information from rugby player biographies sourced from a wiki. The biographies typically follow a sequence starting with biographical information, followed by position, amateur clubs, senior clubs, and finally, international career.

Data Structure and Classification Requirements:
Input Format: The input will consist of a player's biography in text form. 
The data within each career section (amateur, senior clubs, international) is usually organized by teams, starting with years, teams, apps and points. 
Pay careful attention to having the correct number of years and ensure accuracy when extracting and organizing the data in the correct order.
Output Format: data should be output in JSON format, following this schema:
JSON Validity: Ensure that the JSON output is valid. Every opening { or [ properly closed by a corresponding } or ].
No Extra Text: Only return the JSON object. Do not include any additional text, explanations, or markdown in your response with the JSON. 

{
  "name": "string",
  "successfully_scrape": "Yes",
  "date_of_birth": {
    "full": "string",
    "date": "string"
  },
  "place_of_birth": "string",
  "height": {
    "meters": "number"
  },
  "weight": {
    "kg": "number",
  },
  "education": {
    "school": "string",
    "university": "string"
  },
  "position": ["string"],
  "current_team": "string",
  "career": {
    "amateur": [
      {
        "years": ["string"],
        "team": ["string"],
        "apps": ["integer"],
        "points": ["integer"]
      }
    ],
    "senior_club": [
      {
        "years": ["string"],
        "team": ["string"],
        "apps": ["integer"],
        "points": ["integer"]
      }
    ],
    "international": [
      {
        "years": ["string"],
        "team": ["string"],
        "apps": ["integer"],
        "points": ["integer"]
      }
    ]
  }
}
"""

# response = ollama.chat(model='llama3', messages=[{'role': 'system', 'content': system_promt}])
# response
# =============================================================================
# # Now we build the 
# =============================================================================

# Assuming df is your DataFrame and 'Names' is the column with the names
# Define your scrapp function here
df = pd.read_csv('./data/IP.csv')

# url = df.iloc[1000]["url"]
# scrap_rugby_wiki_wllama(url, name)
# Initialize an empty list to store the results

# Iterate over each name in the DataFrame
failed_indices = []
for i in range(df.shape[0]):
    try:
        url = df["url"][i]
        name = df["name"][i]

        # Apply the scrapp function and store the result
        result = scrap_rugby_wiki_wllama(url, name)
        pn = df['name'][i].replace('"', '')

        # Save the result to a JSON file
        with open(f"./data/IrishPP/{i}_{pn}.json", "w", encoding='utf-8') as f:
            f.write(result.replace('\u00A0', ' ')
                          .replace("—", "-")
                          .replace("–", "-")
                          .replace("\u2264", "")
                          .replace("≥", "")
                          .replace("?"," "))

        # Logging progress
        if (i % 1) == 0:
            print(f"Currently Finished {i+1} of {df.shape[0]}.")
            print(f"---- Perc: {round((i/df.shape[0]) * 100, 2)}%.")
            print(f"---- Name: {df['name'][i]}.")
    
    except Exception as e:
        # Log the failed index and the exception message
        failed_indices.append(i)
        print(f"Failed at index {i} with error: {e}")
print("Finished.")

# Optionally, save the list of failed indices for later use
with open("./failed_indices_IrishPP.txt", "w") as f:
    for index in failed_indices:
        f.write(f"{index}\n")

print(f"Failed indices: {failed_indices}")

# # You can then assign these results to a new column in the DataFrame
# df['scrapped_data'] = results
# df.to_pickle("./data/IP_scapped_wllama.pkl")

