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
# import numpy as ny
import requests
import ollama

# =============================================================================
# define the functions, and the system prompt
# =============================================================================

def scrap_rugby_wiki_wllama(url):
    response = requests.get(url)
    soup = BeautifulSoup(response.text, 'html.parser')
    tables = soup.find_all('table', class_='infobox')
    st = ""
    if tables:
        infobox_table = tables[0]
        
        # Extract all text while preserving the table structure
        table_text = infobox_table.get_text(separator="\n", strip=True)
        st = table_text + ""
        # Output the unstructured text
        rugbybio_json = get_llama_cleaned(st)
    else:
        print("No 'infobox' tables found.")
        rugbybio_json = "Not found"
    
    return rugbybio_json

def get_llama_cleaned(text):
    response = ollama.chat(
        model="llama3",
        messages=[
            {
                'role': 'system', 
                'content': system_promt
            },
            {
                "role": "user",
                "content": text,
            },
        ],
    )
    return response['message']['content']

system_promt = """"You are an advanced AI assistant created to perform text extraction analysis on rugby player bios from a wiki. 
I need you to classify each text you receive and provide your analysis using the following JSON schema:: 
    {
  "name": "string",
  "date_of_birth": {
    "full": "string",
    "date": "string",
    "age": "integer"
  },
  "place_of_birth": "string",
  "height": {
    "meters": "number",
    "feet_inches": "string"
  },
  "weight": {
    "kg": "number",
    "lbs": "number",
    "stones_lbs": "string"
  },
  "education": {
    "school": "string",
    "university": "string"
  },
  "position": [
    "string"
  ],
  "current_team": "string",
  "career": {
    "amateur": [
      {
        "years": "string",
        "team": "string",
        "apps": "integer",
        "points": "integer"
      }
    ],
    "senior": [
      {
        "years": "string",
        "team": "string",
        "apps": "integer",
        "points": "integer"
      }
    ],
    "international": [
      {
        "years": "string",
        "team": "string",
        "apps": "integer",
        "points": "integer"
      }
    ]
  }
} 
Always respond with a valid JSON object adhering to this schema. Do not include any other text or messages in your response.  Exclude markdown.
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
# scrap_rugby_wiki_wllama(url)
# Initialize an empty list to store the results
results = []
i = 1
# Iterate over each name in the DataFrame
for name in df['url']:
    # Apply the scrapp function and store the result
    result = scrap_rugby_wiki_wllama(name)
    results.append(result)
    if (i % 1) == 0: 
        print(f"Currently Finished {i} of {df.shape[0]}.")
        print(f"---- Name: {df['name'][i]}.")
        print(f"---- Result: {result}.")
    i = i + 1


# You can then assign these results to a new column in the DataFrame
df['scrapped_data'] = results
df.to_pickle("./data/IP_scapped_wllama.pkl")

