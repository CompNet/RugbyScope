# -*- coding: utf-8 -*-
"""
Created on Wed Aug 28 08:48:06 2024

@author: David.OSullivan
"""


# =============================================================================
# what packages to use
# =============================================================================

from bs4 import BeautifulSoup
import pandas as pd
import requests
from io import StringIO
import json
import datetime
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


def scrap_rugby_wiki_standard(url, name):
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
        rugbybio_json = parse_rugby_player_info(st)
    else:
        # print("No 'infobox' tables found.")
        rugbybio_json = "fail"
    
    return rugbybio_json

def if_missing_infobox(name):
    text = """{
  "name": \"""" + str(name) + """\",
  "successfully_scrape": "no",
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
        "teams": [""],
        "apps": [null],
        "points": [null]
      }
    ],
    "international": [
      {
        "years": [""],
        "teams": [""],
        "apps": [null],
        "points": [null]
      }
    ]
  }
}
    """
    return text


def parse_rugby_player_info(data):
    """
    Parses rugby player information from a structured text and returns it as a JSON object.
    """
    # Initialize the output dictionary with the required schema
    output = {
        "name": "",
        "successfully_scrape": "yes",
        "date_of_birth": {
            "full": "",
            "date": ""
        },
        "place_of_birth": "",
        "height": {
            "meters": 0.0
        },
        "weight": {
            "kg": 0.0
        },
        "education": {
            "school": "",
            "university": ""
        },
        "position": [],
        "career": {
            "amateur": [],
            "senior_club": [],
            "international": []
        }
    }
    
    data = (data.replace('\u00A0', ' ')
                  .replace("—", "-")
                  .replace("–", "-")
                  .replace("\u2264", "")
                  .replace("≥", "")
                  .replace("?","0")
                  .replace("\xa0", " "))
                  
    lines = data.splitlines()
    current_section = ""
    in_valid_section = False
    
    for line in lines:
        # Remove unnecessary spaces and skip empty lines
        line = line.strip().lower()
        if not line:
            continue

        # Extract full name
        if "Full name" in line:
            output["name"] = line.split("|")[1].strip()

        # Extract date of birth
        if "date of birth" in line:
            dob_info = line.split("|")[1].strip()
            match = re.search(r'\(([\d-]+)\)', dob_info)
            if match:
                output["date_of_birth"]["full"] = dob_info
                output["date_of_birth"]["date"] = match.group(1)

        # Extract place of birth
        if "place of birth" in line:
            output["place_of_birth"] = line.split("|")[1].strip()

        # Extract height
        if "height" in line:
            height_info = line.split("|")[1].strip()
            match1 = re.search(r'([\d.]+) m', height_info)
            match2 = re.search(r'([\d.]+) cm', height_info)
            if match1:
                output["height"]["meters"] = match1.group(0)
            if match2:
                output["height"]["meters"] = match2.group(0)

        # Extract weight
        if "weight" in line:
            weight_info = line.split("|")[1].strip()
            match = re.search(r'(\d+) kg', weight_info)
            if match:
                output["weight"]["kg"] = match.group(0)

        # Extract education
        if line.startswith("school"):
            output["education"]["school"] = line.split("|")[1].strip()

        # Extract positions
        if "position(s)" in line:
            positions = line.split("|")[1].strip()
            output["position"] = [pos.strip() for pos in positions.split(',')]

        cl = False
        # Detect the start of a career section
        if line.startswith("amateur team(s)"):
            current_section = "amateur"
            cl = True
            in_valid_section = True
        elif line.startswith("senior career") or line.startswith("provincial / state sides"):
            current_section = "senior_club"
            cl = True
            in_valid_section = True
        elif line.startswith("international career"):
            current_section = "international"
            cl = True
            in_valid_section = True
        elif line.startswith("national sevens team"):
            current_section = "national_sevens"
            cl = False
            in_valid_section = False
        elif line.startswith("correct as"):
            in_valid_section = False  # Disable processing after correct as of
            
        # Only process lines in valid sections
        if in_valid_section and "|" in line and not line.startswith("years"):
            parts = [part.strip() for part in line.split("|")]
            if len(parts) == 4:
                years, team, apps, points = parts
                if apps == "":
                    apps = 0
                if points == "":
                    points = 0
                else:
                    points = points.strip("()")
                if years:
                    output["career"][current_section].append({
                        "years": years,
                        "teams": team.title(),
                        "apps": apps,
                        "points": points
                    })
    return output

# =============================================================================
# # Now we build the 
# =============================================================================

# Assuming df is your DataFrame and 'Names' is the column with the names
# Define your scrapp function here
df = pd.read_csv('./data/IP.csv')


# Iterate over each name in the DataFrame
failed_indices = []
for i in range(df.shape[0]-300,df.shape[0]):
    try:
        # i = 1
        url = df["url"].loc[i]
        name = df["name"].loc[i]

        # Apply the scrapp function and store the result
        result = scrap_rugby_wiki_standard(url, name)
        if result == "fail":
            continue
        else: 
            result["name"] = name
            result["url"] = url
            result["accessed_at"] = datetime.datetime.now().date().isoformat()
        pn = df['name'][i].replace('"', '')
        result = json.dumps(result, indent=4)
        
        
        # Save the result to a JSON file
        with open(f"./data/IrishPP/{i}_{pn}.json", "w", encoding='utf-8') as f:
            f.write(result)

        # Logging progress
        if (i % 1) == 0:
            print(f"Currently Finished {i+1} of {df.shape[0]}.")
            print(f"---- Perc: {round((i/df.shape[0]) * 100, 2)}%.")
            print(f"---- Name: {df['name'][i]}.")
    
    except Exception as e:
        # Log the failed index and the exception message
        failed_indices.append((i,name))
        print(f"Failed at index {i} with error: {e}")
print("Finished.")

# Optionally, save the list of failed indices for later use
with open("./failed_indices_IrishPP.txt", "w") as f:
    for index in failed_indices:
        f.write(f"{index}\n")

print(f"Failed indices: {failed_indices}")


