# -*- coding: utf-8 -*-
"""
Created on Sun Jan 12 17:28:01 2025

@author: David.OSullivan

contains all the packages and functions required for scraping wikipedia

"""


from bs4 import BeautifulSoup
import pandas as pd
import requests
from io import StringIO
import json
import datetime
import re
import os
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
        rugbybio_json = add_team_url(rugbybio_json, infobox_table)
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

def add_team_url(json_data, infobox_table, car_stages=["amateur", "senior_club", "international"]):
    for car_stage in car_stages:
        team_stints = json_data["career"][car_stage]
        if len(team_stints) > 0:
            for team_stint in team_stints:
                search_term = team_stint["teams"]
                search_term = search_term[1:] if search_term.startswith("→") else search_term
                search_term = search_term.split("/")[0]
                # Find the link in the infobox table that matches the team name
                link = infobox_table.find('a', string=re.compile(re.escape(search_term), re.IGNORECASE))
                if link and link.get("href"):
                    # Add the href to the JSON data
                    team_stint["team_link"] = link["href"]
    return json_data



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
                    points = extract_number(points, full_clean= False)
                    # points = points.strip("()")
                if years:
                    output["career"][current_section].append({
                        "years": years,
                        "teams": team.title(),
                        "apps": apps,
                        "points": points
                    })
    return output


def pad_vec_start(vec, true_len, w = ""):
    x = true_len - len(vec)
    if x > 0:
        for j in range(x): 
            vec.insert(0, w)
    return vec

def check_if_standard_failed(json_data):
    car = json_data.get("career").get("international")
    if len(car) > 0:
        points = car[0].get("points")
        try: 
            points = int(points)
            return True
        except:
            points = car[0].get("points")
            return False
    else:
        return True

def combine_comma_separated_values(dates_list):
    # Initialize a new list to hold the combined results
    combined_dates = []
    # Initialize index
    i = 0
    # Iterate through the list using a while loop
    while i < len(dates_list):
        if dates_list[i] == ', ':
            # Combine the previous, current (comma), and next elements
            combined_dates[-1] = combined_dates[-1] + "-" + dates_list[i + 1]
            i += 2  # Skip the next element as it has been combined
        else:
            # Append the current element if not a comma
            combined_dates.append(dates_list[i])
            i = i + 1  # Move to the next element
    return combined_dates


def extract_number(input_text, full_clean = True):
    if full_clean == True:
        # Remove text in square brackets
        input_text = re.sub(r'1 \(not recognised as an official test cap\)', '', input_text)
        input_text = re.sub(r'\d+ \(0 tests\)', '', input_text)
        input_text = re.sub(r'\(41t\)', '41', input_text)
        input_text = re.sub(r'citation needed', '', input_text)
        input_text = re.sub(r'\(60\(12t\)\)', '60', input_text)
        input_text = re.sub(r'28\?', '28', input_text)
        input_text = re.sub(r'>55', '55', input_text)
        input_text = re.sub(r'44 \(\?\)', '44', input_text)
        input_text = re.sub(r'\(11t\)', '11', input_text)
        input_text = re.sub(r'\(\?\?\)', '', input_text)
        input_text = re.sub(r'\?\?', '', input_text)

        
        input_text = re.sub(r'\[.*?\]', '', input_text)
        input_text = re.sub(r'[Uu]nknown', '', input_text)
        input_text = re.sub(r'[Pp]ts', '', input_text)
        input_text = re.sub(r'[Dd]rop', '', input_text)
        input_text = re.sub(r'[Pp]ens', '', input_text)
        input_text = re.sub(r'[Cc]onv', '', input_text)
        input_text = re.sub(r'[Tt]ries', '', input_text)
        input_text = re.sub(r'[Tt]ry', '', input_text)
        input_text = re.sub(r'[Tt]ests', '', input_text)
        input_text = re.sub(r'[Gg]ames', '', input_text)
        input_text = re.sub(r't\)', '\)', input_text)
        input_text = re.sub(r't', '', input_text)
        
        input_text = re.sub(r'gls', '', input_text)
        input_text = re.sub(r'gfm', '', input_text)
        input_text = re.sub(r'25 from 5', '', input_text)
        input_text = re.sub(r'goal', '', input_text)
        
        
        input_text = re.sub(r';', '', input_text)
        input_text = re.sub(r':', '', input_text)
        input_text = re.sub(r'5t+10', '5', input_text)
        input_text = re.sub(r']', '', input_text)
        input_text = re.sub(r'\[', '', input_text)
        input_text = re.sub(r'–', '', input_text)
        input_text = re.sub(r' ', '', input_text)
        input_text = re.sub(r'\?\(\?\)', '', input_text)
        input_text = re.sub(r'\?\?', '', input_text)
        input_text = re.sub(r'(-)', '', input_text)
        input_text = re.sub(r'\(\(\)\)', '', input_text)
        # input_text = re.sub(r'...', '', input_text)
        input_text = re.sub(r'\.\.', '', input_text)
        input_text = re.sub(r'…', '', input_text)
        
        input_text = re.sub(r'(15june2021)', '', input_text)
        input_text = re.sub(r'Twoersions', '', input_text)
        input_text = re.sub(r'manager', '', input_text)
        input_text = "41" if input_text == "(41" else input_text
        input_text = "41" if input_text == "41)" else input_text
        input_text = re.sub(r'75\u200a150', '75', input_text)
        input_text = re.sub(r'>100', '100', input_text)
        input_text = re.sub(r'1es10', '10', input_text)
        input_text = re.sub(r'5\(andgeorgiaworldcup\)', '5', input_text)
        input_text = re.sub(r'duikers/b', '', input_text)
        input_text = re.sub(r'our', '', input_text)
        input_text = re.sub(r'\(155\(31\\\)\)', '155', input_text)
        input_text = re.sub(r'\(120\(24\\\)\)', '120', input_text)
        input_text = re.sub(r'3=1', '3', input_text)
        input_text = re.sub(r'12\(seasoninerrupeddueocovid19\)', '12', input_text)
        input_text = re.sub(r'43apps10in.n.', '43', input_text)
        input_text = re.sub(r'102T', '10', input_text)
        input_text = re.sub(r'5namens', '5', input_text)
        input_text = re.sub(r'202nds', '20', input_text)
        input_text = re.sub(r'n/a', '0', input_text)
        input_text = re.sub(r'34\(\?\)', '34', input_text)
        input_text = re.sub(r'—', '0', input_text)
        input_text = re.sub(r'duikers', '0', input_text)
        input_text = re.sub(r'24a', '24', input_text)
        input_text = re.sub(r'\.', '0', input_text)
    
    # Match number in brackets first
    match_brackets = re.search(r"\((\d+)\)", input_text)
    if match_brackets:
        return int(match_brackets.group(1))
    # Match a standalone number if no brackets
    match_number = re.fullmatch(r"\d+", input_text)
    if match_number:
        return int(match_number.group())    
    # Otherwise return the entire input
    return input_text


def find_career_data_in_row(row):
    try: 
        # Extract the relevant columns
        dates = row.find('th', class_='infobox-label').get_text(separator="\n")
        teams = row.find('td', class_='infobox-data infobox-data-a').get_text(separator="\n").splitlines()
        # handling some very strange cases
        if "\n[\n2\n]\n" in dates: 
            dates = re.sub(r'\n\[\n\d+\n\]\n', '', dates)
            teams_new = []
            for team in teams:
                if team not in ["", " "]:
                    teams_new.append(team)
            teams = teams_new
        dates = dates.splitlines()
    
        apps = row.find('td', class_='infobox-data infobox-data-b').get_text(separator="\n").splitlines()
        points = row.find('td', class_='infobox-data infobox-data-c').get_text(separator="\n").splitlines()
        
        
        # Clean up points list (remove empty strings and parentheses)
        points = [p.replace("(", "") for p in points]
        points = [p.replace(")", "") for p in points]
        
        # again one person has a strange date
        dates = [d.replace("19xx-", "-") for d in dates]
        
        dates = combine_comma_separated_values(dates)
        teams = [team for team in teams if team == '' or team.strip()]
        
        teams = pad_vec_start(teams, len(dates))
        apps = pad_vec_start(apps, len(dates))
        points = pad_vec_start(points, len(dates))
    
        json_career_data = []
        for i in range(len(dates)):
            json_career_data.append({"years": dates[-(i+1)], "teams": teams[-(i+1)], 
                                     "apps": apps[-(i+1)], "points": points[-(i+1)]})
        return json_career_data
    except AttributeError:
        return []



def scrape_wiki_alt(url):
    response = requests.get(url)
    soup = BeautifulSoup(response.text, 'html.parser')
    tables = soup.find_all('table', class_='infobox')
    # st = ""
    if tables:
        infobox_table = tables[0]
        # Extract all text while preserving the table structure
        # table_text = infobox_table.get_text(separator="\n", strip=True)
        # st = table_text + ""
        
        rows = infobox_table.find_all('tr')
        rugbybio_json = get_career_alt(rows)
        rugbybio_json = add_team_url(rugbybio_json, infobox_table)
    return rugbybio_json

def get_career_alt(rows):
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

    # lines = data.splitlines()
    current_section = ""
    in_valid_section = False
    # for row in rows: 
    for row in rows:
        cols = row.find_all(['td', 'th'])
        if len(cols) == 0:
            continue
        col_texts = [col.get_text(strip=True) for col in cols][0]
        line = col_texts.lower()
        
        line = (line.replace('\u00A0', ' ')
                      .replace("—", "-")
                      .replace("–", "-")
                      .replace("\u2013", "-")
                      .replace("\u2264", "")
                      .replace("≥", "")
                      .replace("?","0")
                      .replace("\xa0", " "))
        
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
        if in_valid_section and not "(Points)" in row and not cl:
            car = find_career_data_in_row(row)
            if len(car) !=0 and car[0]["teams"] != "Team":
                for c in car:
                    output["career"][current_section].append(c)
    # output = json.loads(output)
    return output



def check_car(json_data, car_stages = ["amateur","senior_club","international"]):
    for car_stage in car_stages:
        car = json_data.get("career").get(car_stage)
        if len(car) > 0:
            points = car[0].get("points")
            # if points == 0: points = "0"
            points = str(points)
            points = points.replace(",","")
            points = points.replace("()","")
                
            date = car[0]["years"]
            match = re.search(r'\d{6}|\d{8}', date)
            if points == "":
                points = "0"
                
            if points.isdigit() and not match:
                points = int(points)
            else:
                url = json_data.get("url")
                json_data_us = scrape_wiki_alt(url)
                json_data["career"][car_stage] = json_data_us["career"][car_stage]
    return json_data


def change_car_value_to_int(json_data, 
                            car_stages = ["amateur","senior_club","international"], 
                            values = ["apps", "points"]):
    car = json_data.get("career")
    for car_stage in car_stages:
        for value in values:
            selected_car = car[car_stage]
            if len(selected_car) > 0:
                for i in range(len(selected_car)):
                    selected_value = selected_car[i][value]
                    if isinstance(selected_value, str):
                        selected_value = selected_value.replace(",","").strip().replace("+","")
                        selected_value = extract_number(selected_value) # recover the number of brackets are present
                    if selected_value in ["", "?", "(0)", "()","-", ")"]:
                        selected_value = 0
                    json_data["career"][car_stage][i][value] = int(selected_value)   
    return json_data

def convert_height(json_data):
    height_str = json_data["height"]["meters"]
    
    if type(height_str) == type(0.0):
        return json_data
    
    if len(height_str) > 0:
        # Check if the string contains "m" for meters
        if 'cm' in height_str:
            height_cm = float(height_str.replace('cm', '').strip())
            height = height_cm / 100  # Convert centimeters to meters
        elif 'm' in height_str:
            height = float(height_str.replace('m', '').strip())
        # Check if the string contains "cm" for centimeters
    else:
        height = 0
    json_data["height"]["meters"] = height
    return json_data

def convert_weight(json_data):
    weight_str = json_data["weight"]["kg"]
    
    if type(weight_str) == type(0.0):
        return json_data
    
    if len(weight_str) > 0:
        # Remove the "kg" and any surrounding whitespace, then convert to float
        weight = float(weight_str.replace('kg', '').strip())
        json_data["weight"]["kg"] = weight
    else: 
        json_data["weight"]["kg"] = 0
    return json_data


def clean_all_text_fields(json_data):
    
    def replace_text(text):
        text_to_replace = [('\u00A0', ' '),("—", "-"),("–", "-"),("\u2013", "-"),
                           ("\u2264", ""), ("≥", ""), ("?",""),("\xa0", " "),
                           ("—","-"),("−","-")]
        for i in range(len(text_to_replace)):
            text.replace(text_to_replace[i][0], text_to_replace[i][1])
        # remove square brackets (references)
        text = re.sub(r'\[.*?\]', '', text)
        return text
    
    keys = ["name", "successfully_scrape", "place_of_birth"]
    for key in keys:
        json_data["name"] = json_data["name"].strip()
        json_data["name"] = replace_text(json_data["name"])
        
        
    json_data["education"]["school"] = json_data["education"]["school"].strip()
    json_data["education"]["school"] = replace_text(json_data["education"]["school"])
    json_data["education"]["university"] = json_data["education"]["university"].strip()
    json_data["education"]["university"] = replace_text(json_data["education"]["university"])
    
    pos = json_data["position"]
    if len(pos)>0:
        for i in range(len(pos)): 
            json_data["position"][i] = json_data["position"][i].strip()
            json_data["position"][i] = replace_text(json_data["position"][i])
        
    
    car_stages = ["amateur", "senior_club", "international"]
    for car_stage in car_stages:
        car = json_data["career"][car_stage]
        if len(car) > 0: 
            for i in range(len(car)):
                text_info = ["years","teams"]
                for info in text_info:
                    json_data["career"][car_stage][i][info] = car[i][info].strip().lower()
                    json_data["career"][car_stage][i][info] = replace_text(json_data["career"][car_stage][i][info])
    return json_data


def create_directory(directory):
    """
    Creates a directory if it doesn't already exist.

    Parameters:
    directory (str): The path of the directory to create.
    """
    if not os.path.exists(directory):
        os.makedirs(directory)
        print(f"Directory '{directory}' created.")
    else:
        print(f"Directory '{directory}' already exists.")

def combine_json_files(input_files, output_file):
    """
    Combines multiple JSON files into a single JSON file.

    Parameters:
    input_files (list): List of paths to the input JSON files.
    output_file (str): Path to the output JSON file.
    """
    combined_data = []

    for file in input_files:
        with open(file, 'r',encoding='utf-8') as f:
            data = json.load(f)
            combined_data.append(data)

    with open(output_file, 'w') as f:
        json.dump(combined_data, f, indent=4)

    print(f"Combined JSON data written to '{output_file}'")


def list_files_in_directory(directory_path):
    # Get a list of all files and directories in the specified directory
    files_and_dirs = os.listdir(directory_path)
    
    # Filter out only files
    files = [f for f in files_and_dirs if os.path.isfile(os.path.join(directory_path, f))]
    
    return files


# =============================================================================
# scraping function using just the url
# =============================================================================

# def scrape_wiki_profile(url_df, output_dir = "./data/profiles/"):
#     """
#     Take in a pandas data frame that contains a col colled 'url' and tries to scape those profiles
#     Parameters
#     ----------
#     url_df : Pandas data frame.
#         DESCRIPTION.
#     output_dir : String, optional
#         Where should the output be placed. The default is "./data/profiles/".
# 
#     Returns
#     -------
#     None.
# 
#     """
#     # set these up for the team you want
#     
#     # set these up for the team you want
#     # nation = players_url_df["nation"].iloc[nation_i]
#     # output_dir = f"./data/{nation}/"
#     # player_profiles = f"./data/profile_links_{nation}.csv"
#     
#     # setup the folders
#     create_directory(output_dir)
#     first_scrape_out_dir = f"{output_dir}PP/"
#     create_directory(first_scrape_out_dir)
#     cleaned_json_out_dir = f"{output_dir}PP_cleaned/"
#     create_directory(cleaned_json_out_dir)
#     output_json = f"{output_dir}PP_combined_profile.json"
#     
#     # =============================================================================
#     # First scraping method:  
#     # =============================================================================
# 
#     # Assuming df is your DataFrame and 'Names' is the column with the names
#     # Define your scrapp function here
#     df = url_df
#     
#     # Iterate over each name in the DataFrame
#     failed_indices = []
#     for i in range(0, df.shape[0]):
#         try:
#             # i = 1
#             url = df["url"].loc[i]
#             # name = df["name"].loc[i]
#             name = "none"
# 
#             # Apply the scrapp function and store the result
#             result = scrap_rugby_wiki_standard(url, name)
#             if result == "fail":
#                 continue
#             else: 
#                 result["name"] = name
#                 result["url"] = url
#                 result["accessed_at"] = datetime.datetime.now().date().isoformat()
#             pn = df['name'][i].replace('"', '')
#             result = json.dumps(result, indent=4)
#             
# 
#             # Save the result to a JSON file
#             with open(f"{first_scrape_out_dir}{i}_{pn}.json", "w", encoding='utf-8') as f:
#                 f.write(result)
# 
#             # Logging progress
#             if (i % 1) == 0:
#                 print(f"FS: Currently Finished {i+1} of {df.shape[0]}.")
#                 print(f"---- Perc: {round((i/df.shape[0]) * 100, 2)}%.")
#                 print(f"---- Url: {df['url'][i]}.")
#         
#         except Exception as e:
#             # Log the failed index and the exception message
#             failed_indices.append((i))
#             print(f"Failed at index {i} with error: {e}")
#     print("Finished.")
# 
#     # print any failed indices to files
#     if(len(failed_indices) > 0 ): 
#         # Optionally, save the list of failed indices for later use
#         with open(f"{output_dir}/failed_indices.txt", "w") as f:
#             for index in failed_indices:
#                 f.write(f"{index}\n")
#     print(f"Failed indices: {failed_indices}")
# 
#     # =============================================================================
#     # Second scraping method
#     # =============================================================================
# 
#     # Some of the wiki's wont have scraped properly, the following checks for this
#     # and if it has failed it used a differt method (works line by line).
# 
#     files = list_files_in_directory(first_scrape_out_dir)
#     files = [f for f in files if f != 'desktop.ini']
#     data_list = []
#     for i in range(len(files)):
#         # Read in the json
#         with open(first_scrape_out_dir + files[i], 'r', encoding='utf-8', errors='replace') as file:
#             data = file.read()
#         json_data = json.loads(data) # load in the first method
#         json_data = check_car(json_data) # check and rescrape if requred
#         
#         # with open("./data/IrishPP_all/" + files[i], "w", encoding='utf-8') as f:
#         #     json.dump(json_data, f, indent=4)
#         # Save the result to a JSON file
#         results = json.dumps(json_data, indent=4, ensure_ascii=False)
#         with open(first_scrape_out_dir + files[i], "w", encoding='utf-8') as f:
#             f.write(results)
#             
#         # data_list.append({'index': i, 'type': str(type(points)), 
#         #                   'points': points, "url": json_data.get("url")})
#         # Logging progress
#         if (i % 1) == 0:
#             print(f"SS: Currently Finished {i+1} of {len(files)}.")
#             print(f"---- Perc: {round((i/len(files)) * 100, 2)}%.")
#             print(f"---- Name: {json_data['name']}.")
# 
#     # df = pd.DataFrame(data_list)
#     # # Display the DataFrame
#     # df_str = df[df['type'] == "<class 'str'>"]
# 
# 
#     # =============================================================================
#     # Standardise the data
#     # =============================================================================
# 
#     # Some of the data will have some formating errors in it.(i.e., meters vs cm)
#     # The following reformats the data and cleans up the data. 
# 
#     files = list_files_in_directory(first_scrape_out_dir)
#     files = [f for f in files if f not in ('desktop.ini',"777_Brian McKechnie.json")]
#     for i in range(len(files)):
#         # try: 
#             # Read in the json
#         with open(first_scrape_out_dir + files[i], 'r', encoding='utf-8', errors='replace') as file:
#             data = file.read()
#         json_data = json.loads(data)
#         
#         json_data = change_car_value_to_int(json_data)
#         json_data = convert_weight(json_data)
#         json_data = convert_height(json_data)
#         json_data = clean_all_text_fields(json_data)
#         # with open("./data/IrishPP_all/" + files[i], "w", encoding='utf-8') as f:
#         #     json.dump(json_data, f, indent=4)
#         # Save the result to a JSON file
#         results = json.dumps(json_data, indent=4, ensure_ascii=False)
#         with open(cleaned_json_out_dir + files[i], "w", encoding='utf-8') as f:
#             f.write(results)
#         # except: 
#         #     print(f"failed in {i}.")
#         #     break
#             
#         # Logging progress
#         if (i % 1) == 0:
#             print(f"ND: Currently Finished {i+1} of {len(files)}.")
#             print(f"---- Perc: {round((i/len(files)) * 100, 2)}%.")
#             print(f"---- Name: {json_data['name']}.")
# 
# 
#     # =============================================================================
#     # combine all the json files together
#     # =============================================================================
# 
#     files = list_files_in_directory(cleaned_json_out_dir)
#     files = [f for f in files if f != 'desktop.ini']
#     files_paths = []
#     for file in files:
#         files_paths.append(cleaned_json_out_dir + file)
#     combine_json_files(files_paths, output_json)
# 

# =============================================================================
# functions for converting json to csv file
# =============================================================================


# def list_files_in_directory(directory_path):
#     # Get a list of all files and directories in the specified directory
#     files_and_dirs = os.listdir(directory_path)
    
#     # Filter out only files
#     files = [f for f in files_and_dirs if os.path.isfile(os.path.join(directory_path, f))]
    
#     return files

# def clean_json_file(input_file_path, output_file_directory, output_file_name):
    
#     # Ensure the output directory exists
#     os.makedirs(output_file_directory, exist_ok=True)
    
#     # Read in the json
#     with open(input_file_path, 'r', encoding='utf-8', errors='replace') as file:
#         data = file.read()

#     try: 
#         # Use regex to find the JSON part and remove bracket from data part
#         json_data = re.search(r'{.*}', data, re.DOTALL).group(0)
#         json_data = clean_brackets_in_json(json_data)
#         # Parse the JSON to ensure it's valid
#         # this should work without error
#         parsed_data = json.loads(json_data)
#     except:
#         print("Trying adding closing bracket.")
#         try:
#             json_data = json_data + "\n}"
#             parsed_data = json.loads(json_data)
#             print("Added closing bracket succfully.")
#         except: 
#             print("Adding closing bracket failed! Savind file name.")
#             return input_file_path

#     # Define the output file path
#     output_file_path = os.path.join(output_file_directory, output_file_name)
    
#     parsed_data = convert_apps_and_points_to_numeric(parsed_data)
    
#     # Write the cleaned JSON data to the output file
#     with open(output_file_path, 'w') as output_file:
#         json.dump(parsed_data, output_file)
#     print(f"Cleaned JSON data has been saved to {output_file_path}")
#     return "Success"
    

def convert_apps_and_points_to_numeric(data):
    if isinstance(data, dict):
        for key, value in data.items():
            if key in ['apps', 'points']:
                new_values = []
                if isinstance(value, list): 
                    for v in value: 
                        new_values.append(try_convert_to_numeric(v))
                        data[key] = new_values
                else: 
                    data[key] = try_convert_to_numeric(value)
            else:
                convert_apps_and_points_to_numeric(value)
    elif isinstance(data, list):
        for item in data:
            convert_apps_and_points_to_numeric(item)
    return data

def try_convert_to_numeric(value):
    # If it's already a number, return it as is
    if isinstance(value, int):
        return value
    # If it's a string, try to convert to int or float
    try:
        return int(value)
    except: 
        return None  # Return the original value if conversion fails

    
def get_car_years(json_data, car_stage):
    years = []
    results = []
    car = json_data["career"][car_stage]
    for i in range(len(car)):
        years.append(car[i]["years"].replace("?", "").replace("c.",""))
    for year in years: 
        year_split = re.split(r'[-,–,—,−,/]', year)
        for ys in year_split: 
            ys = ys.strip()
            if ys != "":
                results.append(int(ys))
    return(results)

def is_any_years_end(json_data, car_stages = ["senior_club","international"]):
    results = []
    end_car = ("-", "–", "—", "−")
    for car_stage in car_stages:
        car = json_data["career"][car_stage]
        for i in range(len(car)):
            year_str = car[i]["years"]
            if year_str.endswith(end_car):
                results.append(True)
            else:
                results.append(False)
    return any(results)

def vetorise_car_info(json_data, car_stage = "international", value = "apps"):
    vec = []
    car = json_data["career"][car_stage]
    for i in range(len(car)):
        vec.append(car[i][value])
    return vec

def create_row(json_data):
    scy = get_car_years(json_data, "senior_club")
    icy = get_car_years(json_data, "international")
    
    if not scy: scy = [-9]
    if not icy: icy = [-9]
    
    sca = sum(vetorise_car_info(json_data, "senior_club", "apps"))
    scp = sum(vetorise_car_info(json_data, "senior_club", "points"))

    ica = sum(vetorise_car_info(json_data, "international", "apps"))
    icp = sum(vetorise_car_info(json_data, "international", "points"))
    
    url = json_data["url"]
    
    data = {
            # "id": [i],
            'name': [json_data.get("name")],
            'dob': [json_data.get("date_of_birth").get("date")],
            'pob': [json_data.get("place_of_birth")],
            'height': [json_data.get("height").get("meters")],
            'weight': [json_data.get("weight").get("kg")],
            # "position": [json_data.get("position")],
            "no_position": [len(json_data.get("position"))],
            
            "is_still_playing": is_any_years_end(json_data),
            
            "no_senior_club": [len(json_data.get("career").get("senior_club"))],
            "senior_club_start": min(scy),
            "senior_club_end": max(scy),
            "senior_club_apps": sca,
            "senior_club_points": scp,
                
            "no_internaional_club": [len(json_data.get("career").get("international"))],
            "internaional_club_start": min(icy),
            "internaional_club_end": max(icy),
            "internaional_club_apps": ica,
            "internaional_club_points": icp,
            "url": url,
            
            # "is_still_playing":
            }
    df = pd.DataFrame(data)
    return(df)


def create_row_postions(json_data):
    
    pos = json_data["position"]
    url = json_data["url"]
    df = pd.DataFrame()
    if len(pos) == 0:
        return df
    for p in pos:
        data = {
            'name': [json_data.get("name")], 'dob': [json_data.get("date_of_birth").get("date")],
            'pob': [json_data.get("place_of_birth")],
            "position": [p], 
            "url": url
            }
        temp = pd.DataFrame(data)
        df = pd.concat([df, temp], ignore_index=True)
    return df


def create_row_career(json_data, car_stages = ["amateur", "senior_club", "international"]):
    df = pd.DataFrame()
    car = json_data["career"]
    url = json_data["url"]
    
    for car_stage in car_stages: 
       cs = car[car_stage] 
       if len(cs) == 0:
           return df
       else:
           for i in range(len(cs)):
               data = {
                   'name': [json_data.get("name")], 'dob': [json_data.get("date_of_birth").get("date")],
                   'pob': [json_data.get("place_of_birth")],
                   "career_stage" : car_stage, 
                   "years": cs[i]["years"], 
                   "teams": cs[i]["teams"], 
                   "apps": cs[i]["apps"], 
                   "points": cs[i]["points"],
                   "url": url,
                   }
               temp = pd.DataFrame(data)
               df = pd.concat([df, temp], ignore_index=True)
    
    return df



def convert_to_pandas(files, input_dir = "./data/IrishPP_formated/"):
    df = pd.DataFrame()
    for file in files:
        input_file_path = input_dir + file
        print(input_file_path)
        with open(input_file_path, 'r', encoding='utf-8', errors='replace') as file:
            data = file.read()
        json_data = json.loads(data)
        if json_data.get('successfully_scrape') == "yes":
            temp = create_row(json_data)
            df = pd.concat([df, temp], ignore_index=True)
        print(f"Finished {file}.")
    return df


def find_files_with_pattern(directory, pattern="_combined_profile"):
    matching_files = []
    
    for root, _, files in os.walk(directory):
        for file in files:
            if pattern in file:
                matching_files.append(os.path.join(root, file))
    
    return matching_files




def scrape_wiki_profiles_first(url_df, first_scrape_out_dir):
    # Assuming df is your DataFrame and 'Names' is the column with the names
    # Define your scrapp function here
    df = url_df
    
    # Iterate over each name in the DataFrame
    failed_indices = []
    for i in range(0, df.shape[0]):
        try:
            # i = 1
            url = df["url"].loc[i]
            name = df["name"].loc[i]
            # name = "none"

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
            with open(f"{first_scrape_out_dir}{i}_{pn}.json", "w", encoding='utf-8') as f:
                f.write(result)
            
            # Logging progress
            if (i % 1) == 0:
                print(f"FS: Currently Finished {i+1} of {df.shape[0]}.")
                print(f"---- Perc: {round((i/df.shape[0]) * 100, 2)}%.")
                print(f"---- Url: {df['url'][i]}.")
        
        except Exception as e:
            # Log the failed index and the exception message
            failed_indices.append((i))
            print(f"Failed at index {i} with error: {e}")
    print("Finished.")

    # # print any failed indices to files
    # if(len(failed_indices) > 0 ): 
    #     # Optionally, save the list of failed indices for later use
    #     with open(f"{output_dir}/failed_indices.txt", "w") as f:
    #         for index in failed_indices:
    #             f.write(f"{index}\n")
    # print(f"Failed indices: {failed_indices}")


def scrape_wiki_profiles_second(first_scrape_out_dir):
    # =============================================================================
    # Second scraping method
    # =============================================================================

    # Some of the wiki's wont have scraped properly, the following checks for this
    # and if it has failed it used a differt method (works line by line).

    files = list_files_in_directory(first_scrape_out_dir)
    files = [f for f in files if f != 'desktop.ini']
    data_list = []
    for i in range(len(files)):
        # Read in the json
        with open(first_scrape_out_dir + files[i], 'r', encoding='utf-8', errors='replace') as file:
            data = file.read()
        json_data = json.loads(data) # load in the first method
        json_data = check_car(json_data) # check and rescrape if requred
        
        # with open("./data/IrishPP_all/" + files[i], "w", encoding='utf-8') as f:
        #     json.dump(json_data, f, indent=4)
        # Save the result to a JSON file
        results = json.dumps(json_data, indent=4, ensure_ascii=False)
        with open(first_scrape_out_dir + files[i], "w", encoding='utf-8') as f:
            f.write(results)
            
        # data_list.append({'index': i, 'type': str(type(points)), 
        #                   'points': points, "url": json_data.get("url")})
        # Logging progress
        if (i % 1) == 0:
            print(f"SS: Currently Finished {i+1} of {len(files)}.")
            print(f"---- Perc: {round((i/len(files)) * 100, 2)}%.")
            print(f"---- Name: {json_data['name']}.")

    # df = pd.DataFrame(data_list)
    # # Display the DataFrame
    # df_str = df[df['type'] == "<class 'str'>"]

def standardise_data(first_scrape_out_dir, cleaned_json_out_dir, target = "na"):
    # =============================================================================
    # Standardise the data
    # =============================================================================

    # Some of the data will have some formating errors in it.(i.e., meters vs cm)
    # The following reformats the data and cleans up the data. 

    files = list_files_in_directory(first_scrape_out_dir)
    files = [f for f in files if f not in ('desktop.ini')]
    if target != "na": 
        index = files.index(target)
        files = files[index:] 
    for i in range(len(files)):
      # Logging progress
        # try: 
            # Read in the json
        with open(first_scrape_out_dir + files[i], 'r', encoding='utf-8', errors='replace') as file:
            data = file.read()
        json_data = json.loads(data)
        
        if (i % 1) == 0:
            print(f"ND: Currently working on {i+1} of {len(files)}.")
            print(f"---- Perc: {round((i/len(files)) * 100, 2)}%.")
            print(f"---- Name: {json_data['name']}.")
            print(f"---- Name: {json_data['url']}.")
        


        json_data = change_car_value_to_int(json_data)
        json_data = convert_weight(json_data)
        json_data = convert_height(json_data)
        json_data = clean_all_text_fields(json_data)
        # with open("./data/IrishPP_all/" + files[i], "w", encoding='utf-8') as f:
        #     json.dump(json_data, f, indent=4)
        # Save the result to a JSON file
        results = json.dumps(json_data, indent=4, ensure_ascii=False)
        with open(cleaned_json_out_dir + files[i], "w", encoding='utf-8') as f:
            f.write(results)
        # except: 
        #     print(f"failed in {i}.")
        #     break
            
        

