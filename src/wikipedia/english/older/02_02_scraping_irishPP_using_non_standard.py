# -*- coding: utf-8 -*-
"""
Created on Thu Aug 29 21:42:08 2024

@author: David.OSullivan
"""

from bs4 import BeautifulSoup
import pandas as pd
import requests
# from io import StringIO
import json
# import numpy as ny

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

def find_career_data_in_row(row):
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
# =============================================================================
# 
# =============================================================================

def check_car(json_data, car_stages = ["amateur","senior_club","international"]):
    for car_stage in car_stages:
        car = json_data.get("career").get(car_stage)
        if len(car) > 0:
            points = car[0].get("points")
            if points == 0: points = "0"
            points = points.replace(",","")
                
            date = car[0]["years"]
            match = re.search(r'\d{8}', date)
            if points == "":
                points = "0"
                
            if points.isdigit() and not match:
                points = int(points)
            else:
                url = json_data.get("url")
                json_data_us = scrape_wiki_alt(url)
                json_data["career"][car_stage] = json_data_us["career"][car_stage]
    return json_data


   
files = list_files_in_directory("./data/IrishPP/")
data_list = []
for i in range(len(files)):
    # Read in the json
    with open("./data/IrishPP/" + files[i], 'r', encoding='utf-8', errors='replace') as file:
        data = file.read()
    json_data = json.loads(data)
    json_data = check_car(json_data)
            

    # with open("./data/IrishPP_all/" + files[i], "w", encoding='utf-8') as f:
    #     json.dump(json_data, f, indent=4)
    # Save the result to a JSON file
    results = json.dumps(json_data, indent=4, ensure_ascii=False)
    with open("./data/IrishPP_all/" + files[i], "w", encoding='utf-8') as f:
        f.write(results)
        
    data_list.append({'index': i, 'type': str(type(points)), 
                      'points': points, "url": json_data.get("url")})
    # Logging progress
    if (i % 1) == 0:
        print(f"Currently Finished {i+1} of {len(files)}.")
        print(f"---- Perc: {round((i/len(files)) * 100, 2)}%.")
        print(f"---- Name: {json_data['name']}.")

df = pd.DataFrame(data_list)
# Display the DataFrame
df_str = df[df['type'] == "<class 'str'>"]
# =============================================================================
# 
# =============================================================================

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
                    if isinstance(selected_value,str):
                        selected_value = selected_value.replace(",","").strip().replace("+","")
                    
                    if selected_value in ["", "?"]:
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



files = list_files_in_directory("./data/IrishPP_all/")
data_list = []
for i in range(len(files)):
    try: 
        # Read in the json
        with open("./data/IrishPP_all/" + files[i], 'r', encoding='utf-8', errors='replace') as file:
            data = file.read()
        json_data = json.loads(data)
        
        json_data = change_car_value_to_int(json_data)
        json_data = convert_weight(json_data)
        json_data = convert_height(json_data)
        json_data = clean_all_text_fields(json_data)
        # with open("./data/IrishPP_all/" + files[i], "w", encoding='utf-8') as f:
        #     json.dump(json_data, f, indent=4)
        # Save the result to a JSON file
        results = json.dumps(json_data, indent=4, ensure_ascii=False)
        with open("./data/IrishPP_clean/" + files[i], "w", encoding='utf-8') as f:
            f.write(results)
    except: 
        print(f"failed in {i}.")
        break
        
    # Logging progress
    if (i % 1) == 0:
        print(f"Currently Finished {i+1} of {len(files)}.")
        print(f"---- Perc: {round((i/len(files)) * 100, 2)}%.")
        print(f"---- Name: {json_data['name']}.")
