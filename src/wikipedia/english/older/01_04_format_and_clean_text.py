# -*- coding: utf-8 -*-
"""
Created on Thu Aug 22 16:16:08 2024

@author: David.OSullivan
"""
# =============================================================================
# import packages
# =============================================================================

import re
import json
import os
import pandas as pd
# =============================================================================
# define gobal vars
# =============================================================================

input_dir = "./data/IrishPP/"
output_dir = "./data/IrishPP_formated"

# =============================================================================
# Define functions
# =============================================================================

def clean_brackets_in_json(json_data):
    # Regex pattern to find numbers in parentheses
    pattern = re.compile(r'\(([^)]+)\)')
    
    # Replace the pattern with just the number (removing the parentheses)
    cleaned_data = pattern.sub(r'\1', json_data)
    
    return cleaned_data


def list_files_in_directory(directory_path):
    # Get a list of all files and directories in the specified directory
    files_and_dirs = os.listdir(directory_path)
    
    # Filter out only files
    files = [f for f in files_and_dirs if os.path.isfile(os.path.join(directory_path, f))]
    
    return files

def clean_json_file(input_file_path, output_file_directory, output_file_name):
    
    # Ensure the output directory exists
    os.makedirs(output_file_directory, exist_ok=True)
    
    # Read in the json
    with open(input_file_path, 'r', encoding='utf-8', errors='replace') as file:
        data = file.read()

    try: 
        # Use regex to find the JSON part and remove bracket from data part
        json_data = re.search(r'{.*}', data, re.DOTALL).group(0)
        json_data = clean_brackets_in_json(json_data)
        # Parse the JSON to ensure it's valid
        # this should work without error
        parsed_data = json.loads(json_data)
    except:
        print("Trying adding closing bracket.")
        try:
            json_data = json_data + "\n}"
            parsed_data = json.loads(json_data)
            print("Added closing bracket succfully.")
        except: 
            print("Adding closing bracket failed! Savind file name.")
            return input_file_path

    # Define the output file path
    output_file_path = os.path.join(output_file_directory, output_file_name)
    
    parsed_data = convert_apps_and_points_to_numeric(parsed_data)
    
    # Write the cleaned JSON data to the output file
    with open(output_file_path, 'w') as output_file:
        json.dump(parsed_data, output_file)
    print(f"Cleaned JSON data has been saved to {output_file_path}")
    return "Success"
    

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

def get_years_ends(vals):
    results = []
    for v in vals: 
        v_split = v.split("-")
        for vs in v_split: 
            results.append(int(vs))
    return(results)

def create_row(json_data):
    scy = get_years_ends(json_data.get("career").get("senior_club")[0]["years"])
    icy = get_years_ends(json_data.get("career").get("international")[0]["years"])

    sca = sum(json_data.get("career").get("senior_club")[0]["apps"])
    scp = sum(json_data.get("career").get("senior_club")[0]["points"])

    ica = sum(json_data.get("career").get("international")[0]["apps"])
    icp = sum(json_data.get("career").get("international")[0]["points"])

    data = {
            # "id": [i],
            'name': [json_data.get("name")],
            'dob': [json_data.get("date_of_birth").get("full")],
            'pob': [json_data.get("place_of_birth")],
            'heigh': [json_data.get("height").get("meters")],
            'weight': [json_data.get("weight").get("kg")],
            "position": [json_data.get("position")],
            "no_position": [len(json_data.get("position"))],
            
            "no_senior_club": [len(json_data.get("career").get("senior_club")[0].get("team"))],
            "senior_club_start": min(scy),
            "senior_club_end": max(scy),
            "senior_club_apps": sca,
            "senior_club_points": scp,
                
            "no_internaional_club": [len(json_data.get("career").get("international")[0].get("team"))],
            "internaional_club_start": min(icy),
            "internaional_club_end": max(icy),
            "internaional_club_apps": ica,
            "internaional_club_points": icp
            
            # "is_still_playing":
            }
    df = pd.DataFrame(data)
    return(df)

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
# =============================================================================
# run code
# =============================================================================

# read in the orginal files, check if proper json and correct format if possible
files = list_files_in_directory("./data/IrishPP/")
failed_files = []
i=0
for file in files: 
    input_file_path = input_dir + file
    ff = clean_json_file(input_file_path, output_dir, file)
    if ff != "Success":
        failed_files.append(ff)
        # i = i + 1
        # if i == 2: 
        #     break

# now read in the cleaned json files and fix smaller formatting issues
# i.e., for app and point make sure they are number

len(failed_files)

input_dir = "./data/IrishPP_formated/"
files = list_files_in_directory(input_dir)

df = pd.DataFrame()
failed_files = []
for file in files:
    input_file_path = input_dir + file
    try:
        with open(input_file_path, 'r', encoding='utf-8', errors='replace') as file:
            data = file.read()
            json_data = json.loads(data)
        if json_data.get('successfully_scrape') == "Yes":
            temp = create_row(json_data)
            df = pd.concat([df, temp], ignore_index=True)
    except:
        failed_files.append(file)   
    print(f"Finished {file}.")

len(files)

len(failed_files)

df.to_csv('./data/IrishPP.csv', index=False)