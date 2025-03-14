# -*- coding: utf-8 -*-
"""
Created on Sun Jan 12 17:29:50 2025

@author: David.OSullivan
"""

# =============================================================================
# run the setup file for packages and functions to be loaded
# =============================================================================

# import runpy
# # runpy.run_path('./code/_setup.py')
# from _setup import *

from bs4 import BeautifulSoup
import pandas as pd
import requests
from io import StringIO
import json
import datetime
import re

# =============================================================================
# setup the file to read in and directories
# =============================================================================

# # set these up for the team you want
# nation = "Ireland"

players_url_df = pd.read_csv("./data/International_players_list_by_contry.csv")
for nation_i in range(19, players_url_df.shape[0]):  
    
    # set these up for the team you want
    nation = players_url_df["nation"].iloc[nation_i]
    output_dir = f"./data/{nation}/"
    player_profiles = f"./data/profile_links_{nation}.csv"
    
    # setup the folders
    create_directory(output_dir)
    first_scrape_out_dir = f"{output_dir}{nation}PP/"
    create_directory(first_scrape_out_dir)
    cleaned_json_out_dir = f"{output_dir}{nation}_cleaned/"
    create_directory(cleaned_json_out_dir)
    output_json = f"{output_dir}{nation}_combined_profile.json"
    print(f"Currently working on: ({nation_i}) {nation}")

    # =============================================================================
    # First scraping method:  
    # =============================================================================
    
    # Assuming df is your DataFrame and 'Names' is the column with the names
    # Define your scrapp function here
    df = pd.read_csv(player_profiles)
    
    # Iterate over each name in the DataFrame
    failed_indices = []
    for i in range(0, df.shape[0]):
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
            with open(f"{first_scrape_out_dir}{i}_{pn}.json", "w", encoding='utf-8') as f:
                f.write(result)
    
            # Logging progress
            if (i % 1) == 0:
                print(f"{nation} FS: Currently Finished {i+1} of {df.shape[0]}.")
                print(f"---- Perc: {round((i/df.shape[0]) * 100, 2)}%.")
                print(f"---- Name: {df['name'][i]}.")
        
        except Exception as e:
            # Log the failed index and the exception message
            failed_indices.append((i,name))
            print(f"Failed at index {i} with error: {e}")
    print("Finished.")
    
    # print any failed indices to files
    if(len(failed_indices) > 0 ): 
        # Optionally, save the list of failed indices for later use
        with open(f"{output_dir}/failed_indices.txt", "w") as f:
            for index in failed_indices:
                f.write(f"{index}\n")
    print(f"Failed indices: {failed_indices}")
    
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
            print(f"{nation} SS: Currently Finished {i+1} of {len(files)}.")
            print(f"---- Perc: {round((i/len(files)) * 100, 2)}%.")
            print(f"---- Name: {json_data['name']}.")
    
    # df = pd.DataFrame(data_list)
    # # Display the DataFrame
    # df_str = df[df['type'] == "<class 'str'>"]
    
    
    # =============================================================================
    # Standardise the data
    # =============================================================================
    
    # Some of the data will have some formating errors in it.(i.e., meters vs cm)
    # The following reformats the data and cleans up the data. 
    
    files = list_files_in_directory(first_scrape_out_dir)
    files = [f for f in files if f not in ('desktop.ini',"777_Brian McKechnie.json")]
    data_list = []
    for i in range(len(files)):
        # try: 
            # Read in the json
        with open(first_scrape_out_dir + files[i], 'r', encoding='utf-8', errors='replace') as file:
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
        with open(cleaned_json_out_dir + files[i], "w", encoding='utf-8') as f:
            f.write(results)
        # except: 
        #     print(f"failed in {i}.")
        #     break
            
        # Logging progress
        if (i % 1) == 0:
            print(f"{nation} ND: Currently Finished {i+1} of {len(files)}.")
            print(f"---- Perc: {round((i/len(files)) * 100, 2)}%.")
            print(f"---- Name: {json_data['name']}.")
    
    
    # =============================================================================
    # combine all the json files together
    # =============================================================================
    
    files = list_files_in_directory(cleaned_json_out_dir)
    files = [f for f in files if f != 'desktop.ini']
    files_paths = []
    for file in files:
        files_paths.append(cleaned_json_out_dir + file)
    combine_json_files(files_paths, output_json)
    

