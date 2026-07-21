# -*- coding: utf-8 -*-
"""
Created on Sun Jan 12 17:29:50 2025

@author: David.OSullivan
"""

# =============================================================================
# run the setup file for packages and functions to be loaded
# =============================================================================

import sys
sys.path.append("./src/wikipedia/english/")
from _setup import *  # noqa: F403


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

# # set these up for the team you wanta
# nation = "Ireland"

players_url_df = pd.read_csv("./data/wikipedia/International_players_list_by_contry.csv")

def get_all_htmls(): 
    def save_html(url, path):
        headers = {"User-Agent": "Mozilla/5.0"}
        try:
            r = requests.get(url, headers=headers)
            if r.status_code == 200:
                with open(path, "w", encoding="utf-8") as f:
                    f.write(r.text)
                    print(f"---- {url} retrived and saved.")
            else: 
                print("xxxx {url} could not retrive")
        except Exception as e:
            print(f"Error fetching {url}: {e}")
        return False

    players_url_df = pd.read_csv("./data/wikipedia/International_players_list_by_contry.csv")
    total_nations = players_url_df.shape[0]

    for nation_i in range(total_nations):  
        
        # set these up for the team you want
        nation = players_url_df["nation"][nation_i]
        output_dir = f"./data/wikipedia/english/{nation}/"
        player_profiles = f"./data/wikipedia/english/profile_links/profile_links_{nation}.csv"
        output_dir_html = f"{output_dir}/player_html/"

        # setup the folders
        create_directory(output_dir)
        create_directory(output_dir_html)

        # first_scrape_out_dir = f"{output_dir}{nation}PP/"
        # create_directory(first_scrape_out_dir)
        # cleaned_json_out_dir = f"{output_dir}{nation}_cleaned/"
        # create_directory(cleaned_json_out_dir)
        # output_json = f"{output_dir}{nation}_combined_profile.json"

        df = pd.read_csv(player_profiles)

        # Iterate over each name in the DataFrame
        failed_indices = []
        # for i in range(0, 200):
        for i in range(0, df.shape[0]):
            try:
                # Logging progress
                if (i % 1) == 0:
                    print(f"Currently working nation: {nation} ({nation_i} of {total_nations})")
                    print(f"{nation} FS: Currently Working on {i+1} of {df.shape[0]}.")
                
                # i = 1
                url = df["url"].loc[i]
                # name = df["name"].loc[i]

                if pd.isna(url):
                    print("Skipped one.")
                    continue

                # Apply the scrapp function and store the result
                # result = scrap_rugby_wiki_standard(url)

                save_html(url = url, path = f"{output_dir_html}{i}.html")

                # if result == "fail":
                #     failed_indices.append((i, url))
                #     print(f"failed at index {i} and url {url}.")
                #     continue

                # Logging progress
                if (i % 1) == 0:
                    # print(f"{nation} FS: Currently Finished {i+1} of {df.shape[0]}.")
                    print(f"---- Perc: {round((i/df.shape[0]) * 100, 2)}%.")
                    print(f"---- Name: {df['name'][i]}.")
                    print(f"---- URL: {url}.")
            except Exception as e:
                # Log the failed index and the exception message
                failed_indices.append((i,url))
                print(f"Failed at index {i} with error: {e}")
        print("Finished.")


get_all_htmls()

def get_player_profile_info_1st():
    players_url_df = pd.read_csv("./data/wikipedia/International_players_list_by_contry.csv")
    total_nations = players_url_df.shape[0]
    for nation_i in range(0, total_nations):  
        
        # set these up for the team you want
        nation = players_url_df["nation"][nation_i]
        output_dir = f"./data/wikipedia/english/{nation}/"
        # player_profiles = f"./data/wikipedia/english/profile_links/profile_links_{nation}.csv"
        
        # setup the folders
        create_directory(output_dir)
        first_scrape_out_dir = f"{output_dir}PP/"
        create_directory(first_scrape_out_dir)

        cleaned_json_out_dir = f"{output_dir}PP_cleaned/"
        create_directory(cleaned_json_out_dir)
        output_json = f"{output_dir}{nation}_combined_profile.json"
        print(f"Currently working on: ({nation_i}) {nation}")

        # =============================================================================
        # First scraping method:  
        # =============================================================================
        
        # Assuming df is your DataFrame and 'Names' is the column with the names
        # Define your scrapp function here
        # df = pd.read_csv(player_profiles)

        html_dir = f"{output_dir}player_html/"
        files = list_files_in_directory(html_dir)
        files = [f for f in files if f != 'desktop.ini']
        json_files = [f[:-5] + '.json' if f.endswith('.html') else f for f in files]
        
        # Iterate over each name in the DataFrame
        failed_indices = []
        for i in range(0, len(files)):
            try:
                # Logging progress
                if (i % 1) == 0:
                    print(f"{nation} FS: Currently Working on {i+1} of {len(files)}.")
                
                # i = 1
                file_path = files[i]
                

                # Apply the scrapp function and store the result
                result = get_info_box_standard(f"{html_dir}{file_path}")
                if result == "fail":
                    failed_indices.append((i, file_path))
                    print(f"failed at index {i} and url {file_path}.")
                    continue
                else: 
                    # result["name"] = name
                    # result["url"] = url
                    result["accessed_at"] = datetime.datetime.now().date().isoformat()
                # pn = df['name'][i].replace('"', '')
                result = json.dumps(result, indent=4)
                
        
                # Save the result to a JSON file
                # with open(f"{first_scrape_out_dir}{i}_{pn}.json", "w", encoding='utf-8') as f:
                with open(f"{first_scrape_out_dir}{json_files[i]}", "w", encoding='utf-8') as f:
                    f.write(result)
        
                # Logging progress
                # if (i % 1) == 0:
                    # print(f"{nation} FS: Currently Finished {i+1} of {df.shape[0]}.")
                    # print(f"---- Perc: {round((i/len(file_path)) * 100, 2)}%.")
                    # print(f"---- Name: {df['name'][i]}.")
                    # print(f"---- URL: {url}.")
            
            except Exception as e:
                # Log the failed index and the exception message
                failed_indices.append((i,file_path))
                print(f"Failed at index {i} with error: {e}")
        print("Finished.")
        
        # print any failed indices to files
        if(len(failed_indices) > 0 ): 
            # Optionally, save the list of failed indices for later use
            with open(f"{output_dir}/{nation_i}_failed_indices.txt", "w") as f:
                for index in failed_indices:
                    f.write(f"{index}\n")
        print(f"Failed indices: {failed_indices}")
    return True

get_player_profile_info_1st()

def get_player_profile_info_2nd(base_folder = "/data/wikipedia/english/"):
    # =============================================================================
    # Second scraping method
    # =============================================================================
    
    # Some of the wiki's wont have scraped properly, the following checks for this
    # and if it has failed it used a differt method (works line by line).
    players_url_df = pd.read_csv("./data/wikipedia/International_players_list_by_contry.csv")
    total_nations = players_url_df.shape[0]

    for nation_i in range(0, total_nations):
        nation = players_url_df["nation"][nation_i]
        file_path = f"./{base_folder}{nation}/PP/"

        files = list_files_in_directory(file_path)
        files = [f for f in files if f != 'desktop.ini']
        # data_list = []
        for i in range(len(files)):
            # Read in the json
            with open(file_path + files[i], 'r', encoding='utf-8', errors='replace') as file:
                data = file.read()
            json_data = json.loads(data) # load in the first method
            json_data = check_car(json_data) # check and rescrape if requred
            
            # with open("./data/IrishPP_all/" + files[i], "w", encoding='utf-8') as f:
            #     json.dump(json_data, f, indent=4)
            # Save the result to a JSON file
            results = json.dumps(json_data, indent=4, ensure_ascii=False)
            with open(file_path + files[i], "w", encoding='utf-8') as f:
                f.write(results)
                
            # data_list.append({'index': i, 'type': str(type(points)), 
            #                   'points': points, "url": json_data.get("url")})
            # Logging progress
            if (i % 1) == 0:
                print(f"{nation} SS: Currently Finished {i+1} of {len(files)}.")
                print(f"---- Perc: {round((i/len(files)) * 100, 2)}%.")
                print(f"---- Name: {json_data['name']}.")
                print(f"---- File save:{file_path + files[i]}.")
        

get_player_profile_info_2nd()


# =============================================================================
# update
# =============================================================================

# Need code that will do 1) add in tbe name and the url for all users

# stranardise the data

def fix_small_issues_file():
    def remove_footnotes(text):
        """
        Removes square-bracketed footnotes or citations from text.
        Example: "This is an example [1][2]." -> "This is an example."
        """
        # Remove anything in square brackets (including multiple digits or letters)
        cleaned_text = re.sub(r'\s*\[[^\]]*\]', '', text)
        return cleaned_text.strip()

    def sanitise_dict(obj):
        """
        Recursively apply remove_footnotes() to all string values
        inside nested dicts, lists, or tuples.
        """
        if isinstance(obj, dict):
            return {k: sanitise_dict(v) for k, v in obj.items()}
        elif isinstance(obj, list):
            return [sanitise_dict(item) for item in obj]
        elif isinstance(obj, tuple):
            return tuple(sanitise_dict(item) for item in obj)
        elif isinstance(obj, str):
            return remove_footnotes(obj)
        else:
            return obj
    
    def extract_place(s: str) -> str:
        """
        Remove leading date and return the place name.
        Assumes a 4-digit year (YYYY) separates date from place.
        """
        # Find a 4-digit year
        match = re.search(r"(18|19|20)\d{2}", s)
        if not match:
            # No year found → return original
            return s.strip()
        
        # Position just after the year
        end_of_year = match.end()
        
        # Return everything after the year
        place = s[end_of_year:].strip()
        
        return place
    
    players_url_df = pd.read_csv("./data/wikipedia/International_players_list_by_contry.csv")
    total_nations = players_url_df.shape[0]
    base_folder = "/data/wikipedia/english/"
    
    for nation_i in range(0, total_nations):
        nation = players_url_df["nation"][nation_i]
        file_path = f"./{base_folder}{nation}/PP/"


        # set these up for the team you want
        nation = players_url_df["nation"][nation_i]
        # output_dir = f"./data/wikipedia/english/{nation}/"
        player_profiles_file_path = f"./data/wikipedia/english/profile_links/profile_links_{nation}.csv"
        player_profiles = pd.read_csv(player_profiles_file_path)


        files = list_files_in_directory(file_path)
        files = [f for f in files if f != 'desktop.ini']

        # data_list = []
        for i in range(len(files)):
            # Read in the json
            with open(file_path + files[i], 'r', encoding='utf-8', errors='replace') as file:
                data = file.read()
            json_data = json.loads(data) # load in the first method
            num = int(files[i].replace('.json', ''))
            # fix the foot note problems [1] problems
            json_data = sanitise_dict(json_data)
            
            # Add the url and name to dataset
            if json_data["name"] == "" or json_data["name"] == ".":
                json_data["name"] = player_profiles.loc[num,"name"].lower()
                print(f"Added name to JSON for file {files[i]}.")
            
            json_data["profile_url"] = player_profiles.loc[num,"url"]

            # # fix issue with data place of birth/death
            json_data["place_of_birth"] = extract_place(json_data["place_of_birth"])
            json_data["place_of_death"] = extract_place(json_data["place_of_death"])

            results = json.dumps(json_data, indent=4, ensure_ascii=False)
            with open(file_path + files[i], "w", encoding='utf-8') as f:
                f.write(results)
                
            # data_list.append({'index': i, 'type': str(type(points)), 
            #                   'points': points, "url": json_data.get("url")})
            # Logging progress
            if (i % 1) == 0:
                print(f"{nation} Fixing small issues: Currently Finished {i+1} of {len(files)}.")
                print(f"---- Perc: {round((i/len(files)) * 100, 2)}%.")
                print(f"---- Name: {json_data['name']}.")
                print(f"---- File save:{file_path + files[i]}.")
        
    return 1


fix_small_issues_file()


def standardise_player_profile_info():
    
    players_url_df = pd.read_csv("./data/wikipedia/International_players_list_by_contry.csv")
    total_nations = players_url_df.shape[0]
    base_folder = "/data/wikipedia/english/"
    
    for nation_i in range(0, total_nations):
        nation = players_url_df["nation"][nation_i]
        file_path = f"./{base_folder}{nation}/PP/"
        out_file_path = f"./{base_folder}{nation}/PP_cleaned/"

        # set these up for the team you want
        nation = players_url_df["nation"][nation_i]
        # output_dir = f"./data/wikipedia/english/{nation}/"
        player_profiles_file_path = f"./data/wikipedia/english/profile_links/profile_links_{nation}.csv"
        # player_profiles = pd.read_csv(player_profiles_file_path)

        files = list_files_in_directory(file_path)
        files = [f for f in files if f != 'desktop.ini']

        # data_list = []
        for i in range(len(files)):
            
            # Read in the json
            with open(file_path + files[i], 'r', encoding='utf-8', errors='replace') as file:
                data = file.read()
            json_data = json.loads(data) # load in the first method
            num = int(files[i].replace('.json', ''))
           
            # Logging progress
            if (i % 1) == 0:
                print(f"{nation} Standardising PP: Currently working on {i+1} of {len(files)}.")
                print(f"---- Perc: {round((i/len(files)) * 100, 2)}%.")
                print(f"---- FN: {files[i]}.")
                print(f"---- Name: {json_data['name']}.")

            json_data = change_car_value_to_int(json_data)
            json_data = convert_weight(json_data)
            json_data = convert_height(json_data)
            json_data = clean_all_text_fields(json_data)

            results = json.dumps(json_data, indent=4, ensure_ascii=False)
            with open(out_file_path + files[i], "w", encoding='utf-8') as f:
                f.write(results)
                
            # data_list.append({'index': i, 'type': str(type(points)), 
            #                   'points': points, "url": json_data.get("url")})
            # Logging progress
            if (i % 1) == 0:
                print(f"---- File save:{out_file_path + files[i]}.")
        
    return 1

standardise_player_profile_info()

# fix the issues with points in carears
# fix the issues with date of birth, place of birth
# fix the issues with date of dealth, and place of dealth
# look at all the positions and use vincents classifications



def combine_all_jsons_finds():
    players_url_df = pd.read_csv("./data/wikipedia/International_players_list_by_contry.csv")
    total_nations = players_url_df.shape[0]
    base_folder = "/data/wikipedia/english/"
    output_json = f".{base_folder}all_capped_players_profile.json"
    all_files = []
    for nation_i in range(0, total_nations):
        nation = players_url_df["nation"][nation_i]
        file_path = f"./{base_folder}{nation}/PP_cleaned/"
        # out_file_path = f"./{base_folder}{nation}/PP_cleaned/"

        files = list_files_in_directory(file_path)
        files = [f for f in files if f != 'desktop.ini']

        for file in files:
            all_files.append(file_path + file)
  
    combine_json_files(all_files, output_json)

combine_all_jsons_finds()
