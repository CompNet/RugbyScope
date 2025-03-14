# -*- coding: utf-8 -*-
"""
Created on Fri Feb  7 12:29:55 2025

@author: David.OSullivan
"""

from _setup import *

wikidata_df = pd.read_csv("./data/wikidata/players_descr.csv")
wikidata_df['url'] = "https://en.wikipedia.org/wiki/" + wikidata_df["wikipediaEn"]
wikidata_df['name'] = wikidata_df["playerLabel"]
wikidata_df['origWdId'] = wikidata_df["playerId"]
wikidata_df['origName'] = wikidata_df["playerLabel"]


output_dir = "./data/wikipedia_all_en_players/"
# setup the folders
create_directory(output_dir)
first_scrape_out_dir = f"{output_dir}PP/"
create_directory(first_scrape_out_dir)
cleaned_json_out_dir = f"{output_dir}PP_cleaned/"
create_directory(cleaned_json_out_dir)
output_json = f"{output_dir}PP_combined_profile.json"

url_df = wikidata_df

scrape_wiki_profiles_first(url_df, first_scrape_out_dir)
scrape_wiki_profiles_second(first_scrape_out_dir)
standardise_data(first_scrape_out_dir, cleaned_json_out_dir)

files = list_files_in_directory(cleaned_json_out_dir)
files = [f for f in files if f != 'desktop.ini']
files_paths = []
for file in files:
    files_paths.append(cleaned_json_out_dir + file)
combine_json_files(files_paths, output_json)


### add extra info from orginal url df: 

dir = "./data/wikipedia_all_en_players/"
input_json_file = dir + "PP_combined_profile.json"


def add_wikidata_info(json_data, wikidata_df, name_key="name"):
    # Iterate over JSON data and add the wikidata_id if a match is found
    # for i in range(10):
    #     json_element = json_data[i]
    #     json_name = json_element.get(name_key)
    #     for j in range(wikidata_df.shape[0]): 
    #         if json_name == wikidata_df.iloc[j]["origName"]:
    #             json_element["origWdId"] = wikidata_df.iloc[j]["origWdId"]
    #             json_element["origName"] = wikidata_df.iloc[j]["origName"]
    #     print(f"Finished searching the {i} of {len(json_data)}.")
    # Create a dictionary for fast lookup: {origName: (origWdId, origName)}
    wikidata_lookup = dict(zip(wikidata_df["origName"], zip(wikidata_df["origWdId"], wikidata_df["origName"])))
    # Process the first 10 entries in json_data
    for i in range(len(json_data)):  # Ensure we don't go out of range
        json_element = json_data[i]
        json_name = json_element.get(name_key)
        if json_name in wikidata_lookup:
            json_element["origWdId"], json_element["origName"] = wikidata_lookup[json_name]
        # print(f"Finished searching {i+1} of {len(json_data)}.")
    return json_data

 

with open(input_json_file, 'r', encoding='utf-8', errors='replace') as file:
        json_data = json.load(file)

json_data_processed = add_wikidata_info(json_data, wikidata_df)

with open(input_json_file, 'w') as f:
    json.dump(json_data_processed, f, indent=4)
