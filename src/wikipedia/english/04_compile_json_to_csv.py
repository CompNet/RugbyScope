# -*- coding: utf-8 -*-
"""
Created on Sun Jan 12 17:28:01 2025

@author: David.OSullivan


"""

import sys
sys.path.append("./src/wikipedia/english/")
from _setup import *  # noqa: F403

# dir = "./data/wikipedia/english/"
# input_json_file = dir + "all_capped_players_profile.json"
# output_player_info_file = dir + "PP_player_info_capped_players_2.csv"
# output_player_stint_file = dir + "PP_stint_info_capped_players_2.csv"

dir = "./data/wikipedia/english/"
input_json_file = dir + "all_wikidata_players_profile_4.json"
output_player_info_file = dir + "/raw0/PP_player_info_wikidata_players_4.csv"
output_player_stint_file = dir + "/raw0/PP_stint_info_wikidata_players_4.csv"


# load the json. 
with open(input_json_file, 'r', encoding='utf-8', errors='replace') as file:
        json_data = json.load(file)

#### 
# Create player info data frame
####

print(f"Starting to create the player info dataset: {output_player_info_file}")


def create_player_info(json_entry):
    # Handle the pos variable. 
    pos = json_entry.get("position")
    if len(pos) > 1:
        pos_string = " ; ".join(pos)
    elif len(pos) == 0:
        pos_string = ""
    else:
        pos_string = pos[0]
    
    data = {
        "origWdId": json_entry.get("origWdId", ""),
        "origName": json_entry.get("origName", ""),
        "debugComment": "",
        "wiki_Name": json_entry.get("name"),
        "wpPage": json_entry.get("profile_url"),
        "birthDate": json_entry.get("date_of_birth").get("date"),
        "birthPlace": "",
        "birthPlaceWP": json_entry.get("place_of_birth"),
        "deathDate": json_entry.get("date_of_death").get("date"),
        "deathPlace": "",
        "deathPlaceWP": json_entry.get("place_of_death"),
        "height": json_entry.get("height").get("meters"),
        "weight": json_entry.get("weight").get("kg"),
        "positions": pos_string,
        "currentTeam": "",
    }
    df = pd.DataFrame([data])
    return df 
# 
info_df = pd.DataFrame()
for i in range(len(json_data)):
    json_entry = json_data[i]
    # print(json_entry)
    temp = create_player_info(json_entry)
    # print(temp)
    info_df = pd.concat([info_df, temp], ignore_index=True)
    # print(info_df)

info_df.to_csv(output_player_info_file, index=False)
print(f"Finished creating the player info dataset: {output_player_info_file}")


####
# Now create the career string data frame
####
print(f"Starting to create the stint dataset: {output_player_stint_file}")
def create_row_stint(json_entry, car_stages = ["amateur", "senior_club", "international"]):
    df = pd.DataFrame()
    car = json_entry["career"]
    url = json_entry["profile_url"]
    
    for car_stage in car_stages: 
       cs = car[car_stage] 
       if len(cs) > 0:
           for i in range(len(cs)):
                
            team_link_entry = cs[i].get("team_link", "")
            if not team_link_entry:
                team_link_entry = ""
                # try:
                #     if len(cs[i]["team_link"]) > 0:
                #         team_link_entry = cs[i]["team_link"]
                #         pass
                #     else: 
                #         team_link_entry = ""
                # except KeyError:
                #     print(f"Missing 'team_link' key in cs[{i}]: {cs[i]}")
                #     print(url)
                #     print("\n\n")
                #     print(car)
                #     team_link_entry = ""
                       
            data = {
                "origWdId": json_entry.get("origWdId", ""),
                "origName": json_entry.get("origName", ""),
                'wiki_name': [json_entry.get("name")], 
                "wpPage": url,
                "stintType" : car_stage, 
                "timePeriod": cs[i]["years"], 
                "teamName": cs[i]["teams"], 
                "teamWP": team_link_entry,
                "matchesPlayed": cs[i]["apps"], 
                "pointsScored": cs[i]["points"],
                }
            temp = pd.DataFrame(data)
            df = pd.concat([df, temp], ignore_index=True)
    return df

stint_df = pd.DataFrame()
for i in range(len(json_data)):
    json_entry = json_data[i]
    # print(json_entry)
    temp = create_row_stint(json_entry)
    # print(temp)
    stint_df = pd.concat([stint_df, temp], ignore_index=True)
    # print(info_df)

stint_df.to_csv(output_player_stint_file, index=False)
print(f"Finished creating the player strint dataset: {output_player_stint_file}")
