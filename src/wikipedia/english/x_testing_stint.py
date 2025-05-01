from _setup import *


import sys
from _setup import *

dir = "./data/wikipedia/english/"
input_json_file = dir + "PP_combined_profile.json"
output_player_info_file = dir + "PP_player_info.csv"
output_player_stint_file = dir + "PP_stint_info.csv"

# load the json. 
with open(input_json_file, 'r', encoding='utf-8', errors='replace') as file:
        json_data = json.load(file)

#### 

def create_row_stint(json_entry, car_stages = ["amateur", "senior_club", "international"]):
    df = pd.DataFrame()
    car = json_entry["career"]
    url = json_entry["url"]
    
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
                "wpPage": json_entry.get("url"),
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

i_range = range(len(json_data))
print(i_range)
# need to fix it if it is not their the url that is. 
stint_df = pd.DataFrame()
for i in i_range:
    json_entry = json_data[i]
    # print(json_entry)
    temp = create_row_stint(json_entry)

    # print(temp)
    stint_df = pd.concat([stint_df, temp], ignore_index=True)
    # print(info_df)
    if i % 500 == 0: print(i/max(i_range))

print(stint_df)

