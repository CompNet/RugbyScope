

from _setup import *

url = "https://en.wikipedia.org/wiki/Sbu_Nkosi"
# df = scrap_rugby_wiki_standard(url, "jimmy!")

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

print(rugbybio_json)



# json_data = df
# print(json_data)
# # json_data = change_car_value_to_int(json_data)
# #

# car_stages = ["amateur","senior_club","international"]
# values = ["apps", "points"]


print(extract_number("(4112)"))

# car = json_data.get("career")
# for car_stage in car_stages:
#     for value in values:
#         selected_car = car[car_stage]
#         if len(selected_car) > 0:
#             for i in range(len(selected_car)):
#                 selected_value = selected_car[i][value]
#                 if isinstance(selected_value, str):
#                     selected_value = selected_value.replace(",","").strip().replace("+","")
#                     print(selected_value)
#                     selected_value = extract_number(selected_value) # recover the number of brackets are present
#                     print(selected_value)
#                 if selected_value in ["", "?", "(0)", "()","-", ")"]:
#                     selected_value = 0
#                 json_data["career"][car_stage][i][value] = int(selected_value)  



