

from _setup import *


df = scrap_rugby_wiki_standard("https://en.wikipedia.org/wiki/Tim_Fairbrother", "Dan Cole")

# print(df)

# print(df["career"]["international"])

# print("\n\n\n")
# print("second method")
# df2 = scrape_wiki_alt("https://en.wikipedia.org/wiki/Tim_Fairbrother")

# print(df2)

# print("\n\n")
# print(df2["career"]["international"])

# print("\n\n")
# before = df2["career"]["international"][0]["apps"]
# after = extract_number(before)
# print(f"before clean no is: {before}")
# print(f"cleaned no is: {after}")



# is_eq = df2["career"]["international"] == df2["career"]["international"]
# json_data = change_car_value_to_int(df2)
# json_data = convert_weight(json_data)
# json_data = convert_height(json_data)
# json_data = clean_all_text_fields(json_data)


# print("cleaned int data\n\n")
# print(json_data["career"]["international"])

# output_dir = "./data/wikipedia/english/"
# files = f"{output_dir}PP/" + "3693_James Hadfield.json"

# # Logging progress
# # try: 
#     # Read in the json
# with open(files, 'r', encoding='utf-8', errors='replace') as file:
#     data = file.read()
# json_data = json.loads(data)


# json_data = change_car_value_to_int(json_data)
# json_data = convert_weight(json_data)
# json_data = convert_height(json_data)
# json_data = clean_all_text_fields(json_data)

# print(json_data)
# output_dir = "./data/wikipedia/english/"
# first_scrape_out_dir = f"{output_dir}PP/"
# print(list_files_in_directory(first_scrape_out_dir)[1:10])

response = requests.get("https://en.wikipedia.org/wiki/Stephen_Perofeta")
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


for car_stage in ["international"]:
    team_stints = rugbybio_json["career"][car_stage]
    if len(team_stints) > 0:
        for team_stint in team_stints:
            
            search_term = team_stint["teams"]
            print(search_term)
            # Find the link in the infobox table that matches the team name
            link = infobox_table.find('a', string=re.compile(re.escape(search_term), re.IGNORECASE))
            print(link)
            if link and link.get("href"):
                # Add the href to the JSON data
                team_stint["team_link"] = link["href"]