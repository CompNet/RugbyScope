# -*- coding: utf-8 -*-
"""
Created on Tue Aug 27 21:11:31 2024

@author: David.OSullivan
"""

url = "https://en.wikipedia.org/wiki/Keith_Earls"
response = requests.get(url)
soup = BeautifulSoup(response.text, 'html.parser')
tables = soup.find_all('table', class_='infobox')
st = ""
if tables:
    infobox_table = tables[0]
    # Extract all text while preserving the table structure
    table_text = infobox_table.get_text(separator="\n", strip=True)
    st = table_text + ""
    # Output the unstructured text
    rugbybio_json = get_llama_cleaned(st)
print(st)


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
    rugbybio_json = get_llama_cleaned(st)
else:
    print("No 'infobox' tables found.")
    rugbybio_json = if_missing_infobox(name)
    
print(st)


# =============================================================================
# 
# =============================================================================
lines = st.splitlines()


def parse_rugby_player_info(data):
    """
    Parses rugby player information from a structured text and returns it as a JSON object.
    """
    # Initialize the output dictionary with the required schema
    output = {
        "name": "",
        "successfully_scrape": "Yes",
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
            match = re.search(r'([\d.]+) m', height_info)
            if match:
                output["height"]["meters"] = match.group(0)

        # Extract weight
        if "weight" in line:
            weight_info = line.split("|")[1].strip()
            match = re.search(r'(\d+) kg', weight_info)
            if match:
                output["weight"]["kg"] = match.group(0)

        # Extract education
        if "school" in line:
            output["education"]["school"] = line.split("|")[1].strip()

        # Extract positions
        if "position(s)" in line:
            positions = line.split("|")[1].strip()
            output["position"] = [pos.strip() for pos in positions.split(',')]

        # Detect the start of a career section
        if "amateur team(s)" in line:
            current_section = "amateur"
            in_valid_section = True
        elif "senior career" in line:
            current_section = "senior_club"
            in_valid_section = True
        elif "international career" in line:
            current_section = "international"
            in_valid_section = True
        elif "national sevens team" in line:
            current_section = "national_sevens"
            in_valid_section = False
        elif "correct as" in line:
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
                    points = points.strip("()")
                if years:
                    output["career"][current_section].append({
                        "years": years,
                        "team": team.title(),
                        "apps": apps,
                        "points": points
                    })


    return output

print(st)
parse_rugby_player_info(st)



response = requests.get("https://en.wikipedia.org/wiki/Beauden_Barrett")
soup = BeautifulSoup(response.text, 'html.parser')
tables = soup.find_all('table', class_='infobox')
# st = ""
if tables:
    infobox_table = tables[0]
    # Extract all text while preserving the table structure
    # table_text = infobox_table.get_text(separator="\n", strip=True)
    # st = table_text + ""
    st = get_infobox_tables(infobox_table)
print(st)

parse_rugby_player_info(st)
