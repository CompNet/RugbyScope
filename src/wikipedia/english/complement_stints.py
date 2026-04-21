########################################################################
# Retrieves missing stints from English Wikipedia infoboxes.
# Ignore player info, as it was already retrieved separately.
#
# Vincent Labatut
# 04/2026
########################################################################
import time
from matplotlib import text
import requests
import re
import pandas as pd
from bs4 import BeautifulSoup
from os import path

import sys
sys.path.append("src/common")
from mylogging import *




########################################################################
# start recording log
start_rec_log("RetrievalEnWP")




########################################################################
# init file constants

# folder path
table_folder = path.join("data", "wikipedia", "english", "raw")

# base url of the website
base_url = "https://en.wikipedia.org/wiki/{}"

session = requests.Session()
session.headers.update({
    "User-Agent": "RugbyCareerExtractor/1.0 (contact: you@example.com)"
})




########################################################################
# relevant info sections
YOUTH_CAREER = {"Youth career", "Amateur team(s)"}

# ignored sections
CAREER_DISC = {"Senior Career", "International career", "Coaching career", "National sevens team"}




########################################################################
# load list of players
merged_table = pd.read_csv(path.join("data", "fusion", "players_06_enwp.csv"))
player_number = merged_table.shape[0]




########################################################################
# init lists to store extracted data
stint_info = []
all_sections_found = []



########################################################################
# returns the stints and career sections for the specified page.
########################################################################
def parse_youth_and_amateur_stints(soup, orig_id, orig_name, player_page):

    stints = []
    sections_found = []

    keep_section = False

    def normalize_section(text):
        if not text:
            return None
        return text.strip()

    def is_target_section(text):
        if not text:
            return False
        t = text.strip().lower()
        return any(sec.lower() in t for sec in YOUTH_CAREER)

    def parse_int(text):
        if not text:
            return None
        text = text.strip().strip("()")
        return int(text) if text.isdigit() else None

    table = soup.find("table", class_="infobox")
    if not table:
        return stints, sections_found

    for row in table.find_all("tr"):

        # Detect section header
        header = row.find("th", class_="infobox-header")
        if header:
            section_title = normalize_section(header.get_text(strip=True))
            sections_found.append(section_title)

            keep_section = is_target_section(section_title)
            continue

        # Skip if not in a target section
        if not keep_section:
            continue

        data_cells = row.find_all("td")
        year_cell = row.find("th")

        if not data_cells:
            continue

        years = year_cell.get_text(strip=True) if year_cell else None

        # Team + URL
        team_cell = data_cells[0]
        link = team_cell.find("a")

        if link:
            team = link.get_text(strip=True) or None
            url = link.get("href")
        else:
            team = team_cell.get_text(strip=True) or None
            url = None

        # Apps / Points (optional)
        apps = parse_int(data_cells[1].get_text(strip=True)) if len(data_cells) > 1 else None
        points = parse_int(data_cells[2].get_text(strip=True)) if len(data_cells) > 2 else None

        stints.append([orig_id, orig_name, player_page, header, years, team, url, apps, points])

    return stints, sections_found


########################################################################
# loop over players
name = ""
p = 1
for player_page in ["Christopher_Hilsenbeck"]:  # Christophe_Dominici Antoine_Dupont Fabien_Galthié Jonathan_Sexton Faf_de_Klerk
    orig_name = ""
    orig_id = ""
# for _, player in merged_table.iterrows():
#     player_page = player["wikipediaEn"]
#     orig_name = player["fullName"]
#     orig_id = player["wikidataId"]
    tlog(0, f"Processing player {p}/{player_number}: {orig_name} ({orig_id})")

    comment = ""

    # no english wikipedia page for this player
    if pd.isnull(player_page):
        tlog(2, f"No English Wikipedia page for this player")
        comment = "No EN WP page"

    # there is a english wikipedia page for this player
    else:
        url = base_url.format(player_page)
        tlog(2, f"Processing URL: {url}")

        # fetch webpage
        while True:
            try:
                response = session.get(url)
                response.raise_for_status()  # Raise an error for HTTP status codes (4xx, 5xx)
                break
            except requests.Timeout:
                tlog(4, "Timeout occurred, trying again")
                time.sleep(3)
            except requests.RequestException as e:
                print(f"Request failed: {e}")
                break

        # check if the request was successful
        if response.status_code == 200:
            # parse the HTML content using BeautifulSoup
            soup = BeautifulSoup(response.content, "html.parser")

            # get the infobox element
            temp = soup.find_all("table", class_=["infobox"])
            if len(temp) > 0:
                infobox_elt = temp[0]
            else:
                tlog(2, f"Could not find any infobox: skipping the rest of the extraction process")
                comment = "No infobox found"
                p = p + 1
                continue

            # # get career sections
            stints, sections_found = parse_youth_and_amateur_stints(soup, orig_id, orig_name, player_page)

            if not stints:
                comment = "No stint found"
                tlog(2, comment)
            else:
                stint_info.extend(stints)
                print(stints)

            all_sections_found.extend(sections_found)
            print(sections_found)

    # record stints
    if len(stint_info) > 0:
        stint_df = pd.DataFrame(stint_info, columns=["origWdId", "origName", "wpPage", "stintType", "timePeriod", "teamName", "teamWP", "matchesPlayed", "pointsScored"])
        stint_df.to_csv(path.join(table_folder, "stint_info2.csv"), index=False)

    # record sections found
    if len(all_sections_found) > 0:
        with open(path.join(table_folder, "sections.txt"), "w", encoding="utf-8") as f:
            for sections_found in all_sections_found:
                f.write(",".join(sections_found) + "\n")

    p = p + 1




########################################################################
# stop recording log
end_rec_log()
