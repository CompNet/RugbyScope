########################################################################
# Retrieves missing stints from English Wikipedia infoboxes.
# Ignore player info, as it was already retrieved separately.
#
# Vincent Labatut
# 04/2026
########################################################################
from random import random
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




########################################################################
# load list of players
merged_table = pd.read_csv(path.join("data", "fusion", "players_06_enwp.csv"))
player_number = merged_table.shape[0]




########################################################################
# init lists to store extracted data
stint_info = []
sections_found = set()
all_sections_found = []




########################################################################
# returns the targeted stints for the specified page.
########################################################################
def extract_targeted_career(soup, orig_id, orig_name, player_page):
    results = []

    for targeted_section in YOUTH_CAREER:

        # Find the targeted career header
        header = soup.find("th", class_="infobox-header", string=lambda x: x and targeted_section in x)
        if not header:
            continue

        # Go to the parent row
        row = header.find_parent("tr")

        # Iterate over next rows until next section header
        for sib in row.find_next_siblings("tr"):
            # Stop if next section starts
            if sib.find("th", class_="infobox-header"):
                break

            cells = sib.find_all(["th", "td"])
            if not cells:
                continue

            # Default values
            years = ""
            team = ""
            team_url = ""
            apps = ""
            points = ""

            if len(cells) >= 1:
                years = cells[0].get_text(strip=True)
                if years == "Years" or years.startswith("Correct as"):
                    continue  # Skip header row

            if len(cells) >= 2:
                team_cell = cells[1]
                team = team_cell.get_text(strip=True)

                link = team_cell.find("a")
                if link and link.get("href"):
                    team_url = link.get("href")

            if len(cells) >= 3:
                apps = cells[2].get_text(strip=True)

            if len(cells) >= 4:
                points = cells[3].get_text(strip=True)

            # Clean points like "(123)" → "123"
            if points.startswith("(") and points.endswith(")"):
                points = points[1:-1].strip()

            results.append([orig_id, orig_name, player_page, targeted_section, years, team, team_url, apps, points])

    return results




########################################################################
# Returns the list of all career section headers found in the infobox.
########################################################################
def extract_career_headers(soup):
    headers = []

    # Find the main infobox first (safer if page has multiple tables)
    infobox = soup.find("table", class_="infobox")
    if not infobox:
        return headers

    for th in infobox.find_all("th", class_="infobox-header"):
        text = th.get_text(strip=True)
        headers.append(text)

    return headers




########################################################################
# loop over players
name = ""
p = 1
# for player_page in ["Faf_de_Klerk"]:  # Christopher_Hilsenbeck Christophe_Dominici Antoine_Dupont Fabien_Galthié Jonathan_Sexton Faf_de_Klerk
#     orig_name = ""
#     orig_id = ""
for _, player in merged_table.iterrows():
    player_page = player["wikipediaEn"]
    orig_name = player["fullName"]
    orig_id = player["wikidataId"]
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

            # get targeted career stints
            stints = extract_targeted_career(soup, orig_id, orig_name, player_page)

            if not stints:
                comment = "No stint found"
                tlog(2, comment)
            else:
                stint_info.extend(stints)
                print(stints)

            # get all career section titles
            sections = extract_career_headers(soup)
            sections_found.update(sections)
            all_sections_found.append([orig_id] + sections)
            #print(sections_found)

    # record stints
    if len(stint_info) > 0:
        stint_df = pd.DataFrame(stint_info, columns=["origWdId", "origName", "wpPage", "stintType", "timePeriod", "teamName", "teamWP", "matchesPlayed", "pointsScored"])
        stint_df.to_csv(path.join(table_folder, "stint_info2.csv"), index=False)

    # record sections found
    with open(path.join(table_folder, "sections.txt"), "w", encoding="utf-8") as f:
        for sf in sorted(sections_found):
            f.write(sf + "\n")
    # record sections for each player
    with open(path.join(table_folder, "all_sections.txt"), "w", encoding="utf-8") as f:
        for sf in all_sections_found:
            f.write(", ".join(sf) + "\n")

    p = p + 1
    time.sleep(0.5)



########################################################################
# stop recording log
end_rec_log()
