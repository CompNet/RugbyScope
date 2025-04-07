########################################################################
# Retrieves player information from Italian Wikipedia infoboxes.
#
# Vincent Labatut
# 04/2025
########################################################################
import time
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
start_rec_log("RetrievalItWP")




########################################################################
# init file constants

# folder path
table_folder = path.join("data", "wikipedia", "italian", "raw")

# base url of the website
base_url = "https://it.wikipedia.org/wiki/{}"




########################################################################
# init HTML constants
BIOGRAPHY = "Dati biografici"
RUGBY_UNION = "Rugby a 15"

COUNTRY_BIRTH = "Paese"
COUNTRY_SPORT = "Union"
CITIZENSHIP = "Nazionalità"
HEIGHT = "Altezza"
WEIGHT = "Peso"
POSITIONS = "Ruolo"
CUR_TEAM = "Squadra"

SECTION_TITLE = "sinottico_divisione"

# relevant info sections
CAREER1 = "Carriera"
CAREER2 = "Carriera rugby a 15"
CAREER_SECTIONS = {CAREER1, CAREER2}
YOUTH_CAREER1 = "Attività giovanile"
YOUTH_CAREER2 = "Giovanili"
CLUB_CAREER1 = "Attività di club"
CLUB_CAREER2 = "Squadre di club"
FRAN_CAREER1 = "Attività in franchise"
FRAN_CAREER2 = "Attività infranchise"
PROV_CAREER = "Attività provinciale"
INTNL_CAREER1 = "Attività da giocatore internazionale"
INTNL_CAREER2 = "Nazionale"
CAREER_MAP = {
    YOUTH_CAREER1: "Youth",
    YOUTH_CAREER2: "Youth",
    CLUB_CAREER1: "Senior",
    CLUB_CAREER2: "Senior",
    FRAN_CAREER1: "Senior",
    FRAN_CAREER2: "Senior",
    PROV_CAREER: "Regional",
    INTNL_CAREER1: "International",
    INTNL_CAREER2: "International"
}

# irrelevant info sections
CAREER_DISC = {
    "Atletica leggera", "Attività da allenatore", "Attività di club (rugby a 13)", "Attività internazionale", 
    "Carriera arbitrale", 
    "Palmarès", "Palmarès internazionale"
}




########################################################################
# load list of players
merged_table = pd.read_csv(path.join("data", "fusion", "players_03_frwp.csv"))
player_number = merged_table.shape[0]




########################################################################
# init lists to store extracted data
stint_info = []
player_info = []
diff_sect = [] # this is for debug



########################################################################
# loop over players
name = ""
p = 1
# for player_page in ["Jonathan_Sexton"]:  # Christophe_Dominici Antoine_Dupont Alun_Wyn_Jones Richie_McCaw Sergio_Parisse Brian_O%27Driscoll Fabien_Galthié Jonathan_Sexton 
#     orig_name = "test"
#     orig_id = "test"
for _, player in merged_table.iterrows():
    player_page = player["wikipediaIt"]
    orig_name = player["fullName"]
    orig_id = player["wikidataId"]
    tlog(0, f"Processing player {p}/{player_number}: {orig_name} ({orig_id})")

    has_career = False
    name0 = name
    name = ""
    comment = ""
    height = ""
    weight = ""
    positions = ""
    positions_url = ""
    birth_country = ""
    sport_country = ""
    current_team = ""
    current_team_url = ""

    # no italian wikipedia page for this player
    if pd.isnull(player_page):
        tlog(2, f"No Italian Wikipedia page for this player")
        comment = "No IT WP page"

    # there is a italian wikipedia page for this player
    else:
        url = base_url.format(player_page)
        tlog(2, f"Processing URL: {url}")

        # fetch webpage
        while True:
            try:
                response = requests.get(url)
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
            temp = soup.find_all("table", class_=["infobox", "infobox_v3"])
            if len(temp) > 0:
                infobox_elt = temp[0]
            else:
                tlog(2, f"Could not find any infobox: skipping the rest of the extraction process")
                comment = "No infobox found"
                player_info.append([orig_id, orig_name, comment, name, player_page, height, weight, positions, birth_country, sport_country, current_team])
                p = p + 1
                continue

            # player name
            th_elt = infobox_elt.find("tr", ).find("th")
            if th_elt:
                name = th_elt.get_text(strip=True)
                tlog(2, f"Name: '{name}'")
                if name0 == name:
                    tlog(4, "Same name used for two distinct players (WARNING)")
            else:
                tlog(2, f"Could not find the name (rugby probably not the main activity)")
#             bio_elt = infobox_elt.find("th", string=BIOGRAPHY)
# #            if bio_elt is None:     # alternate section name
# #                bio_elt = infobox_elt.find("caption", string=BIOGRAPHY)
#             if bio_elt is None:
#                 tlog(2, f"Infobox not properly formatted: skipping the rest of the extraction process")
#                 comment = "Infobox format problem"
#                 player_info.append([orig_id, orig_name, comment, name, player_page, height, weight, positions, birth_country, sport_country, current_team])
#                 p = p + 1
#                 continue

            # birth country
            country_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == COUNTRY_BIRTH)
            if country_elt:
                birth_country = country_elt.find_next_siblings()[0].get_text(strip=True)
                tlog(2, f"Birth country: {birth_country}")
            else:
                country_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == CITIZENSHIP)
                if country_elt:
                    birth_country = country_elt.find_next_siblings()[0].get_text(strip=True)
                    tlog(2, f"Citizenship: {birth_country}")
                else:
                    tlog(2, f"Could not find birth country or citizenship")

            # height
            height_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == HEIGHT)
            if height_elt:
                height = height_elt.find_next_siblings()[0].get_text(strip=True)
                height = height.replace(u"\xa0", u" ")
                height = height.replace(",", ".")
                height = height.strip()
                if height != "":
                    pattern = r"(\d.?\d?\d?)(?: ?\(.*\))?(?:\[\d+\])? *c?m.*"
                    vals = re.findall(pattern, height)
                    height = vals[0]
                    if "." in height:
                        height = int(float(height) * 100)
                    tlog(2, f"Height: {height} cm")
            else:
                tlog(2, f"Could not find height")

            # weight
            weight_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == WEIGHT)
            if weight_elt:
                weight = weight_elt.find_next_siblings()[0].get_text(strip=True)
                weight = weight.replace(u"\xa0", u" ")
                weight = weight.strip()
                if weight != "":
                    pattern = r"(\d+)(?: ?\(.*\))?(?:\[\d+\])? *kg.*"
                    vals = re.findall(pattern, weight)
                    weight = vals[0]
                    tlog(2, f"Weight: {weight} kg")
            else:
                tlog(2, f"Could not find weight")

            # positions
            pos_elt = infobox_elt.find("th", string=lambda text: text in [POSITIONS])
            if pos_elt:
                td_elts = pos_elt.find_next_siblings()
                if len(td_elts) > 0:
                    td_elt = td_elts[0]
                    a_elts = td_elt.find_all("a", recursive = False)
                    if a_elts:
                        positions = "; ".join(a.get_text(strip=True) for a in a_elts)
                    else:
                        positions = td_elt.get_text(strip=True)
                    tlog(2, f"Positions: {positions}")
                else:
                    tlog(2, f"Could not find positions")
            else:
                tlog(2, f"Could not find positions")

            # current team
            team_elt = infobox_elt.find("th", string=lambda text: text in [CUR_TEAM])
            if team_elt:
                td_elt = team_elt.find_next_siblings()[0]
                a_elts = td_elt.find_all("a", recursive = False)
                if len(a_elts) > 0:
                    current_team = "; ".join(a.get_text(strip=True) for a in a_elts)
                    current_team_url = "; ".join(a["href"] for a in a_elts)
                    if len(a_elts) > 1:
                        tlog(2, f"WARNING: found several current teams!")
                else:
                    current_team = td_elt.get_text(strip=True)
                tlog(2, f"Current team: {current_team} ({current_team_url})")
            else:
                tlog(2, f"Could not find current team")

            # sport country
            country_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == COUNTRY_SPORT)
            if country_elt:
                td_elt = country_elt.find_next_siblings()[0]
                a_elts = td_elt.find_all("a", recursive = False)
                if len(a_elts) > 0:
                    sport_country = "; ".join(a.get_text(strip=True) for a in a_elts)
                    if len(a_elts) > 1:
                        tlog(2, f"WARNING: found several sport countries!")
                else:
                    country_elt = td_elt.get_text(strip=True)
                tlog(2, f"Sport country: {sport_country}")
            else:
                tlog(2, f"Could not find sport country")

            # get career sections
            ru_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == RUGBY_UNION)
            if ru_elt:
                career_elt = ru_elt.find_next(lambda tag: tag.name == "th" and tag.get_text(strip=True) in CAREER_SECTIONS)
            else:
                career_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) in CAREER_SECTIONS)
            if career_elt:
                row_elt = career_elt.parent.find_next_siblings()[0]
                while row_elt:
                    if row_elt.name == "tr" and SECTION_TITLE in row_elt.get("class", []):
                        th_elt = row_elt.find("th")
                        section = th_elt.get_text(strip=True)
                        section = re.sub(r"\[\d+\]", "", section)
                        section = re.sub(r"[¹²³]", "", section)

                        if section in CAREER_MAP.keys():
                            stint_type = CAREER_MAP[section]
                            
                            row_elt = th_elt.parent.find_next_siblings()[0]
                            tbody_elt = row_elt.find_all("td")[0].find_all("table")[0].find_all("tbody")[0]
                            tr_elt = tbody_elt.find_all("tr")[0]
                            
                            # possibly skip first row
                            if(tr_elt.has_attr("style") and tr_elt["style"] == "visibility: collapse"):
                                tr_elt = tr_elt.find_next_siblings()[0]

                            while tr_elt:
                                # period 
                                period_elt = tr_elt.find_all("td")[0]
                                period = period_elt.get_text(strip = True, separator = "; ")

                                # team
                                team_elt = period_elt.find_next_siblings()[0]
                                a_elts = team_elt.find_all("a", recursive = False)
                                if len(a_elts) == 0:
                                    team = team_elt.get_text(strip = True)
                                    team_url = ""
                                else:
                                    team = "; ".join(a_elt.get_text(strip = True) for a_elt in a_elts)
                                    team_url = "; ".join(a_elt["href"] for a_elt in a_elts)

                                # stats
                                stats_elts = team_elt.find_next_siblings()
                                if len(stats_elts) > 0:
                                    stats_elt = stats_elts[0]
                                    for br in stats_elt.find_all("br"):
                                        br.replace_with(u";;")
                                    stats_str = stats_elt.get_text(strip = True)
                                    # clean final string
                                    stats_str = stats_str.replace(u"\xa0", u" ")
                                    stats_str = stats_str.replace(".", "")  # thousands dot
                                    stats_str = stats_str.strip()
                                    stats_str = stats_str.replace("  ", " ")
                                    # handle the case where there are several stints in the same <tr>
                                    stats_strs = stats_str.split(";;")
                                    matches_lst = []
                                    points_lst = []
                                    for str in stats_strs:
                                        str = str.strip()
                                        if str.startswith("("):
                                            str = "? " + str
                                        # retrieve values
                                        if str in ["", "?", "-", "–", "()", "ND", "N.D.", "nd"]:
                                            matches_lst.append("")
                                            points_lst.append("")
                                        else:
                                            if "(" in str:
                                                pattern = r"^\+?(?: *de)? *(\d+\+?|\?+|-)[^\d]*\( *(\d? ?\d+\+?|\?+|-) *\)"
                                                vals = re.findall(pattern, str)
                                                matches_lst.append(vals[0][0])
                                                points_lst.append(vals[0][1])
                                            else:
                                                pattern = r"^(\d+\+?|\?+|-)"
                                                vals = re.findall(pattern, str)
                                                matches_lst.append(vals[0])
                                                points_lst.append("")
                                    matches = "; ".join(matches_lst)
                                    points = "; ".join(points_lst)
                                else:
                                    matches = ""
                                    points = ""

                                # create stint
                                stint = [orig_id, orig_name, name, player_page, stint_type, period, team, team_url, matches, points]
                                stint_info.append(stint)
                                has_career = True
                                tlog(6, f"{stint})")

                                # go to next row
                                tr_elts = tr_elt.find_next_siblings()
                                if len(tr_elts) == 0:
                                    tr_elt = None
                                else:
                                    tr_elt = tr_elts[0]

                        # not a relevant section
                        elif section not in CAREER_DISC:
                            # in case of types of stints never seen before
                            tlog(4, f"Unknown section detected in this page: " + section)
                            diff_sect = set(diff_sect).union([section])
                            diff_df = pd.DataFrame(diff_sect, columns = ["Title"])
                            diff_df.to_csv(path.join(table_folder, "debug__new_sections.csv"), index=False)

                    # turn to next section
                    row_elts = row_elt.find_next_siblings()
                    if len(row_elts) == 0:
                        row_elt = None
                    else:
                        row_elt = row_elts[0]
            else:
                tlog(2, "Could not find the 'career' section")

            if not has_career:
                comment = "No stint found"
                tlog(2, comment)

    # record player info
    player_info.append([orig_id, orig_name, comment, name, player_page, height, weight, positions, positions_url, birth_country, sport_country, current_team, current_team_url])
    player_df = pd.DataFrame(player_info, columns=["origWdId", "origName", "debugComment", "itName", "wpPage", "height", "weight", "positions", "positionsWP", "birthCountry", "sportCountry", "currentTeam", "currentTeamWP"])
    player_df.to_csv(path.join(table_folder, "player_info.csv"), index=False)
                
    # record stints
    if has_career:
        stint_df = pd.DataFrame(stint_info, columns=["origWdId", "origName", "itName", "wpPage", "stintType", "timePeriod", "teamName", "teamWP", "matchesPlayed", "pointsScored"])
        stint_df.to_csv(path.join(table_folder, "stint_info.csv"), index=False)
                
    p = p + 1




########################################################################
# stop recording log
end_rec_log()
