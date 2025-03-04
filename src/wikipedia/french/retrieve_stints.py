########################################################################
# Retrieves player information from French Wikipedia infoxes.
#
# Vincent Labatut
# 03/2025
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
start_rec_log("WikipediaFr")




########################################################################
# init file constants

# folder path
table_folder = path.join("data", "wikipedia", "french", "raw")

# base url of the website
base_url = "https://fr.wikipedia.org/wiki/{}"




########################################################################
# init HTML constants
IDENTITY = "Fiche d'identité"
BIRTH = "Naissance"
DEATH = "Décès"
HEIGHT = "Taille"
POSITIONS = "Poste"

# relevant info sections
YOUTH_CAREER = "Carrière en junior"
SENIOR_CAREER = "Carrière en senior"
INTNL_CAREER = "Carrière en équipe nationale"
CAREER_MAP = {
    YOUTH_CAREER: "Youth",
    SENIOR_CAREER: "Senior",
    INTNL_CAREER: "International"
}

# irrelevant info sections
COACH_CAREER = "Carrière d'entraîneur"
NATREF_CAREER = "Désignations nationales"
INTREF_CAREER = "Désignations internationales"
CAREER_DISC = {COACH_CAREER, NATREF_CAREER, INTREF_CAREER}




########################################################################
# load list of players
merged_table = pd.read_csv(path.join("data", "fusion", "players_02_ja-wp.csv"))
player_number = merged_table.shape[0]




########################################################################
# init lists to store extracted data
stint_info = []
player_info = []
diff_sect = [] # this is for debug



########################################################################
# loop over players
p = 1
# for player_page in ["Jonathan_Sexton"]:  # Christophe_Dominici Antoine_Dupont Fabien_Galthié Jonathan_Sexton
#     orig_name = ""
#     orig_id = ""
for _, player in merged_table.iterrows():
    player_page = player["wikipediaFr"]
    orig_name = player["fullName"]
    orig_id = player["wikidataId"]
    tlog(0, f"Processing player {p}/{player_number}: {orig_name} ({orig_id})")

    has_career = False
    name = ""
    comment = ""
    birth_date = ""
    birth_place = ""
    birth_place_url = ""
    death_date = ""
    death_place = ""
    death_place_url = ""
    height = ""
    weight = ""
    positions = ""
    current_team = ""

    # no french wikipedia page for this player
    if pd.isnull(player_page):
        tlog(2, f"No French Wikipedia page for this player")
        comment = "No FR WP page"

    # there is a french wikipedia page for this player
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
            temp = soup.find_all("div", {"class": "infobox"})
            if len(temp) > 0:
                infobox_elt = temp[0]
            else:
                tlog(2, f"Could not find any infobox: skipping the rest of the extraction process")
                continue

            # player name
            div_elt = infobox_elt.find("div", ).find("div")
            if div_elt:
                name = div_elt.get_text(strip=True)
                tlog(2, f"Name: '{name}'")
            else:
                tlog(2, f"Could not find the name (rugby probably not the main activity)")
            caption_elt = infobox_elt.find("caption", string=IDENTITY)
            id_elt = caption_elt.find_next_siblings()[0]

            # birth information
            birth_elt = id_elt.find("th", string=BIRTH)
            if birth_elt:
                td_elt = birth_elt.find_next_siblings()[0]
                # birth date
                time_elt = td_elt.find("time")
                birth_date = time_elt["datetime"]
                tlog(2, f"Birth date: {birth_date}")
                # birth place
                br_elt = time_elt.find_next_siblings("br")[0]
                a_elt = time_elt.find_next_siblings("a")[0]
                birth_place = a_elt.get_text(strip=True)
                birth_place_url = a_elt["href"]
                tlog(2, f"Birth place: {birth_place}")
            else:
                tlog(2, f"Could not find birth information")

            # death information
            death_elt = id_elt.find("th", string=DEATH)
            if death_elt:
                td_elt = death_elt.find_next_siblings()[0]
                # death date
                time_elt = td_elt.find("time")
                death_date = time_elt["datetime"]
                tlog(2, f"Death date: {death_date}")
                # death place
                br_elt = time_elt.find_next_siblings("br")[0]
                a_elt = time_elt.find_next_siblings("a")[0]
                death_place = a_elt.get_text(strip=True)
                death_place_url = a_elt["href"]
                tlog(2, f"Death place: {death_place}")
            else:
                tlog(2, f"Could not find death information")

            # height
            height_elt = id_elt.find("th", string=HEIGHT)
            if height_elt:
                height = height_elt.find_next_siblings()[0].get_text(strip=True)
                height = height.replace(u"\xa0", u" ")
                height = height.replace(",", ".")
                pattern = r"(\d.?\d\d?) ?c?m.*"
                vals = re.findall(pattern, height)
                height = vals[0]
                if "." in height:
                    height = int(float(height) * 100)
                tlog(2, f"Height: {height} cm")
            else:
                tlog(2, f"Could not find height")

            # positions
            pos_elt = id_elt.find("th", string=POSITIONS)
            if pos_elt:
                tmp_elt = pos_elt.find_next_siblings()[0]
                a_elts = tmp_elt.find_all("a", recursive = False)
                if a_elts:
                    positions = "; ".join(a.get_text(strip=True) for a in a_elts)
                else:
                    positions = tmp_elt.get_text(strip=True)
                tlog(2, f"Positions: {positions}")
            else:
                tlog(2, f"Could not find positions")

            # get career sections
            stint_types = []; periods = []; teams = []; urls = []; matches = []; points = []
            table_elt = caption_elt.parent
            table_elt = table_elt.find_next_siblings()[0]
            while table_elt and table_elt.name == "table":
                caption_elt = table_elt.find("caption")
                section = caption_elt.get_text(strip=True)
                if section in CAREER_MAP.keys():
                    stint_type = CAREER_MAP[section]
                    tbody_elt = caption_elt.find_next_siblings()[0]
                    tr_elt = tbody_elt.find_all("tr")[1]  # [0] = table header
                    td_elts = tr_elt.find_all("td")
                    # init elements containing info
                    periods_elt = td_elts[0].find("span").contents
                    teams_elt = td_elts[1].find("span").contents
                    stats_elt = None
                    if len(td_elts) > 2:
                        stats_elt = td_elts[2].contents
                    # loop over br-separated periods
                    r = 0
                    while r < len(periods_elt):
                        period_elt = periods_elt[r]
                        # possibly skip <s> elements
                        if period_elt.name is not None and period_elt.name == "s":
                            r = r + 1
                            period_elt = periods_elt[r]
                        # handle <br> elements
                        if period_elt.name is not None and period_elt.name == "br":
                            periods.append("")
                        # otherwise, extract content
                        else:   # should be text node
                            periods.append(period_elt.strip())
                            r = r + 1   # skip next br
                        r = r + 1       # go to next row
                    # loop over br-separated teams
                    r = 0
                    while r < len(teams_elt):
                        team_elt = teams_elt[r]
                        # possibly skip whitespaces
                        if team_elt.name is None and team_elt.strip() == "":
                            r = r + 1
                            team_elt = teams_elt[r]
                        # handle <br> elements
                        if team_elt.name is not None and team_elt.name == "br":
                            teams.append("")
                            urls.append("")
                        # otherwise, extract content
                        else:
                            if team_elt.name is not None:
                                # possibly skip flag
                                if team_elt.name == "span" and team_elt.has_attr("class") and "flagicon" in team_elt["class"]:
                                    r = r + 1
                                    team_elt = teams_elt[r]
                                # possibly skip empty line
                                if team_elt.name is None and team_elt.strip() == "":
                                    r = r + 1
                                    team_elt = teams_elt[r]
                                # retrieve team name
                                teams.append(team_elt.get_text(strip=True))
                                # case where url is inside a <span>
                                if team_elt.name == "span":
                                    a_elt = team_elt.find("a", recursive = False) # skip flag <span>
                                    if a_elt:
                                        urls.append(a_elt["href"])
                                elif team_elt and team_elt.name == "a":
                                    urls.append(team_elt["href"])
                                else:
                                    urls.append("")
                            else:   # should be text node
                                teams.append(team_elt.strip())
                                urls.append("")
                            r = r + 1   # skip next br
                            # possibly skip empty line
                            if len(teams_elt) > r and teams_elt[r].name is None and teams_elt[r].strip() == "":
                                r = r + 1
                        r = r + 1       # go to next row
                    stint_types = [stint_type] * len(teams)
                    # check list lengths are consistent
                    if len(periods) != len(teams):
                        if len(periods) == 0:
                            periods = [""] * len(teams)
                        else:
                            raise Exception("Inconsistent numbers of periods/teams")
                    # loop over br-separated stats
                    if not stats_elt:
                        matches = [""] * len(periods)
                        points = [""] * len(periods)
                    else:
                        r = 0
                        while r < len(stats_elt):
                            stat_elt = stats_elt[r]
                            # possibly skip <s> elements
                            if stat_elt.name is not None and stat_elt.name == "s":
                                r = r + 1
                                stat_elt = stats_elt[r]
                            # possibly skip empty element
                            if stat_elt.name is None and stat_elt.strip() == "":
                                r = r + 1
                                stat_elt = stats_elt[r]
                            # if <br> => no value
                            if stat_elt.name is not None and stat_elt.name == "br":
                                matches.append("")
                                points.append("")
                            # otherwise, extract content
                            else:
                                if stat_elt.name is None:
                                    str = stat_elt.strip()
                                else:
                                    str = stat_elt.get_text(strip=True).strip()
                                r = r + 1   # next element
                                # possibly skip <s> element
                                if r < len(stats_elt) and stats_elt[r].name is not None and stats_elt[r].name == "s":
                                    r = r + 1
                                    stat_elt = stats_elt[r]
                                    str = str + stat_elt.strip()
                                    r = r + 1
                                # retrieve values
                                pattern = r"^(\d+|\?)[^\d]*\((\d+|\?)\)"
                                vals = re.findall(pattern, str)
                                matches.append(vals[0][0])
                                points.append(vals[0][1])
                                # possibly skip <sup> element
                                while r < len(stats_elt) and stats_elt[r].name is not None and stats_elt[r].name == "sup":
                                    r = r + 1
                            r = r + 1       # go to next row
                    # check list lengths are consistent
                    if len(periods) != len(matches):
                        # if completely empty: complement
                        if len(matches) == 0:
                            matches = [""] * len(periods)
                        # if missing values, we assume that the last ones are missing (makes sense graphically speaking)
                        elif len(periods) > len(matches):
                            matches.extend([""] * (len(periods) - len(matches)))
                        # otherwise, problem
                        else:
                            raise Exception("Inconsistent numbers of periods/match numbers")
                    if len(periods) != len(points):
                        # if completely empty: complement
                        if len(points) == 0:
                            points = [""] * len(periods)
                        # if missing values, we assume that the last ones are missing (makes sense graphically speaking)
                        elif len(periods) > len(points):
                            points.extend([""] * (len(periods) - len(points)))
                        # otherwise, problem
                        else:
                            raise Exception("Inconsistent numbers of periods/points scored")

                # in case of types of stints never seen before
                elif section not in CAREER_DISC:
                    tlog(4, f"New section detected in this page: " + section)
                    diff_sect = set(diff_sect).union(section)
                    diff_df = pd.DataFrame(diff_sect)
                    diff_df.to_csv(path.join(table_folder, "debug__new_sections.csv"), index=False)

                # turn to next section
                table_elt = table_elt.find_next_siblings()[0]

            # create stint
            for s in range(len(periods)):
                stint = [orig_id, orig_name, name, player_page, stint_types[s], periods[s], teams[s], urls[s], matches[s], points[s]]
                stint_info.append(stint)
                has_career = True
                tlog(4, f"Stint: {stint})")

            if not has_career:
                comment = "No stint found"
                tlog(2, comment)

    # record player info
    player_info.append([orig_id, orig_name, comment, name, player_page, birth_date, birth_place, birth_place_url, death_date, death_place, death_place_url, height, positions])
    player_df = pd.DataFrame(player_info, columns=["origWdId", "origName", "debugComment", "frName", "wpPage", "birthDate", "birthPlace", "birthPlaceWP", "deathDate", "deathPlace", "deathPlaceWP", "height", "positions"])
    player_df.to_csv(path.join(table_folder, "player_info.csv"), index=False)
                
    # record stints
    if has_career:
        stint_df = pd.DataFrame(stint_info, columns=["origWdId", "origName", "frName", "wpPage", "stintType", "timePeriod", "teamName", "teamWP", "matchesPlayed", "pointsScored"])
        stint_df.to_csv(path.join(table_folder, "stint_info.csv"), index=False)
                
    p = p + 1




########################################################################
# stop recording log
end_rec_log()
