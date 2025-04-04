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
start_rec_log("WikipediaIt")




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

MISC_INFO = "Autres informations"
COUNTRY_BIRTH = "Paese"
HEIGHT = "Altezza"
WEIGHT = "Peso"
COUNTRY_SPORT = "Union"
POSITIONS = "Ruolo"
CUR_TEAM = "Squadra"

# relevant info sections
CAREER = "Carriera"
YOUTH_CAREER = "Attività giovanile"
CLUB_CAREER = "Attività di club"
FRAN_CAREER = "Attività in franchise"
PROV_CAREER = "Attività provinciale"
INTNL_CAREER = "Attività da giocatore internazionale"
CAREER_MAP = {
    YOUTH_CAREER: "Youth",
    CLUB_CAREER: "Senior",
    FRAN_CAREER: "Senior",
    PROV_CAREER: "Regional",
    INTNL_CAREER: "International"
}

# irrelevant info sections
CAREER_DISC = {"Attività da allenatore"}
# MODIF_ICON = "Voir et modifier les données sur Wikidata"
# ON_LOAN = "Prêté à"




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
# for player_page in ["Jonathan_Sexton"]:  # Christophe_Dominici Antoine_Dupont Fabien_Galthié Jonathan_Sexton
#     orig_name = ""
#     orig_id = ""
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
    birth_country = ""
    sport_country = ""
    current_team = ""

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
            temp = soup.find_all("div", class_=["infobox", "infobox_v3"])
            if len(temp) > 0:
                infobox_elt = temp[0]
            else:
                tlog(2, f"Could not find any infobox: skipping the rest of the extraction process")
                comment = "No infobox found"
                player_info.append([orig_id, orig_name, comment, name, player_page, height, weight, positions, birth_country, sport_country, current_team])
                p = p + 1
                continue

            # player name
            div_elt = infobox_elt.find("div", ).find("div")
            if div_elt:
                name = div_elt.get_text(strip=True)
                tlog(2, f"Name: '{name}'")
                if name0 == name:
                    tlog(4, "Same name used for two distinct players (WARNING)")
            else:
                tlog(2, f"Could not find the name (rugby probably not the main activity)")
            caption_elt = infobox_elt.find("caption", string=IDENTITY)
            if caption_elt is None:     # alternate section name
                caption_elt = infobox_elt.find("caption", string=BIOGRAPHY)
            if caption_elt is None:
                tlog(2, f"Infobox not properly formatted: skipping the rest of the extraction process")
                comment = "Infobox format problem"
                player_info.append([orig_id, orig_name, comment, name, player_page, height, weight, positions, birth_country, sport_country, current_team])
                p = p + 1
                continue
            tmp_elts = caption_elt.find_next_siblings()
            # certain pages have several infoxes, only one contains personal info
            if len(tmp_elts) == 0:
                temp = soup.find_all("table", {"class": "infobox"})
                if len(temp) == 0:
                    tlog(2, f"Could not find the table in the infobox: skipping the rest of the extraction process")
                    comment = "Infobox format problem"
                    player_info.append([orig_id, orig_name, comment, name, player_page, height, weight, positions, birth_country, sport_country, current_team])
                    p = p + 1
                    continue
                else:
                    table_elt = temp[0]
                    id_elt = table_elt.find("tbody")
            else:
                id_elt = caption_elt.find_next_siblings()[0]

            # birth information
            #birth_elt = id_elt.find("th", string=BIRTH)
            birth_elt = id_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == BIRTH)
            if birth_elt:
                td_elt = birth_elt.find_next_siblings()[0]
                # birth date
                time_elt = td_elt.find("time")
                if time_elt:
                    if time_elt.has_attr("datetime"):
                        birth_date = time_elt["datetime"]
                    else:
                        birth_date = time_elt.get_text(strip=True)
                    br_elts = time_elt.find_next_siblings("br")
                else:
                    tmp_elt = td_elt.contents[0]
                    if tmp_elt.name is None:
                        birth_date = tmp_elt.strip()
                    else:
                        birth_date = td_elt.get_text(strip=True)
                    br_elts = td_elt.find_all("br")
                tlog(2, f"Birth date: {birth_date}")
                # birth place
                if len(br_elts) == 0:   # different type of infobox
                    br_elts = td_elt.find_next_siblings("br")
                    if len(br_elts) == 0:   # no birth place
                        a_elt = None
                    else:
                        tmp_elts = br_elts[0].find_next_siblings()
                        if len(tmp_elts) > 0: 
                            if tmp_elts[0].name is not None and tmp_elts[0].name == "a":
                                a_elt = tmp_elts[0]
                            else:
                                a_elt = tmp_elts[0].find_all("a")[0]
                        else:   # no url
                            birth_place = br_elts[0].next_sibling.strip()
                else:
                    tmp_elts = br_elts[0].find_next_siblings()
                    if len(tmp_elts) > 0: 
                        tmp_elt = tmp_elts[0]
                        if tmp_elt.name == "span":
                            a_elt = tmp_elt.find_all("a")[0]
                        else:
                            a_elt = br_elts[0].find_next_siblings("a")[0]
                    else:   # no url
                        birth_place = br_elts[0].next_sibling.strip()                    
                if a_elt:
                    birth_place = a_elt.get_text(strip=True)
                    birth_place_url = a_elt["href"]
                # display value
                if birth_place == "":
                    tlog(2, f"Could not find birth place")
                else:
                    tlog(2, f"Birth place: {birth_place} ({birth_place_url})")
            else:
                tlog(2, f"Could not find birth information")

            # death information
            #death_elt = id_elt.find("th", string=DEATH)
            death_elt = id_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == DEATH)
            if death_elt:
                td_elt = death_elt.find_next_siblings()[0]
                # death date
                time_elt = td_elt.find("time")
                if time_elt:
                    if time_elt.has_attr("datetime"):
                        death_date = time_elt["datetime"]
                    else:
                        death_date = time_elt.get_text(strip=True)
                    br_elts = time_elt.find_next_siblings("br")
                else:
                    tmp_elt = td_elt.contents[0]
                    if tmp_elt.name is None:
                        death_date = tmp_elt.strip()
                    else:
                        death_date = td_elt.get_text(strip=True)
                    br_elts = td_elt.find_all("br")
                tlog(2, f"Death date: {death_date}")
                # death place
                if len(br_elts) == 0:       # different type of infobox
                    br_elts = td_elt.find_next_siblings("br")
                    if len(br_elts) == 0:   # no death place
                        a_elt = None
                    else:
                        tmp_elts = br_elts[0].find_next_siblings()
                        if len(tmp_elts) > 0: 
                            if tmp_elts[0].name is not None and tmp_elts[0].name == "a":
                                a_elt = tmp_elts[0]
                            else:
                                a_elt = tmp_elts[0].find_all("a")[0]
                        else:   # no url
                            death_place = br_elts[0].next_sibling.strip()
                else:
                    tmp_elts = br_elts[0].find_next_siblings()
                    if len(tmp_elts) > 0: 
                        tmp_elt = tmp_elts[0]
                        if tmp_elt.name == "span":
                            a_elt = tmp_elt.find_all("a")[0]
                        else:
                            a_elt = br_elts[0].find_next_siblings("a")[0]
                    else:   # no url
                        death_place = br_elts[0].next_sibling.strip()
                if a_elt:
                    death_place = a_elt.get_text(strip=True)
                    death_place_url = a_elt["href"]
                # display value
                if death_place == "":
                    tlog(2, f"Could not find death place")
                else:
                    tlog(2, f"Death place: {death_place} ({death_place_url})")
            else:
                tlog(2, f"Could not find death information")

            # possibly go to a different section
            if caption_elt.get_text(strip=True) == BIOGRAPHY:
                caption_elt = infobox_elt.find("caption", string=MISC_INFO)
                if caption_elt:
                    id_elt = caption_elt.find_next_siblings()[0]
                else:
                    tlog(2, f"Infobox not properly formatted: skipping the rest of the extraction process")
                    comment = "Infobox format problem"
                    player_info.append([orig_id, orig_name, comment, name, player_page, height, weight, positions, birth_country, sport_country, current_team])
                    p = p + 1
                    continue

            # height
            #height_elt = id_elt.find("th", string=HEIGHT)
            height_elt = id_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == HEIGHT)
            if height_elt:
                height = height_elt.find_next_siblings()[0].get_text(strip=True)
                height = height.replace(u"\xa0", u" ")
                height = height.replace(",", ".")
                height = height.strip()
                if height != "":
                    pattern = r"(\d.?\d?\d?) *c?m.*"
                    vals = re.findall(pattern, height)
                    height = vals[0]
                    if "." in height:
                        height = int(float(height) * 100)
                    tlog(2, f"Height: {height} cm")
            else:
                tlog(2, f"Could not find height")

            # positions
            pos_elt = id_elt.find("th", string=lambda text: text in [POSITIONS1, POSITIONS3])
            if pos_elt:
                td_elt = pos_elt.find_next_siblings()[0]
                a_elts = td_elt.find_all("a", recursive = False)
                if a_elts:
                    positions = "; ".join(a.get_text(strip=True) for a in a_elts)
                else:
                    positions = td_elt.get_text(strip=True)
                tlog(2, f"Positions: {positions}")
            # different type of infobox
            else:
                pos_elt = id_elt.find("th", string=POSITIONS2)
                if pos_elt:
                    td_elt = pos_elt.find_next_siblings()[0]
                    a_elts = td_elt.find_all("a", title=lambda t: t != MODIF_ICON, recursive = True)
                    if a_elts:
                        positions = "; ".join(a.get_text(strip=True) for a in a_elts)
                    else:
                        positions = td_elt.get_text(strip=True)
                else:
                    tlog(2, f"Could not find positions")

            # get career sections
            table_elt = caption_elt.parent
            table_elt = table_elt.find_next_siblings()[0]
            while table_elt and table_elt.name == "table":
                caption_elt = table_elt.find("caption")
                section = caption_elt.get_text(strip=True)
                if section in CAREER_MAP.keys():
                    stint_types = []; periods = []; teams = []; urls = []; matches = []; points = []
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
                    skip_periods = []
                    while r < len(periods_elt):
                        period_str = ""
                        # possibly skip whitespaces, <s> <i> <b> and <u> <span> elements
                        while r < len(periods_elt) and (periods_elt[r].name is None and periods_elt[r].strip() == "" or
                                                        periods_elt[r].name is not None and (periods_elt[r].name in ["s", "i", "b", "u", "span"])):
                            if periods_elt[r].name is not None and (periods_elt[r].name in ["i", "b", "u"] or
                                                                    periods_elt[r].name == "span" and (not periods_elt[r].has_attr("title") or periods_elt[r]["title"] != ON_LOAN)):
                                skip_periods.append(len(periods))  # list of rows to skip in teams and stats too
                            r = r + 1
                        # concatenate text and hyperlink content
                        while r < len(periods_elt) and (periods_elt[r].name is None or periods_elt[r].name in ["a", "sup", "s"]):
                            if periods_elt[r].name is None:
                                period_str = period_str.strip() + " " + periods_elt[r].strip()
                            elif periods_elt[r].name not in ["sup", "s"]:
                                period_str = period_str.strip() + " " + periods_elt[r].get_text(strip=True)
                            r = r + 1
                        # clean final string
                        period_str = period_str.replace(u"\xa0", u" ")
                        period_str = period_str.replace(" -", "-")
                        period_str = period_str.replace("- ", "-")
                        period_str = period_str.strip()
                        periods.append(period_str)
                        if r < len(periods_elt): 
                            r = r + 1   # next row
                    # possibly add a last one
                    if len(periods_elt) > 0 and periods_elt[r-1].name is not None and periods_elt[r-1].name in ["br", "div"]:
                        periods.append("")
                    
                    # loop over br-separated teams
                    r = 0
                    skip_teams = []
                    while r < len(teams_elt):
                        team_str = ""
                        url_str = ""
                        # possibly skip whitespaces, <s> <i> <b> and <u> elements, flags, arrows
                        while r < len(teams_elt) and (teams_elt[r].name is None and teams_elt[r].strip() == "" or
                                                      teams_elt[r].name is not None and (teams_elt[r].name in ["s", "i", "b", "u"] or
                                                                                         teams_elt[r].name == "span" and teams_elt[r].has_attr("class") and "flagicon" in teams_elt[r]["class"] or
                                                                                         teams_elt[r].name == "span" and teams_elt[r].has_attr("title") and teams_elt[r]["title"] == ON_LOAN)):
                            if teams_elt[r].name is not None and teams_elt[r].name in ["i", "b", "u"]:
                                skip_teams.append(len(teams))  # list of rows to skip in periods and stats too
                            r = r + 1
                        # retrieve team name
                        while r < len(teams_elt) and (teams_elt[r].name is None or teams_elt[r].name != "br" and
                                                      (teams_elt[r].name !="div" or not teams_elt[r].has_attr("class") or not "clear" in teams_elt[r]["class"])):
                            if teams_elt[r].name is None:
                                team_str = team_str.strip() + " " + teams_elt[r].strip()
                            else:
                                team_str = team_str.strip() + " " + teams_elt[r].get_text(strip=True)
                                # case where url is inside <span>
                                if teams_elt[r].name == "span":
                                    a_elt = teams_elt[r].find("a", recursive = False) # skip flag <span>
                                    if a_elt:
                                        url_str = a_elt["href"]
                                # case where url is inside <a>
                                elif teams_elt[r].name == "a":
                                    url_str = teams_elt[r]["href"]
                            r = r + 1
                        # clean final string
                        team_str = team_str.replace(u"\xa0", u" ")
                        team_str = team_str.replace("  ", " ")
                        team_str = team_str.strip()
                        teams.append(team_str)
                        urls.append(url_str)
                        if r < len(teams_elt): 
                            r = r + 1   # next row
                    # possibly add a last one
                    if len(teams_elt) > 0 and teams_elt[r-1].name is not None and teams_elt[r-1].name in ["br", "div"]:
                        teams.append("")
                        urls.append("")
                    
                    # check that list lengths are consistent
                    if len(periods) != len(teams):
                        if len(periods) == 0:
                            periods = [""] * len(teams)
                        # if missing values, we assume that it is the last ones (makes sense graphically speaking)
                        elif len(periods) < len(teams):
                            periods.extend([""] * (len(teams) - len(periods)))
                        # otherwise, problem
                        else: #len(periods) > len(teams):
                            raise Exception("Too many periods compared to teams")
                    
                    # handle skipped rows
                    skip_rows = skip_periods + skip_teams
                    # add empty rows to skipped rows
                    for i in range(len(periods)):
                        if periods[i] == "" and teams[i] == "":
                            skip_rows.append(i)
                    # sort and remove redundant values
                    skip_rows = list(set(skip_rows))
                    skip_rows.sort()
                    # possibly remove these superfluous entries
                    for index in sorted(skip_rows, reverse=True):
                        del teams[index]
                        del urls[index]
                        del periods[index]
                    # complete stint types list
                    stint_types = [stint_type] * len(teams)

                    # check that list lengths are consistent
                    if len(periods) != len(teams):
                        if len(periods) == 0:
                            periods = [""] * len(teams)
                        elif len(skip_rows) > 0:
                            for index in sorted(skip_rows, reverse=True):
                                del periods[index]
                        # if missing values, we assume that it is the last ones (makes sense graphically speaking)
                        if len(periods) < len(teams):
                            periods.extend([""] * (len(teams) - len(periods)))
                        # otherwise, problem
                        elif len(periods) > len(teams):
                            raise Exception("Inconsistent numbers of periods/teams")
                    
                    # loop over br-separated stats
                    if not stats_elt:
                        matches = [""] * len(periods)
                        points = [""] * len(periods)
                    else:
                        r = 0
                        while r < len(stats_elt):
                            stat_str = ""
                            # possibly skip whitespaces, <s> elements
                            while r < len(stats_elt) and (stats_elt[r].name is None and stats_elt[r].strip() == "" or
                                                        stats_elt[r].name is not None and stats_elt[r].name == "s"):
                                r = r + 1
                            # retrieve stat string
                            while r < len(stats_elt) and (stats_elt[r].name is None or stats_elt[r].name != "br"):
                                # if text: add to string
                                if stats_elt[r].name is None:
                                    stat_str = stat_str.strip() + " " + stats_elt[r].strip()
                                # anything else but <s> or <sup>: also add to string
                                elif stats_elt[r].name not in ["s", "sup"]:
                                    stat_str = stat_str.strip() + " " + stats_elt[r].get_text(strip=True).strip()
                                r = r + 1
                            # clean final string
                            stat_str = stat_str.replace(u"\xa0", u" ")
                            stat_str = stat_str.strip()
                            stat_str = stat_str.replace("  ", " ")
                            # retrieve values
                            if stat_str in ["", "?", "-", "–", "()"]:
                                matches.append("")
                                points.append("")
                            else:
                                if "(" in stat_str:
                                    pattern = r"^\+?(?: *de)? *(\d+\+?|\?+|-)[^\d]*\( *(\d? ?\d+\+?|\?+|-) *\)"
                                    vals = re.findall(pattern, stat_str)
                                    matches.append(vals[0][0])
                                    points.append(vals[0][1])
                                else:
                                    pattern = r"^(\d+\+?|\?+|-)"
                                    vals = re.findall(pattern, stat_str)
                                    matches.append(vals[0])
                                    points.append("")
                            if r < len(stats_elt): 
                                r = r + 1   # next row
                        # possibly add a last one
                        if len(stats_elt) > 0 and stats_elt[r-1].name is not None and stats_elt[r-1].name == "br":
                            matches.append("")
                            points.append("")
                    
                    # check list lengths are consistent
                    if len(periods) != len(matches):
                        # if completely empty: complement
                        if len(matches) == 0:
                            matches = [""] * len(periods)
                        # if missing values, we assume that the last ones are missing (makes sense graphically speaking)
                        if (len(periods) + len(skip_rows)) > len(matches):
                            matches.extend([""] * (len(periods) + len(skip_rows) - len(matches)))
                        # if rows to skip, delete superfluous entries
                        if len(skip_rows) > 0:
                            for index in sorted(skip_rows, reverse=True):
                                del matches[index]
                        # if lengths are still different, there's a problem
                        if len(periods) != len(matches):
                            raise Exception("Inconsistent numbers of periods/match numbers")
                    if len(periods) != len(points):
                        # if completely empty: complement
                        if len(points) == 0:
                            points = [""] * len(periods)
                        # if missing values, we assume that the last ones are missing (makes sense graphically speaking)
                        if (len(periods) + len(skip_rows)) > len(points):
                            points.extend([""] * (len(periods) + len(skip_rows) - len(points)))
                        # if rows to skip, delete superfluous entries
                        if len(skip_rows) > 0:
                            for index in sorted(skip_rows, reverse=True):
                                del points[index]
                        # if lengths are still different, there's a problem
                        if len(periods) != len(points):
                            raise Exception("Inconsistent numbers of periods/points scored")

                    # create stints
                    if len(periods) > 0:
                        tlog(4, f"Stints in section '{section}'")
                    for s in range(len(periods)):
                        stint = [orig_id, orig_name, name, player_page, stint_types[s], periods[s], teams[s], urls[s], matches[s], points[s]]
                        stint_info.append(stint)
                        has_career = True
                        tlog(6, f"{stint})")

                # in case of types of stints never seen before
                elif section not in CAREER_DISC:
                    tlog(4, f"Unknown section detected in this page: " + section)
                    diff_sect = set(diff_sect).union(section)
                    diff_df = pd.DataFrame(diff_sect, columns = ["Title"])
                    diff_df.to_csv(path.join(table_folder, "debug__new_sections.csv"), index=False)

                # turn to next section
                table_elt = table_elt.find_next_siblings()[0]

            if not has_career:
                comment = "No stint found"
                tlog(2, comment)

    # record player info
    player_info.append([orig_id, orig_name, comment, name, player_page, height, weight, positions, birth_country, sport_country, current_team])
    player_df = pd.DataFrame(player_info, columns=["origWdId", "origName", "debugComment", "itName", "wpPage", "height", "weight", "positions", "birthCountry", "sportCountry", "currentTeam"])
    player_df.to_csv(path.join(table_folder, "player_info.csv"), index=False)
                
    # record stints
    if has_career:
        stint_df = pd.DataFrame(stint_info, columns=["origWdId", "origName", "itName", "wpPage", "stintType", "timePeriod", "teamName", "teamWP", "matchesPlayed", "pointsScored"])
        stint_df.to_csv(path.join(table_folder, "stint_info.csv"), index=False)
                
    p = p + 1




########################################################################
# stop recording log
end_rec_log()
