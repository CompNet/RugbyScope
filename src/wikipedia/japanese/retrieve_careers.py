########################################################################
# Retrieves player information from japanese Wikipedia infoxes.
#
# Vincent Labatut
# 02/2025
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
start_rec_log("WikipediaJa")




########################################################################
# init file constants

# folder path
table_folder = path.join("data", "wikipedia", "japanese", "raw")

# base url of the website
base_url = "https://ja.wikipedia.org/wiki/{}"




########################################################################
# init HTML constants
BIRTH = "出生"
BIRTH_DATE = "生年月日"
BIRTH_PLACE = ["出身地", "出生地", "生誕地"]
DEATH_DATE = "没年月日"
DEATH_PLACE = ["出身校", "死没地"]
HEIGHT = "身長"
WEIGHT = "体重"
RUGBY_UNION_CAREER = "ラグビーユニオンでの経歴"
POSITIONS = "ポジション"
CURRENT_TEAM = "在籍チーム"
PERIOD = "年"

# relevant info sections
YOUTH_CAREER = "ユース経歴"
AMAT_CAREER = "アマチュア経歴"
SENIOR_CAREER = "シニア経歴"
STATE_CAREER = "州代表"
SUPRUG_CAREER = "スーパーラグビー"
INTNL_CAREER = "代表"
CAREER_MAP = {
    YOUTH_CAREER: "Youth",
    AMAT_CAREER: "Amateur",
    SENIOR_CAREER: "Senior",
    SUPRUG_CAREER: "Senior",
    STATE_CAREER: "Regional",
    INTNL_CAREER: "International"
}

# irrelevant info sections
COACH_CAREER = "コーチ歴"
SEVENS_CAREER = "7人制代表"
REFEREE_CAREER = "レフリー歴"
CAREER_DISC = {SEVENS_CAREER, COACH_CAREER, REFEREE_CAREER}




########################################################################
def clean_score_str(text):
    """Clean strings representing points cored or matches played.

    :param text (str): text to clean.
    :returns: input string after cleaning (can be empty).
    """

    text = re.sub(r"\(?\?\)?", "?", text)
    # text = re.sub(r"\?", "", text)
    text = re.sub(r"\[\d+\]", "", text)
    text = re.sub(r" +", " ", text)
    text = text.strip()

    return text




########################################################################
# load list of players
merged_table = pd.read_csv(path.join("data", "dbpedia", "tables", "players_01_wd-dbp.csv"))
player_number = merged_table.shape[0]




########################################################################
# init lists to store extracted data
career_steps = []
player_info = []
diff_sect = [] # this is for debug



########################################################################
# loop over players
p = 1
# for player_page in ["%E3%82%A4%E3%83%AA%E3%82%A2%E3%83%BB%E3%82%BC%E3%83%89%E3%82%AE%E3%83%8B%E3%82%BC"]:
#     orig_name = ""
#     orig_id = ""
for _, player in merged_table.iterrows():
    player_page = player["wikipediaJa"]
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

    # no japanese wikipedia page for this player
    if pd.isnull(player_page):
        tlog(2, f"No Japanese Wikipedia page for this player")
        comment = "No WP JA page"

    # there is a japanese wikipedia page for this player
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
            temp = soup.find_all("table", {"class": "infobox"})
            if len(temp) > 0:
                infobox_elt = temp[0]
            else:
                tlog(2, f"Could not find any infobox: skipping the rest of the extraction process")
                continue

            # player name
            caption_elt = infobox_elt.find("caption", itemprop="name")
            if caption_elt:
                name = caption_elt.get_text(strip=True)
                tlog(2, f"Name: {name}")
            else:
                tlog(2, f"Could not find the name (rugby probably not the main activity)")

            # birth information
            th_elt = infobox_elt.find("th", string=BIRTH_DATE)
            if th_elt:
                # birth date
                td_elt = th_elt.find_next_siblings()[0]
                span_elt = td_elt.find("span", {"class": "bday"})
                if span_elt:
                    birth_date = span_elt.get_text(strip=True)
                else:
                    text = td_elt.get_text(strip=True)
                    pattern = r"(\d+)[^\d]*(\d+|\?+)[^\d]*(\d+|\?+)[^\d]*"
                    vals = re.findall(pattern, text)[0]
                    month = vals[1]
                    if re.match(r"\?+", month):
                        month = "1"
                    day = vals[2]
                    if re.match(r"\?+", day):
                        day = "1"
                    birth_date = vals[0] + "-" + f"{int(month):02d}" + "-" + f"{int(day):02d}"
                tlog(2, f"Birth date: {birth_date}")
                # birth place
                th_elt = infobox_elt.find("th", string=lambda text: text in BIRTH_PLACE)
                if th_elt:
                    td_elt = th_elt.find_next_siblings()[0]
                    a_elts = td_elt.find_all("a")
                    for a_elt in a_elts:
                        if not a_elt.has_attr("class") or a_elt["class"] != "mw-file-description":
                            if birth_place == "":
                                birth_place = a_elt.get_text(strip=True)
                                birth_place_url = a_elt["href"]
                            else:
                                birth_place = birth_place + "; " + a_elt.get_text(strip=True)
                                birth_place_url = birth_place_url + "; " + a_elt["href"]
                    tlog(2, f"Birth place: {birth_place}")
            else:
                th_elts = infobox_elt.find_all("th") # for some reason, infobox_elt.find("th", string=BIRTH) does not work...
                th_elt = None
                for tmp in th_elts:
                    if tmp.get_text(strip=True) == BIRTH:
                        th_elt = tmp
                        break
                if th_elt:
                    # birth date
                    td_elt = th_elt.find_next_siblings()[0]
                    birth_date = td_elt.find("span", {"class": "bday"}).get_text(strip=True)
                    tlog(2, f"Birth date: {birth_date}")
                    # birth place
                    br_elt = td_elt.find_all("br")[0]
                    a_elts = br_elt.find_next_siblings("a")
                    for a_elt in a_elts:
                        if not a_elt.has_attr("class") or a_elt["class"] != "mw-file-description":
                            if birth_place == "":
                                birth_place = a_elt.get_text(strip=True)
                                birth_place_url = a_elt["href"]
                            else:
                                birth_place = birth_place + "; " + a_elt.get_text(strip=True)
                                birth_place_url = birth_place_url + "; " + a_elt["href"]
                    tlog(2, f"Birth place: {birth_place}")
                else:
                    tlog(2, f"Could not find birth information")

            # death information
            th_elt = infobox_elt.find("th", string=DEATH_DATE)
            if th_elt:
                # death date
                td_elt = th_elt.find_next_siblings()[0]
                death_elt = td_elt.find("span", {"class": "dday deathdate"})
                if death_elt:
                    death_date = death_elt.get_text(strip=True)
                else:
                    text = td_elt.get_text(strip=True)
                    pattern = r"(\d+)[^\d]*(\d+|\?+)[^\d]*(\d+|\?+)[^\d]*"
                    vals = re.findall(pattern, text)[0]
                    month = vals[1]
                    if re.match(r"\?+", month):
                        month = "1"
                    day = vals[2]
                    if re.match(r"\?+", day):
                        day = "1"
                    death_date = vals[0] + "-" + f"{int(month):02d}" + "-" + f"{int(day):02d}"
                tlog(2, f"Death date: {death_date}")
                # death place
                th_elt = infobox_elt.find("th", string=lambda text: text in DEATH_PLACE)
                if th_elt:
                    td_elt = th_elt.find_next_siblings()[0]
                    a_elts = td_elt.find_all("a")
                    death_place = ""
                    death_place_url = ""
                    for a_elt in a_elts:
                        if not a_elt.has_attr("class") or a_elt["class"] != "mw-file-description":
                            if death_place == "":
                                death_place = a_elt.get_text(strip=True)
                                death_place_url = a_elt["href"]
                            else:
                                death_place = death_place + "; " + a_elt.get_text(strip=True)
                                death_place_url = death_place_url + "; " + a_elt["href"]
                    tlog(2, f"Death place: {death_place}")
                else:
                    tlog(2, f"Could not find death place")

            # height
            th_elt = infobox_elt.find("th", string=HEIGHT)
            if th_elt:
                height = th_elt.find_next_siblings()[0].get_text(strip=True)
                if height != "":
                    height = height.replace(u"\xa0", u" ")
                    height = height.replace(",", ".")   # in case a comma is used as a decimal separator
                    pattern = r"(\d.?\d\d?) ?c?m.*"
                    vals = re.findall(pattern, height)
                    height = vals[0]
                    if "." in height:
                        height = int(float(height) * 100)
                    tlog(2, f"Height: {height} cm")
            else:
                tlog(2, f"Could not find height")

            # weight
            th_elt = infobox_elt.find("th", string=WEIGHT)
            if th_elt:
                weight = th_elt.find_next_siblings()[0].get_text(strip=True)
                weight = weight.replace(u"\xa0", u" ")
                pattern = r"(\d+[.,]?\d*) ?(?:kg|キログラム).*"
                weight = re.findall(pattern, weight)[0]
                tlog(2, f"Weight: {weight} kg")
            else:
                tlog(2, f"Could not find weight")

            # get career table
            tr_elt = infobox_elt.find("tr", string=RUGBY_UNION_CAREER)
            if tr_elt:
                career_elt = tr_elt.find_next_siblings()[0]

                # positions
                th_elt = career_elt.find("th", string=POSITIONS)
                if th_elt:
                    td_elt = th_elt.find_next_siblings()[0]
                    a_elts = td_elt.find_all("a")
                    if len(a_elts) > 0:
                        positions = [elt.get_text(strip=True) for elt in a_elts]
                        positions = "; ".join(positions)
                    else:
                        positions = td_elt.get_text(strip=True)
                        positions = positions.replace(u"\xa0", u" ")
                        positions = re.sub(r", *", "; ", positions)
                    tlog(2, f"Positions: {positions}")
                else:
                    tlog(2, f"Could not find positions")

                # current team
                th_elt = career_elt.find("th", string=CURRENT_TEAM)
                if th_elt:
                    td_elt = th_elt.find_next_siblings()[0]
                    current_team = td_elt.get_text(strip=True)
                    current_team = current_team.replace(u"\xa0", u" ")
                    a_elt = td_elt.find("a")
                    if a_elt:
                        current_team_url = a_elt["href"]
                    tlog(2, f"Current team: {current_team} ({current_team_url})")
                else:
                    tlog(2, f"Could not find current team")

                # career steps
                for section in CAREER_MAP.keys():
                    tr_elt = career_elt.find("tr", string=section)
                    if tr_elt:
                        while True:
                            tr_elts = tr_elt.find_next_siblings()
                            if len(tr_elts) == 0:
                                break
                            tr_elt = tr_elts[0]
                            th_elt = tr_elt.find("th")
                            if not th_elt:
                                break
                            if th_elt.has_attr("colspan") and th_elt["colspan"] == "4":
                                break
                            text = th_elt.get_text(strip=True)
                            if text != PERIOD:
                                # time period
                                period = ""
                                period_elts = th_elt.contents
                                if len(period_elts) == 0:
                                    period = th_elt.get_text(strip=True)
                                for period_elt in period_elts:
                                    if period_elt.name is None:
                                        period = period + period_elt.strip()
                                    elif period_elt.name == "br":
                                        period = period + "; "
                                    else:
                                        period = period + period_elt.get_text(strip=True)

                                # team or club
                                team = ""
                                team_url = ""
                                td_elt = th_elt.find_next_siblings()[0]
                                team_elts = td_elt.contents
                                if len(team_elts) == 0:
                                    team = td_elt.get_text(strip=True)
                                else:
                                    first_child = next((child for child in td_elt.children if child.name), None)
                                    if not first_child is None and first_child.name == "span" and not first_child.has_attr("title") and not "flagicon" in first_child.get("class", []) and not "mw-image-border" in first_child.get("class", []):
                                        team_elts = first_child.children
                                for team_elt in team_elts:
                                    if team_elt.name is None:
                                        team = team + team_elt.strip()
                                    elif team_elt.name == "br":
                                        team = team + "; "
                                        team_url = team_url + "; "
                                    else:
                                        team = team + team_elt.get_text(strip=True)
                                        if team_elt.name == "a":
                                            team_url = team_url + team_elt["href"]

                                # matches played
                                matches_played = ""
                                td_elts = td_elt.find_next_siblings()
                                if len(td_elts) > 0:
                                    td_elt = td_elts[0]
                                    matches_elts = td_elt.contents
                                    if len(matches_elts) == 0:
                                        matches_played = td_elt.get_text(strip=True)
                                    for matches_elt in matches_elts:
                                        if matches_elt.name is None:
                                            matches_played = matches_played + matches_elt.strip()
                                        elif matches_elt.name == "br":
                                            matches_played = matches_played + "; "
                                        else:
                                            matches_played = matches_played + matches_elt.get_text(strip=True)
                                    matches_played = clean_score_str(matches_played)

                                    # points scored
                                    points_scored = ""
                                    pattern = r"(\d+)"
                                    td_elt = td_elt.find_next_siblings()[0]
                                    points_elts = td_elt.contents
                                    if len(points_elts) == 0:
                                        text = td_elt.get_text(strip=True)
                                        tmp = re.findall(pattern, text)
                                        if len(tmp) > 0:
                                            points_scored = tmp[0]
                                    for points_elt in points_elts:
                                        if points_elt.name is None:
                                            text = points_elt.strip()
                                            tmp = re.findall(pattern, text)
                                            if len(tmp) > 0:
                                                points_scored = points_scored + tmp[0]
                                        elif points_elt.name == "br":
                                            points_scored = points_scored + "; "
                                        else:
                                            text = points_elt.get_text(strip=True)
                                            tmp = re.findall(pattern, text)
                                            if len(tmp) > 0:
                                                points_scored = points_scored + tmp[0]
                                    points_scored = clean_score_str(points_scored)
                                
                                # create step
                                step = [orig_id, orig_name, name, player_page, CAREER_MAP[section], period, team, team_url, matches_played, points_scored]
                                career_steps.append(step)
                                has_career = True
                                tlog(4, f"Career step: {step})")

                if not has_career:
                    comment = "Career steps not found"
                    tlog(2, comment)

                # in case of types of career steps never seen before
                section_elts = career_elt.find_all("th", colspan="4")
                career_sections = [elt.get_text(strip=True) for elt in section_elts]
                diff = set(career_sections) - set(CAREER_MAP.keys()) - set(CAREER_DISC)
                if len(diff) > 0:
                    tlog(4, f"New sections detected in this page: " + "; ".join(diff))
                    diff_sect = set(diff_sect).union(diff)
                    diff_df = pd.DataFrame(diff_sect)
                    diff_df.to_csv(path.join(table_folder, "debug__new_sections.csv"), index=False)

            # no career found
            else:
                comment = "Career block not found"
                tlog(2, comment)

    # record player info
    player_info.append([orig_id, orig_name, comment, name, player_page, birth_date, birth_place, birth_place_url, death_date, death_place, death_place_url, height, weight, positions, current_team])
    player_df = pd.DataFrame(player_info, columns=["origWdId", "origName", "debugComment", "jaName", "wpPage", "birthDate", "birthPlace", "birthPlaceWP", "deathDate", "deathPlace", "deathPlaceWP", "height", "weight", "positions", "currentTeam"])
    player_df.to_csv(path.join(table_folder, "player_info.csv"), index=False)
                
    # record career steps
    if has_career:
        career_df = pd.DataFrame(career_steps, columns=["origWdId", "origName", "jaName", "wpPage", "stepType", "timePeriod", "teamName", "teamWP", "matchesPlayed", "pointsScored"])
        career_df.to_csv(path.join(table_folder, "player_careers.csv"), index=False)
                
    p = p + 1




########################################################################
# stop recording log
end_rec_log()
