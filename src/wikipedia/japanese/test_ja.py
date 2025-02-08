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
from mylogging import tlog




########################################################################
# init file constants

# folder path
table_folder = path.join("data", "wikipedia", "japanese")

# base url of the website
base_url = "https://ja.wikipedia.org/wiki/{}"




########################################################################
# init HTML constants
BIRTH = "出生"
BIRTH_DATE = "生年月日"
BIRTH_PLACE = "出身地"
DEATH_DATE = "没年月日"
DEATH_PLACE = "出身校"
HEIGHT = "身長"
WEIGHT = "体重"
RUGBY_UNION_CAREER = "ラグビーユニオンでの経歴"
POSITIONS = "ポジション"
CURRENT_TEAM = "在籍チーム"
YOUTH_CAREER = "ユース経歴"
SENIOR_CAREER = "シニア経歴"
INTNL_CAREER = "代表"
PERIOD = "年"

CAREER_MAP = {
  YOUTH_CAREER: "Youth",
  SENIOR_CAREER: "Senior",
  INTNL_CAREER: "International"
}




########################################################################
# load list of players
merged_table = pd.read_csv(path.join("data", "dbpedia", "tables", "fusion_players_wd-dbp.csv"))
player_number = merged_table.shape[0]




########################################################################
# loop over players
#player_page = "%E3%82%A2%E3%83%B3%E3%83%88%E3%83%AF%E3%83%BC%E3%83%8C%E3%83%BB%E3%83%87%E3%83%A5%E3%83%9D%E3%83%B3"
p = 1
for _, player in merged_table.iterrows():
    player_page = player["wikipediaJa"]
    orig_name = player["fullName"]
    orig_id = player["wikidataId"]
    tlog(0, f"Processing player {p}/{player_number}: {orig_name} ({orig_id})")

    # no japanese wikipedia page for this player
    if pd.isnull(player_page):
        tlog(2, f"No Japanese Wikipedia page for this player")

    # there is a japanese wikipedia page for this player
    else:
        url = base_url.format(player_page)
        tlog(2, f"Processing URL: {url}")

        # fetch webpage
        response = requests.get(url)

        # check if the request was successful
        if response.status_code == 200:
            # parse the HTML content using BeautifulSoup
            soup = BeautifulSoup(response.content, "html.parser")

            # get the infobox element
            infobox_elt = soup.find_all("table", {"class": "infobox"})[0]

            # player name
            caption_elt = infobox_elt.find("caption", itemprop="name")
            if caption_elt:
                name = caption_elt.get_text(strip=True)
                tlog(4, f"Name: {name}")
            else:
                name = ""
                tlog(4, f"Could not find the name")

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
                    pattern = r"(\d+)[^\d]*(\d+)[^\d]*(\d+)[^\d]*"
                    vals = re.findall(pattern, text)[0]
                    birth_date = vals[0] + "-" + f"{int(vals[1]):02d}" + "-" + f"{int(vals[2]):02d}"
                tlog(4, f"Birth date: {birth_date}")
                # birth place
                th_elt = infobox_elt.find("th", string=BIRTH_PLACE)
                td_elt = th_elt.find_next_siblings()[0]
                a_elts = td_elt.find_all("a")
                birth_place = ""
                birth_place_url = ""
                for a_elt in a_elts:
                    if not a_elt.has_attr("class") or a_elt["class"] != "mw-file-description":
                        if birth_place == "":
                            birth_place = a_elt.get_text(strip=True)
                            birth_place_url = a_elt["href"]
                        else:
                            birth_place = birth_place + "; " + a_elt.get_text(strip=True)
                            birth_place_url = birth_place_url + "; " + a_elt["href"]
                tlog(4, f"Birth place: {birth_place}")
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
                    tlog(4, f"Birth date: {birth_date}")
                    # birth place
                    br_elt = td_elt.find_all("br")[0]
                    a_elts = br_elt.find_next_siblings("a")
                    birth_place = ""
                    birth_place_url = ""
                    for a_elt in a_elts:
                        if not a_elt.has_attr("class") or a_elt["class"] != "mw-file-description":
                            if birth_place == "":
                                birth_place = a_elt.get_text(strip=True)
                                birth_place_url = a_elt["href"]
                            else:
                                birth_place = birth_place + "; " + a_elt.get_text(strip=True)
                                birth_place_url = birth_place_url + "; " + a_elt["href"]
                    tlog(4, f"Birth place: {birth_place}")
                else:
                    birth_date = ""
                    birth_place = ""
                    birth_place_url = ""
                    tlog(4, f"Could not find birth information")

            # death date
            th_elt = infobox_elt.find("th", string=DEATH_DATE)
            if th_elt:
                td_elt = th_elt.find_next_siblings()[0]
                death_date = td_elt.find("span", {"class": "bday"}).get_text(strip=True)
                tlog(4, f"Death date: {birth_date}")
                # death place
                th_elt = infobox_elt.find("th", string=DEATH_PLACE)
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
                tlog(4, f"Death place: {death_place}")

            # height
            th_elt = infobox_elt.find("th", string=HEIGHT)
            if th_elt:
                height = th_elt.find_next_siblings()[0].get_text(strip=True)
                height = height.replace(u"\xa0", u" ")
                pattern = r"(\d.\d\d) m .*"
                vals = re.findall(pattern, height)
                height = int(float(vals[0]) * 100)
                tlog(4, f"Height: {height}")
            else:
                tlog(4, f"Could not find height")

            # weight
            th_elt = infobox_elt.find("th", string=WEIGHT)
            if th_elt:
                weight = th_elt.find_next_siblings()[0].get_text(strip=True)
                weight = weight.replace(u"\xa0", u" ")
                pattern = r"(\d\d) kg .*"
                weight = re.findall(pattern, weight)
                tlog(4, f"Weight: {weight}")
            else:
                tlog(4, f"Could not find weight")

            # get career table
            tr_elt = infobox_elt.find("tr", string=RUGBY_UNION_CAREER)
            if tr_elt:
                career_elt = tr_elt.find_next_siblings()[0]

                # positions
                th_elt = career_elt.find("th", string=POSITIONS)
                positions = th_elt.find_next_siblings()[0].get_text(strip=True)
                positions = positions.replace(u"\xa0", u" ")

                # current team
                th_elt = career_elt.find("th", string=CURRENT_TEAM)
                td_elt = th_elt.find_next_siblings()[0]
                current_team = td_elt.get_text(strip=True)
                current_team = current_team.replace(u"\xa0", u" ")
                current_team_url = td_elt.find("a")["href"]

                # record player info
                # TODO

                # career steps
                career_steps = []
                for section in [YOUTH_CAREER, SENIOR_CAREER, INTNL_CAREER]:
                    tr_elt = career_elt.find("tr", string=section)
                    if tr_elt:
                        while True:
                            tr_elt = tr_elt.find_next_siblings()[0]
                            if not tr_elt:
                                break
                            th_elt = tr_elt.find("th")
                            if not th_elt:
                                break
                            if th_elt.has_attr("colspan") and th_elt["colspan"] == "4":
                                break
                            text = th_elt.get_text(strip=True)
                            if text != PERIOD:
                                period = text
                                # team or club
                                td_elt = th_elt.find_next_siblings()[0]
                                team = td_elt.get_text(strip=True)
                                team_url = td_elt.find("a")["href"]
                                # matches played
                                td_elt = td_elt.find_next_siblings()[0]
                                matches_played = td_elt.get_text(strip=True)
                                # points scored
                                td_elt = td_elt.find_next_siblings()[0]
                                points_scored = td_elt.get_text(strip=True)
                                pattern = r"(\d+)"
                                points_scored = re.findall(pattern, points_scored)[0]
                                # create step
                                step = [name, player_page, CAREER_MAP[section], period, team, team_url, matches_played, points_scored]
                                career_steps.append(step)
                
                # record career steps
                # TODO

            # no career found
            else:
                tlog(4, f"Could not find any career information")
                
    p = p + 1
