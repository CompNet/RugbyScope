########################################################################
# Retrieves player information from Spanish Wikipedia infoboxes.
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
start_rec_log("RetrievalEsWP")




########################################################################
# init file constants

# folder path
table_folder = path.join("data", "wikipedia", "spanish", "raw")

# base url of the website
base_url = "https://es.wikipedia.org/wiki/{}"




########################################################################
# init HTML constants
BIOGRAPHY = "Datos personales"

FULL_NAME = "Nombre completo"
BIRTH = "Nacimiento"
BIRTH_COUNTRY = "País"
DEATH = "Fallecimiento"
CITIZENSHIP = "Nacionalidad(es)"
HEIGHT = "Altura"
WEIGHT = "Peso"
POSITIONS = "Posición"

COUNTRY_TEAM = "Selección"
COUNTRY_MATCHES = "Part."
COUNTRY_POINTS = "Puntos"
COUNTRY_START = "Debut"

CAREER = "Trayectoria"

IGNORE_DATES = ["Fecha desconocida", "fecha desconocida", "Siglo", "Posterior a"]




########################################################################
# load list of players
merged_table = pd.read_csv(path.join("data", "fusion", "players_04_itwp.csv"))
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
# for player_page in ["Juan_Cruz_Mall%C3%ADa"]:  # Christophe_Dominici Antoine_Dupont Alun_Wyn_Jones Richie_McCaw Sergio_Parisse Brian_O%27Driscoll Fabien_Galthié Jonathan_Sexton
#     orig_name = "test"                                 # Juli%C3%A1n_Montoya Juan_Gonz%C3%A1lez_(rugbista) Juan_Cruz_Mall%C3%ADa
#     orig_id = "test"
for _, player in merged_table.iterrows():
    player_page = player["wikipediaEs"]
    orig_name = player["fullName"]
    orig_id = player["wikidataId"]
    tlog(0, f"Processing player {p}/{player_number}: {orig_name} ({orig_id})")

    has_career = False
    name0 = name
    name = ""
    full_name = ""
    birth_date = ""
    birth_place = ""
    birth_place_url = ""
    citizenship = ""
    death_date = ""
    death_place = ""
    death_place_url = ""
    comment = ""
    height = ""
    weight = ""
    positions = ""
    positions_url = ""
    #
    nat_team = ""
    nat_url = ""
    nat_start = ""
    nat_matches = ""
    nat_points = ""

    # no spanish wikipedia page for this player
    if pd.isnull(player_page):
        tlog(2, f"No Spanish Wikipedia page for this player")
        comment = "No ES WP page"

    # there is a spanish wikipedia page for this player
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
            infobox_elts = soup.find_all("table", class_=["infobox", "infobox_v3"])
            if len(infobox_elts) == 0:
                tlog(2, f"Could not find any infobox: skipping the rest of the extraction process")
                comment = "No infobox found"
                player_info.append([orig_id, orig_name, comment, name, full_name, player_page, birth_date, birth_place, birth_place_url, death_date, death_place, death_place_url, height, weight, positions, positions_url, citizenship])
                p = p + 1
                continue
            if len(infobox_elts) > 1:
                infobox_elts = [table for table in infobox_elts if "rugby" in table.get_text() or POSITIONS in table.get_text()]
                if len(infobox_elts) > 1:
                    tlog(2, f"WARNING: there are several infoboxes")

            for infobox_elt in infobox_elts:
                # player name
                th_elt = infobox_elt.find("tr", ).find("th")
                if th_elt:
                    name = th_elt.get_text(strip=True)
                    tlog(2, f"Name: '{name}'")
                    if name0 == name:
                        tlog(4, "Same name used for two distinct players (WARNING)")
                else:
                    tlog(2, f"Could not find the name (rugby probably not the main activity)")
                # full name
                name_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True).replace(u"\xa0", u" ") == FULL_NAME)
                if name_elt:
                    full_name = name_elt.find_next_siblings()[0].get_text(strip=True)
                    full_name = full_name.replace(r"\[\d+\]", "")
                    tlog(2, f"Full name: '{full_name}'")

                # birth information
                birth_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == BIRTH)
                if birth_elt:
                    temp_elts = birth_elt.find_next_siblings()[0].contents
                    i = 0
                    if temp_elts[i].name is None and temp_elts[i].strip() == "":
                        i = i + 1
                    # edit: the page sometimes starts with the date instead of the place
                    txt = birth_elt.find_next_siblings()[0].get_text(strip=True)
                    if txt.startswith(tuple(IGNORE_DATES)):
                        temp_elt = temp_elts[i]
                        birth_date = "?"
                        temp_elts = temp_elt.find_next_siblings("br")
                        if len(temp_elts) > 0:
                            temp_elt = temp_elts[0].next_sibling
                        else:
                            temp_elt = None
                    elif re.search(r"^\d", txt):
                        temp_elt = temp_elts[i]
                        birth_date = temp_elt.get_text(strip=True)
                        birth_date = birth_date.replace(u"\xa0", u" ")
                        if re.search(r"\d{4}", birth_date):
                            pattern = r"(.*\d{4})(?: \(\d+ años\))?"
                            vals = re.findall(pattern, birth_date)
                            birth_date = vals[0]
                            tlog(2, f"Birth date: {birth_date}")
                            temp_elts = temp_elt.find_next_siblings("br")
                            if len(temp_elts) > 0:
                                temp_elt = temp_elts[0].next_sibling
                            else:
                                temp_elt = None
                        else:
                            tlog(2, f"Ignored birth date (no year): {birth_date}")
                            birth_date = "?"
                    else:
                        temp_elt = temp_elts[i]
                    # birth place
                    if temp_elt:
                        if temp_elt.name is None:
                            birth_place = temp_elt.strip()
                        elif temp_elt.name == "a":
                            birth_place = temp_elt.get_text(strip=True)
                            birth_place_url = temp_elt["href"]
                        if birth_place != "":
                            tlog(2, f"Birth place: {birth_place} ({birth_place_url})")
                        else:
                            tlog(2, f"Could not find the birth place")
                    else:
                        tlog(2, f"Could not find the birth place")
                    # birth date
                    if birth_date == "":
                        temp_elts = temp_elt.find_next_siblings("br") 
                        temp_elt = temp_elts[0].next_sibling
                        if temp_elt.get_text(strip=True).startswith(tuple(IGNORE_DATES)):
                            birth_date = ""
                        else:
                            if not re.search(r"^\d", temp_elt.get_text(strip=True)):
                                temp_elt = temp_elts[1].next_sibling
                            birth_date = temp_elt.get_text(strip=True)
                            birth_date = birth_date.replace(u"\xa0", u" ")
                            pattern = r"(.*\d{4})(?: \(\d+ años\))?"
                            vals = re.findall(pattern, birth_date)
                            birth_date = vals[0]
                            tlog(2, f"Birth date: {birth_date}")
                else:
                    tlog(2, f"Could not find the birth section")

                # citizenship
                citi_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == BIRTH_COUNTRY)
                if citi_elt:
                    temp_elt = citi_elt.find_next_siblings()[0]
                    citizenship = temp_elt.get_text(strip=True)
                    tlog(2, f"Citizenship: {citizenship}")
                else:
                    citi_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == CITIZENSHIP)
                    if citi_elt:
                        temp_elt = citi_elt.find_next_siblings()[0]
                        citizenship = temp_elt.get_text(strip=True)
                        tlog(2, f"Citizenship: {citizenship}")
                    else:
                        tlog(2, f"Could not find the citizenship")

                # death information
                death_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == DEATH)
                if death_elt:
                    temp_elts = death_elt.find_next_siblings()[0].contents
                    i = 0
                    if temp_elts[i].name is None and temp_elts[i].strip() == "":
                        i = i + 1
                    # edit: the page sometimes starts with the date instead of the place
                    txt = death_elt.find_next_siblings()[0].get_text(strip=True)
                    if re.search(r"^\d", txt):
                        temp_elt = temp_elts[i]
                        death_date = temp_elt.get_text(strip=True)
                        death_date = death_date.replace(u"\xa0", u" ")
                        pattern = r"(.*\d{4})(?: \(\d+ años\))?"
                        vals = re.findall(pattern, death_date)
                        death_date = vals[0]
                        tlog(2, f"Death date: {death_date}")
                        temp_elts = temp_elt.find_next_siblings("br")
                        if len(temp_elts) > 0:
                            temp_elt = temp_elts[0].next_sibling
                        else:
                            temp_elt = None
                    else:
                        temp_elt = temp_elts[i]
                    # death place
                    if temp_elt:
                        if temp_elt.name is None:
                            death_place = temp_elt.strip()
                        elif temp_elt.name == "a":
                            death_place = temp_elt.get_text(strip=True)
                            death_place_url = temp_elt["href"]
                        if death_place != "":
                            tlog(2, f"Death place: {death_place} ({death_place_url})")
                        else:
                            tlog(2, f"Could not find the death place")
                    else:
                        tlog(2, f"Could not find the death place")
                    # death date
                    if death_date == "":
                        temp_elt = temp_elt.find_next_siblings("br")[0]
                        temp_elt = temp_elt.next_sibling
                        death_date = temp_elt.get_text(strip=True)
                        death_date = death_date.replace(u"\xa0", u" ")
                        pattern = r"(.+\d{4})(?: \(\d+ años\))?"
                        vals = re.findall(pattern, death_date)
                        death_date = vals[0]
                        tlog(2, f"Death date: {death_date}")

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
                    if weight != "" and re.search(r"^\d", weight):
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

                # national selection
                nati_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == COUNTRY_TEAM)
                if nati_elt:
                    temp_elt = nati_elt.find_next_siblings()[0]
                    nat_team = temp_elt.get_text(strip=True)
                    a_elts = temp_elt.find_all("a", recursive = False)
                    if len(a_elts) > 0:
                        nat_url = a_elts[0]["href"]
                    tlog(2, f"National team: {nat_team} ({nat_url})")
                else:
                    tlog(2, f"Could not find a national team")
                # starting year
                nati_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == COUNTRY_START)
                if nati_elt:
                    temp_elt = nati_elt.find_next_siblings()[0]
                    nat_start = temp_elt.get_text(strip=True)
                    pattern = r"^.*(\d{4})"
                    vals = re.findall(pattern, nat_start)
                    nat_start = vals[0]
                    tlog(4, f"Start date: {nat_start}")
                else:
                    tlog(4, f"Could not find a start date")
                # matches played
                nati_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == COUNTRY_MATCHES)
                if nati_elt:
                    temp_elt = nati_elt.find_next_siblings()[0]
                    nat_matches = temp_elt.get_text(strip=True)
                    pattern = r"(\d+|\?+)(?: *\((\d+|\?+)(?: ?pts)?\))?"
                    vals = re.findall(pattern, nat_matches)
                    nat_matches = vals[0][0]
                    nat_points = vals[0][1]
                    tlog(4, f"Matches played: {nat_matches}")
                    if nat_points != "":
                        tlog(4, f"Points scored: {nat_points}")
                else:
                    tlog(4, f"Could not find matches played")
                # points scored
                nati_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == COUNTRY_POINTS)
                if nati_elt:
                    temp_elt = nati_elt.find_next_siblings()[0]
                    nat_points = temp_elt.get_text(strip=True)
                    tlog(4, f"Points scored: {nat_points}")
                elif nat_points == "":
                    tlog(4, f"Could not find points scored")
                # add to stints
                if nat_team != "":
                    stint = [orig_id, orig_name, name, player_page, nat_start, "", nat_team, nat_url, nat_matches, nat_points]
                    stint_info.append(stint)
                    tlog(6, f"{stint})")

                # get career sections
                traj_elt = infobox_elt.find(lambda tag: tag.name == "th" and tag.get_text(strip=True) == CAREER)
                if traj_elt:
                    tlog(2, "Club stints:")
                    career_elt = traj_elt.parent.find_next_siblings()[0]
                    ul_elts = career_elt.find_all("ul")
                    if len(ul_elts) > 0:
                        has_career = True
                        ul_elt = ul_elts[0]
                        li_elt = ul_elt.find_all("li")[0]
                        while li_elt:
                            # retrieve information
                            str = li_elt.get_text(strip=True)
                            if ")" in str:
                                #pattern = r"^(.+) *\((\d+|\?+)[-–]?(\d+|\?+|[Aa]ctualidad|[Aa]ct\.|[Pp]resente)?\)"
                                # team name
                                pattern = r"^(.+) *\((.*)\)"
                                vals = re.findall(pattern, str)
                                team = vals[0][0]
                                period = vals[0][1]
                                # period(s)
                                pattern = r"(\d+|\?+)[-–]?(\d+|\?+|[Aa]ctualidad|[Aa]ct\.|[Pp]resente)?"
                                vals = re.findall(pattern, period)
                                start_years = []
                                end_years = []
                                for i in range(len(vals)):
                                    start_years.append(vals[i][0])
                                    end_years.append(vals[i][1])
                            else:
                                team = str
                                start_years = [""]
                                end_years = [""]
                            a_elts = li_elt.find_all("a", recursive = False)
                            if len(a_elts) > 0:
                                team_url = a_elts[0]["href"]
                            else:
                                team_url = ""
                            # add to stints
                            for start_year, end_year in zip(start_years, end_years):
                                stint = [orig_id, orig_name, name, player_page, start_year, end_year, team, team_url, "", ""]
                                stint_info.append(stint)
                                tlog(4, f"{stint})")
                            # next item
                            li_elts = li_elt.find_next_siblings("li")
                            if len(li_elts) > 0:
                                li_elt = li_elts[0]
                            else:
                                li_elt = None
                    else:
                        tlog(2, "Could not find any stint in 'career' section")
                else:
                    tlog(2, "Could not find the 'career' section")

            if not has_career:
                comment = "No stint found"
                tlog(2, comment)

    # record player info
    player_info.append([orig_id, orig_name, comment, name, full_name, player_page, birth_date, birth_place, birth_place_url, death_date, death_place, death_place_url, height, weight, positions, positions_url, citizenship])
    player_df = pd.DataFrame(player_info, columns=["origWdId", "origName", "debugComment", "esName", "esFullName", "wpPage", "birthDate", "birthPlace", "birthPlaceWP", "deathDate", "deathPlace", "deathPlaceWP", "height", "weight", "positions", "positionsWP", "citizenship"])
    player_df.to_csv(path.join(table_folder, "player_info.csv"), index=False)
                
    # record stints
    if has_career:
        stint_df = pd.DataFrame(stint_info, columns=["origWdId", "origName", "esName", "wpPage", "startYear", "endYear", "teamName", "teamWP", "matchesPlayed", "pointsScored"])
        stint_df.to_csv(path.join(table_folder, "stint_info.csv"), index=False)
                
    p = p + 1




########################################################################
# stop recording log
end_rec_log()
