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
# sys.path.append("src/common")
# from mylogging import tlog




########################################################################
# init constants

# folder path
table_folder = path.join("data", "wikipedia", "japanese")

# base url of the website
base_url = "https://ja.wikipedia.org/wiki/{}"

# text content
BIRTH_DATE = "生年月日"
BIRTH_PLACE = "出身地"
HEIGHT = "身長"
WEIGHT = "体重"
RUGBY_UNION_CAREER = "ラグビーユニオンでの経歴"




########################################################################
player_page = "%E3%82%A2%E3%83%B3%E3%83%88%E3%83%AF%E3%83%BC%E3%83%8C%E3%83%BB%E3%83%87%E3%83%A5%E3%83%9D%E3%83%B3"

url = base_url.format(player_page)

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
    name = caption_elt.get_text(strip=True)

    # birth date
    th_elt = infobox_elt.find("th", string=BIRTH_DATE)
    td_elt = th_elt.find_next_siblings()[0]
    birth_date = td_elt.find("span", {"class": "bday"}).get_text(strip=True)
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

    # death date
    # TODO
    # death place
    # TODO

    # weight
    th_elt = infobox_elt.find("th", string=HEIGHT)
    height = th_elt.find_next_siblings()[0].get_text(strip=True)
    height = height.replace(u"\xa0", u" ")
    pattern = r"(\d.\d\d) m .*"
    vals = re.findall(pattern, height)
    height = int(float(vals[0]) * 100)

   # height
   