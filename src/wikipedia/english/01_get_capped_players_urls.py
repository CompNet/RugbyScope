# -*- coding: utf-8 -*-
"""
Created on Sun Jan 12 20:23:41 2025

@author: David.OSullivan
"""

from _setup import *


def get_rugby_capped_players(url, name_col = 0):
    headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.5993.90 Safari/537.36"
    }

    response = requests.get(url, headers = headers)
    soup = BeautifulSoup(response.content, 'html.parser')

    # Find the table by its title
    tables = soup.find_all('table', class_= ["wikitable", 'sortable'])

    players = []

    for table in tables:
        headers = table.find_all("th") # find table headers
        if headers: # if headers are their
            # see if it has name? which is what we want (name of player)
            is_name_present = any(header.text.strip() == "Name" for header in headers)
            if is_name_present == True: # if that is their as well
                rows = table.find_all('tr') # find all the rows
                for row in rows[1:]:  # Skip the header row
                    cols = row.find_all('td') # find table data in the row
                    if len(cols) > 1: # does it have more than one data entry (col)
                        # Extract the player's name and URL
                        col = cols[name_col]  # First column contains the name and link
                        name = col.get_text(strip=True)
                        link = col.find('a')
                        full_url = f"https://en.wikipedia.org{link['href']}" if link else None
                        players.append({'name': name, 'url': full_url})
    df = pd.DataFrame(players)
    return df


df = pd.read_csv("./data/wikipedia/International_players_list_by_contry.csv")
for i in range(df.shape[0]):
    caps_url = df["url"].iloc[i]
    nation = df["nation"].iloc[i]
    name_col = int(df["name_col"].iloc[i])
    url_df = get_rugby_capped_players(caps_url, name_col)
    url_df.to_csv(f'./data/wikipedia/english/profile_links/profile_links_{nation}.csv', index=False)
    print(f"Finished with {nation}.")
    print(f"--- Number of rows: {url_df.shape[0]}")
