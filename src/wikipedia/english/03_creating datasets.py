# -*- coding: utf-8 -*-
"""
Created on Mon Sep  2 16:07:28 2024

@author: David.OSullivan
"""




# =============================================================================
# 
# =============================================================================

files = find_files_with_pattern("./data/")
df = pd.DataFrame()
failed_files = []
for file_i in files:
    with open(file_i, 'r', encoding='utf-8', errors='replace') as file:
        data = file.read()
        json_data = json.loads(data)
        for i in range(0, len(json_data)):
            try:
                json_data_row = json_data[i]
                # if json_data_row.get('successfully_scrape') == "yes":
                temp = create_row(json_data_row)
                df = pd.concat([df, temp], ignore_index=True)  
            except:
                print(f"Failed: {json_data_row['url']}")
df.to_csv('./data/all_bios.csv', index=False)


# =============================================================================
# create player positions dataset
# =============================================================================


df = pd.DataFrame()
failed_files = []
for file_i in files:
    with open(file_i, 'r', encoding='utf-8', errors='replace') as file:
        data = file.read()
        json_data = json.loads(data)
    for i in range(0, len(json_data)):
        try:
            json_data_row = json_data[i]
            # if json_data_row.get('successfully_scrape') == "yes":
            temp = create_row_postions(json_data_row)
            df = pd.concat([df, temp], ignore_index=True)
        except:
            print(f"Failed: {json_data_row['url']}")
df.to_csv('./data/all_positions.csv', index=False)


# =============================================================================
# teams
# =============================================================================


df = pd.DataFrame()
failed_files = []
failed_count = 0
for file_i in files:
    with open(file_i, 'r', encoding='utf-8', errors='replace') as file:
        data = file.read()
        json_data = json.loads(data)
    for i in range(0, len(json_data)):
        try:
            json_data_row = json_data[i]
            # if json_data_row.get('successfully_scrape') == "yes":
            temp = create_row_career(json_data_row)
            df = pd.concat([df, temp], ignore_index=True)
        except:
            failed_count = failed_count + 1
            print(f"Failed: {json_data_row['url']}")
failed_count
df.to_csv('./data/all_career.csv', index=False)



