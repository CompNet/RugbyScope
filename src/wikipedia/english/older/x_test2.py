# -*- coding: utf-8 -*-
"""
Created on Tue Aug 20 18:19:06 2024

@author: David.OSullivan
"""
# =============================================================================
# 
# =============================================================================

response = requests.get('https://en.wikipedia.org//wiki/Stephen_Ferris')
soup = BeautifulSoup(response.text, 'html.parser')
tables = soup.find_all('table', class_='infobox')
st = ""

infobox_table = tables[0]
# Extract all text while preserving the table structure
table_text = infobox_table.get_text(separator="\n", strip=True)
st = st + table_text 

scrapp("https://en.wikipedia.org//wiki/Stephen_Ferris")

# =============================================================================
# 
# =============================================================================

response = requests.get("https://en.wikipedia.org//wiki/Stephen_Ferris")
soup = BeautifulSoup(response.text, 'html.parser')
tables = soup.find_all('table', class_='infobox')
st = ""
if tables:
    infobox_table = tables[0]
    sr = scrape_rough(soup)
    
    # Extract all text while preserving the table structure
    table_text = infobox_table.get_text(separator="\n", strip=True)
    st = table_text + ""


result = scrap_rugby_wiki_wllama("https://en.wikipedia.org//wiki/Stephen_Ferris")

i = 0
url = df["url"][i]
name = df["name"][i]
# Apply the scrapp function and store the result
result = scrap_rugby_wiki_wllama(url, name)

# with open(f'./data/test.json', 'w', encoding='utf-8') as f:
#     json.dump(result, f, ensure_ascii=False, indent=4)

f = open("./data/test4.json", "w")
f.write(result.replace('\u00A0', ' '))
f.close()


# Open the JSON file
with open('./data/test.json', 'r') as file:
    # Load the JSON data into a Python dictionary
    data = json.load(file)
    
    
json.loads(result.replace('\u00A0', ' '))


# =============================================================================
# 
# =============================================================================

file = '1002_Tony Buckley.json'
with open(input_file_path, 'r', encoding='utf-8') as file:
    data = file.read()

# Use regex to find the JSON part
json_data = re.search(r'{.*}', data, re.DOTALL).group(0)
json_data = clean_brackets_in_numbers(json_data)
# Parse the JSON to ensure it's valid
try: 
    parsed_data = json.loads(json_data)
except:
    print("Missing clossing bracket.")
    try:
        parsed_data = json_data + "\n}"
        print("Added closing bracket succfully.")
    except: 
        print("Adding closing bracket failed! Savind file name.")
        failed_files.append(file)

# Define the output file path
output_file_path = os.path.join(output_file_directory, output_file_name)

# Write the cleaned JSON data to the output file
with open(output_file_path, 'w', encoding='utf-8') as output_file:
    json.dump(parsed_data, output_file, indent=4)


print(f"Cleaned JSON data has been saved to {output_file_path}")


# =============================================================================
# #
# =============================================================================
