

from _setup import *

url = "https://en.wikipedia.org/wiki/Ben_Prescott"
df = scrap_rugby_wiki_standard(url, "jimmy!")

json.dumps(df, indent=2)
# json_data = df
# print(json_data)
# # json_data = change_car_value_to_int(json_data)
# #

# car_stages = ["amateur","senior_club","international"]
# values = ["apps", "points"]


print(extract_number("(4112)"))

# car = json_data.get("career")
# for car_stage in car_stages:
#     for value in values:
#         selected_car = car[car_stage]
#         if len(selected_car) > 0:
#             for i in range(len(selected_car)):
#                 selected_value = selected_car[i][value]
#                 if isinstance(selected_value, str):
#                     selected_value = selected_value.replace(",","").strip().replace("+","")
#                     print(selected_value)
#                     selected_value = extract_number(selected_value) # recover the number of brackets are present
#                     print(selected_value)
#                 if selected_value in ["", "?", "(0)", "()","-", ")"]:
#                     selected_value = 0
#                 json_data["career"][car_stage][i][value] = int(selected_value)  



