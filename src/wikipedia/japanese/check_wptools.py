import wptools

page = wptools.page("Antoine Dupont", lang='fr')
print(page)
page.get_parse()
page.data['infobox']["position"]
