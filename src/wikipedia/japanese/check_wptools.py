########################################################################
# Play with the wptools package, specially designed to retrieve information
# from Wikipedia infoboxes.
#
# Conclusion: still works, outputs the infobox content as a flat list of
# key/value pairs.
#
# Limitation: we lost the hierarchical structure of the infobox, which is
# often necessary to interpret its content and extract it properly. Thus,
# this library does not seem to fit our needs.
#
# Vincent Labatut
# 02/2025
########################################################################
import wptools




page = wptools.page("Antoine Dupont", lang='fr')
print(page)
page.get_parse()
page.data['infobox']["position"]
