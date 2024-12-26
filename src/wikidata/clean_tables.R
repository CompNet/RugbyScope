# Functions used to clean certain data fields from the Wikidata tables.
#
# Vincent Labatut
# 12/2024
########################################################################




########################################################################
# Takes the player table and returns a normalized column representing
# their country, by combining two fields: `sportCountry` and `citizenship`.
# The function also normalizes certain country names.
#
# players: player table.
# 
# returns: vector of countries, one for each player in the table.
########################################################################
get_clean_countries <- function(players) {
  # use field sportCountry whenever available, otherwise field citizenship
  all_countries <- sapply(1:nrow(players), function(p) {
    if (is.na(players[p, "sportCountryLabels"]))
      players[p, "citizenshipLabels"]
    else
      players[p, "sportCountryLabels"]
  })
  # possibly split multiple values (arbitrarily keep the first one)
  all_countries <- sapply(all_countries, function(country) strsplit(country, "; ")[[1]][1])

  # normalize country names
  all_countries[all_countries == "United Kingdom of Great Britain and Ireland"] <- "United Kingdom"
  all_countries[all_countries == "Colony of New Zealand"] <- "New Zealand"
  all_countries[all_countries == "British Raj"] <- "India"
  all_countries[all_countries == "Irish Free State"] <- "Ireland"
  all_countries[all_countries == "中華民國"] <- "Taiwan"
  all_countries[all_countries == "Chinese Taipei"] <- "Taiwan"
  all_countries[all_countries == "Russian Empire"] <- "Russia"
  all_countries[all_countries == "People's Republic of China"] <- "China"
  all_countries[all_countries == "Kingdom of the Netherlands"] <- "Netherlands"
  all_countries[all_countries == "Czech Republic"] <- "Czechia"
  all_countries[all_countries == "United States of America"] <- "U.S.A."
  all_countries[all_countries == "Southern Rhodesia"] <- "Zimbabwe"
  all_countries[all_countries == "Democratic Republic of the Congo"] <- "R.C. of the Congp"
  # all_countries[all_countries == "England"] <- "United Kingdom"

  return (all_countries)
}
