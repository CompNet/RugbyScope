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




########################################################################
# Takes the player table and returns a normalized column representing
# their rugby positions (possibly mutliple positions by player).
#
# players: player table.
# 
# returns: vector of strings, one for each player in the table.
########################################################################
get_clean_positions <- function(players) {
  all_positions <- players[, "positionLabels"]
  # possibly split multiple values (arbitrarily keep the first one)
  # all_positions <- sapply(all_positions, function(position) strsplit(position, "; ")[[1]])
  all_positions <- strsplit(all_positions, "; ")

  # see all existing values
  # table(unlist(all_positions))

  # normalize positions
  map <- c()
  map["centre"] <- "Centre"
  # map["center"] <- "Centre"                   # not a WD rugby union position, generally a WD error
  # map["five-eighth"] <- "Scrum-half"          # not a WD rugby union position, generally a WD error
  map["flanker"] <- "Flanker"
  map["fly-half"] <- "Fly-half"
  # map["forward"] <- "Forward"                 # not a WD rugby union position, generally a WD error
  map["fullback"] <- "Fullback"
  map["hooker"] <- "Hooker"
  map["lock"] <- "Lock"
  map["Loosehead prop"] <- "Prop"             # not a WD rugby union position, generally a WD error
  map["number 8"] <- "Number 8"
  map["prop"] <- "Prop"
  map["scrum-half"] <- "Scrum-half"
  # map["second row"] <- "Second row"           # not a WD rugby union position, generally a WD error
  # map["Standoff"] <- "Fly-half"               # not a WD rugby union position, generally a WD error
  map["third line"] <- "Third row"
  map["utility back"] <- "Utility back"
  map["winger"] <- "Winger"
  for (p in 1:length(all_positions)) {
    positions <- all_positions[[p]]

    # normalize rugby positions
    for (position in names(map))
      positions[positions == position] <- map[position]

    # remove positions from other sports
    idx <- which(!(positions %in% map))
    if (length(idx) > 0)
      positions <- positions[-idx]
    if (length(positions) == 0)
      positions <- NA

    all_positions[[p]] <- positions
  }

  # collapse to get strings again
  result <- sapply(all_positions, function(positions) paste0(positions, collapse = "; "))

  return(result)
}
