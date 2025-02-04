########################################################################
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
get_merged_countries <- function(players) {
  # normalize country names in both fields
  cz <- get_clean_countries(players, field = "citizenshipLabels")
  sc <- get_clean_countries(players, field = "sportCountryLabels")

  # use field `sportCountry` whenever available, otherwise field `citizenship`
  all_countries <- sc
  idx <- which(is.na(all_countries))
  if (length(idx) > 0)
    all_countries[idx] <- cz[idx]

  # possibly split multiple values (arbitrarily keep the first one)
  all_countries <- sapply(all_countries, function(country) strsplit(country, "; ")[[1]][1])
  all_countries[all_countries == "NA"] <- NA

  return(all_countries)
}




########################################################################
# Takes the player table and returns a normalized vector representing
# their country, as specified by parameter `field`.
#
# players: player table.
# field: the name of the column containing country names.
# 
# returns: vector of countries, one for each player in the table.
########################################################################
get_clean_countries <- function(players, field) {
  # retrieve the country names
  all_countries <- players[, field]

  # possibly split multiple values
  all_countries <- sapply(all_countries, function(all_country) strsplit(all_country, "; ")[[1]])

  # see all existing values
  # sort(table(unlist(all_countries)))

  # define conversion map
  map <- c()
  map["Basque Country"] <- "Spain"
  map["British Raj"] <- "India"
  map["Chinese Taipei"] <- "Taiwan"
  map["Colony of New Zealand"] <- "New Zealand"
  map["Czech Republic"] <- "Czechia"
  map["Democratic Republic of the Congo"] <- "D.R. of the Congo"
  map["Dominion of India"] <- "India"
  # map["England"] <- "United Kingdom"
  map["Empire of Japan"] <- "Japan"
  map["German Democratic Republic"] <- "Germany"
  map["German Reich"] <- "Germany"
  map["Irish Free State"] <- "Ireland"
  map["Kingdom of Denmark"] <- "Denmark"
  map["Kingdom of Italy"] <- "Italy"
  map["Kingdom of the Netherlands"] <- "Netherlands"
  map["Northern Ireland"] <- "Ireland"
  map["People's Republic of China"] <- "China"
  map["Republica Moldova"] <- "Moldova"
  map["Republica of the Congo"] <- "Congo"
  map["Rhodesia"] <- "Zimbabwe"
  map["Russian Empire"] <- "Russia"
  map["Socialist Federal Republic of Yugoslavia"] <- "Yugoslavia"
  map["South-West Africa"] <- "Namibia"
  map["Southern Rhodesia"] <- "Zimbabwe"
  map["Soviet Union"] <- "U.S.S.R."
  map["The Bahamas"] <- "Bahamas"
  map["United Kingdom of Great Britain and Ireland"] <- "United Kingdom"
  map["United States of America"] <- "U.S.A."
  map["United States"] <- "U.S.A."
  map["中華民國"] <- "Taiwan"

  # normalize countries
  for (c in 1:length(all_countries)) {
    countries <- all_countries[[c]]

    # normalize country names
    for (country in names(map))
      countries[countries == country] <- map[country]

    # remove duplicates
    countries <- unique(countries)

    # update list
    all_countries[[c]] <- countries
  }

  # collapse to get strings again
  result <- sapply(all_countries, function(countries) paste0(countries, collapse = "; "))
  names(result) <- NULL

  # remove empty strings
  idx <- which(result == "")
  if (length(idx) > 0)
    result[idx] <- NA
  result[result == "NA"] <- NA

  return(result)
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
  # possibly split multiple values
  # all_positions <- sapply(all_positions, function(position) strsplit(position, "; ")[[1]])    #  arbitrarily keep the first one
  all_positions <- strsplit(all_positions, "; ")

  # see all existing values
  # table(unlist(all_positions))

  # RUGBY POSITIONS
  #####
  # Forward
  # ├─ 1st Row = Front Row
  # │  ├─ Prop
  # │  │  ├─ Loosehead Prop
  # │  │  └─ Tighthead Prop
  # │  └─ Hooker
  # ├─ 2nd Row = Lock
  # │  ├─ Loosehead Lock
  # │  └─ Tighthead lock
  # └─ 3rd Row = Back Row = Loose Forward
  #    ├─ Flanker = Wing Forward
  #    │  ├─ Openside Flanker
  #    │  └─ Blindside Flanker
  #    └─ Number 8 = Eightman
  # Back
  # ├─ Half-Back
  # │  ├─ Scrum-Half
  # │  └─ Fly-Half = First Five-Eighth = Out-Half = Stand-Off"
  # ├─ Three-Quarter
  # │  ├─ Centre
  # │  │  ├─ Inside Centre = Inside Back = Second Five-Eighth
  # │  │  └─ Outside Centre = Outside Back = Centre Three-Quarter = Outside Half
  # │  └─ Winger = Wing Three Quarter
  # │     ├─ Left Winger
  # │     └─ Right Winger
  # └─ Fullback
  
  # define conversion map
  map <- c()
  map["centre"] <- "Centre"
  map["center"] <- "Centre"                   # not a WD rugby union position, generally a WD error
  map["five-eighth"] <- "Scrum-Half"          # not a WD rugby union position, generally a WD error
  map["flanker"] <- "Flanker"
  map["fly-half"] <- "Fly-Half"
  map["forward"] <- "Forward"                 # not a WD rugby union position, generally a WD error
  map["fullback"] <- "Fullback"
  map["hooker"] <- "Hooker"
  map["lock"] <- "Lock"
  map["Loosehead prop"] <- "Loosehead Prop"   # not a WD rugby union position, generally a WD error
  map["number 8"] <- "Number 8"
  map["prop"] <- "Prop"
  map["scrum-half"] <- "Scrum-Half"
  map["second row"] <- "Lock"                 # not a WD rugby union position, generally a WD error
  map["Standoff"] <- "Fly-Half"               # not a WD rugby union position, generally a WD error
  map["third line"] <- "3rd row"
  map["utility back"] <- "Utility Back"
  map["winger"] <- "Winger"

  # clean positions
  for (p in 1:length(all_positions)) {
    positions <- all_positions[[p]]

    # normalize positions names
    for (position in names(map))
      positions[positions == position] <- map[position]

    # remove positions from other sports
    idx <- which(!(positions %in% map))
    if (length(idx) > 0)
      positions <- positions[-idx]
    if (length(positions) == 0)
      positions <- NA

    # update list
    all_positions[[p]] <- positions
  }

  # collapse to get strings again
  result <- sapply(all_positions, function(positions) paste0(positions, collapse = "; "))
  result[result == "NA"] <- NA
  names(result) <- NULL

  return(result)
}
