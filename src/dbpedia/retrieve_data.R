########################################################################
# Script designed to extract raw data from DBpedia, and record
# them as tables.
#
# The SPARQL queries are available in folder `queries/dbpedia`.
#
# Note: this script runs correctly as of 22/12/2024, but that
# may not be true in the future, depending on the evolution of 
# DBpedia. The CSV files produced by this scripts are 
# available in folder `out/dbpedia/tables.`
#
# Vincent Labatut
# 12/2024
########################################################################
library("readtext")
library("httr2")
library("dplyr")




########################################################################
# paths
query_folder <- file.path("queries", "dbpedia")
table_folder <- file.path("data", "dbpedia", "tables")




########################################################################
# Takes a query and the name of a CSV file, process the query using
# DBpedia, and record the resulting table as a CSV file. The table is
# also returned by the function.
#
# query: the query to process.
# file: the CSV file to create.
#
# returns: the table corresponding to the query answer.
########################################################################
query_dbpedia <- function(query, file) {
  endpoint <- "https://dbpedia.org/sparql"

  # send the query
  req <- request(endpoint) |>
          req_url_query(query = query, format = "text/csv")
  response <- req_perform(req)

  # check the status code
  if (resp_status(response) == 200) {
    content <- resp_body_string(response)

    # write as CSV file
    fileConn <- file(file)
    writeLines(content, fileConn)
    close(fileConn)

    # read to return the content as a table
    result <- read.csv(file)

    # turn empty strings into NAs
    result <- result %>% mutate(across(where(is.character), ~ na_if(., "")))
  } else {
    # print error message if the request failed
    stop(paste("Error:", resp_status(response)))
  }

  return(result)
}




########################################################################
# extraction of player table
cat("Retrieving the list of players from DBpedia\n")

# load query file
query <- readtext(file.path(query_folder, "players_list.sparql"))$text

# run query and get the players (must use a paginated access due to 10000 hit DBpedia limit)
tab_file <- file.path(table_folder, "players_descr.csv")
go_on <- TRUE
page_nbr <- 1
pg_limit <- 10000
players <- NA
while (go_on) {
  cat("Processing page ", page_nbr, "\n", sep = "")
  offset <- (page_nbr - 1) * pg_limit
  pg_query <- gsub("xxxxxx", offset, query, fixed = TRUE)
  page <- query_dbpedia(pg_query, file = tab_file)
  if (page_nbr == 1)
    players <- page
  else
    players <- rbind(players, page)
  go_on <- nrow(page) == pg_limit
  page_nbr <- page_nbr + 1
}
cat("Dimension of the players table:", paste(dim(players), collapse = ", "), "\n")

# remove manually detected outliers
patterns <- c("Justice_of_the", "Keeper_of_", "Rugby_World_Cup", "Tennis_Channel",
  "dog-free_zone", "3M_computer", "5_lever_lock", "President_", "Postmaster-General",
  "Positional_advantage", "Post-structural", "Port_Mayaca", "Presidential_Assistant",
  "Presiding_bishop", "Primacy_of_Simon_Peter", "Prime_Minister", "Private_trustee",
  "Procurator_", "Production_", "_position", "Protovestiarios", "Provincial_",
  "Provost_of_", "Psychologism", "_minister", "Government", "1°_West", "A-A_line",
  "Conservator_of", "_governor", "Acton's_Lock", "Actualism", "_manager", "_supremacy",
  "/Arkansas", "Assistant_", "Attorney_", "Attitude_", "/Australian", "Rugby_Association",
  "Secretary", "Chairman_", "Chair_of_", "Chadron_State_Eagles", "/Chief_",
  "China's_", "Acronical_place", "Alaska_Anchorage_Seawolves", "Inspector",
  "Intendant", "InterLock", "Interchange", "Winona_State_Warriors", "TSIG"
)
for (pattern in patterns) {
  idx <- which(grepl(pattern, players[, "player"], fixed = TRUE))
  if (length(idx) > 0) {
    players <- players[-idx, ]
    cat("Removed ", length(idx), " items based on pattern \"", pattern, "\"\n", sep = "")
  } else {
    cat("Did not find pattern \"", pattern, "\"\n", sep = "")
  }
}

# remove empty columns (last time I checked: "firstNames", "givenNames", "lastNames")
idx <- which(sapply(1:ncol(players), function(col) all(is.na(players[, col]))))
if (length(idx) > 0)
  players <- players[, -idx]
cat("Removed ", length(idx), " empty columns\n", sep = "")

# merge fullname-related fields
for (p in 1:nrow(players)) {
  if (is.na(players[p, "fullNames"])) {
    players[p, "fullNames"] <- players[p, "birthNames"]
  } else {
    if (!is.na(players[p, "birthNames"])) {
      names1 <- strsplit(players[p, "fullNames"], "; ")[[1]]
      names2 <- strsplit(players[p, "birthNames"], "; ")[[1]]
      names <- union(names1, names2)
      players[p, "fullNames"] <- paste(names, collapse = "; ")
    }
  }
}
players <- players[, -which(colnames(players) == "birthNames")]
cat("Merged birthNames into fullNames\n", sep = "")

# remove DBpedia URL part from place names
players[, "birthPlaces"] <- gsub("http://dbpedia.org/resource/", "", players[, "birthPlaces"], fixed = TRUE)
players[, "birthPlaces"] <- gsub("_", " ", players[, "birthPlaces"], fixed = TRUE)
players[, "deathPlaces"] <- gsub("http://dbpedia.org/resource/", "", players[, "deathPlaces"], fixed = TRUE)
players[, "deathPlaces"] <- gsub("_", " ", players[, "deathPlaces"], fixed = TRUE)

# remove Wikidata URL part from WD ids
players[, "wikidataId"] <- gsub("http://www.wikidata.org/entity/", "", players[, "wikidataId"], fixed = TRUE)

# clean rugby positions
players[, "positions"] <- gsub("http://dbpedia.org/resource/", "", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub("_(rugby_union)", "", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub("_(sports)", "", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub("_", " ", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub("; Rugby union positions", "", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub("Rugby union positions;", "", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub("Rugby union/", "", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub(" .", "", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub("  /  ", "; ", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub(" / ", "; ", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub("/", "; ", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub(" and ", "; ", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub(" or ", "; ", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub(", ", "; ", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub("[", "", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub("]", "", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub(" -", "-", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub("- ", "-", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub("^-", "", players[, "positions"], fixed = FALSE)
players[, "positions"] <- gsub("^:", "", players[, "positions"], fixed = FALSE)
players[, "positions"] <- gsub("^;", "", players[, "positions"], fixed = FALSE)
players[, "positions"] <- gsub("^2;", "", players[, "positions"], fixed = FALSE)
players[, "positions"] <- gsub(";+", ";", players[, "positions"], fixed = FALSE)
players[, "positions"] <- gsub(" ; ", "; ", players[, "positions"], fixed = TRUE)
players[, "positions"] <- gsub("(rugby union)", "", players[, "positions"], fixed = TRUE)
players[, "positions"] <- trimws(players[, "positions"])
players[, "positions"][which(players[, "positions"] %in% c("unknown", "tbc", "m", "-", "--", "?", "1"))] <- NA

# clean birth/death dates
for (colname in c("birthDates", "deathDates")) {
  cat(">>>> Processing field ", colname, "\n", sep = "")
  players[, colname] <- gsub("^;", "", players[, colname], fixed = FALSE)
  players[, colname] <- trimws(players[, colname])

  # replace certain problematic strings (identical patterns)
  patterns <- c()
  patterns["after-1983"] <- "01/01/1983"
  patterns["April"] <- ""
  patterns["August"] <- ""
  patterns["Feb 1880"] <- "01/02/1880"
  patterns["February"] <- ""
  patterns["http://dbpedia.org/resource/Argentina; http://dbpedia.org/resource/Buenos_Aires"] <- ""
  patterns["January"] <- ""
  patterns["January→March 1950"] <- "01/01/1950"
  patterns["July"] <- ""
  patterns["July→September 1860"] <- "01/07/1860"
  patterns["July→September 1862"] <- "01/07/1862"
  patterns["July→September 1960"] <- "01/07/1960"
  patterns["June"] <- ""
  patterns["June→September 1858"] <- "01/06/1858"
  patterns["March"] <- ""
  patterns["May"] <- ""
  patterns["Never"] <- ""
  patterns["November"] <- ""
  patterns["p"] <- ""
  patterns["October"] <- ""
  patterns["October→December 1861"] <- "01/10/1861"
  patterns["October→December 1893"] <- "01/10/1893"
  patterns["September"] <- ""
  for (p in 1:length(patterns)) {
    idx <- which(trimws(players[, colname]) == names(patterns)[p])
    if (length(idx) > 0) {
      players[idx, colname] <- patterns[p]
      cat("Fixed ", length(idx), " dates based on pattern \"", names(patterns)[p], "\"\n", sep = "")
    } else {
      cat("Did not find pattern \"", names(patterns)[p], "\"\n", sep = "")
    }
  }
  # remove "circa" and variants at the beginning of dates
  patterns <- c("circa-", "circa", "c. ", "c.", "ca. ", "c",
    "unknown", "Unknown", "date of birth unknown", "Date of birth unknown", "date unknown",
    "Daniel Maiava", "Sauniatu, Samoa", ", Beckenham", "?", "≥",
    "Auckland, New Zealand", "pre ", "Tauranga, New Zealand",
    "early", "Q1 ", "first ¼ ", "first ¼", "first", "registered first ¼",
    "second quarter of ", "second quarter ", "second ¼ ", "second ¼", "second",
    "Register Q3 ", "third quarter", "third ¼ ", "Third ¼ ", "third ¼", "Reg Q3, ",
    "fourth ¼ ", "fourth ¼", "fourth  ¼ "
  )
  for (pattern in patterns) {
    idx <- which(startsWith(players[, colname], pattern))
    if (length(idx) > 0) {
      players[idx, colname] <- substr(players[idx, colname], start = nchar(pattern) + 1, stop = nchar(players[idx, colname]))
      cat("Fixed ", length(idx), " dates based on pattern \"", pattern, "\"\n", sep = "")
    } else {
      cat("Did not find pattern \"", pattern, "\"\n", sep = "")
    }
  }
  #
  # replace character months by ints
  patterns <- c("January ", "February ", "March ", "April ", "May ", "June ", "July ", "August ", "September ", "October ", "November ", "December ")
  for (pattern in patterns) {
    idx <- which(startsWith(players[, colname], pattern))
    if (length(idx) > 0) {
      players[idx, colname] <- substr(players[idx, colname], start = nchar(pattern) + 1, stop = nchar(players[idx, colname]))
      players[idx, colname] <- paste(sprintf("01/%02d", which(patterns == pattern)), "/", players[idx, colname], sep = "")
      cat("Fixed ", length(idx), " dates based on pattern \"", pattern, "\"\n", sep = "")
    } else {
      cat("Did not find pattern \"", pattern, "\"\n", sep = "")
    }
  }
  #
  # remove incorrect year values
  years <- as.integer(players[, colname])
  idx <- which(!is.na(years) & years < 1800)
  players[idx, colname] <- NA
  cat("Removed ", length(idx), " incorrect year values\n", sep = "")
  #
  # complement incomplete dates
  years <- as.integer(players[, colname])
  idx <- which(!is.na(years))
  players[idx, colname] <- paste("01/01/", trimws(players[idx, colname]), sep = "")
  cat("Complemented ", length(idx), " incomplete dates\n", sep = "")
  #
  # remove incorrect alternate dates
  idx <- which(grepl("; ", players[, colname], fixed = TRUE))
  if (length(idx) > 0) {
    players[idx, colname] <- substr(players[idx, colname], start = 1, stop = nchar("xxxx-xx-xx"))
  }
  cat("Removed ", length(idx), " incorrect alternate dates\n", sep = "")
  #
  # remove dates without year
  idx <- which(startsWith(players[, colname], "--"))
  if (length(idx) > 0)
    players <- players[-idx, ]
  cat("Removed ", length(idx), " dates without a year\n", sep = "")
  # remove incorrect dates
  idx <- which(players[, colname] == "-")
  if (length(idx) > 0)
    players <- players[-idx, ]
  cat("Removed ", length(idx), " incorrect dates\n", sep = "")
}
# debug
# idx <- which(nchar(sort(unique(players[, "deathDates"])))!=10)
# sort(unique(players[, "deathDates"]))[idx]

# normalize/merge weight-related fields
for (colname in c("weightA", "weightB")) {
  # remove non-numerical values
  patterns <- c("Bloody heavy", "Cruiserweight", "Heavyweight",
    "http://dbpedia.org/resource/Super_welterweight", "or", "to",
    "ru_position = Flanker, Scrum half", "Unknown", "unknown"
  )
  for (pattern in patterns) {
    idx <- which(players[, colname] == pattern)
    if (length(idx) > 0)
      players[idx, colname] <- NA
  }

  # remove values that are too small
  weights <- as.numeric(players[, colname])
  idx <- which(weights < 40)
  if (length(idx) > 0)
    players[idx, colname] <- NA
}
#
# weightB seems more reliable than weightA: using it in priority
idx <- which(!is.na(players[, "weightB"]))
players[idx, "weightA"] <- players[idx, "weightB"]
colnames(players)[which(colnames(players) == "weightA")] <- "weights"
players <- players[, -which(colnames(players) == "weightB")]
#
# debug
# sort(unique(players[, "weightA"]))

# normalize/merge height-related fields
for (colname in c("heightA", "heightB")) {
  # remove non-numerical values
  patterns <- c("Bexley, New South Wales, Australia", "or",
    ")", ".", "[2]", "0", "1999-12-18"
  )
  for (pattern in patterns) {
    idx <- which(players[, colname] == pattern)
    if (length(idx) > 0)
      players[idx, colname] <- NA
  }

  # remove units
  players[, colname] <- gsub(" *m *", "", players[, colname], fixed = FALSE)

  # possibly convert meters to centimeters
  heights <- as.numeric(players[, colname])
  idx <- which(heights < 2.14)
  if (length(idx) > 0)
    players[idx, colname] <- heights[idx] * 100

  # remove values that are too samll or too large
  heights <- as.numeric(players[, colname])
  idx <- which(heights < 140 | heights > 214)
  if (length(idx) > 0)
    players[idx, colname] <- NA
}
#
# heightB seems more reliable than heightA: using it in priority
idx <- which(!is.na(players[, "heightB"]))
players[idx, "heightA"] <- players[idx, "heightB"]
colnames(players)[which(colnames(players) == "heightA")] <- "heights"
players <- players[, -which(colnames(players) == "heightB")]
#
# debug
# sort(unique(players[, "heightA"]))

# display a few details for verification
cat("Dimension of the players table:", paste(dim(players), collapse = ", "), "\n")
cat("Classes of the columns: ", paste(apply(players, 2, class), collapse = ", "), "\n")
cat("Top of the table:\n")
print.data.frame(players[1:10, ])

# replacing empty strings by NAs
players <- players %>% mutate(across(where(is.character), ~ na_if(., "")))

# export table as a CSV
write.csv(x = players, file = file.path(table_folder, "players_descr.csv"), row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# extraction of team list
cat("Retrieving the list of teams from DBpedia\n")

# load query file
query <- readtext(file.path(query_folder, "teams_list.sparql"))$text

# run query and get the teams
tab_file <- file.path(table_folder, "teams_descr.csv")
teams <- query_dbpedia(query, file = tab_file)
cat("Dimension of the teams table:", paste(dim(teams), collapse = ", "), "\n")

# remove manually detected outliers
patterns <- c("Till_I_Get_My_Way", "Breakin'_Away", "I'm_Born_Again",
  "It's_My_Life", "Millie_Pulled_a_Pistol_on_Santa", "Mr._Nutz",
  "See_What_a_Fool_I've_Been"
)
for (pattern in patterns) {
  idx <- which(grepl(pattern, teams[, "team"], fixed = TRUE))
  if (length(idx) > 0) {
    teams <- teams[-idx, ]
    cat("Removed ", length(idx), " items based on pattern \"", pattern, "\"\n", sep = "")
  } else {
    cat("Did not find pattern \"", pattern, "\"\n", sep = "")
  }
}

# remove empty columns (last time I checked: "teamNames2")
idx <- which(sapply(1:ncol(teams), function(col) all(is.na(teams[, col]))))
if (length(idx) > 0)
  teams <- teams[, -idx]
cat("Removed ", length(idx), " empty columns\n", sep = "")

# clean team names
idx <- which(startsWith(teams[, "teamNames"], "; "))
teams[idx, "teamNames"] <- substr(teams[idx, "teamNames"], start = nchar("; ") + 1, stop = nchar(teams[idx, "teamNames"]))

# merge name-related columns (teamNAmes & fullNames)
for (t in 1:nrow(teams)) {
  if (is.na(teams[t, "teamNames"])) {
    teams[t, "teamNames"] <- teams[t, "fullNames"]
  } else {
    names0 <- teams[t, "teamLabel"]
    names1 <- strsplit(teams[t, "teamNames"], "; ")[[1]]
    names2 <- c()
    if (!is.na(teams[t, "fullNames"])) {
      names2 <- strsplit(teams[t, "fullNames"], "; ")[[1]]
    }
    names <- setdiff(union(names1, names2), names0)
    names <- paste(names, collapse = "; ")
    if (names == "")
      names <- NA
    teams[t, "teamNames"] <- names
  }
}
teams <- teams[, -which(colnames(teams) == "fullNames")]
cat("Merged fullNames into teamNames\n", sep = "")

# remove Wikidata URL part from WD ids
teams[, "wikidataId"] <- gsub("http://www.wikidata.org/entity/", "", teams[, "wikidataId"], fixed = TRUE)

# display a few details for verification
cat("Dimension of the teams table:", paste(dim(teams), collapse = ", "), "\n")
cat("Classes of the columns: ", paste(apply(teams, 2, class), collapse = ", "), "\n")
cat("Top of the table:\n")
print.data.frame(teams[1:10, ])

# replacing empty strings by NAs
teams <- teams %>% mutate(across(where(is.character), ~ na_if(., "")))

# export table as a CSV
write.csv(x = teams, file = file.path(table_folder, "teams_descr.csv"), row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# extraction of player careers
########################################################################
# this requires field "careerStation", but apparentiyl it is not filled for
# most rugby players (only a handful of exceptions), whereas I've seen it
# filled for football players. Here is an example of query to run on
# https://dbpedia.org/sparql
########################################################################
# PREFIX dbpedia: <http://dbpedia.org/resource/>
# SELECT
#   ?playerName ?station
# WHERE
# { ?player rdf:type dbo:RugbyPlayer.
# #{ BIND(dbpedia:Antoine_Dupont AS ?player).
# #{ BIND(dbpedia:Alexandre_Lacazette AS ?player).
#   ?player rdfs:label ?playerName;
#               dbo:careerStation ?station.
# }
# ORDER BY ?playerName
########################################################################
