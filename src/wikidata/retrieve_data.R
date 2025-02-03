########################################################################
# Script designed to extract raw data from Wikidata, and record
# them as tables.
#
# We had to break down the queries in small bits in order
# for them to run on the Wikidata server.
#
# The SPARQL queries are available in folder `queries/wikidata`.
#
# Note: this script runs correctly as of 22/12/2024, but that
# may not be true in the future, depending on the evolution of 
# Wikidata data. The CSV files produced by this scripts are 
# available in folder `out/wikidata/tables.`
#
# Vincent Labatut
# 12/2024
#
# setwd("C:/Users/Vincent/eclipse/workspaces/Test/RugbyScope/RugbyScope")
# source("src/wikidata/retrieve_data.R")
########################################################################
library("readtext")
library("WikidataR")
library("dplyr")

source("src/common/logging.R")




########################################################################
# paths
query_folder <- file.path("queries", "wikidata")
table_folder <- file.path("data", "wikidata", "tables")




########################################################################
# extraction of player list
tlog("Retrieving the list of player IDs from WD")

# load query file
query <- readtext(file.path(query_folder, "players_list.sparql"))$text

# run query and get list of ids
player_ids <- query_wikidata(query)$playerId
tlog("Number of player IDs retrieved: ", length(player_ids))




########################################################################
# extraction of player information
tlog("Retrieving the individual information of each player (may take a while)")
col_names <- c(
  "playerId", "playerLabel",
  "firstnameLabels", "lastnameLabels", "sexLabel",
  "dobMax", "dobFormat", "pobLabels", "dodMax", "dodFormat", "podLabels",
  "citizenshipLabels", "sportCountryLabels",
  "positionLabels", "careerStartYears", "careerEndYears",
  "masses", "heights",
  "espnScrumIds", "allRugbyIds", "googleKnowlIds", "itsRugbyIds", "rugbyDatabaseIds",
  "wikipediaEn", "wikipediaFr", "wikipediaIt", "wikipediaEs", "wikipediaJa"
)

# init players table
players <- as.data.frame(matrix(NA, nrow=length(player_ids), ncol=length(col_names)))
colnames(players) <- col_names

# the query is split in 2, to avoid times-out
for (q in 1:2) {
  tlog("Processing player query ", q, "/2")
  # load query file
  query <- readtext(file.path(query_folder, paste0("players_info", q, ".sparql")))$text
  # remove the comments/spaces/newlines, otherwise the query is too long
  query <- gsub("#[^\r\n]*[\r\n]+", "\n", query)
  query <- gsub("  +", " ", query)
  query <- gsub("[\r\n]+", "\n", query)
  query <- gsub(" *[\r\n] *", "\n", query)

  # run query for each player
  tlog.start.loop(0, length(player_ids), "Looping over players")
  for (p in 1:length(player_ids)) {
    # get player ID
    player_id <- player_ids[p]
    tlog.loop(2, p, "++++++++++++ Processing (", q, ") player ", player_id, " (", p, "/", length(player_ids), ")")

    # run query
    pl_query <- gsub("QQQQQQ", player_id, query, fixed = TRUE)
    row <- -1
	  while (!all(is.na(row)) && all(row == -1)) {
  	  row <- tryCatch(expr = {query_wikidata(pl_query)}, error = function(e) -1)
      if (!all(is.na(row)) && all(row == -1)) {
        tlog(4, "HTTP ERROR while retrieving the data: probably a temporary problem, trying again")
        Sys.sleep(5)
      }
    }
    print.data.frame(row)
    if (nrow(row) > 1)
      stop(paste0("ERROR: several rows returned for one player (some field probably contains multiple values). Player ID= "), player_id)

    # add to table
    idx <- which(players[, "playerId"] == player_id)
    if (length(idx) == 0)
      idx <- p
    cols <- setdiff(intersect(colnames(row), col_names), "playerId")
    players[idx, cols] <- row[1, cols]
    players[idx, "playerId"] <- player_id
  }
  tlog.end.loop(0, "Player loop completed")
}

# display a few details for verification
tlog("Dimension of the players table: ", paste(dim(players), collapse = ", "))
tlog("Classes of the columns: ", paste(apply(players, 2, class), collapse = ", "))
tlog("Top of the table:")
print.data.frame(players[1:5, ])

# replace empty strings by NAs
players <- players %>% mutate(across(where(is.character), ~ na_if(., "")))
# export table as a CSV
write.csv(x = players, file = file.path(table_folder, "players_descr.csv"), row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# extraction of team list
tlog("Retrieving the list of team IDs from WD")

# load query file
query <- readtext(file.path(query_folder, "teams_list.sparql"))$text

# run query and get list of ids
team_ids <- query_wikidata(query)$teamId
tlog("Number of teams IDs retrieved: ", length(team_ids))

# remove the generic concepts
concepts <- c(
  "Q43009164" # rugby union club
)
idx <- which(team_ids %in% concepts)
if (length(idx) > 0)
  team_ids <- team_ids[-idx]




########################################################################
# extraction of team information
tlog("Retrieving the individual information of each team (may take a while)")
col_names <- c(
  "teamId", "teamLabel", "teamTypeLabel",
  "inceptionMax", "inceptionFormat",
  "terminationMax", "terminationFormat",
  "altNames", "nicknameLabels", "affiliationLabels",
  "countryLabels", "competitionLabels",
  "homeVenueLabels", "homeVenueCapacities", "locationLabels",
  "allRugbyIds", "googleKnowlIds",
  "wikipediaEn", "wikipediaFr", "wikipediaIt", "wikipediaEs", "wikipediaJa"
)

# init teams table
teams <- as.data.frame(matrix(NA, nrow=length(team_ids), ncol=length(col_names)))
colnames(teams) <- col_names

# the query is split in 2, to avoid times-out
for (q in 1:2) {
  tlog("Processing team query ", q, "/2")
  # load query file
  query <- readtext(file.path(query_folder, paste0("teams_info", q, ".sparql")))$text
  # remove the comments/spaces/newlines, otherwise the query is too long
  query <- gsub("#[^\r\n]*[\r\n]+", "\n", query)
  query <- gsub("  +", " ", query)
  query <- gsub("[\r\n]+", "\n", query)
  query <- gsub(" *[\r\n] *", "\n", query)

  # run query for each team
  tlog.start.loop(0, length(team_ids), "Looping over teams")
  for (t in 1:length(team_ids)) {
  # for (t in which(is.na(teams[, "teamLabel"]))) {
    # get team ID
    team_id <- team_ids[t]
    tlog.loop(2, t, "++++++++++++ Processing (", q, ") team ", team_id, " (", t, "/", length(team_ids), ")")

    # run query
    tm_query <- gsub("QQQQQQ", team_id, query, fixed = TRUE)
    row <- -1
	  while (!all(is.na(row)) && all(row == -1)) {
  	  row <- tryCatch(expr = {query_wikidata(tm_query)}, error = function(e) -1)
      if (!all(is.na(row)) && all(row == -1)) {
        tlog(4, "HTTP ERROR while retrieving the data: probably a temporary problem, trying again")
        Sys.sleep(5)
      }
    }
    print.data.frame(row)
    if (nrow(row) > 1)
      stop(paste0("ERROR: several rows returned for one team (some field probably contains multiple values). Team ID= "), team_id)

    # add to table
    idx <- which(teams[, "teamId"] == team_id)
    if (length(idx) == 0)
      idx <- t
    cols <- setdiff(intersect(colnames(row), col_names), "teamId")
    teams[idx, cols] <- row[1, cols]
    teams[idx, "teamId"] <- team_id
  }
  tlog.end.loop(0, "Team loop completed")
}

# display a few details for verification
tlog("Dimension of the teams table: ", paste(dim(teams), collapse = ", "))
tlog("Classes of the columns: ", paste(apply(teams, 2, class), collapse = ", "))
tlog("Top of the table:\n")
print.data.frame(teams[1:5, ])

# teams without a proper name
idx <- which(grepl("Q[0-9]+", teams[, "teamLabel"], fixed = FALSE))
tlog("Teams without a name: ", length(idx))
if (length(idx) > 0)
  print(teams[idx, ])

# remove redundant alt names
for (t in 1:length(team_ids)) {
  main_name <- teams[t, "teamLabel"]
  if (!is.na(teams[t, "nicknameLabels"]))
    nick_names <- strsplit(teams[t, "nicknameLabels"], "; ")[[1]]
  else
    nick_names <- c()
  if (!is.na(teams[t, "altNames"]))
    alt_names <- strsplit(teams[t, "altNames"], "; ")[[1]]
  else
    alt_names <- c()
  alt_names0 <- setdiff(toupper(alt_names), union(toupper(main_name), toupper(nick_names)))
  idx <- match(alt_names0, toupper(alt_names))
  alt_names <- alt_names[idx]
  if (length(alt_names) == 0)
    teams[t, "altNames"] <- NA
  else
    teams[t, "altNames"] <- paste(alt_names, collapse = "; ")
}

# replace empty strings by NAs
teams <- teams %>% mutate(across(where(is.character), ~ na_if(., "")))
# export table as a CSV
write.csv(x = teams, file = file.path(table_folder, "teams_descr.csv"), row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# extraction of player careers
tlog("Retrieving players' careers (may take a while)\n")
col_names <- c(
  "playerId",
  "teamId",
  "startYear", "endYear",
  "matchesPlayed", "pointsScored"
)

# init careers table
careers <- as.data.frame(matrix(NA, nrow = 1, ncol=length(col_names)))
colnames(careers) <- col_names
careers <- careers[-1, , drop = FALSE]

# load query file
query <- readtext(file.path(query_folder, "career_steps.sparql"))$text
# remove the comments/spaces/newlines, otherwise the query is too long
query <- gsub("#[^\r\n]*[\r\n]+", "\n", query)
query <- gsub("  +", " ", query)
query <- gsub("[\r\n]+", "\n", query)
query <- gsub(" *[\r\n] *", "\n", query)

# run query for each player
tlog.start.loop(0, length(player_ids), "Looping over player careers")
for (p in 1:length(player_ids)) {
  # get player ID
  player_id <- player_ids[p]
  tlog.loop(2, p, "++++++++++++ Processing player ", player_id, " (", p, "/", length(player_ids), ")")

  # run query
  cr_query <- gsub("QQQQQQ", player_id, query, fixed = TRUE)
  rows <- -1
  while (!all(is.na(rows)) && all(rows == -1)) {
    rows <- tryCatch(expr = {query_wikidata(cr_query)}, error = function(e) -1)
    if (!all(is.na(rows)) && all(rows == -1)) {
      tlog(4, "HTTP ERROR while retrieving the data: probably a temporary problem, trying again")
      Sys.sleep(5)
    }
  }
  print.data.frame(rows)

  # add to table
  if (!all(is.na(rows))) {
    comp_rows <- matrix(NA, nrow = nrow(rows), ncol = ncol(careers))
    colnames(comp_rows) <- col_names
    comp_rows <- as.data.frame(comp_rows)
    cols <- intersect(colnames(rows), col_names)
    comp_rows[, cols] <- rows[, cols]
    comp_rows[, "playerId"] <- rep(player_id, nrow(comp_rows))
    careers <- rbind(careers, comp_rows)
  }
}
tlog.end.loop(0, "Career loop completed")

# display a few details for verification
tlog("Dimension of the careers table: ", paste(dim(careers), collapse = ", "))
tlog("Classes of the columns: ", paste(apply(careers, 2, class), collapse = ", "))
tlog("Top of the table:\n")
print.data.frame(careers[1:10, ])

# add player and team names
idx <- match(careers[, "playerId"], players[, "playerId"])
plyr_names <- players[idx, "playerLabel"]
idx <- match(unlist(careers[, "teamId"]), teams[, "teamId"])
team_names <- teams[idx, "teamLabel"]
careers <- cbind(careers[, "playerId"], playerName = plyr_names, careers[, "teamId"], teamName = team_names, careers[,3:ncol(careers)])
colnames(careers)[1] <- "playerId"
colnames(careers)[3] <- "teamId"

# replace empty strings by NAs
careers <- careers %>% mutate(across(where(is.character), ~ na_if(., "")))
# export table as a CSV
write.csv(x = careers, file = file.path(table_folder, "players_careers.csv"), row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# temporary code, used to perform some tests and debugging
# idx <- match(players0[,"playerId"], players[,"playerId"])
# print(which(is.na(idx)))
# players[idx, c("wikipediaEn", "wikipediaFr", "wikipediaIt", "wikipediaEs", "wikipediaJa")] <- players0[, c("wikipediaEn", "wikipediaFr", "wikipediaIt", "wikipediaEs", "wikipediaJa")]
# idx <- which(sapply(1:nrow(players), function(p) all(is.na(players[p,c("wikipediaEn", "wikipediaFr", "wikipediaIt", "wikipediaEs", "wikipediaJa")])))) 
# print(cbind(idx, players[idx, "playerId"]))
# players[which(players[, "playerId"] == "Q96707312"), "wikipediaJa"] <- "フィシプナ・トゥイアキ"




########################################################################
# "Q2868073","Association Sportive Bayonnaise","rugby union club","1905-01-01","1905-1-1",NA,NA,NA,"Les violets",NA,"France",NA,NA,NA,NA,NA,"122fpt6j",NA,"Association_sportive_bayonnaise",NA,NA,NA
# "Q1672729","Iris Club lillois","rugby union club","1898-01-01","1898-1-1","1941-01-01","1941-1-1",NA,NA,NA,"France",NA,NA,NA,NA,NA,"1232hsb_","Iris_Club_lillois","Iris_Club_lillois",NA,NA,NA
