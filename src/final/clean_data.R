########################################################################
# The merged tables still contain some inconsistencies, that we fixed here.
#
# 05/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/clean_data.R")
########################################################################
library("stringi")
library("stringr")
library("dplyr")
library("httr")
library("jsonlite")
library("magrittr")

source("src/common/logging.R")
source("src/common/norm_names.R")
source("src/common/norm_teams.R")




########################################################################
# start logging
start.rec.log("CleanMergedTables")




########################################################################
# paths
data_folder <- file.path("data")
#
fusion_folder <- file.path(data_folder, "fusion")

languages <- c("En", "Fr", "It", "Es", "Ja")




########################################################################
# load previously merged tables
tlog("Loading merged tables")

teams <- read.csv(file.path(fusion_folder, "teams_07_enwp.csv"))
tlog(2, "Raw number of teams: ", nrow(teams))

players <- read.csv(file.path(fusion_folder, "players_06_enwp.csv"))
tlog(2, "Raw number of players: ", nrow(players))

stints <- read.csv(file.path(fusion_folder, "stints_05_enwp.csv"))
tlog(2, "Raw number of stints: ", nrow(stints))




########################################################################
# check that the stints are consistent with the teams and players (names, ids...)

# check that all rugbyscopeIds in the stint table appear in the team table
# (some teams were deleted in the data extraction process)
tlog("Checking that all rugbyscopeIds in the stint table appear in the team table")
idx <- which(!stints[, "teamRsId"] %in% teams[, "rugbyscopeId"])
if (length(idx) > 0) {
  ids <- sort(unique(stints[idx, "teamRsId"]))
  tlog(2, "Number of unique ids missing from the team table: ", length(ids))
  print(ids)
}

# check that, in the final stint file, team names match main names in team table
# (some team main names changed several times over the data extraction process)
tlog("Checking that all team names in the stint table match the main names from  the team table")
idx <- which(!stints[, "teamName"] %in% teams[, "fullName"])
if (length(idx) > 0) {
  ids <- sort(unique(stints[idx, "teamRsId"]))
  tlog(2, "Number of unique names differing from the team table: ", length(ids))
  print(ids)

  print(sapply(ids, function(id) {
    r1 <- which(stints[, "teamRsId"] == id)[1]
    r2 <- which(teams[, "rugbyscopeId"] == id)
    paste0(
      "\"", stints[r1, "teamWdId"], "\",",
      stints[r1, "teamRsId"], ",",
      "\"", stints[r1, "teamName"], "\"",
      " >>>>>>>>>> ",
      "\"", teams[r2, "wikidataId"], "\",",
      teams[r2, "rugbyscopeId"], ",",
      "\"", teams[r2, "fullName"], "\""
    )
  }))
}
#stints[which(stints[,"teamRsId"]==2687),"teamName"]

# check that, in the final stint file, player names match main names in player table
tlog("Checking that all player names in the stint table match the main names from  the player table")
idx <- which(!stints[, "playerName"] %in% players[, "fullName"])
if (length(idx) > 0) {
  ids <- sort(unique(stints[idx, "playerId"]))
  tlog(2, "Number of unique names differing from the player table: ", length(ids))
  print(ids)

  print(sapply(ids, function(id) {
    r1 <- which(stints[, "playerId"] == id)[1]
    r2 <- which(players[, "wikidataId"] == id)
    paste0(
      "\"", stints[r1, "playerId"], "\",",
      "\"", stints[r1, "playerName"], "\"",
      " >>>>>>>>>> ",
      "\"", players[r2, "wikidataId"], "\",",
      "\"", players[r2, "fullName"], "\""
    )
  }))
}




########################################################################
# resolve all WP redirections, as the URL possibly have changed since the retrieval of the first tables

# resolve wikipedia redirections for teams
tlog("Resolving Wikipedia redirections and updating URLs in teams table")
for (l in 1:length(languages)) {
  tlog(2, "Resolving redirections for language ", languages[l], "(", l,"/", length(languages),")")

  col_name <- paste0("wikipedia", languages[l])
  old_urls <- teams[, col_name]
  new_urls <- old_urls

  # loop over URLs and solve redirections
  tlog(4, "Looping over URLs")
  for (r in 1:length(old_urls)) {
    url <- old_urls[r]
    # if (r %% 100 == 0)
      tlog(6, "Solving redirections for entry ", r, "/", length(old_urls), " (", url, ")")

    if (!is.na(url)) {
      if (url == "") {
        new_urls[r] <- NA
      } else {
        # solve redirection
        new_urls[r] <- solve_redirections(name = url, lang = str_to_lower(languages[l]))
        Sys.sleep(6)
      }
    }

    if (!is.na(url) && is.na(new_urls[r]))
      tlog(6, "Lost URL: ", r)
  }

  # display some stats
  tlog(4, "Number of lost URLs: ", length(which(!is.na(old_urls) & is.na(new_urls))))
  tlog(4, "Number of changed URLs: ", length(which(!is.na(old_urls) & !is.na(new_urls) & old_urls != new_urls)))

  # update team table with new URLs
  teams[, col_name] <- new_urls
}

# resolve wikipedia redirections for players
tlog("Resolving Wikipedia redirections and updating URLs in players table")
for (l in 1:length(languages)) {
  tlog(2, "Resolving redirections for language ", languages[l], "(", l,"/", length(languages),")")

  col_name <- paste0("wikipedia", languages[l])
  old_urls <- players[, col_name]
  new_urls <- old_urls

  # loop over URLs and solve redirections
  tlog(4, "Looping over URLs")
  for (r in 1:length(old_urls)) {
    url <- old_urls[r]
    # if (r %% 100 == 0)
      tlog(6, "Solving redirections for entry ", r, "/", length(old_urls), " (", url, ")")

    if (!is.na(url)) {
      if (url == "") {
        new_urls[r] <- NA
      } else {
        # solve redirection
        new_urls[r] <- solve_redirections(name = url, lang = str_to_lower(languages[l]))
        Sys.sleep(6)
      }
    }

    if (!is.na(url) && is.na(new_urls[r]))
      tlog(6, "Lost URL: ", r)
  }

  # display some stats
  tlog(4, "Number of lost URLs: ", length(which(!is.na(old_urls) & is.na(new_urls))))
  tlog(4, "Number of changed URLs: ", length(which(!is.na(old_urls) & !is.na(new_urls) & old_urls != new_urls)))

  # update team table with new URLs
  players[, col_name] <- new_urls
}




########################################################################
# verifications in teams table

# check that each team url is unique
tlog("Checking URL uniqueness in teams table")
for (l in 1:length(languages)) {
  tlog(2, "Processing language ", languages[l], "(", l,"/", length(languages),")")

  col_name <- paste0("wikipedia", languages[l])
  urls <- teams[, col_name]
  tt <- table(urls[!is.na(urls)])
  if (any(tt > 1)) {
    tlog(4, "Non-unique URLs found for language ", languages[l])
    print(tt[tt > 1])
  }
}

# check team types
tlog("Checking team types")
print(table(teams[, "type"]))
# change "club" type name
idx <- which(teams[, "type"] == "Club")
teams[idx, "type"] <- "Club/franchise team"
tlog("Fixed the type of ", length(idx), " team types")
# check team types (again)
tlog("Checking team types")
print(table(teams[, "type"]))

# check the presence of commas in team names
tlog("Look for commas in team names")
fields <- c("fullName", "altNames")
for (field in fields) {
  idx <- which(grepl(",", teams[, field], fixed = TRUE))
  print(teams[idx, c("rugbyscopeId", field)])
}




########################################################################
# verifications in players table

# check that each team url is unique
tlog("Checking URL uniqueness in players table")
for (l in 1:length(languages)) {
  tlog(2, "Processing language ", languages[l], "(", l,"/", length(languages),")")

  col_name <- paste0("wikipedia", languages[l])
  urls <- players[, col_name]
  tt <- table(urls[!is.na(urls)])
  if (any(tt > 1)) {
    tlog(4, "Non-unique URLs found for language ", languages[l])
    print(tt[tt > 1])
  }
}

# check the presence of commas in player names
tlog("Look for commas in player names")
fields <- c("fullName", "firstName", "lastName", "altNames")
for (field in fields) {
  idx <- which(grepl("[,)(]", players[, field], fixed = FALSE))
  print(players[idx, c("wikidataId", field)])
}

# list players without a birthdate
tlog("Players without a birthdate:")
idx <- which(is.na(players[, "birthDate"]))
print(cbind(players[idx, c("wikidataId", "fullName")], paste0("http://www.wikipedia.org/wiki/", players[idx, "wikipediaEn"])))




########################################################################
# record the updated tables

# players
tab.file <- file.path(data_folder, "players_01.csv")
write.csv(players, tab.file, row.names = FALSE, fileEncoding = "UTF-8")

# teams
tab.file <- file.path(data_folder, "teams_01.csv")
write.csv(teams, tab.file, row.names = FALSE, fileEncoding = "UTF-8")

# stints
tab.file <- file.path(data_folder, "stints_01.csv")
write.csv(stints, tab.file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# stop logging
end.rec.log()
