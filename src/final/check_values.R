########################################################################
# Performs various checks and verification on the three data tables, in
# order to detect issues in the data.
#
# 06/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/check_values.R")
########################################################################
library("stringi")
library("stringr")
library("dplyr")
library("httr")
library("jsonlite")
library("magrittr")

source("src/common/logging.R")




########################################################################
# start logging
start.rec.log("CheckValues")




########################################################################
# paths
data_folder <- file.path("data", "fusion")




########################################################################
# load tables
tlog("Loading cleaned tables")

teams <- read.csv(file.path(data_folder, "teams_08.csv"))
tlog(2, "Number of teams: ", nrow(teams))

players <- read.csv(file.path(data_folder, "players_08.csv"))
tlog(2, "Number of players: ", nrow(players))

stints <- read.csv(file.path(data_folder, "stints_16.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# check players

# names: nothing special to do
# "fullName" "firstName" "lastName"

# alt names: remove redundant names and sort remaining names
# "altNames"
split_altnames <- strsplit(players[, "altNames"], ";", fixed = TRUE)
split_altnames <- lapply(split_altnames, function(an) if(all(is.na(an))) NA else trimws(an))
temp <- rep(NA, nrow(players))
for (p in 1:nrow(players)) {
  if (!all(is.na(split_altnames[[p]])))
    temp[p] <- paste0(sort(setdiff(split_altnames[[p]], players[p, "fullName"])), collapse = "; ")
}
# verification
idx <- which(players[, "altNames"] != temp)
print(length(idx))
if (length(idx) > 0)
  for (i in idx) print(c(players[i, "fullName"], players[i, "altNames"], temp[i]))
# replace "" by NA
temp[temp == ""] <- NA
# update table
players[, "altNames"] <- temp


# dates: check birth < death
# "birthDate" "deathDate" "careerStartYears" "careerEndYears"

# places: check no coma remains
# "birthPlaces" "deathPlaces"

# countries: verify normalization
# "citizenships" "sportCountries"

# positions: verify unicity and normalization
# "positions"

# weight/height: check consistency
# "weights" "heights"

# ids: check unicity
# "wikidataId" "espnScrumIds" "allRugbyIds" "googleKnowlIds" "itsRugbyIds" "rugbyDatabaseIds" "dbpediaId"

# urls: check unicity
# "wikipediaEn" "wikipediaFr" "wikipediaIt" "wikipediaEs" "wikipediaJa"




########################################################################
# check teams

# ids: check unicity
# "rugbyscopeId" "wikidataId" "allRugbyIds" "googleKnowlIds" "dbpediaId"

# alt names: remove redundant names and sort remaining names
# "fullName" "altNames"

# categories: verify normalization
# "type" "affiliations" "countries" "competitions" "tier"

# numeric: check formating
# "homeVenueCapacities"

# place names: nothing to do
# "homeVenueNames" "locations"

# dates: check inception < termination
# "inceptionDate" "terminationDate"

# urls: check unicity
# "wikipediaEn" "wikipediaFr" "wikipediaIt" "wikipediaEs" "wikipediaJa"

# comments: nothing to do
# "comments"




########################################################################
# check stints

# player: check existence in player table and name consistency
# "playerId" "playerName"

# categories: check normalization
# "type"

# team: check existence in team table and name consistency
# "teamWdId" "teamRsId" "teamName"

# dates: check start < end
# "startYear" "endYear"

# stats: check consistency
# "matchesPlayed" "pointsScored"

# source: sort and remove redundant values, check normalization
# "dataSource"




########################################################################
# stop logging
end.rec.log()
