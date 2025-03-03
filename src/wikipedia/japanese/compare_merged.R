########################################################################
# Compare the data retrieved from the Japanese version of Wikipedia to
# our merged tables based on Wikidata, DBpedia, and manually curated lists.
#
# 02/2025 Vincent Labatut
########################################################################
library("dplyr")
library("stringi")
library("stringr")

source("src/common/logging.R")
source("src/common/norm_teams.R")




########################################################################
# load our tables
tlog("Loading our own tables")

fus_teams <- read.csv(file.path("data", "fusion", "teams_02_ref.csv"))
tlog(2, "Raw number of teams: ", nrow(fus_teams))

fus_players <- read.csv(file.path("data", "fusion", "players_01_wd-dbp.csv"))
tlog(2, "Raw number of players: ", nrow(fus_players))

fus_stints <- read.csv(file.path("data", "wikidata", "tables", "stints.csv"))
tlog(2, "Raw number of stints: ", nrow(fus_stints))




########################################################################
# load Wikipedia JA tables
tlog("Loading WP JA tables")
wp_folder <- file.path("data", "wikipedia", "japanese", "raw")

wp_players <- read.csv(file.path(wp_folder, "player_info.csv"))
tlog(2, "Raw number of players: ", nrow(wp_players))

wp_stints <- read.csv(file.path(wp_folder, "stint_info.csv"))
tlog(2, "Raw number of stints: ", nrow(wp_stints))




########################################################################
# a few stats regarding the Wikipedia JA player table
tlog("Wikipedia JA stats:")

tlog("Retrieval outcome for individual players:")
table(wp_players[, "debugComment"])

# remove players with no stints
idx <- which(wp_players[, "debugComment"] %in% c("No career block found", "No stint found", "No WP JA page"))
wp_players <- wp_players[-idx, ]

# detect irregular values
sort(unique(wp_players[, "birthDate"]))
sort(unique(wp_players[, "deathDate"]))
sort(unique(wp_players[, "height"]))
sort(unique(wp_players[, "weight"]))

# number of players by country
tlog("Number of players by country:")
idx  <- match(wp_players[, "origWdId"], fus_players[, "wikidataId"])
print(sort(table(fus_players[idx, "citizenships"])))

# distribution of stints by player
tlog("Distribution of stints by player:")
stint_nbrs <- sapply(wp_players[, "origWdId"], function(id) {
  length(which(wp_stints[, "origWdId"] == id) > 0)
})
print(table(stint_nbrs))




########################################################################
# a few stats regarding the Wikipedia JA stint table

# detect irregular values
sort(unique(wp_stints[, "matchesPlayed"]))
sort(unique(wp_stints[, "pointsScored"]))
