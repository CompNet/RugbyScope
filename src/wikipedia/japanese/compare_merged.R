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

our_teams <- read.csv(file.path("data", "wikidata", "tables", "teams_02_ref.csv"))
tlog(2, "Raw number of teams: ", nrow(our_teams))

our_players <- read.csv(file.path("data", "dbpedia", "tables", "players_01_wd-dbp.csv"))
tlog(2, "Raw number of players: ", nrow(our_players))

our_careers <- read.csv(file.path("data", "wikidata", "tables", "careers.csv"))
tlog(2, "Raw number of career steps: ", nrow(our_careers))




########################################################################
# load Wikipedia JA tables
tlog("Loading WP JA tables")
wp_folder <- file.path("data", "wikipedia", "japanese", "raw")

wp_players <- read.csv(file.path(wp_folder, "player_info.csv"))
tlog(2, "Raw number of players: ", nrow(wp_players))

wp_careers <- read.csv(file.path(wp_folder, "player_careers.csv"))
tlog(2, "Raw number of career steps: ", nrow(wp_careers))




########################################################################
# a few stats regarding the Wikipedia JA player table
tlog("Wikipedia JA stats:")

tlog("Retrieval outcome for individual players:")
table(wp_players[, "debugComment"])

# remove players with no career steps
idx <- which(wp_players[, "debugComment"] %in% c("Career block not found", "Career steps not found", "No WP JA page"))
wp_players <- wp_players[-idx, ]

# detect irregular values
sort(unique(wp_players[, "birthDate"]))
sort(unique(wp_players[, "deathDate"]))
sort(unique(wp_players[, "height"]))
sort(unique(wp_players[, "weight"]))

# number of players by country
tlog("Number of players by country:")
idx  <- match(wp_players[, "origWdId"], our_players[, "wikidataId"])
print(sort(table(our_players[idx, "citizenships"])))

# distribution of career steps by player
tlog("Distribution of career steps by player:")
step_nbrs <- sapply(wp_players[, "origWdId"], function(id) {
  length(which(wp_careers[, "origWdId"] == id) > 0)
})
print(table(step_nbrs))




########################################################################
# a few stats regarding the Wikipedia JA career table

# detect irregular values
sort(unique(wp_careers[, "matchesPlayed"]))
sort(unique(wp_careers[, "pointsScored"]))
