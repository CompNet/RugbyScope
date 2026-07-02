########################################################################
# Complement missing start date in first stint, using birthdate.
#
# 06/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/complement_firststints.R")
########################################################################
library("stringi")
library("stringr")
library("dplyr")
library("httr")
library("jsonlite")
library("magrittr")
library("skimr")
library("DataExplorer")
library("summarytools")

source("src/common/logging.R")
source("src/common/norm_stints.R")




########################################################################
# start logging
start.rec.log("ComplementFirstStints")




########################################################################
# load tables
tlog("Loading data tables")

data_folder <- file.path("data", "fusion")

players <- read.csv(file.path(data_folder, "players_11.csv"))
tlog(2, "Number of players: ", nrow(players))

teams <- read.csv(file.path(data_folder, "teams_09.csv"))
tlog(2, "Number of teams: ", nrow(teams))

stints <- read.csv(file.path(data_folder, "stints_19_birthyears.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# a single stint with NA-NA dates is generally the very first stint:
# use start year from second stint as end year of this stint

changes <- 0
for (p in 1:nrow(players)) {
  player_id <- players[p, "wikidataId"]
  if (p %% 1000 == 0)
    tlog(2, "Processing player ", player_id, "(", p, "/", nrow(players), ")")

  player_stints <- stints[stints[, "playerId"] == player_id, ]
  idx <- match(player_stints[, "teamRsId"], teams[, "rugbyscopeId"])
  player_teams <- teams[idx, ]

  idx <- which(is.na(player_stints[, "startYear"]) & is.na(player_stints[, "endYear"]) & player_teams[, "type"] != "Invitational team")
  if (length(idx) == 1 && nrow(player_stints) > 1 && !is.na(player_stints[idx + 1, "startYear"])) {
    tlog(4, "Detected the situation for player ", player_id, "(", p, "/", length(idx), ")")
    
    next_year <- player_stints[idx + 1, "startYear"]
    #next_year <- min(player_stints[player_teams[, "type"] %in% c("Club/franchise team", "Military/police team", "Regional team", "National university team"), "startYear"], na.rm = TRUE)
    if (length(next_year) == 1) {
      player_stints[idx, "endYear"] <- next_year
      tlog(4, "Single NA-NA stint: end year set to ", next_year)
      print(player_stints)

      changes <- changes + 1
      #readline("Press enter to continue")
      # update main table
      stints[stints[, "playerId"] == player_id, ] <- player_stints
    }
  }

}
cat("Number of changes: ", changes, "\n")
# NOTE: complements 3,793 missing dates




########################################################################
# use birth date to add missing start year in first stint

changes <- 0
idx <- which(!is.na(players[, "birthDate"]))
for (p in 1:length(idx)) {
  player_id <- players[idx[p], "wikidataId"]
  if (p %% 1000 == 0)
    tlog(2, "Processing player ", player_id, "(", p, "/", length(idx), ")")
  birth_year <- players[idx[p], "birthDate"] %>% as.Date() %>% format("%Y") %>% as.integer()

  player_stints <- stints[stints[, "playerId"] == player_id, ]
  idx2 <- match(player_stints[, "teamRsId"], teams[, "rugbyscopeId"])
  player_teams <- teams[idx2, ]

  # missing NA in first stint: use birth date
  idx3 <- which.min(player_stints[, "endYear"])
  if (length(idx3) > 0 && is.na(player_stints[idx3, "startYear"]) && player_stints[idx3, "endYear"] >= (birth_year + 18)) {
    tlog(2, "Detected the stituation for player ", player_id, "(", p, "/", length(idx), ")")
    
    potential_start <- birth_year + 18
    empty_stint_nbr <- length(which(is.na(player_stints[, "startYear"]) & is.na(player_stints[, "endYear"])))
    # we apply the change only if there are no NA-NA stints (= alternatives) or if the resulting stint is not tool long
    if (empty_stint_nbr == 0 && (player_stints[idx3, "endYear"] - potential_start) <= 4 ||
        empty_stint_nbr > 0 && (player_stints[idx3, "endYear"] - potential_start) <= 2) { # without these constraints: 3,862 changes

      player_stints[idx3, "startYear"] <- potential_start
      tlog(4, "Missing first start year: set to ", player_stints[idx3, "startYear"])
      print(player_stints)

      changes <- changes + 1
      #readline("Press enter to continue")
      # update main table
      stints[stints[, "playerId"] == player_id, ] <- player_stints
    }
  }
}
cat("Number of changes: ", changes, "\n")
# NOTE: complements 2,379 missing dates




########################################################################
# record stints table
stints <- order_stints(stints)
tab_file <- file.path(data_folder, "stints_20_firststint.csv")
write.csv(stints, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# stop logging
end.rec.log()
