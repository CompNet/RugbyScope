########################################################################
# Complement missing start date in first stint, using birthdate.
#
# 06/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/complement_birthdates.R")
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

stints <- read.csv(file.path(data_folder, "stints_18_raw.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# use birth date to add missing first stint start years
idx <- which(is.na(players[, "birthDate"]))
for (p in 1:length(idx)) {
  player_id <- players[idx[p], "wikidataId"]
  if (p %% 1000 == 0)
    tlog(2, "Processing player ", player_id, "(", p, "/", length(idx), ")")
  birth_year <- players[p, "birthDate"] %>% as.Date() %>% format("%Y") %>% as.integer()

  player_stints <- stints[stints[, "playerId"] == player_id, ]
  idx <- match(player_stints[, "teamRsId"], teams[, "rugbyscopeId"])
  player_teams <- teams[idx, ]

  # a single full NA-NA stint generally is the very first stint:
  # use start year from second stint as end year
  idx <- which(is.na(player_stints[, "startYear"]) & is.na(player_stints[, "endYear"]))
  if (length(idx) == 1 && nrow(player_stints) > 1 && !is.na(player_stints[idx + 1, "startYear"])) {
    tlog(2, "Processing player ", player_id, "(", p, "/", length(idx), ")")
    player_stints[idx, "endYear"] <- player_stints[idx + 1, "startYear"]
    tlog(4, "Single NA-NA stint: end year set to ", player_stints[i, "endYear"])
  }
  # TODO : move out of birthdate constraint

  # missing NA is first stint: use birth date
  idx <- which.min(player_stints[, "endYear"])
  if (length(idx) > 0 && !is.na(player_stints[idx, "startYear"]) && player_stints[idx, "startYear"] >= (birth_year + 18)) {
    tlog(2, "Processing player ", player_id, "(", p, "/", length(idx), ")")
    player_stints[idx, "startYear"] <- birth_year + 18
    tlog(4, "Missing first start year: set to ", player_stints[i, "startYear"])
  }

  if (changed)
    #readline("Press enter to continue")
}




########################################################################
# use birthdate to complement missing youth team years

changes <- 0
for (p in 21077:nrow(players)) {
  if (p %% 1000 == 0)
    tlog(2, "Processing player ", p, "/", nrow(players))

  player_id <- players[p, "wikidataId"]
  birth_year <- players[p, "birthDate"] %>% as.Date() %>% format("%Y") %>% as.integer()
  player_stints <- stints[stints[, "playerId"] == player_id, ]
  idx <- match(player_stints[, "teamRsId"], teams[, "rugbyscopeId"])
  player_teams <- teams[idx, ]

  if (!is.na(birth_year) & any(is.na(player_stints[, "startYear"]) & player_teams[, "type"] %in% c("National U18 team", "National U19 team", "National U20 team", "National U21 team", "National U23 team"))) {
    tlog(2, "Dealing with player ", player_id, " (", p, "/", nrow(players), ")")
    print(player_stints)

    # U18 stint
    i <- which(player_teams[, "type"] == "National U18 team")
    if (length(i) > 0) {
      if (is.na(player_stints[i, "startYear"])) {
        player_stints[i, "startYear"] <- rep(birth_year + 17, length(i))
        tlog(4, "U18 start: ", paste0(player_stints[i, "startYear"], collapse = ","))
        changes <- changes + 1
      }
      if (is.na(player_stints[i, "endYear"])) {
        player_stints[i, "endYear"] <- rep(birth_year + 18, length(i))
        tlog(4, "U18 end: ", paste0(player_stints[i, "endYear"], collapse = ","))
        changes <- changes + 1
      }
    }

    # U19 stint
    i <- which(player_teams[, "type"] == "National U19 team")
    if (length(i) > 0) {
      if (is.na(player_stints[i, "startYear"])) {
        player_stints[i, "startYear"] <- rep(birth_year + 18, length(i))
        tlog(4, "U19 start: ", paste0(player_stints[i, "startYear"], collapse = ","))
        changes <- changes + 1
      }
      if (is.na(player_stints[i, "endYear"])) {
        player_stints[i, "endYear"] <- rep(birth_year + 19, length(i))
        tlog(4, "U19 end: ", paste0(player_stints[i, "endYear"], collapse = ","))
        changes <- changes + 1
      }
    }

    # U20 stint
    i <- which(player_teams[, "type"] == "National U20 team")
    if (length(i) > 0) {
      if (is.na(player_stints[i, "startYear"])) {
        player_stints[i, "startYear"] <- rep(birth_year + 19, length(i))
        tlog(4, "U20 start: ", paste0(player_stints[i, "startYear"], collapse = ","))
        changes <- changes + 1
      }
      if (is.na(player_stints[i, "endYear"])) {
        player_stints[i, "endYear"] <- rep(birth_year + 20, length(i))
        tlog(4, "U20 end: ", paste0(player_stints[i, "endYear"], collapse = ","))
        changes <- changes + 1
      }
    }

    # U21 stint
    i <- which(player_teams[, "type"] == "National U21 team")
    if (length(i) > 0) {
      if (is.na(player_stints[i, "startYear"])) {
        player_stints[i, "startYear"] <- rep(birth_year + 20, length(i))
        tlog(4, "U21 start: ", paste0(player_stints[i, "startYear"], collapse = ","))
        changes <- changes + 1
      }
      if (is.na(player_stints[i, "endYear"])) {
        player_stints[i, "endYear"] <- rep(birth_year + 21, length(i))
        tlog(4, "U21 end: ", paste0(player_stints[i, "endYear"], collapse = ","))
        changes <- changes + 1
      }
    }

    # U23 stint
    i <- which(player_teams[, "type"] == "National U23 team")
    if (length(i) > 0) {
      if (is.na(player_stints[i, "startYear"])) {
        player_stints[i, "startYear"] <- rep(birth_year + 22, length(i), )
        tlog(4, "U23 start: ", paste0(player_stints[i, "startYear"], collapse = ","))
        changes <- changes + 1
      }
      if (is.na(player_stints[i, "endYear"])) {
        player_stints[i, "endYear"] <- rep(birth_year + 23, length(i))
        tlog(4, "U23 end: ", paste0(player_stints[i, "endYear"], collapse = ","))
        changes <- changes + 1
      }
    }

    # update main table
    stints[stints[, "playerId"] == player_id, ] <- player_stints
  }
}
tlog(2, "Number of changes: ", changes)




########################################################################
# record players table
players[, "birthDate"] <- as.Date(players[, "birthDate"])
players[, "deathDate"] <- as.Date(players[, "deathDate"])
#
tab_file <- file.path(data_folder, "players_12.csv")
write.csv(players, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# record stints table
tab_file <- file.path(data_folder, "stints_19_birthyears.csv")
write.csv(stints, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# stop logging
end.rec.log()
