########################################################################
# Complement missing birthdates using youth teams, and vice-versa.
#
# 06/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/complement_birthdate.R")
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
start.rec.log("ComplementBirthDates")




########################################################################
# load tables
tlog("Loading data tables")

data_folder <- file.path("data", "fusion")

players <- read.csv(file.path(data_folder, "players_11.csv"))
tlog(2, "Number of players: ", nrow(players))

teams <- read.csv(file.path(data_folder, "teams_09.csv"))
tlog(2, "Number of teams: ", nrow(teams))

stints <- read.csv(file.path(data_folder, "stints_18.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# check the age of players during yout stints
idx <- which(!is.na(players[, "birthDate"]))
start_age <- list("U18" = c(), "U19" = c(), "U20" = c(), "U21" = c(), "U23" = c())
end_age <- list("U18" = c(), "U19" = c(), "U20" = c(), "U21" = c(), "U23" = c())
for (p in 1:length(idx)) {
  player_id <- players[idx[p], "wikidataId"]
  if (p %% 1000 == 0)
    tlog("Processing player ", player_id, "(", p, "/", length(idx), ")")

  player_stints <- stints[stints[, "playerId"] == player_id, ]
  player_teams <- teams[match(player_stints[, "teamRsId"], teams[, "rugbyscopeId"]), ]

  birth_year <- as.integer(format(as.Date(players[idx[p], "birthDate"]), "%Y"))

  # U18 stint
  i <- which(player_teams[, "type"] == "National U18 team")
  if (length(i) > 1) {
    tlog(2, "WARNING U18: ", paste0(players[idx[p], ], collapse = ","))
    print(player_stints[i, ])
    i <- i[1]
  }
  if (length(i) > 0) {
    if (!is.na(player_stints[i, "startYear"]))
      start_age[["U18"]] <- c(start_age[["U18"]], player_stints[i, "startYear"] - birth_year)
    if (!is.na(player_stints[i, "endYear"]))
      end_age[["U18"]] <- c(end_age[["U18"]], player_stints[i, "endYear"] - birth_year)
  }

  # U19 stint
  i <- which(player_teams[, "type"] == "National U19 team")
  if (length(i) > 1) {
    tlog(2, "WARNING U19: ", paste0(players[idx[p], ], collapse = ","))
    print(player_stints[i, ])
    i <- i[1]
  }
  if (length(i) > 0) {
    if (!is.na(player_stints[i, "startYear"]))
      start_age[["U19"]] <- c(start_age[["U19"]], player_stints[i, "startYear"] - birth_year)
    if (!is.na(player_stints[i, "endYear"]))
      end_age[["U19"]] <- c(end_age[["U19"]], player_stints[i, "endYear"] - birth_year)
  }

  # U20 stint
  i <- which(player_teams[, "type"] == "National U20 team")
  if (length(i) > 1) {
    tlog(2, "WARNING U20: ", paste0(players[idx[p], ], collapse = ","))
    print(player_stints[i, ])
    i <- i[1]
  }
  if (length(i) > 0) {
    if (!is.na(player_stints[i, "startYear"]))
      start_age[["U20"]] <- c(start_age[["U20"]], player_stints[i, "startYear"] - birth_year)
    if (!is.na(player_stints[i, "endYear"]))
      end_age[["U20"]] <- c(end_age[["U20"]], player_stints[i, "endYear"] - birth_year)
  }

  # U21 stint
  i <- which(player_teams[, "type"] == "National U21 team")
  if (length(i) > 1) {
    tlog(2, "WARNING U21: ", paste0(players[idx[p], ], collapse = ","))
    print(player_stints[i, ])
    i <- i[1]
  }
  if (length(i) > 0) {
    if (!is.na(player_stints[i, "startYear"]))
      start_age[["U21"]] <- c(start_age[["U21"]], player_stints[i, "startYear"] - birth_year)
    if (!is.na(player_stints[i, "endYear"]))
      end_age[["U21"]] <- c(end_age[["U21"]], player_stints[i, "endYear"] - birth_year)
  }

  # U23 stint
  i <- which(player_teams[, "type"] == "National U23 team")
  if (length(i) > 1) {
    tlog(2, "WARNING U23: ", paste0(players[idx[p], ], collapse = ","))
    print(player_stints[i, ])
    i <- i[1]
  }
  if (length(i) > 0) {
    if (!is.na(player_stints[i, "startYear"]))
      start_age[["U23"]] <- c(start_age[["U23"]], player_stints[i, "startYear"] - birth_year)
    if (!is.na(player_stints[i, "endYear"]))
      end_age[["U23"]] <- c(end_age[["U23"]], player_stints[i, "endYear"] - birth_year)
  }
}
print(start_age)
print(lapply(start_age, function(a) table(a, useNA = "always") ))
print(end_age)
print(lapply(end_age, function(a) table(a, useNA = "always") ))

# use birth date to add missing youth team years
idx <- which(is.na(players[, "birthDate"]))
for (p in 1:length(idx)) {
  player_id <- players[idx[p], "wikidataId"]
  player_stints <- stints[stints[, "playerId"] == player_id, ]
  player_teams <- teams[match(player_stints[, "teamRsId"], teams[, "rugbyscopeId"]), ]

  birth_year <- NA

  # U18 stint
  i <- which(player_teams[, "type"] == "National U18 team")
  if (length(i) > 0) {
    if (!is.na(player_stints[i, "endYear"]))
      birth_year <- player_stints[i, "endYear"] - 18
    else if (!is.na(player_stints[i, "startYear"]))
      birth_year <- player_stints[i, "endYear"] - 17
  }

  # U19 stint
  if (is.na(birth_year)) {
    i <- which(player_teams[, "type"] == "National U19 team")
    if (length(i) > 0) {
      if (!is.na(player_stints[i, "endYear"]))
        birth_year <- player_stints[i, "endYear"] - 19
      else if (!is.na(player_stints[i, "startYear"]))
        birth_year <- player_stints[i, "endYear"] - 18
    }
  }

  # U20 stint
  if (is.na(birth_year)) {
    i <- which(player_teams[, "type"] == "National U20 team")
    if (length(i) > 0) {
      if (!is.na(player_stints[i, "endYear"]))
        birth_year <- player_stints[i, "endYear"] - 20
      else if (!is.na(player_stints[i, "startYear"]))
        birth_year <- player_stints[i, "endYear"] - 18
    }
  }

  # U21 stint
  if (is.na(birth_year)) {
    i <- which(player_teams[, "type"] == "National U21 team")
    if (length(i) > 0) {
      if (!is.na(player_stints[i, "endYear"]))
        birth_year <- player_stints[i, "endYear"] - 21
      else if (!is.na(player_stints[i, "startYear"]))
        birth_year <- player_stints[i, "endYear"] - 19
    }
  }

  # U23 stint
  if (is.na(birth_year)) {
    i <- which(player_teams[, "type"] == "National U23 team")
    if (length(i) > 0) {
      if (!is.na(player_stints[i, "endYear"]))
        birth_year <- player_stints[i, "endYear"] - 23
      else if (!is.na(player_stints[i, "startYear"]))
        birth_year <- player_stints[i, "endYear"] - 21
    }
  }

  if (!is.na(birth_year))
    players[idx[p], "birthDate"] <- as.Date(paste0(birth_year, "-01-01"))
}




########################################################################
# stop logging
end.rec.log()
