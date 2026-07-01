########################################################################
# Complement missing birthdates using youth teams, and vice-versa.
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
start.rec.log("ComplementBirthDates")




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
#print(start_age)
print(lapply(start_age, function(a) table(a, useNA = "always") ))
#print(end_age)
print(lapply(end_age, function(a) table(a, useNA = "always") ))


# use birth date to add missing youth team years
idx <- which(is.na(players[, "birthDate"]))
for (p in 1:length(idx)) {
  player_id <- players[idx[p], "wikidataId"]
  tlog(2, "Processing player ", player_id, "(", p, "/", length(idx), ")")

  player_stints <- stints[stints[, "playerId"] == player_id, ]
  player_teams <- teams[match(player_stints[, "teamRsId"], teams[, "rugbyscopeId"]), ]

  birth_year <- c()

  # U18 stint
  i <- which(player_teams[, "type"] == "National U18 team")
  if (length(i) > 0) {
    if (!is.na(player_stints[i, "startYear"]))
      birth_year <- c(birth_year, player_stints[i, "startYear"] - 18)
    if (!is.na(player_stints[i, "endYear"]))
      birth_year <- c(birth_year, player_stints[i, "endYear"] - 17)
    tlog(4, "U18: ", paste0(birth_year, collapse = ","))
  }

  # U19 stint
  i <- which(player_teams[, "type"] == "National U19 team")
  if (length(i) > 0) {
    if (!is.na(player_stints[i, "startYear"]))
      birth_year <- c(birth_year, player_stints[i, "startYear"] - 19)
    if (!is.na(player_stints[i, "endYear"]))
      birth_year <- c(birth_year, player_stints[i, "endYear"] - 18)
    tlog(4, "U19: ", paste0(birth_year, collapse = ","))
  }

  # U20 stint
  i <- which(player_teams[, "type"] == "National U20 team")
  if (length(i) > 0) {
    if (!is.na(player_stints[i, "startYear"]))
      birth_year <- c(birth_year, player_stints[i, "startYear"] - 20)
    if (!is.na(player_stints[i, "endYear"]))
      birth_year <- c(birth_year, player_stints[i, "endYear"] - 19)
    tlog(4, "U20: ", paste0(birth_year, collapse = ","))
  }

  # U21 stint
  i <- which(player_teams[, "type"] == "National U21 team")
  if (length(i) > 0) {
    if (!is.na(player_stints[i, "startYear"]))
      birth_year <- c(birth_year, player_stints[i, "startYear"] - 21)
    if (!is.na(player_stints[i, "endYear"]))
      birth_year <- c(birth_year, player_stints[i, "endYear"] - 20)
    tlog(4, "U21: ", paste0(birth_year, collapse = ","))
  }

  # U23 stint
  i <- which(player_teams[, "type"] == "National U23 team")
  if (length(i) > 0) {
    if (!is.na(player_stints[i, "startYear"]))
      birth_year <- c(birth_year, player_stints[i, "startYear"] - 23)
    if (!is.na(player_stints[i, "endYear"]))
      birth_year <- c(birth_year, player_stints[i, "endYear"] - 22)
    tlog(4, "U23: ", paste0(birth_year, collapse = ","))
  }

  if (length(birth_year) > 0) {
    bi_ye <- as.integer(names(sort(table(birth_year), decreasing = TRUE))[1])
    tlog(4, "Final: ", bi_ye)
    players[idx[p], "birthDate"] <- paste0(bi_ye, "-01-01")

    #readline("Press enter to continue")
  }
}
# note: complements just 3 missing birthdates




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
