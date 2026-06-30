########################################################################
# Performs various checks and verification on the stints data table, in
# order to detect issues in the data.
#
# 06/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/check_stints.R")
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
start.rec.log("CheckStints")




########################################################################
# load tables
tlog("Loading data tables table")

data_folder <- file.path("data", "fusion")

players <- read.csv(file.path(data_folder, "players_11.csv"))
tlog(2, "Number of players: ", nrow(players))

teams <- read.csv(file.path(data_folder, "teams_09.csv"))
tlog(2, "Number of teams: ", nrow(teams))

stints <- read.csv(file.path(data_folder, "stints_18.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# ids: check player/team exists
# "playerId" "teamRsId"

pids <- stints[, "playerId"]
idx <- match(pids, players[, "wikidataId"])
print(stints[is.na(idx), ])

tids <- stints[, "teamRsId"]
idx <- match(tids, teams[, "rugbyscopeId"])
print(stints[is.na(idx), ])




########################################################################
# info: check player/team info is synched
# "playerName" "teamName" "teamWdId

pids <- stints[, "playerId"]
idx <- match(pids, players[, "wikidataId"])
print(stints[players[idx, "fullName"] != stints[, "playerName"], ])

tids <- stints[, "teamRsId"]
idx <- match(tids, teams[, "rugbyscopeId"])
print(stints[teams[idx, "fullName"] != stints[, "teamName"], ])
print(stints[!is.na(stints[, "teamWdId"]) & teams[idx, "wikidataId"] != stints[, "teamWdId"], ])




########################################################################
# types: check normalization
# "type"

print(sort(table(stints[, "type"], useNA = "always")))
which(stints[, "type"] == "Junior")
#
idx <- which(is.na(stints[, "type"]) & stints[, "dataSource"] != "WD")
stints[idx, ]




########################################################################
# types: check validty, compare to team/player years
# "startYear" "endYear"

# check internal validity
head(sort(unique(stints[, "startYear"])))
tail(sort(unique(stints[, "startYear"])))
head(sort(unique(stints[, "endYear"])))
tail(sort(unique(stints[, "endYear"])))
idx <- which(stints[, "startYear"] > stints[, "endYear"])
if (length(idx) > 0)
  print(stints[idx, ])

# check if stints are anterior to player's birth date
idx <- match(stints[, "playerId"], players[, "wikidataId"])
idx2 <- which(!is.na(stints[, "startYear"]) & !is.na(players[idx, "birthDate"]) & stints[, "startYear"] <= as.integer(format(players[idx, "birthDate"], "%Y")))
print(stints[idx2, ])

# check if stints are posterior to player's death date
idx <- match(stints[, "playerId"], players[, "wikidataId"])
idx2 <- which(!is.na(stints[, "startYear"]) & !is.na(players[idx, "deathDate"]) & stints[, "startYear"] > as.integer(format(players[idx, "endDate"], "%Y")))
print(stints[idx2, ])

# check if stints are anterior to team's inception
idx <- match(stints[, "teamRsId"], teams[, "rugbyscopeId"])
idx2 <- which(!is.na(stints[, "startYear"]) & !is.na(teams[idx, "inceptionDate"]) & stints[, "startYear"] < as.integer(format(teams[idx, "inceptionDate"], "%Y")))
#head(stints[idx2, ])
length(idx2)
stints[idx2[order(stints[idx2, "teamName"])], ]

# check if stints are posterior to team's termination
idx <- match(stints[, "teamRsId"], teams[, "rugbyscopeId"])
idx2 <- which(!is.na(stints[, "startYear"]) & !is.na(teams[idx, "terminationDate"]) & stints[, "startYear"] > as.integer(format(teams[idx, "terminationDate"], "%Y")))
#head(stints[idx2, ])
length(idx2)
stints[idx2[order(stints[idx2, "teamName"])], ]

# fix FR stint : check 1--10 line above, same exact dates > need a fix
flagged <- 0
for (player_id in players[, "wikidataId"]) {
  ps <- which(stints[, "playerId"] == player_id)
  if (length(ps) > 0 && stints[ps[1], "dataSource"] == "frWP") {
    start_year1 <- stints[ps[1], "startYear"]
    end_year1 <- stints[ps[1], "endYear"]
    for (i in 1:10) {
      if (ps[1] - i > 0) {
        start_year2 <- stints[ps[1] - i, "startYear"]
        end_year2 <- stints[ps[1] - i, "endYear"]
        if (!is.na(start_year1) && !is.na(start_year2) && start_year1 == start_year2 &&
            !is.na(end_year1) && !is.na(end_year2) && end_year1 == end_year2) {
          tlog(2, "ALERT:")
          print(stints[c(ps[1] - i, ps[1]), ])
          flagged <- flagged + 1
        }
      }
    }
  }
}
tlog("Number of potential issues detected: ", flagged)

# check career durations
durations <- c()
for (p in 1:nrow(players)) {
  player_stints <- stints[stints[, "playerId"] == players[p, "wikidataId"], ]
  start_year <- min(c(player_stints[, "startYear"], player_stints[, "endYear"]), na.rm = TRUE)
  end_year <- max(c(player_stints[, "startYear"], player_stints[, "endYear"]), na.rm = TRUE)
  if (!is.na(start_year) && !is.infinite(start_year) && !is.na(end_year) && !is.infinite(end_year))
    durations <- c(durations, end_year - start_year + 1)
  else
    durations <- c(durations, NA)
}
print(sort(unique(durations)))
print(stints[stints[, "playerId"] %in% players[which(durations == 85), "wikidataId"], ])

# check stint gaps
stints <- read.csv(file.path(data_folder, "stints_18.csv"))
gaps <- c()
for (p in 1:nrow(players)) {
  player_stints <- stints[stints[, "playerId"] == players[p, "wikidataId"], ]
  gg <- 0
  if (nrow(player_stints) > 1) {
    for (s1 in 1:(nrow(player_stints) - 1)) {
      gs <- 0
      if (!is.na(player_stints[s1, "endYear"])) {
        gs <- sapply(player_stints[(s1 + 1):nrow(player_stints), "startYear"], function(year) if (is.na(year)) 0 else max(year - player_stints[s1, "endYear"], 0))
        #print(gs)
      }
      gg <- max(c(gg, min(gs)))
    }
  }
  gaps <- c(gaps, gg)
}
print(sort(unique(gaps)))
print(stints[stints[, "playerId"] %in% players[which(gaps == 26), "wikidataId"], ])




########################################################################
# stats: check validity
# "matchesPlayed" "pointsScored"

# check numbers of matches played
durations <- sapply(1:nrow(stints), function(i) max(1, stints[i, "endYear"] - stints[i, "startYear"]))
sort(unique(stints[, "matchesPlayed"]))
#which(stints[, "matchesPlayed"] == 2748)
tail(sort(unique(stints[, "matchesPlayed"] / durations)), 250)
tail(stints[order((stints[, "matchesPlayed"] / durations), na.last = FALSE), ])
#hh=hist(stints[, "matchesPlayed"] / durations, breaks=50);plot(hh$breaks, c(hh$counts,0), log="y")

# check numbers of points scored
sort(unique(stints[, "pointsScored"]))
#which(stints[, "pointsScored"] == 20040)
tail(sort(unique(stints[, "pointsScored"] / stints[, "matchesPlayed"])), 250)
tail(stints[order((stints[, "pointsScored"] / stints[, "matchesPlayed"]), na.last = FALSE), ], 100)




########################################################################
# source: order and remove redundancy
# "dataSource"

# display source distribution
split_src <- strsplit(stints[, "dataSource"], ";", fixed = TRUE)
split_src <- lapply(split_src, function(an) if(all(is.na(an))) NA else trimws(an))
print(table(unlist(split_src), useNA = "always"))

# fix redundancies
temp <- rep(NA, nrow(stints))
for (p in 1:nrow(stints)) {
  if (!all(is.na(split_src[[p]])))
    temp[p] <- paste0(sort(unique(split_src[[p]])), collapse = "; ")
}

# verification
idx <- which(stints[, "dataSource"] != temp)
print(length(idx))
if (length(idx) > 0)
  for (i in idx) print(c(stints[i, "dataSource"], temp[i]))

# update table
stints[, "dataSources"] <- temp




########################################################################
# adjust column names
colnames(stints)[colnames(stints) == "dataSource"]  <- "dataSources"




########################################################################
# summarize the table properties

# basic stats
summary(stints)

# a bit more advanced
skim(stints)

# full report
dfSummary(stints)
create_report(stints)




########################################################################
# record players table
tab_file <- file.path(data_folder, "stints_18.csv")
write.csv(stints, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# stop logging
end.rec.log()
