########################################################################
# Add (back) the stint types, which for some reason were discarded during
# the earlier data processing steps.
#
# 06/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/add_stint_types.R")
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
source("src/final/fun_stints.R")




########################################################################
# start logging
start.rec.log("AddStintTypes")




########################################################################
# paths
data_folder <- file.path("data", "fusion")

wp_folder <- file.path("data", "wikipedia")




########################################################################
# load WP stint tables

en_stints <- read.csv(file.path(wp_folder, "english", "stints.csv"))
fr_stints <- read.csv(file.path(wp_folder, "french", "stints.csv"))
it_stints <- read.csv(file.path(wp_folder, "italian", "stints.csv"))
ja_stints <- read.csv(file.path(wp_folder, "japanese", "stints.csv"))
# es_stints <- read.csv(file.path(wp_folder, "spanish", "stints.csv"))

all_stints <- list(enWP=en_stints, frWP=fr_stints, itWP=it_stints, jaWP=ja_stints)




########################################################################
# load previously merged tables
tlog("Loading cleaned tables")

teams <- read.csv(file.path(data_folder, "teams_08.csv"))
tlog(2, "Number of teams: ", nrow(teams))

players <- read.csv(file.path(data_folder, "players_08.csv"))
tlog(2, "Number of players: ", nrow(players))

stints <- read.csv(file.path(data_folder, "stints_12.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# normalize stint types

# english
tlog("English version;")
print(table(en_stints[,"stintType"]))
en_map <- c(
  "amateur"="Junior",
  "Amateur team(s)"="Junior",
  "international"="International",
  "International career"="International",
  "senior_club"="Senior",
  "Youth career"="Junior"
)
en_stints[,"stintType"] <- en_map[en_stints[,"stintType"]]
print(table(en_stints[,"stintType"]))

# french
tlog("French version;")
print(table(fr_stints[,"stintType"]))
fr_map <- c(
  "International"="International",
  "Senior"="Senior",
  "Senir"="Senior",
  "Youth"="Junior"
)
fr_stints[,"stintType"] <- fr_map[fr_stints[,"stintType"]]
print(table(fr_stints[,"stintType"]))

# italian
tlog("Italian version;")
print(table(it_stints[,"stintType"]))
it_map <- c(
  "International"="International",
  "Regional"="Regional",
  "Senior"="Senior",
  "Youth"="Junior"
)
it_stints[,"stintType"] <- it_map[it_stints[,"stintType"]]
print(table(it_stints[,"stintType"]))

# japanese
tlog("Japanese version;")
print(table(ja_stints[,"stintType"]))
ja_map <- c(
  "Amateur"="Junior",
  "International"="International",
  "Regional"="Regional",
  "Senior"="Senior",
  "Youth"="Junior"
)
ja_stints[,"stintType"] <- ja_map[ja_stints[,"stintType"]]
print(table(ja_stints[,"stintType"]))

# spanish
#tlog("Spanish version;")
#print(table(es_stints[,"stintType"]))




########################################################################
# match merged stints to WP stints

for (s in 1:nrow(stints)) {
  # retrive data sources for this stint
  sources <- sort(unique(trimws(unlist(strsplit(stints[s, "dataSource"], ";")))))

  # try to match with original stints
  original_types <- c()
  for(source in sources) {
    old_stints <- all_stints
  }

  player_id <- stints[s, "playerId"]

  "playerId"
  "teamRsId"
  "startYear"
  "endYear"
  

}




########################################################################
# stop logging
end.rec.log()
