########################################################################
# Loads the raw Japanese Wikipedia tables and performs some basic cleaning.
#
# 02/2025 Vincent Labatut
########################################################################
library("stringi")
library("stringr")
library("dplyr")

source("src/common/logging.R")
source("src/common/norm_teams.R")




########################################################################
# load WP JA tables
tlog("Loading Wikipedia JA tables")
folder <- file.path("data", "wikipedia", "japanese")

players <- read.csv(file.path(folder, "raw", "player_info.csv"))
tlog(2, "Raw number of players: ", nrow(players))
players <- players %>% mutate(across(where(is.character), ~ na_if(., "")))

careers <- read.csv(file.path(folder, "raw", "player_careers.csv"))
tlog(2, "Raw number of career steps: ", nrow(careers))
careers <- careers %>% mutate(across(where(is.character), ~ na_if(., "")))




########################################################################
# clean the player table
tlog(0, "Cleaning the player table")
# TODO

# some rows are only NAs after the first column
idx <- which(sapply(1:nrow(players), function(r) all(is.na(players[r, 2:ncol(players)]))))
if (length(idx) > 0) {
  tlog(2, "Found ", length(idx), "/", nrow(players), " empty rows")
  players <- players[-idx, ]
}

# normalize positions
map <- c()
map["1/2 Melee"] <- "Scrum-Half"
map["1/2 Ouverture"] <- "Fly-Half"
map["2eme ligne"] <- "2nd Row"
map["3eme ligne"] <- "3rd Row"
map["5/8"] <- "Fly-Half"
map["Ailier"] <- "Winger"
map["Arrière"] <- "Fullback"
map["Centre"] <- "Centre"
map["Full back"] <- "Fullback"
map["Hooker"] <- "Hooker"
map["Lock"] <- "2nd Row"
map["Pilier"] <- "Prop"
map["Prop"] <- "Prop"
map["Talonneur"] <- "Hooker"
map["Wing"] <- "Winger"
idx <- which(!is.na(players[, "position"]))
if (length(idx) > 0)
  players[, "position"] <- map[players[, "position"]]

# remove superfluous columns
cols <- which(colnames(players) %in% c("teamId", "teamName"))
players <- players[, -cols]
# rename id column
col <- which(colnames(players) == "playerId")
colnames(players)[col] <- "customId"




########################################################################
# clean the career table
tlog(0, "Cleaning the career table")

# remove erroneous career steps
idx <- which(careers[, "period"] == "ERROR 404")
if (length(idx) > 0) {
  tlog(2, "Found ", length(idx), "/", nrow(careers), " incorrect career steps")
  careers <- careers[-idx, ]
}

# remove players involved only in removed teams
idx <- which(careers[, "teamId"] %in% removed_teams)
player_ids <- sort(unique(careers[idx, "playerId"]))
except_teams <- c()   # non-removed teams employing players that played for removed teams (debug)
remove_list <- c()    # players to remove (only played in removed teams)
for (player_id in player_ids) {
  ii <- which(careers[, "playerId"] == player_id)
  player_teams <- setdiff(unique(careers[ii, "teamId"]), removed_teams)
  except_teams <- union(except_teams, player_teams)
  if (length(player_teams) == 0)
    remove_list <- c(remove_list, player_id)
  # else
  #   print(player_teams)
}
if (length(remove_list) > 0) {
  idx <- match(remove_list, players[, "customId"])
  tlog(2, "Found ", length(idx), "/", nrow(players), " players involved exclusively in removed teams")
  players <- players[-idx, ]
}

# remove career steps involving removed teams
idx <- which(careers[, "teamId"] %in% removed_teams)
if (length(idx) > 0) {
  tlog(2, "Found ", length(idx), "/", nrow(careers), " career steps involving a removed team")
  careers <- careers[-idx, ]
}

# update team names based on team table (some teams were renamed)
idx <- match(careers[, "teamId"], teams[, "customId"])
careers[, "teamName"] <- teams[idx, "shortName"]

# normalize date format, split start/end years
tlog(2, "Normalizing dates")
careers[, "startYear"] <- NA
careers[, "endYear"] <- NA
str <- strsplit(careers[, "period"], "/")
for (s in 1:length(str)) {
  if (!all(is.na(str[[s]]))) {
    if (length(str[[s]]) == 1)
      period <- c(str[[s]], str[[s]])
    else
      period <- str[[s]]
    for(p in 1:2)
    if (as.integer(period[p]) < 27)
      period[p] <- paste0("20", period[p])
    else if (as.integer(period[p]) < 100)
      period[p] <- paste0("19", period[p])
    careers[s, c("startYear", "endYear")] <- period
  }
}

# remove now superflous period column
col <- which(colnames(careers) == "period")
careers <- careers[, -col]




########################################################################
# record all three cleaned tables as new files

# record player table
tab_file <- file.path(folder, "players_descr.csv")
tlog(2, "Record player table as: ", tab_file)
write.csv(players, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# record career table
tab_file <- file.path(folder, "players_careers.csv")
tlog(2, "Record career table as: ", tab_file)
write.csv(careers, tab_file, row.names = FALSE, fileEncoding = "UTF-8")
