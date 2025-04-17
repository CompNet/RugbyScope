########################################################################
# Loads the raw Spanish Wikipedia tables and performs some basic cleaning.
#
# 04/2025 Vincent Labatut
########################################################################
library("stringi")
library("stringr")
library("dplyr")
library("httr")
library("jsonlite")

source("src/common/logging.R")
source("src/common/norm_names.R")
source("src/common/norm_teams.R")




########################################################################
# load ES WP tables
tlog("Loading Wikipedia ES tables")
folder <- file.path("data", "wikipedia", "spanish")

players <- read.csv(file.path(folder, "raw", "player_info.csv"))
tlog(2, "Raw number of players: ", nrow(players))
players <- players %>% mutate(across(where(is.character), ~ na_if(., "")))

stints <- read.csv(file.path(folder, "raw", "stint_info.csv"))
tlog(2, "Raw number of stints: ", nrow(stints))
stints <- stints %>% mutate(across(where(is.character), ~ na_if(., "")))




########################################################################
# clean the player table
tlog(0, "Cleaning the player table")

# show debug message distribution
tlog(2, "Debug messages from retrieval:")
print(table(players[, "debugComment"]))

# filter out players with no spanish page
idx <- which(players[, "debugComment"] == "No ES WP page")
tlog(2, "Removing players without a spanish WP page: ", length(idx), "/", nrow(players))
players <- players[-idx, ]
tlog(4, "Remaining players: ", nrow(players))

# normalize positions
tlog(2, "Normalize rugby positions")
all_positions <- players[, "positions"]
all_positions <- gsub("\\[\\d+\\]", "", all_positions, fixed = FALSE)
all_positions <- strsplit(all_positions, "; ")
unique_positions <- sort(unique(trimws(unlist(all_positions))))
# position conversion map
temp <- read.csv(file.path(folder, "maps", "text2position.csv"))
map <- temp[, "position"]
names(map) <- temp[, "text"]
# clean positions
for (p in 1:length(all_positions)) {
  positions <- all_positions[[p]]

  # if (length(positions) == 0) {
  #   positions <- " "
  # } else {
    # normalize positions names
    for (position in names(map))
      positions[positions == position] <- map[position]
  # }

  # update list
  positions <- unique(positions[!is.na(positions)])
  all_positions[[p]] <- positions
}
# collapse to get strings again
all_positions <- sapply(all_positions, function(positions) paste0(positions, collapse = "; "))
all_positions[all_positions == "NA" | all_positions == ""] <- NA
names(all_positions) <- NULL
players[, "positions"] <- all_positions

# heights are ok
#sort(unique(players[, "height"]))
# weights too
#sort(unique(players[, "weight"]))

# translating country names
#### debug: list unique countries
#all_countries <- c(players[, "citizenship"])
#all_countries <- strsplit(all_countries, "; ")
#unique_countries <- sort(unique(trimws(unlist(all_countries))))
#### used to constitute the text2location.csv map used below
# translation map (text to name)
temp <- read.csv(file.path(folder, "maps", "text2location.csv"))
map_fr <- temp[, "location"]
names(map_fr) <- temp[, "text"]
# clean locations
tlog(4, "Substituting locations in the player table")
cols <- c("citizenship")
for (col in cols) {
  tlog(6, "Normalizing \"", col, "\"")
  # split place names
  all_places <- players[, col]
  all_places <- gsub("\\[.+\\]", "", all_places, fixed = FALSE)
  all_places <- strsplit(all_places, "; ")

  # loop over table rows (ie players)
  for (p in 1:length(all_places)) {
    if (p %% 1000 == 0)
      tlog(8, "Processing row #", p, "/", length(all_places))
    places <- all_places[[p]]

    if (length(places) == 0) {
      places <- " "
    } else {
      # translate remaining names
      for (fr_name in names(map_fr))
        places[places == fr_name] <- map_fr[fr_name]

      # remove duplicates
      places <- gsub(", ?", "; ", places, fixed = FALSE)
      places <- unique(unlist(strsplit(places, "; ")))
    }

    # update list
    all_places[[p]] <- places
  }

  # collapse to get strings again
  all_places <- sapply(all_places, function(places) paste0(places, collapse = "; "))
  all_places[all_places == "NA"] <- NA
  names(all_places) <- NULL
  players[, col] <- all_places
}

# rename certain columns
tlog(2, "Rename certain columns")
col <- which(colnames(players) == "origWdId")
colnames(players)[col] <- "wikidataId"
col <- which(colnames(players) == "origName")
colnames(players)[col] <- "fullName"

# remove superfluous columns
sup_cols <- c("debugComment", "wpPage", "currentTeam", "positionWP")
tlog(2, "Remove superfluous columns: ", paste0(sup_cols, collapse = ", "))
cols <- which(colnames(players) %in% sup_cols)
players <- players[, -cols]




########################################################################
# clean the stint table
tlog(0, "Cleaning the stint table")

#### debug: checking the point stats
#sort(unique(stints[, "pointsScored"]))
####
stints[which(stints[, "pointsScored"] == "1.237"), "pointsScored"] <- 1237
stints[which(stints[, "pointsScored"] == "49 (VII)5 (XV)"), "pointsScored"] <- 5

#### debug: checking the match stats
#sort(unique(stints[, "matchesPlayed"]))
####
stints[, "matchesPlayed"] <- gsub("[-?]+", "", stints[, "matchesPlayed"], fixed = FALSE)
stints[which(stints[, "matchesPlayed"] == ""), "matchesPlayed"] <- NA

#### debug: checking the unique period values
#head(sort(unique(stints[, "startYear"])), 50)
#head(sort(unique(stints[, "endYear"])), 50)
####

# fix some specific cases
stints[, "startYear"] <- gsub("\\?+", "", stints[, "startYear"], fixed = FALSE)
stints[, "endYear"] <- gsub("\\?+", "", stints[, "endYear"], fixed = FALSE)
stints[which(stints[, "endYear"] %in% c("act.", "Act.", "actualidad", "Actualidad", "presente")), "endYear"] <- NA
stints[which(stints[, "endYear"] == ""), "endYear"] <- NA

# clean team urls
idx <- which(grepl("action=edit&redlink=1", stints[, "teamWP"], fixed = FALSE))
if (length(idx) > 0)
  stints[idx, "teamWP"] <- NA
#tail(sort(unique(stints[, "teamWP"])))
idx <- which(stints[, "teamWP"] == "")
if (length(idx) > 0)
  stints[idx, "teamWP"] <- NA
#print(length(which(!is.na(stints[, "teamWP"]))))  # 4,953/6,575 non-NAs

# solve wikipedia redirections
old_urls <- stints[, "teamWP"]
for (r in 1:nrow(stints)) {
  url <- stints[r, "teamWP"]
  # if (r %% 100 == 0)
    tlog(4, "Solving redirections for entry ", r, "/", nrow(stints), " (", url, ")")

  if (!is.na(url)) {
    if (url == "")
      stints[r, "teamWP"] <- NA
    else
      # solve redirection
      stints[r, "teamWP"] <- solve_redirections(name = url, lang = "es")
  }

  if (!is.na(url) && is.na(stints[r, "teamWP"]))
    tlog(6, "Difference: ", r)
}
#### debug: check if we lost some URL after the above processing
#print(length(which(!is.na(old_urls) & is.na(stints[, "teamWP"]))))
#print(length(which(!is.na(stints[, "teamWP"]))))  # 4,953/6,575 non-NAs
####




########################################################################
# record both cleaned tables as new files

# record player table
tab_file <- file.path(folder, "players.csv")
tlog(2, "Record player table as: ", tab_file)
write.csv(players, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# record stint table
tab_file <- file.path(folder, "stints.csv")
tlog(2, "Record stint table as: ", tab_file)
write.csv(stints, tab_file, row.names = FALSE, fileEncoding = "UTF-8")
