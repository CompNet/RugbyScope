########################################################################
# Loads the raw Italian Wikipedia tables and performs some basic cleaning.
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
# load IT WP tables
tlog("Loading Wikipedia IT tables")
folder <- file.path("data", "wikipedia", "italian")

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

# filter out players with no italian page
idx <- which(players[, "debugComment"] == "No IT WP page")
tlog(2, "Removing players without a italian WP page: ", length(idx), "/", nrow(players))
players <- players[-idx, ]
tlog(4, "Remaining players: ", nrow(players))

# normalize positions
tlog(2, "Normalize rugby positions")
all_positions <- players[, "positions"]
all_positions <- gsub("\\[\\d+\\]", "", all_positions, fixed = FALSE)
# all_positions <- gsub(Encoding("\xa0"), "_", all_positions, fixed = FALSE)
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
all_countries <- c(players[, "birthCountry"], players[, "sportCountry"])
all_countries <- strsplit(all_countries, "; ")
unique_countries <- sort(unique(trimws(unlist(all_countries))))
# TODO: translate country names

# rename certain columns
tlog(2, "Rename certain columns")
col <- which(colnames(players) == "origWdId")
colnames(players)[col] <- "wikidataId"
col <- which(colnames(players) == "origName")
colnames(players)[col] <- "fullName"

# remove superfluous columns
sup_cols <- c("debugComment", "wpPage", "currentTeam", "birthPlaceWP", "deathPlaceWP")
tlog(2, "Remove superfluous columns: ", paste0(sup_cols, collapse = ", "))
cols <- which(colnames(players) %in% sup_cols)
players <- players[, -cols]




########################################################################
# clean the stint table
tlog(0, "Cleaning the stint table")

#### debug: checking the unique period values
#head(sort(unique(stints[, "timePeriod"])))
####

# fix some specific cases
stints[, "timePeriod"] <- gsub("[−–‐]", "-", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^02/17-2017$", "2017-2017", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^03/2025-$", "2025-", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^10/17-2020$", "2017-2020", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^10/2019-2020$", "2019-2020", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^11/22-01/23$", "2022-2023", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^2005-11/2005$", "2005-2005", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^2010-122011$", "2010-2011", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^2010-122011$", "2010-2011", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^2014-02/17$", "2014-2017", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^2016-10/2019$", "2016-2019", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^2017-10/17$", "2017-2017", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^avant 2005$", "-2005", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^Années 1960$", "-1960", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^été 2006$", "2006", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^Jusqu'en 1999$", "-1999", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^jusqu'en 2004$", "-2004", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^Jusqu'en 2006$", "-2006", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^Sept. 2013$", "2013-2013", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^2017-present$", "2017-", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("(19|20)[?x]{2}", "", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("(19\\d|20\\d)[?x]", "", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("[*?.… ]+", "", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("/", "-", stints[, "timePeriod"], fixed = TRUE)
stints[, "timePeriod"] <- gsub("^-$", "", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("--", "-", stints[, "timePeriod"], fixed = TRUE)
stints[, "timePeriod"] <- gsub("^(\\d{4})$", "\\1-\\1", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^-(\\d{4}-\\d{4})$", "\\1", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^(\\d{4}-\\d{4})-$", "\\1", stints[, "timePeriod"], fixed = FALSE)
# stints <- data.frame(lapply(stints, function(col) gsub("\\[\\d+\\]", "", col, fixed = FALSE)))

# add columns for start/end years
stints <- cbind(stints, matrix(NA, nrow = nrow(stints), ncol = 2))
colnames(stints)[(ncol(stints) - 1):ncol(stints)] <- c("startYear", "endYear")

# split rows containing multiple stints
for (r in 1:nrow(stints)) {
  if (r %% 1000 == 0)
    tlog(4, "Processing row ", r, "/", nrow(stints))
  period <- stints[r, "timePeriod"]

  # there is a hyphen in the period
  if (grepl("-", period, fixed = TRUE)) {
    pers <- as.integer(strsplit(period, "-")[[1]])
    # end year missing
    if (length(pers) < 2 || is.na(pers[2])) {
      start_year <- pers[1]
      end_year <- NA
    # start year missing
    } else if (is.na(pers[1])) {
      start_years <- NA
      end_years <- pers[2]
    # both start and end years present
    } else {
      if (pers[2] < pers[1])
        pers[2] <- pers[2] + floor(pers[1] / 100) * 100
      start_year <- pers[1]
      end_year <- pers[2]
    }
  # no hyphen in the period
  } else {
    start_year <- as.integer(period)
    end_year <- as.integer(period)
  }

  # update table
  stints[r, "startYear"] <- trimws(start_year)
  stints[r, "endYear"] <- trimws(end_year)
  stints[r, "timePeriod"] <- paste0(start_year, "-", end_year)
}
#### debug: check newly created year fields
#sort(unique(stints[, "startYear"]))
#sort(unique(stints[, "endYear"]))
####

# clean team urls
idx <- which(grepl("action=edit&redlink=1", stints[, "teamWP"], fixed = FALSE))
if (length(idx) > 0)
  stints[idx, "teamWP"] <- NA
#tail(sort(unique(stints[, "teamWP"])))
idx <- which(stints[, "teamWP"] == "")
if (length(idx) > 0)
  stints[idx, "teamWP"] <- NA
#print(length(which(!is.na(stints[, "teamWP"]))))  # 41,334/46,999 non-NAs

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
      stints[r, "teamWP"] <- solve_redirections(name = url, lang = "it")
  }

  if (!is.na(url) && is.na(stints[r, "teamWP"]))
    tlog(6, "Difference: ", r)
}
#print(length(which(!is.na(old_urls) & is.na(stints[, "teamWP"]))))
#print(length(which(!is.na(stints[, "teamWP"]))))  # 41,334/46,999 non-NAs




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
