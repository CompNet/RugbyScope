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
#### debug: list unique countries
#all_countries <- c(players[, "birthCountry"], players[, "sportCountry"])
#all_countries <- strsplit(all_countries, "; ")
#unique_countries <- sort(unique(trimws(unlist(all_countries))))
#### used to constitute the text2location.csv map used below
# translation map (text to name)
temp <- read.csv(file.path(folder, "maps", "text2location.csv"))
map_fr <- temp[, "location"]
names(map_fr) <- temp[, "text"]
# clean locations
tlog(4, "Substituting locations in the player table")
cols <- c("birthCountry", "sportCountry")
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
sup_cols <- c("debugComment", "wpPage", "currentTeam")
tlog(2, "Remove superfluous columns: ", paste0(sup_cols, collapse = ", "))
cols <- which(colnames(players) %in% sup_cols)
players <- players[, -cols]




########################################################################
# clean the stint table
tlog(0, "Cleaning the stint table")




      stints <- read.csv(file.path(folder, "raw", "stint_info.csv"))
      tlog(2, "Raw number of stints: ", nrow(stints))
      stints <- stints %>% mutate(across(where(is.character), ~ na_if(., "")))




#### debug: checking the point stats
sort(unique(stints[, "pointsScored"]))
####
stints[, "pointsScored"] <- gsub("[-?]+", "", stints[, "pointsScored"], fixed = FALSE)
stints[, "pointsScored"] <- gsub("(\\d) (\\d)", "\\1\\2", stints[, "pointsScored"], fixed = FALSE)

#### debug: checking the match stats
sort(unique(stints[, "matchesPlayed"]))
####
stints[, "matchesPlayed"] <- gsub("[-?]+", "", stints[, "matchesPlayed"], fixed = FALSE)
stints[, "matchesPlayed"] <- gsub("(\\d) (\\d)", "\\1\\2", stints[, "matchesPlayed"], fixed = FALSE)

#### debug: checking the unique period values
#head(sort(unique(stints[, "timePeriod"])), 50)
####

# fix some specific cases
stints[, "timePeriod"] <- gsub("[/–]", "-", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("(\\d) -", "\\1-", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("- (\\d)", "-\\1", stints[, "timePeriod"], fixed = FALSE)
stints[which(stints[, "timePeriod"] == "?; 2007"), "timePeriod"] <- "; 2007"
stints[which(stints[, "timePeriod"] == "1993-1984"), "timePeriod"] <- "1983-1984"
stints[which(stints[, "timePeriod"] == "1998-1992"), "timePeriod"] <- "1988-1992"
stints[which(stints[, "timePeriod"] == "1998-15"), "timePeriod"] <- "1998-2015"
stints[which(stints[, "timePeriod"] == "1998-10"), "timePeriod"] <- "1998-2010"
#
stints[which(stints[, "timePeriod"] == "?-; 1927"), "timePeriod"] <- "-1927"
stints[which(stints[, "timePeriod"] == "-; 2002"), "timePeriod"] <- "-2002"
stints[which(stints[, "timePeriod"] == "?-; 2004"), "timePeriod"] <- "-2004"
stints[which(stints[, "timePeriod"] == "1900; -; 1913"), "timePeriod"] <- "1900-1913"
stints[which(stints[, "timePeriod"] == "1927; -?"), "timePeriod"] <- "1927-"
stints[which(stints[, "timePeriod"] == "1927; -; 1933"), "timePeriod"] <- "1927-1933"
stints[which(stints[, "timePeriod"] == "1947; -; 1957"), "timePeriod"] <- "1947-1957"
stints[which(stints[, "timePeriod"] == "1954; -; 1962"), "timePeriod"] <- "1954-1962"
stints[which(stints[, "timePeriod"] == "1967; -; 1974"), "timePeriod"] <- "1967-1974"
stints[which(stints[, "timePeriod"] == "1983; -; 1993"), "timePeriod"] <- "1983-1993"
stints[which(stints[, "timePeriod"] == "1989; -; 2004"), "timePeriod"] <- "1989-2004"
stints[which(stints[, "timePeriod"] == "1993; -; 1996"), "timePeriod"] <- "1993-1996"
stints[which(stints[, "timePeriod"] == "1996; -; 1997"), "timePeriod"] <- "1996-1997"
stints[which(stints[, "timePeriod"] == "1997; -"), "timePeriod"] <- "1997-"
stints[which(stints[, "timePeriod"] == "1997; -; 2000"), "timePeriod"] <- "1997-2000"
stints[which(stints[, "timePeriod"] == "1998; -"), "timePeriod"] <- "1998-"
stints[which(stints[, "timePeriod"] == "2000; -"), "timePeriod"] <- "2000-"
stints[which(stints[, "timePeriod"] == "2000; -; 2002"), "timePeriod"] <- "2000-2002"
stints[which(stints[, "timePeriod"] == "2001; -"), "timePeriod"] <- "2001-"
stints[which(stints[, "timePeriod"] == "2001; -; 04"), "timePeriod"] <- "2001-2004"
stints[which(stints[, "timePeriod"] == "2002; -"), "timePeriod"] <- "2002-"
stints[which(stints[, "timePeriod"] == "2002; -; 2003"), "timePeriod"] <- "2002-2003"
stints[which(stints[, "timePeriod"] == "2003; -; 2003"), "timePeriod"] <- "2003-2003"
stints[which(stints[, "timePeriod"] == "2003; -; 2004"), "timePeriod"] <- "2003-2004"
stints[which(stints[, "timePeriod"] == "2004; -"), "timePeriod"] <- "2004-"
stints[which(stints[, "timePeriod"] == "2004; -; 2005"), "timePeriod"] <- "2004-2005"
stints[which(stints[, "timePeriod"] == "2005; -; 2008"), "timePeriod"] <- "2005-2008"
#
stints[, "timePeriod"] <- gsub("; -;", "; ;", stints[, "timePeriod"], fixed = TRUE)
stints[which(stints[, "timePeriod"] == "?"), "timePeriod"] <- NA
stints[which(stints[, "timePeriod"] == "?-oggi"), "timePeriod"] <- NA
stints[which(stints[, "timePeriod"] == "xxxx-03"), "timePeriod"] <- "-2003"
stints[which(stints[, "timePeriod"] == "????-11"), "timePeriod"] <- "-2011"
stints[which(stints[, "timePeriod"] == "????-14"), "timePeriod"] <- "-2014"
stints[which(stints[, "timePeriod"] == "194XX-47"), "timePeriod"] <- "-1947"
stints[which(stints[, "timePeriod"] == "199X-93"), "timePeriod"] <- "-1993"
stints[which(stints[, "timePeriod"] == "199X-96"), "timePeriod"] <- "-1996"
stints[which(stints[, "timePeriod"] == "198X-87"), "timePeriod"] <- "-1987"
stints[which(stints[, "timePeriod"] == "198X-88"), "timePeriod"] <- "-1988"
stints[which(stints[, "timePeriod"] == "198X-92"), "timePeriod"] <- "-1992"
stints[which(stints[, "timePeriod"] == "200X-05"), "timePeriod"] <- "-2005"
stints[which(stints[, "timePeriod"] == "200X-11"), "timePeriod"] <- "-2011"
stints[which(stints[, "timePeriod"] == "200X-07"), "timePeriod"] <- "-2007"
stints[which(stints[, "timePeriod"] == "200X-XX"), "timePeriod"] <- NA
#
stints[, "timePeriod"] <- gsub("(\\d{2})[.xX?]{2}-(\\d{2})$", "-\\1\\2", stints[, "timePeriod"], fixed = FALSE)
#
stints[, "timePeriod"] <- gsub("^[.xX?]+-", "-", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("-[.xX?]+$", "-", stints[, "timePeriod"], fixed = FALSE)
#
stints[, "timePeriod"] <- gsub("\\d{3}[.xX?]", "", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("(\\d{2})[.xX?]{2}-(\\d{2})$", "-\\1\\2", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("\\d{2}[.xX?]{2}", "", stints[, "timePeriod"], fixed = FALSE)
#
stints[, "timePeriod"] <- gsub("(199\\d)-(0\\d)$", "\\1-20\\2", stints[, "timePeriod"], fixed = FALSE)
#
stints[, "timePeriod"] <- gsub("-\\d[.xX?]$", "-", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("-[.xX?]{2}$", "-", stints[, "timePeriod"], fixed = FALSE)
#
stints[, "timePeriod"] <- gsub("; -$", "; ", stints[, "timePeriod"], fixed = FALSE)
stints[which(stints[, "timePeriod"] == "-"), "timePeriod"] <- NA

# add columns for start/end years
stints <- cbind(stints, matrix(NA, nrow = nrow(stints), ncol = 2))
colnames(stints)[(ncol(stints) - 1):ncol(stints)] <- c("startYear", "endYear")

# split rows containing multiple stints
new_stints <- stints[-(1:nrow(stints)), ]
err <- c()
for (r in 1:nrow(stints)) {
  if (r %% 1000 == 0)
    tlog(4, "Processing row ", r, "/", nrow(stints))

  periods <- stints[r, "timePeriod"]
  team_names <- stints[r, "teamName"]
  team_urls <- stints[r, "teamWP"]
  matches_played <- stints[r, "matchesPlayed"]
  points_scored <- stints[r, "pointsScored"]

  # try to split row by semicolon, same number of parts for each field (unless totally empty)
  ll <- 0
  if (is.na(periods) || periods == "") {
    periods <- NA
  } else {
    periods <- trimws(strsplit(periods, ";")[[1]])
    ll <- length(periods)
  }
  if (is.na(team_names) || team_names == "") {
    team_names <- NA
  } else {
    team_names <- trimws(strsplit(team_names, ";")[[1]])
    if (ll > 0 && length(team_names) != ll) {
      err <- union(err, stints[r, "wpPage"])
      tlog(6, "Error (team_names): ", paste0(stints[r, ], collapse = ", "))
    } else
      ll <- length(team_names)
  }
  if (is.na(team_urls) || team_urls == "") {
    team_urls <- NA
  } else {
    team_urls <- trimws(strsplit(team_urls, ";")[[1]])
    if (ll > 0 && length(team_urls) != ll) {
      err <- union(err, stints[r, "wpPage"])
      tlog(6, "Error (team_urls): ", paste0(stints[r, ], collapse = ", "))
    } else
      ll <- length(team_urls)
  }
  if (is.na(matches_played) || matches_played == "") {
    matches_played <- NA
  } else {
    matches_played <- trimws(strsplit(matches_played, ";")[[1]])
    if (ll > 0 && length(matches_played) != ll) {
      err <- union(err, stints[r, "wpPage"])
      tlog(6, "Error (matches_played): ", paste0(stints[r, ], collapse = ", "))
    } else
      ll <- length(matches_played)
  }
  if (is.na(points_scored) || points_scored == "") {
    points_scored <- NA
  } else {
    points_scored <- trimws(strsplit(points_scored, ";")[[1]])
    if (ll > 0 && length(points_scored) != ll) {
      err <- union(err, stints[r, "wpPage"])
      tlog(6, "Error (pointsScored): ", paste0(stints[r, ], collapse = ", "))
    }
  }

  # processing each part of the original row (possibly a single one)
  for (i in 1:ll) {
    # if no period information, nothing special to do
    if (all(is.na(periods)) || is.na(periods[i]) || periods[i] == "") {
      periods_sep <- NA
      start_years <- NA
      end_years <- NA
      years <- NA

    # otherwise, try to split row by comma
    } else {
      periods_sep <- strsplit(periods[i], ",")[[1]]
      # compute the number of years
      years <- c()
      start_years <- c()
      end_years <- c()
      for (p in 1:length(periods_sep)) {
        per <- periods_sep[p]
        # there is a hyphen in the period
        if (grepl("-", per, fixed = TRUE)) {
          pers <- as.integer(strsplit(per, "-")[[1]])
          # end year missing
          if (length(pers) < 2 || is.na(pers[2])) {
            years <- c(years, 1)
            start_years <- c(start_years, pers[1])
            end_years <- c(end_years, NA)
          # start year missing
          } else if (is.na(pers[1])) {
            years <- c(years, 1)
            start_years <- c(start_years, NA)
            end_years <- c(end_years, pers[2])
          # both start and end years present
          } else {
            if (pers[2] < pers[1])
              pers[2] <- pers[2] + floor(pers[1] / 100) * 100
            years <- c(years, pers[2] - pers[1] + 1)
            start_years <- c(start_years, pers[1])
            end_years <- c(end_years, pers[2])
          }
        # no hyphen in the period
        } else {
          start_years <- c(start_years, per)
          end_years <- c(end_years, per)
          per <- paste0(per, "-", per)
          years <- c(years, 1)
        }
        periods_sep[p] <- per
      }
    }

    start_years <- trimws(start_years)
    start_years[start_years == "?"] <- NA
    end_years <- trimws(end_years)
    end_years[end_years == "?"] <- NA

    # init rows
    new_rows <- stints[rep(r, length(periods_sep)), ]
    new_rows[, "timePeriod"] <- periods_sep
    new_rows[, "teamName"] <- rep(team_names[i], length(periods_sep))
    new_rows[, "teamWP"] <- rep(team_urls[i], length(periods_sep))
    new_rows[, "startYear"] <- start_years
    new_rows[, "endYear"] <- end_years

    # adjust stats based on number of years
    if (!is.na(matches_played[i]))
      new_rows[, "matchesPlayed"] <- round(as.integer(matches_played[i]) * years / sum(years))
    if (!is.na(points_scored[i]))
      new_rows[, "pointsScored"] <- round(as.integer(points_scored[i]) * years / sum(years))

    # add rows to table
    new_stints <- rbind(new_stints, new_rows)
  }
}
#### debug: check newly created year fields
#options(warn = 2)
#sort(unique(new_stints[, "startYear"]))
#sort(unique(new_stints[, "endYear"]))
####
#which(new_stints[, "startYear"] > new_stints[, "endYear"])
####

# clean team urls
idx <- which(grepl("action=edit&redlink=1", new_stints[, "teamWP"], fixed = FALSE))
if (length(idx) > 0)
  new_stints[idx, "teamWP"] <- NA
#tail(sort(unique(new_stints[, "teamWP"])))
idx <- which(new_stints[, "teamWP"] == "")
if (length(idx) > 0)
  new_stints[idx, "teamWP"] <- NA
#print(length(which(!is.na(new_stints[, "teamWP"]))))  # 10,995/12,513 non-NAs

# solve wikipedia redirections
old_urls <- new_stints[, "teamWP"]
for (r in 1:nrow(new_stints)) {
  url <- new_stints[r, "teamWP"]
  # if (r %% 100 == 0)
    tlog(4, "Solving redirections for entry ", r, "/", nrow(new_stints), " (", url, ")")

  if (!is.na(url)) {
    if (url == "")
      new_stints[r, "teamWP"] <- NA
    else
      # solve redirection
      new_stints[r, "teamWP"] <- solve_redirections(name = url, lang = "it")
  }

  if (!is.na(url) && is.na(new_stints[r, "teamWP"]))
    tlog(6, "Difference: ", r)
}
#### debug: check if we lost some URL after the above processing
#print(length(which(!is.na(old_urls) & is.na(new_stints[, "teamWP"]))))
#print(length(which(!is.na(new_stints[, "teamWP"]))))  # 10,995/12,513 non-NAs
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
write.csv(new_stints, tab_file, row.names = FALSE, fileEncoding = "UTF-8")
