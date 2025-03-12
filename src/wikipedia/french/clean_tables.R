########################################################################
# Loads the raw French Wikipedia tables and performs some basic cleaning.
#
# 03/2025 Vincent Labatut
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
# load FR WP tables
tlog("Loading Wikipedia FR tables")
folder <- file.path("data", "wikipedia", "french")

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

# filter out players with no french page
idx <- which(players[, "debugComment"] == "No FR WP page")
tlog(2, "Removing players without a french WP page: ", length(idx), "/", nrow(players))
players <- players[-idx, ]
tlog(4, "Remaing players: ", nrow(players))

# normalize positions
tlog(2, "Normalize rugby positions")
all_positions <- players[, "positions"]
all_positions <- gsub("\\[\\d+\\]", "", all_positions, fixed = FALSE)
all_positions <- gsub("、", "; ", all_positions, fixed = TRUE)
# all_positions <- gsub(Encoding("\xa0"), "_", all_positions, fixed = FALSE)
all_positions <- strsplit(all_positions, "; ")
unique_positions <- sort(unique(trimws(unlist(all_positions))))
#table(unique_positions)
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

# heights are ok, and no weight in FR WP
#sort(unique(players[, "height"]))

# clean birth dates
birth_dates <- players[, "birthDate"]
birth_dates <- gsub("[Vv]ers] (\\d{4})-\\d{4}", "\\1-01-01", birth_dates, fixed = FALSE)
birth_dates <- gsub("[Vv]ers (\\d{4})", "\\1-01-01", birth_dates, fixed = FALSE)
birth_dates <- gsub("^(\\d{4})$", "\\1-01-01", birth_dates, fixed = FALSE)
birth_dates <- gsub("^(\\d{4}-\\d{2})$", "\\1-01", birth_dates, fixed = FALSE)
birth_dates <- gsub("Date et lieu inconnus.", "", birth_dates, fixed = TRUE)
birth_dates <- gsub("Date inconnue", "", birth_dates, fixed = TRUE)
birth_dates <- gsub("inconnue", "", birth_dates, fixed = TRUE)
birth_dates <- gsub("Environ 1906-1907", "1906-01-01", birth_dates, fixed = TRUE)
birth_dates[!is.na(birth_dates) & birth_dates == ""] <- NA
#head(sort(unique(birth_dates)), 20)
#tail(sort(unique(birth_dates)), 20)
#which(!is.na(birth_dates) & is.na(as.Date(birth_dates))) 
birth_dates <- as.Date(birth_dates)
players[, "birthDate"] <- birth_dates

# clean death dates
death_dates <- players[, "deathDate"]
death_dates <- gsub("[Vv]ers (\\d{4})", "\\1-01-01", death_dates, fixed = FALSE)
death_dates <- gsub("Date inconnue", "", death_dates, fixed = TRUE)
death_dates <- gsub("[Ii]nconnue", "", death_dates, fixed = FALSE)
death_dates <- gsub("non connu", "", death_dates, fixed = FALSE)
death_dates <- gsub("^(\\d{4}-\\d{2})$", "\\1-01", death_dates, fixed = FALSE)
death_dates <- gsub("^(\\d{4})$", "\\1-01-01", death_dates, fixed = FALSE)
death_dates[!is.na(death_dates) & death_dates == ""] <- NA
#head(sort(unique(death_dates)), 20)
#tail(sort(unique(death_dates)), 20)
#which(!is.na(death_dates) & is.na(as.Date(death_dates))) 
death_dates <- as.Date(death_dates)
players[, "deathDate"] <- death_dates

# birth and death places
tlog(2, "Normalize birth and death places")
all_urls <- c(players[, "birthPlaceWP"], players[, "deathPlaceWP"])
all_urls <- strsplit(all_urls, "; ")
unique_urls <- sort(unique(trimws(unlist(all_urls))))
unique_urls <- unique_urls[!grepl("redlink=1", unique_urls, fixed = TRUE)]
unique_urls <- unique_urls[!startsWith(unique_urls, "#")]
# define conversion map for locations
tlog(4, "Building the conversion maps")
map_url <- c()
for (i in 1:length(unique_urls)) {
  unique_url <- unique_urls[i]
  tlog(6, "Retrieving translation for \"", unique_url, "\" (", i, "/", length(unique_urls), ")")
  title <- get_english_title(unique_url)
  tlog(8, "Result: ", title)
  if (is.null(title))
    title <- unique_url[i]
  map_url[unique_urls[i]] <- title
}
# debug
#which(is.na(map_url))
# conversion map
temp <- read.csv(file.path(folder, "maps", "url2location.csv"))
map_url2 <- temp[, "location"]
names(map_url2) <- temp[, "url"]
map_url <- c(map_url, map_url2)
# translation map
temp <- read.csv(file.path(folder, "maps", "text2location.csv"))
map_fr <- temp[, "location"]
names(map_fr) <- temp[, "text"]
# clean locations
tlog(4, "Substituting in the table")
cols <- c("birthPlace", "deathPlace")
for (col in cols) {
  tlog(6, "Normalizing \"", col, "\"")
  # split place names
  all_places <- players[, col]
  all_places <- gsub("\\[.+\\]", "", all_places, fixed = FALSE)
  all_places <- strsplit(all_places, "; ")
  # split place urls
  all_urls <- players[, paste0(col, "WP")]
  all_urls <- gsub("\\[.+\\]", "", all_urls, fixed = FALSE)
  all_urls <- strsplit(all_urls, "; ")

  # loop over table rows (ie players)
  for (p in 1:length(all_places)) {
    places <- all_places[[p]]
    urls <- all_urls[[p]]

    if (length(places) == 0) {
      places <- " "
    } else {
      # normalize place names
      for (url in names(map_url))
        places[urls == url] <- map_url[url]

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
# debug
#all_places <- c(players[, "birthPlace"], players[, "deathPlace"])
#all_places <- gsub("\\[.+\\]", "", all_places, fixed = FALSE)
#all_places <- strsplit(all_places, "; ")
#all_places <- sort(unique(unlist(all_places)))
#print(tail(all_places))
# debug
#print(head(players[, c("birthPlace", "deathPlace")]))

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

# fix some specific cases
stints[, "timePeriod"] <- gsub("年", "", stints[, "timePeriod"], fixed = TRUE)
stints[, "timePeriod"] <- gsub("2005/2009/2013", "2005,2009,2013", stints[, "timePeriod"], fixed = TRUE)
stints[, "timePeriod"] <- gsub("、", ",", stints[, "timePeriod"], fixed = TRUE)
stints[, "timePeriod"] <- gsub("20111", "2011", stints[, "timePeriod"], fixed = TRUE)
stints[, "timePeriod"] <- gsub("1995–19991999–20032003–20042004–20092009–2010", "1995-1999,1999-2003,2003-2004,2004-2009,2009-2010", stints[, "timePeriod"], fixed = TRUE)
stints[, "timePeriod"] <- gsub("2011,12,13,", "2011,2012,2013", stints[, "timePeriod"], fixed = TRUE)
stints[, "timePeriod"] <- gsub("2002-2014; 2016-2014; 2014-2018", "2002-2013; 2013-2014; 2014-2018", stints[, "timePeriod"], fixed = TRUE)
stints <- data.frame(lapply(stints, function(col) gsub(";([^ ])", "; \\1", col, fixed = FALSE)))
stints <- data.frame(lapply(stints, function(col) gsub(";$", "; ", col, fixed = FALSE)))
stints <- data.frame(lapply(stints, function(col) gsub("\\[\\d+\\]", "", col, fixed = FALSE)))
stints <- data.frame(lapply(stints, function(col) gsub(",;", ", ;", col, fixed = TRUE)))
stints <- data.frame(lapply(stints, function(col) gsub("; （No.370）", "", col, fixed = TRUE)))
stints <- data.frame(lapply(stints, function(col) gsub(" ?(–|−|〜|‐) ?", "-", col, fixed = FALSE)))
stints <- data.frame(lapply(stints, function(col) gsub("？", "?", col, fixed = TRUE)))

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
# debug
#options(warn = 0)
#sort(unique(new_stints[, "startYear"]))
#sort(unique(new_stints[, "endYear"]))

# clean team urls
idx <- which(grepl("action=edit&redlink=1", new_stints[, "teamWP"], fixed = FALSE))
new_stints[idx, "teamWP"] <- NA
#tail(sort(unique(new_stints[, "teamWP"])))
idx <- which(new_stints[, "teamWP"] == "")
new_stints[idx, "teamWP"] <- NA
#print(length(which(!is.na(new_stints[, "teamWP"]))))  # 15435 non-NAs

# solve wikipedia redirections
for (r in 1:nrow(new_stints)) {
  if (r %% 100 == 0)
    tlog(4, "Solving redirections for entry ", r, "/", nrow(new_stints))

  url <- new_stints[r, "teamWP"]
  if (!is.na(url)) {
    if (url == "")
      new_stints[r, "teamWP"] <- NA
    else {
      # possibly solve retrieval error
      if (grepl("\\b(.+)/wiki/.+", url, fixed = FALSE))
        url <- gsub("\\b(.+)/wiki/.+", "\\1", url)

      # solve redirection
      new_stints[r, "teamWP"] <- solve_redirections(name = url, lang = "fr")
    }
  }

  if (!is.na(url) && is.na(new_stints[r, "teamWP"]))
    tlog(6, "Difference: ", r)
}
#print(length(which(!is.na(new_stints[, "teamWP"]))))  # 15435 non-NAs




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
