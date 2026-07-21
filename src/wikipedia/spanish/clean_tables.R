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
# start logging
start.rec.log("CleaningEsWP")




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

# convert birth/death dates
month_map <- c(
  "enero" = "01",
  "febrero" = "02",
  "marzo" = "03",
  "abril" = "04",
  "mayo" = "05",
  "junio" = "06",
  "julio" = "07",
  "agosto" = "08",
  "septiembre" = "09",
  "setiembre" = "09",
  "octubre" = "10",
  "noviembre" = "11",
  "diciembre" = "12"
)
pattern <- "(\\d+) de (.+) de (\\d+)"
#
# fix some specific birth dates
dates <- players[, "birthDate"]
idx <- which(!grepl(pattern, dates))
#print(dates[idx])
dates[which(dates == "?")] <- NA
dates[which(dates == "1880 o años 1870")] <- "1880-01-01"
dates[which(dates == "1885")] <- "1885-01-01"
dates[which(dates == "1887")] <- "1887-01-01"
dates[which(dates == "1894")] <- "1894-01-01"
dates[which(dates == "1908")] <- "1908-01-01"
dates[which(dates == "1910")] <- "1910-01-01"
dates[which(dates == "1912")] <- "1912-01-01"
dates[which(dates == "1919")] <- "1919-01-01"
dates[which(dates == "1929")] <- "1929-01-01"
dates[which(dates == "1939")] <- "1939-01-01"
dates[which(dates == "1953")] <- "1953-01-01"
dates[which(dates == "25/08/1985")] <- "1985-08-25"
# just convert the regular dates
idx <- which(grepl(pattern, dates))
matches <- regexec(pattern, dates[idx], ignore.case = TRUE)
components <- regmatches(dates[idx], matches)
df <- do.call(rbind, lapply(components, function(x) x[-1]))  # Remove full match
colnames(df) <- c("day", "month", "year")
df[, "month"] <- month_map[df[, "month"]]
dates[idx] <- sapply(1:nrow(df), function(i) paste(df[i, "year"], df[i, "month"], sprintf("%02d", as.integer(df[i, "day"])), sep = "-"))
players[, "birthDate"] <- dates
#
# fix some specific death dates
dates <- players[, "deathDate"]
idx <- which(!grepl(pattern, dates) & !is.na(dates))
#print(dates[idx])
dates[which(dates == "1941")] <- "1941-01-01"
dates[which(dates == "1947")] <- "1947-01-01"
dates[which(dates == "1965")] <- "1965-01-01"
dates[which(dates == "1968")] <- "1968-01-01"
dates[which(dates == "1969")] <- "1969-01-01"
dates[which(dates == "2002")] <- "2002-01-01"
dates[which(dates == "junio de 1975")] <- "1975-06-01"
# just convert the regular dates
idx <- which(grepl(pattern, dates))
matches <- regexec(pattern, dates[idx], ignore.case = TRUE)
components <- regmatches(dates[idx], matches)
df <- do.call(rbind, lapply(components, function(x) x[-1]))  # Remove full match
colnames(df) <- c("day", "month", "year")
df[, "month"] <- month_map[df[, "month"]]
dates[idx] <- sapply(1:nrow(df), function(i) paste(df[i, "year"], df[i, "month"], sprintf("%02d", as.integer(df[i, "day"])), sep = "-"))
players[, "deathDate"] <- dates
#
#### debug: check date format is correct
#sort(unique(players[, "birthDate"]))
#sort(unique(players[, "deathDate"]))
####

# translate birth/death places
tlog(2, "Translate birth and death places")
#### debug
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
  title <- get_english_title(unique_url, "es")
  tlog(8, "Result: ", title)
  if (is.null(title))
    title <- unique_url[i]
  map_url[unique_urls[i]] <- title
}
#### debug
#write.csv(data.frame(names(map_url), map_url), file.path(folder, "automatic_url2location.csv"), row.names = FALSE)
#length(which(is.na(map_url)))
####
# conversion map (url to name)
temp <- read.csv(file.path(folder, "maps", "url2location.csv"))
map_url2 <- temp[, "location"]
names(map_url2) <- temp[, "url"]
for (i in 1:length(map_url2))
  map_url[names(map_url2)[i]] <- map_url2[i]
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
    if (p %% 1000 == 0)
      tlog(8, "Processing row #", p, "/", length(all_places))
    places <- all_places[[p]]
    urls <- all_urls[[p]]

    if (length(places) == 0) {
      places <- " "
    } else {
      # normalize place names
      for (url in names(map_url))
        places[urls == url] <- map_url[url]

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
#### debug: take a look at the normalized names
#all_places <- c(players[, "birthPlace"], players[, "deathPlace"])
#all_places <- gsub("\\[.+\\]", "", all_places, fixed = FALSE)
#all_places <- strsplit(all_places, "; ")
#all_places <- sort(unique(unlist(all_places)))
#print(head(all_places, 50))
#print(tail(all_places, 50))
#### debug: list of names without an associated URL
#idx <- match(all_places, map_url)
#idx <- which(is.na(idx))
#print(all_places[idx])
#### debug: visual check
#print(head(players[, c("birthPlace", "deathPlace")]))
####

# translating country names
#### debug: list unique countries
#all_countries <- c(players[, "citizenship"])
#all_countries <- strsplit(all_countries, "; ")
#unique_countries <- sort(unique(trimws(unlist(all_countries))))
#### used to constitute the text2location.csv map used below
# translation map (text to name)
temp <- read.csv(file.path(folder, "maps", "text2location.csv"))
map_es <- temp[, "location"]
names(map_es) <- temp[, "text"]
# clean locations
tlog(4, "Substituting country names in the player table")
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
      for (es_name in names(map_es))
        places[places == es_name] <- map_es[es_name]

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




########################################################################
# stop logging
end.rec.log()
