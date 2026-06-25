########################################################################
# Performs various checks and verification on the players data table, in
# order to detect issues in the data.
#
# 06/2025 Vincent Labatut
#
# setwd("C:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/check_players.R")
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
start.rec.log("CheckPlayers")




########################################################################
# load tables
tlog("Loading players table")

data_folder <- file.path("data", "fusion")

players <- read.csv(file.path(data_folder, "players_11.csv"))
tlog(2, "Number of players: ", nrow(players))

teams <- read.csv(file.path(data_folder, "teams_09.csv"))
tlog(2, "Number of teams: ", nrow(teams))

stints <- read.csv(file.path(data_folder, "stints_16.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# names: complement missing firstnames / lastnames
# "fullName" "firstName" "lastName"

# check initials in fullname
idx <- which(grepl("[A-Z]{2,}", players[, "fullName"]) & (is.na(players[, "firstName"]) | is.na(players[, "lastName"])))
print(players[idx, c("fullName", "firstName", "lastName", "altNames")])
idx <- which(grepl("\\.", players[, "fullName"]) & (is.na(players[, "firstName"]) | is.na(players[, "lastName"])))
print(players[idx, c("fullName", "firstName", "lastName", "altNames")])

# split fullnames
fullnames <- strsplit(players[, "fullName"], " ", fixed = TRUE)
fullnames_length <- sapply(fullnames, length)
#idx <- which(fullnames_length < 2)
#print(players[idx, c("fullName", "firstName", "lastName", "altNames")])
split_firsts <- rep(NA, nrow(players))
split_lasts <- rep(NA, nrow(players))

# leverage fullnames to recover missing firstnames and lastnames
# first for fullnames containing 2 words
idx <- which(fullnames_length == 2 & (is.na(players[, "firstName"]) | is.na(players[, "lastName"])))
#print(players[idx, c("fullName", "firstName", "lastName", "altNames")])
split_firsts[idx] <- sapply(fullnames[idx], function(x) x[1])
split_lasts[idx] <- sapply(fullnames[idx], function(x) x[2])
print(cbind(players[idx, "fullName"], split_firsts[idx], split_lasts[idx]))
# second for fullnames containing 3 words
idx <- which(fullnames_length > 2 & (is.na(players[, "firstName"]) | is.na(players[, "lastName"])))
#print(players[idx, c("fullName", "firstName", "lastName", "altNames")])
particles <- c("da", "dal", "dalla", "de", "del", "den", "des", "di", "du", "el", "la", "le", "san", "st.", "te", "van", "vanden", "von")
split_firsts[idx] <- sapply(fullnames[idx], function(x) if (toupper(x[2]) %in% toupper(particles)) x[1] else "ERROR")
split_lasts[idx] <- sapply(fullnames[idx], function(x) if (toupper(x[2]) %in% toupper(particles)) paste(x[2], x[3]) else "ERROR")
print(cbind(players[idx, "fullName"], split_firsts[idx], split_lasts[idx]))

# complement missing firstnames
idx2 <- which(is.na(players[, "firstName"]))
before <- players[idx2, "firstName"]
players[idx2, "firstName"] <- split_firsts[idx2]
print(cbind(players[idx2, "fullName"], before, players[idx2, "firstName"]))
# complement missing lastnames
idx2 <- which(is.na(players[, "lastName"]))
before <- players[idx2, "lastName"]
players[idx2, "lastName"] <- split_lasts[idx2]
print(cbind(players[idx2, "fullName"], before, players[idx2, "lastName"]))

# add altnames based on non-latin first/lastnames
idx <- which(stri_detect_regex(players[, "firstName"], "[^\\p{Latin}\\p{Common}\\p{Inherited}]") | stri_detect_regex(players[, "firstName"], "[^\\p{Latin}\\p{Common}\\p{Inherited}]"))
print(players[idx, c("fullName", "firstName", "lastName", "altNames")])
alt_names <- rep(NA, nrow(players))
alt_names[idx] <- paste(players[idx, "lastName"], players[idx, "firstName"])
print(cbind(players[idx, c("fullName", "firstName", "lastName")], alt_names[idx]))
players[idx, "altNames"] <- sapply(idx, function(i) if (is.na(players[i, "altNames"])) alt_names[i] else paste0(players[i, "altNames"], "; ", alt_names[i]))

# convert non-latin firstnames
#unique_firstName <- sort(unique(players[, "firstName"]))
#print(unique_firstName)
idx <- which(stri_detect_regex(players[, "firstName"], "[^\\p{Latin}\\p{Common}\\p{Inherited}]"))
print(unique(players[idx, "firstName"]))
before <- players[idx, "firstName"]
players[idx, "firstName"] <- sapply(fullnames[idx], function(x) x[1])
print(cbind(players[idx, "fullName"], before, players[idx, "firstName"]))

# convert non-latin lastnames
#unique_lastName <- sort(unique(players[, "lastName"]))
#print(unique_lastName)
idx <- which(stri_detect_regex(players[, "lastName"], "[^\\p{Latin}\\p{Common}\\p{Inherited}]"))
print(unique(players[idx, "lastName"]))
before <- players[idx, "lastName"]
players[idx, "lastName"] <- sapply(fullnames[idx], function(x) x[2])
print(cbind(players[idx, "fullName"], before, players[idx, "lastName"]))

# check if hyphens are correctly handled
#idx <- which(grepl("-", players[, "fullName"]) & (grepl(";", players[, "firstName"]) | grepl(";", players[, "lastName"])))
#print(players[idx, c("fullName", "firstName", "lastName", "altNames")])




########################################################################
# alt names: remove redundant forms and sort remaining forms
# "altNames"
split_altnames <- strsplit(players[, "altNames"], ";", fixed = TRUE)
split_altnames <- lapply(split_altnames, function(an) if(all(is.na(an))) NA else trimws(an))
temp <- rep(NA, nrow(players))
for (p in 1:nrow(players)) {
  if (!all(is.na(split_altnames[[p]])))
    temp[p] <- paste0(sort(setdiff(split_altnames[[p]], players[p, "fullName"])), collapse = "; ")
}

# verification
idx <- which(players[, "altNames"] != temp)
print(length(idx))
if (length(idx) > 0)
  for (i in idx) print(c(players[i, "fullName"], players[i, "altNames"], temp[i]))

# replace "" by NA
temp[temp == ""] <- NA
# update table
players[, "altNames"] <- temp




########################################################################
# dates: check birth < death and so on
# "birthDate" "deathDate" "careerStartYears" "careerEndYears"

# test birth / death dates
head(sort(unique(players[, "birthDate"])))
tail(sort(unique(players[, "birthDate"])))
head(sort(unique(players[, "deathDate"])))
tail(sort(unique(players[, "deathDate"])))
idx <- which(players[, "birthDate"] > players[, "deathDate"])
if (length(idx) > 0)
  print(players[idx, ])

# test career dates
head(sort(unique(players[, "careerStartYears"])))
tail(sort(unique(players[, "careerStartYears"])))
head(sort(unique(players[, "careerEndYears"])))
tail(sort(unique(players[, "careerEndYears"])))
idx <- which(players[, "careerStartYears"] > players[, "careerEndYears"])
if (length(idx) > 0)
  print(players[idx, ])
#
idx <- which(players[, "careerStartYears"] < players[, "birthDate"] | players[, "careerStartYears"] > players[, "deathDate"])
if (length(idx) > 0)
  print(players[idx, ])
idx <- which(players[, "careerEndYears"] < players[, "birthDate"] | players[, "careerEndYears"] > players[, "deathDate"])
if (length(idx) > 0)
  print(players[idx, ])

# convert to proper date formats
players[, "birthDate"] <- as.Date(players[, "birthDate"])
players[, "deathDate"] <- as.Date(players[, "deathDate"])




########################################################################
# places: clean up a bit, check no comma remains
# "birthPlaces" "deathPlaces"

# try to clean roughly the place order (in the same entry)
split_places <- strsplit(c(players[, "birthPlaces"], players[, "deathPlaces"]), "[,;] *")
split_places_length <- sapply(split_places, length)
place_cat1 <- c()
place_cat2 <- c()
place_cat3 <- c()
place_others <- c()
for (i in 1:nrow(players)) {
  if (!is.na(split_places_length[i])) {
    if (split_places_length[i] == 1)
      place_cat1 <- c(place_cat1, split_places[[i]])
    else if (split_places_length[i] == 2) {
      place_cat1 <- c(place_cat1, split_places[[i]][1])
      place_cat2 <- c(place_cat2, split_places[[i]][2])
    }
    else if (split_places_length[i] == 3) {
      place_cat1 <- c(place_cat1, split_places[[i]][1])
      place_cat2 <- c(place_cat2, split_places[[i]][2])
      place_cat3 <- c(place_cat3, split_places[[i]][3])
    } else {
      place_others <- c(place_others, paste0(split_places[[i]], collapse = "; "))
    }
  }
}
print(sort(unique(place_cat1)))
print(sort(unique(place_cat2)))
print(sort(unique(place_cat3)))
print(sort(unique(place_others)))

print(sort(intersect(place_cat1, place_cat2)))
print(sort(intersect(place_cat1, place_cat3)))
print(sort(intersect(place_cat2, place_cat3)))

# replace remaining commas by semicolons
idx <- which(grepl(",", players[, "birthPlaces"], fixed = TRUE))
if (length(idx) > 0)
  print(players[idx, c("wikidataId", "birthPlaces")])
players[, "birthPlaces"] <- gsub(",", ";", players[, "birthPlaces"], fixed = TRUE)
#
idx <- which(grepl(",", players[, "deathPlaces"], fixed = TRUE))
if (length(idx) > 0)
  print(players[idx, c("wikidataId", "deathPlaces")])
players[, "deathPlaces"] <- gsub(",", ";", players[, "deathPlaces"], fixed = TRUE)




########################################################################
# countries: verify normalization
# "citizenships" "sportCountries"
home_nations <- c("England", "Ireland", "Scotland", "Wales")

print(sort(table(trimws(unlist(strsplit(players[, "citizenships"], ";"))), useNA = "always")))
print(sort(table(trimws(unlist(strsplit(players[, "sportCountries"], ";"))), useNA = "always")))
print(sort(table(trimws(unlist(strsplit(unlist(players[, c("citizenships", "sportCountries")]), ";"))), useNA = "always")))

print(sort(unique(trimws(unlist(strsplit(c(teams[, "countries"], unlist(players[, c("citizenships", "sportCountries")])), ";"))))))



# fix missing sport countries for UK
idx <- which(players[, "citizenships"] == "England")
players[idx, "citizenships"] <- "United Kingdom"
players[idx, "sportCountries"] <- "England"
idx <- which(grepl("England", players[, "citizenships"]))
#
idx <- which(players[, "citizenships"] == "Scotland")
players[idx, "citizenships"] <- "United Kingdom"
players[idx, "sportCountries"] <- "Scotland"
idx <- which(grepl("Scotland", players[, "citizenships"]))
#
idx <- which(players[, "citizenships"] == "Wales")
players[idx, "citizenships"] <- "United Kingdom"
players[idx, "sportCountries"] <- "Wales"
idx <- which(grepl("Wales", players[, "citizenships"]))

idx <- which(!is.na(players[, "citizenships"]) & players[, "citizenships"] == "United Kingdom" & is.na(players[, "sportCountries"]))
print(players[idx, ])

# leverage international stints
for (i in 1:length(idx)) {
  tlog(2, "Process player ", players[idx[i], "fullName"])
  print(players[idx[i], c("fullName", "birthPlaces", "deathDate", "deathPlaces", "citizenships", "sportCountries")])

  player_stints <- stints[stints[, "playerId"] == players[idx[i], "wikidataId"], ]
  sport_countries <- c()
  for (nation in home_nations) {
    if (grepl(nation, paste0(player_stints[, "teamName"], collapse = "; "), fixed = TRUE))
      sport_countries <- c(sport_countries, nation)
  }

  if (length(sport_countries) > 0) {
    players[idx[i], "sportCountries"] <- paste0(sport_countries, collapse = "; ")
    print(players[idx[i], c("fullName", "birthPlaces", "deathDate", "deathPlaces", "citizenships", "sportCountries")])
  }
}
idx <- which(!is.na(players[, "citizenships"]) & players[, "citizenships"] == "United Kingdom" & is.na(players[, "sportCountries"]))
print(players[idx, ])

# add manual annotations
map <- read.csv("data/fusion/_missing_nations.csv")
idx <- match(map[, "url"], players[, "wikipediaEn"])
cbind(players[idx, "wikipediaEn"], map)
players[idx, "sportCountries"] <- map[, "country"]
#
idx <- which(!is.na(players[, "citizenships"]) & players[, "citizenships"] == "United Kingdom" & is.na(players[, "sportCountries"]))
print(players[idx, ])


# dealing with missing citizenships when there are sport countries
idx <- which(is.na(players[, "citizenships"]) & !is.na(players[, "sportCountries"]))
print(players[idx, ])
# home nations
idx2 <- idx[players[idx, "sportCountries"] %in% setdiff(home_nations, "Ireland")]
players[idx2, "citizenships"] <- "United Kingdom"
print(players[idx2, ])
# other countries
idx <- which(is.na(players[, "citizenships"]) & !is.na(players[, "sportCountries"]))
players[idx, "citizenships"] <- players[idx, "sportCountries"]
players[idx, "sportCountries"] <- NA
print(players[idx, ])


# dealing with missing citizenships and sport country
idx <- which(is.na(players[, "citizenships"]))
print(length(idx))
# leverage international stints
for (i in 1:length(idx)) {
  tlog(2, "Process player ", players[idx[i], "fullName"], "(", i, "/", length(idx), ")")
  print(players[idx[i], c("fullName", "birthPlaces", "deathDate", "deathPlaces", "citizenships", "sportCountries")])

  player_stints <- stints[stints[, "playerId"] == players[idx[i], "wikidataId"], ]
  print(player_stints)
  
  if (is.na(players[idx[i], "citizenships"])) {
    mm <- match(player_stints[, "teamRsId"], teams[, "rugbyscopeId"])
    again <- 0
    while (again >= 0) {
      if (again == 0)
        nat_team_names <- player_stints[teams[mm, "type"] == "National senior team" & !grepl("amateur", player_stints[, "teamName"]), "teamName"]
      else {
        tlog(2, "Retry player ", players[idx[i], "fullName"])
        nat_team_names <- player_stints[grepl("National .* team", teams[mm, "type"]), "teamName"]
        nat_team_names <- gsub(" (amateur|A|B|under-\\d{2}|university) ", " ", nat_team_names, fixed = FALSE)
      }
      if (length(nat_team_names) > 0) {
        nations <- sapply(nat_team_names, function(team_name) {
                    ctry <- str_match(team_name, "(.+) national rugby union team")[2]
                    if (is.na(ctry))
                      ctry
                    else {
                      # case of "A" and "B" teams
                      if (substr(ctry, nchar(ctry) - 1, nchar(ctry) - 1) == " ")
                        ctry <- substr(ctry, 1, nchar(ctry) - 2)
                      else
                        ctry
                    }
                  })
        nations <- sort(unique(nations[!is.na(nations)]))
        if (length(nations) == 0) {
          if (again == 0)
            again <- 1
          else
            again <- -1
        } else if (length(nations) == 1) {
          if (nations %in% setdiff(home_nations, "Ireland")) {
            players[idx[i], "citizenships"] <- "United Kingdom"
            players[idx[i], "sportCountries"] <- nations
          } else {
            players[idx[i], "citizenships"] <- nations
          }
          again <- -1
        } else if (length(nations) > 1) {
          if (any(nations %in% home_nations)) {
            players[idx[i], "citizenships"] <- "United Kingdom"
          } else {
            players[idx[i], "sportCountries"] <- paste0(nations, collapse = "; ")
          }
          again <- -1
        }
      } else {
        if (again == 0)
          again <- 1
        else {
          again <- -1
          nation <- readline("Country?")
          if (nation != "") {
            if (nation %in% setdiff(home_nations, "Ireland")) {
              players[idx[i], "citizenships"] <- "United Kingdom"
              players[idx[i], "sportCountries"] <- nation
            } else {
              players[idx[i], "citizenships"] <- nation
            }
          }
        }
      }
    }

    print(players[idx[i], c("fullName", "birthPlaces", "deathDate", "deathPlaces", "citizenships", "sportCountries")])
    # readline("Press enter to continue")
  }
}
idx2 <- which(is.na(players[, "citizenships"]))
print(players[idx2, c("fullName", "birthPlaces", "deathDate", "deathPlaces", "citizenships", "sportCountries")])




########################################################################
# positions: verify unicity and normalization
# "positions"
split_pos <- strsplit(players[, "positions"], ";")
print(sort(unique(trimws(unlist(split_pos)))))

# # sort positions
# pos_str <- sapply(1:nrow(players), function(i) if (!all(is.na(split_pos[[i]]))) paste0(sort(unique(split_pos[[i]])), collapse = "; ") else NA)
# idx <- which(pos_str != players[, "positions"])
# print(cbind(players[idx, "positions"], pos_str[idx]))
# finally, no: keep original order from WP page, as it may have a meaning




########################################################################
# weight/height: check consistency
# "weights" "heights"
print(sort(unique(players[, "weights"])))
print(sort(unique(players[, "heights"])))




########################################################################
# ids: check unicity
# "wikidataId" "espnScrumIds" "allRugbyIds" "googleKnowlIds" "itsRugbyIds" "rugbyDatabaseIds" "dbpediaId"
id_field <- "wikidataId"
tlog(2, "Processing ", id_field)
tt <- table(players[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(players[players[, id_field] %in% duplicates, ])

id_field <- "espnScrumIds"
tlog(2, "Processing ", id_field)
tt <- table(players[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(players[players[, id_field] %in% duplicates, ])

id_field <- "allRugbyIds"
tlog(2, "Processing ", id_field)
tt <- table(players[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(players[players[, id_field] %in% duplicates, ])

id_field <- "googleKnowlIds"
tlog(2, "Processing ", id_field)
tt <- table(players[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(players[players[, id_field] %in% duplicates, ])

id_field <- "itsRugbyIds"
tlog(2, "Processing ", id_field)
tt <- table(players[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(players[players[, id_field] %in% duplicates, ])
# also convert to integers
before <- players[, "itsRugbyIds"]
after <- as.integer(players[, "itsRugbyIds"])
which(!is.na(before) & is.na(after))
players[, "itsRugbyIds"] <- after

id_field <- "rugbyDatabaseIds"
tlog(2, "Processing ", id_field)
tt <- table(players[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(players[players[, id_field] %in% duplicates, ])

id_field <- "dbpediaId"
tlog(2, "Processing ", id_field)
tt <- table(players[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(players[players[, id_field] %in% duplicates, ])


# presence of WD ids in the wrong column
for (i in (1:ncol(players))[-2]) {
  tlog(2, "Processing column ", colnames(players)[i])
  idx <- which(grepl("Q\\d", players[, i]))
  which(length(idx) > 0)
    tlog(4, paste0(idx, collapse = ", "))
}




########################################################################
# urls: check unicity
# "wikipediaEn" "wikipediaFr" "wikipediaIt" "wikipediaEs" "wikipediaJa"
id_field <- "wikipediaEn"
tlog(2, "Processing ", id_field)
tt <- table(players[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(players[players[, id_field] %in% duplicates, ])

id_field <- "wikipediaFr"
tlog(2, "Processing ", id_field)
tt <- table(players[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(players[players[, id_field] %in% duplicates, ])

id_field <- "wikipediaIt"
tlog(2, "Processing ", id_field)
tt <- table(players[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(players[players[, id_field] %in% duplicates, ])

id_field <- "wikipediaEs"
tlog(2, "Processing ", id_field)
tt <- table(players[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(players[players[, id_field] %in% duplicates, ])

id_field <- "wikipediaJa"
tlog(2, "Processing ", id_field)
tt <- table(players[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(players[players[, id_field] %in% duplicates, ])




########################################################################
# adjust column names
colnames(players)[colnames(players) == "firstName"]  <- "firstNames"
colnames(players)[colnames(players) == "lastName"]  <- "lastNames"
#
colnames(players)[colnames(players) == "weights"]  <- "weight"
colnames(players)[colnames(players) == "heights"]  <- "height"
#
colnames(players)[colnames(players) == "careerStartYears"]  <- "careerStartYear"
colnames(players)[colnames(players) == "careerEndYears"]  <- "careerEndYear"
#
colnames(players)[colnames(players) == "espnScrumIds"]  <- "espnScrumId"
colnames(players)[colnames(players) == "allRugbyIds"]  <- "allRugbyId"
colnames(players)[colnames(players) == "googleKnowlIds"]  <- "googleKnowlId"
colnames(players)[colnames(players) == "itsRugbyIds"]  <- "itsRugbyId"
colnames(players)[colnames(players) == "rugbyDatabaseIds"]  <- "rugbyDatabaseId"




########################################################################
# summarize the table properties

# basic stats
summary(players)

# a bit more advanced
skim(players)

# full report
dfSummary(players)
#create_report(players)




########################################################################
# record players table
tab_file <- file.path(data_folder, "players_11.csv")
write.csv(players, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# stop logging
end.rec.log()
