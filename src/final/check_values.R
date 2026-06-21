########################################################################
# Performs various checks and verification on the three data tables, in
# order to detect issues in the data.
#
# 06/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/check_values.R")
########################################################################
library("stringi")
library("stringr")
library("dplyr")
library("httr")
library("jsonlite")
library("magrittr")

source("src/common/logging.R")




########################################################################
# start logging
start.rec.log("CheckValues")




########################################################################
# paths
data_folder <- file.path("data", "fusion")




########################################################################
# load tables
tlog("Loading cleaned tables")

teams <- read.csv(file.path(data_folder, "teams_08.csv"))
tlog(2, "Number of teams: ", nrow(teams))

players <- read.csv(file.path(data_folder, "players_08.csv"))
tlog(2, "Number of players: ", nrow(players))

stints <- read.csv(file.path(data_folder, "stints_16.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# check players

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

# split fullnames to recover missing fistnames and lastnames
# first for fullnames containing 2 words
idx <- which(fullnames_length == 2 & (is.na(players[, "firstName"]) | is.na(players[, "lastName"])))
#print(players[idx, c("fullName", "firstName", "lastName", "altNames")])
split_firsts[idx] <- sapply(fullnames[idx], function(x) x[1])
split_lasts[idx] <- sapply(fullnames[idx], function(x) x[2])
print(cbind(players[idx, "fullName"], split_firsts[idx], split_lasts[idx]))
# then for fullnames containing 3 words
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




# record players table
# tab.file <- file.path(data_folder, "players_09.csv")
# write.csv(players, tab.file, row.names = FALSE, fileEncoding = "UTF-8")









# TODO
# noms composés français deviennent 2 noms
# synch names in player and stint tables






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




# dates: check birth < death
# "birthDate" "deathDate" "careerStartYears" "careerEndYears"
head(sort(unique(players[, "birthDate"])))
tail(sort(unique(players[, "birthDate"])))
head(sort(unique(players[, "deathDate"])))
tail(sort(unique(players[, "deathDate"])))
idx <- which(players[, "birthDate"] >= players[, "deathDate"])
if (length(idx) > 0)
  print(players[idx, ])
#
head(sort(unique(players[, "careerStartYears"])))
tail(sort(unique(players[, "careerStartYears"])))
head(sort(unique(players[, "careerEndYears"])))
tail(sort(unique(players[, "careerEndYears"])))
idx <- which(players[, "careerStartYears"] >= players[, "careerEndYears"])
if (length(idx) > 0)
  print(players[idx, ])
#
idx <- which(players[, "careerStartYears"] < players[, "birthDate"] | players[, "careerStartYears"] > players[, "deathDate"])
if (length(idx) > 0)
  print(players[idx, ])
idx <- which(players[, "careerEndYears"] < players[, "birthDate"] | players[, "careerEndYears"] > players[, "deathDate"])
if (length(idx) > 0)
  print(players[idx, ])




# places: check that no coma remains
# "birthPlaces" "deathPlaces"
idx <- which(grepl(",", players[, "birthPlaces"], fixed = TRUE))
if (length(idx) > 0)
  print(players[idx, c("wikidataId", "birthPlaces")])
players[, "birthPlaces"] <- gsub(",", ";", players[, "birthPlaces"], fixed = TRUE)
#
idx <- which(grepl(",", players[, "deathPlaces"], fixed = TRUE))
if (length(idx) > 0)
  print(players[idx, c("wikidataId", "deathPlaces")])
players[, "deathPlaces"] <- gsub(",", ";", players[, "deathPlaces"], fixed = TRUE)




# countries: verify normalization
# "citizenships" "sportCountries"
print(sort(unique(trimws(unlist(strsplit(players[, "citizenships"], ";"))))))




# positions: verify unicity and normalization
# "positions"




# weight/height: check consistency
# "weights" "heights"




# ids: check unicity
# "wikidataId" "espnScrumIds" "allRugbyIds" "googleKnowlIds" "itsRugbyIds" "rugbyDatabaseIds" "dbpediaId"




# urls: check unicity
# "wikipediaEn" "wikipediaFr" "wikipediaIt" "wikipediaEs" "wikipediaJa"




########################################################################
# check teams

# ids: check unicity
# "rugbyscopeId" "wikidataId" "allRugbyIds" "googleKnowlIds" "dbpediaId"




# alt names: remove redundant names and sort remaining names
# "fullName" "altNames"




# categories: verify normalization
# "type" "affiliations" "countries" "competitions" "tier"




# numeric: check formating
# "homeVenueCapacities"




# place names: nothing to do
# "homeVenueNames" "locations"




# dates: check inception < termination
# "inceptionDate" "terminationDate"




# urls: check unicity
# "wikipediaEn" "wikipediaFr" "wikipediaIt" "wikipediaEs" "wikipediaJa"




# comments: nothing to do
# "comments"




########################################################################
# check stints

# player: check existence in player table and name consistency
# "playerId" "playerName"




# categories: check normalization
# "type"




# team: check existence in team table and name consistency
# "teamWdId" "teamRsId" "teamName"




# dates: check start < end
# "startYear" "endYear"




# stats: check consistency
# "matchesPlayed" "pointsScored"




# source: sort and remove redundant values, check normalization
# "dataSource"




########################################################################
# stop logging
end.rec.log()



# TODO
# synch names in player and stint tables
