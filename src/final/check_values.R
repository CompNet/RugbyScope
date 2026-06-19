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

# names: check unique names
# "fullName" "firstName" "lastName"
fn <- strsplit(players[, "fullName"], " ", fixed = TRUE)
fn_lg <- sapply(fn, length)
idx <- which(fn_lg < 2)
print(players[idx, c("fullName", "firstName", "lastName", "altNames")])
idx <- which(fn_lg > 2)
print(players[idx, c("fullName", "firstName", "lastName", "altNames")])
alt_names <- rep(NA, nrow(players))

# complement missing firstnames
idx <- which(is.na(players[, "firstName"]) & fn_lg > 2)
print(players[idx, c("fullName", "firstName", "lastName", "altNames")])
particles <- c("de", "du", "la", "le", "st.", "van")

# TODO
# AG Lastname > chercher directement deux capitales consécutives
# A.G. Lastname > chercher directement les "."
# noms composés français deviennent 2 noms


# complement missing lastnames

# convert non-latin lastnames
unique_lastnames <- sort(unique(players[, "lastName"]))
print(unique_lastnames)
idx <- which(stri_detect_regex(unique_lastnames, "[^\\p{Latin}\\p{Common}\\p{Inherited}]"))
print(unique_lastnames[idx])
idx2 <- which(players[, "lastName"] %in% unique_lastnames[idx])
print(players[idx2, c("fullName", "firstName", "lastName", "altNames")])
fnames <- players[idx2, "firstName"]
alt_names[idx2] <- paste(players[idx2, "lastName"], fnames)
print(cbind(players[idx2, c("fullName", "firstName", "lastName", "altNames")], alt_names[idx2]))
players[idx2, "firstName"] <- fnames
players[idx2, "lastName"] <- sapply(fn[is.na(fnames)], function(x) x[length(x)])

# convert non-latin firstnames
unique_firstnames <- sort(unique(players[, "firstName"]))
print(unique_firstnames)
idx3 <- which(stri_detect_regex(unique_firstnames, "[^\\p{Latin}\\p{Common}\\p{Inherited}]"))
print(unique_firstnames[idx3])
idx4 <- which(players[, "firstName"] %in% unique_firstnames[idx3])
print(players[idx4, c("fullName", "firstName", "lastName", "altNames")])
alt_names[idx4] <- paste(players[idx4, "lastName"], players[idx4, "firstName"])

fn <- strsplit(players[idx2, "fullName"], " ", fixed = TRUE)
fn_lg <- sapply(fn, length)
idx <- which(fn_lg > 2)
print(players[idx2[idx], c("fullName", "firstName", "lastName", "altNames")])



# 
idx5 <- sort(union(idx2, idx4))
alt_names <- rep(NA, length(idx5))

# todo check ";"
# fix non-asiat names
# check 3-word fullnames




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
# firstName => firstNames
# lastName => lastNames
