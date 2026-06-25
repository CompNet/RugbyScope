########################################################################
# Performs various checks and verification on the stints data table, in
# order to detect issues in the data.
#
# 06/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/check_stints.R")
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
start.rec.log("CheckStints")




########################################################################
# load tables
tlog("Loading stints table")

data_folder <- file.path("data", "fusion")

players <- read.csv(file.path(data_folder, "players_11.csv"))
tlog(2, "Number of players: ", nrow(players))

teams <- read.csv(file.path(data_folder, "teams_09.csv"))
tlog(2, "Number of teams: ", nrow(teams))

stints <- read.csv(file.path(data_folder, "stints_16.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# ids: check player/team exists
# "playerId" "teamRsId"

pids <- stints[, "playerId"]
idx <- match(pids, players[, "wikidataId"])
print(stints[is.na(idx), ])

tids <- stints[, "teamRsId"]
idx <- match(tids, teams[, "rugbyscopeId"])
print(stints[is.na(idx), ])




########################################################################
# info: check player/team info are synched
# "playerName" "teamName" "teamWdId

pids <- stints[, "playerId"]
idx <- match(pids, players[, "wikidataId"])
print(stints[players[idx, "fullName"] != stints[, "playerName"], ])

tids <- stints[, "teamRsId"]
idx <- match(tids, teams[, "rugbyscopeId"])
print(stints[teams[idx, "fullName"] != stints[, "teamName"], ])
print(stints[!is.na(stints[, "teamWdId"]) & teams[idx, "wikidataId"] != stints[, "teamWdId"], ])




########################################################################
# types: check normalization
# "type"

print(sort(table(stints[, "type"], useNA = "always")))
which(stints[, "type"] == "Junior")
#
idx <- which(is.na(stints[, "type"]) & stints[, "dataSource"] != "WD")
stints[idx, ]




########################################################################
# types: check validty, compare to team/player years
# "startYear" "endYear"

# check internal validity
head(sort(unique(stints[, "startYear"])))
tail(sort(unique(stints[, "startYear"])))
head(sort(unique(stints[, "endYear"])))
tail(sort(unique(stints[, "endYear"])))
idx <- which(stints[, "startYear"] > stints[, "endYear"])
if (length(idx) > 0)
  print(stints[idx, ])

# check if stints are posterior to player's birth date
idx <- match(stints[, "playerId"], players[, "wikidataId"])
idx2 <- which(!is.na(stints[, "startYear"]) & !is.na(players[idx, "birthDate"]) & stints[, "startYear"] <= players[idx, "birthDate"])
print(stints[idx2, ])

# check if stints are anterior to player's death date
idx <- match(stints[, "playerId"], players[, "wikidataId"])
idx2 <- which(!is.na(stints[, "startYear"]) & !is.na(players[idx, "deathDate"]) & stints[, "startYear"] <= players[idx, "endDate"])
print(stints[idx2, ])


stints <- read.csv(file.path(data_folder, "stints_16.csv"))
teams <- read.csv(file.path(data_folder, "teams_09.csv"))
teams[, "inceptionDate"] <- as.Date(teams[, "inceptionDate"])
teams[, "terminationDate"] <- as.Date(teams[, "terminationDate"])

# check if stints are posterior to team's inception
idx <- match(stints[, "teamRsId"], teams[, "rugbyscopeId"])
idx2 <- which(!is.na(stints[, "startYear"]) & !is.na(teams[idx, "inceptionDate"]) & stints[, "startYear"] < as.integer(format(teams[idx, "inceptionDate"], "%Y")))
head(stints[idx2, ])
length(idx2)

# check if stints are anterior to team's termination


# fix FR stint : check 1--10 line above, same exact dates > need a fix

stop()




########################################################################
# stats: check validty
# "matchesPlayed" "pointsScored"




########################################################################
# source: order and remove redundancy
# "dataSource"














########################################################################
# ids: check unicity
# "rugbyscopeId" "wikidataId" "allRugbyIds" "googleKnowlIds" "dbpediaId"
id_field <- "rugbyscopeId"
tlog(2, "Processing ", id_field)
tt <- table(stints[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(stints[stints[, id_field] %in% duplicates, ])

id_field <- "wikidataId"
tlog(2, "Processing ", id_field)
tt <- table(stints[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(stints[stints[, id_field] %in% duplicates, ])

id_field <- "allRugbyIds"
tlog(2, "Processing ", id_field)
tt <- table(stints[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(stints[stints[, id_field] %in% duplicates, ])

id_field <- "googleKnowlIds"
tlog(2, "Processing ", id_field)
tt <- table(stints[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(stints[stints[, id_field] %in% duplicates, ])

id_field <- "dbpediaId"
tlog(2, "Processing ", id_field)
tt <- table(stints[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(stints[stints[, id_field] %in% duplicates, ])

# presence of WD ids in the wrong column
for (i in (1:ncol(stints))[-2]) {
  tlog(2, "Processing column ", colnames(stints)[i])
  idx <- which(grepl("Q\\d", stints[, i]))
  which(length(idx) > 0)
    tlog(4, paste0(idx, collapse = ", "))
}

# check multiple ids
which(grepl(";", stints[ ,"allRugbyIds"], fixed = TRUE))
which(grepl(";", stints[ ,"googleKnowlIds"], fixed = TRUE))




########################################################################
# alt names: remove redundant names and sort remaining names
# "fullName" "altNames"
split_altnames <- strsplit(stints[, "altNames"], ";", fixed = TRUE)
split_altnames <- lapply(split_altnames, function(an) if(all(is.na(an))) NA else trimws(an))
temp <- rep(NA, nrow(stints))
for (p in 1:nrow(stints)) {
  if (!all(is.na(split_altnames[[p]])))
    temp[p] <- paste0(sort(setdiff(split_altnames[[p]], stints[p, "fullName"])), collapse = "; ")
}

# verification
idx <- which(stints[, "altNames"] != temp)
print(length(idx))
if (length(idx) > 0)
  for (i in idx) print(c(stints[i, "fullName"], stints[i, "altNames"], temp[i]))

# replace "" by NA
temp[temp == ""] <- NA
# update table
stints[, "altNames"] <- temp

# check if non-latin names as fullnames
idx <- which(stri_detect_regex(stints[, "fullName"], "[^\\p{Latin}\\p{Common}\\p{Inherited}]"))
print(unique(stints[idx, "firstName"]))




########################################################################
# categories: verify normalization
# "type" "countries" "competitions" "tier"

# check names vs. types
print(sort(unique(stints[, "type"])))
idx <- which(stints[, "type"] == "National senior stint")
print(stints[idx, c("fullName", "tier")])
print(as.matrix(sort(stints[idx, "fullName"]), ncol = 1))


# normalize country names
countries <- trimws(unlist(strsplit(stints[, "countries"], ";")))
unique_countries <- sort(unique(countries))
print(unique_countries)
#print(setdiff(unique_countries, names(map)))
sort(table(countries))
#### debug: produce a list of countries, to complement manually
#vals <- setdiff(unique_countries, names(map))
#tab <- cbind(vals, rep(NA, length(vals)))
#tab <- rbind(tab, cbind(names(map), map))
#colnames(tab) <- c("Country", "GoverningBody")
#tab <- tab[order(tab[, 1]), ]
#write.csv(tab, "temptemp.csv", row.names = FALSE)
####
#check stints without country
idx <- which(is.na(stints[, "countries"]))
print(stints[idx, c("rugbyscopeId", "fullName", "countries")])


# check tier values
print(sort(unique(stints[, "tier"])))


########################################################################
# affiliation: complement missing affiliations
# "affiliations"

# display existing affiliations
table(trimws(unlist(strsplit(stints[, "affiliations"], ";"))), useNA = "always")
print(sort(unique(trimws(unlist(strsplit(stints[, "affiliations"], ";"))))))

# read affiliation map
tmp <- read.csv(file.path(res_folder, "_governing_bodies.csv"))
map <- tmp[, "GoverningBody"]
names(map) <- tmp[, "Country"]
# apply to missing values
for (i in 1:length(map)) {
  country <- names(map)[i]
  federation_name <- map[i]

  idx <- which(stints[, "countries"] == country & is.na(stints[, "affiliations"]))
  table(stints[idx, "affiliations"], useNA = "always")
  table(stints[idx, "type"], useNA = "always")
  print(stints[idx, c("fullName", "affiliations")])

  # handle clubs
  idx2 <- idx[stints[idx, "type"] == "Club/franchise stint"]
  if (length(idx2) > 0) {
    print(stints[idx2, c("fullName", "affiliations")])
    stints[idx2, "affiliations"] <- federation_name
  }

  # handle national stints
  if (country == "U.S.A.")
    country <- "United States"
  else if (country == "U.S.S.R.")
    country <- "Soviet Union"
  else if (country == "Czechia")
    country <- "Czech Republic"
  else if (country == "Republic of the Congo")
    country <- "Congo"
  else if (country == "D.R. of the Congo")
    country <- "Democratic Republic of the Congo"
  #
  idx2 <- idx[grepl("national", stints[idx, "type"], ignore.case = TRUE) & grepl(country, stints[idx, "fullName"], ignore.case = TRUE)]
  if (length(idx2) > 0) {
    print(stints[idx2, c("fullName", "affiliations")])
    stints[idx2, "affiliations"] <- federation_name
  }

  # handle regional stints
  idx2 <- idx[stints[idx, "type"] == "Regional stint"]
  if (length(idx2) > 0) {
    print(stints[idx2, c("fullName", "affiliations")])
    stints[idx2, "affiliations"] <- federation_name
  }

  # handle military stints
  idx2 <- idx[stints[idx, "type"] == "Military/police stint"]
  if (length(idx2) > 0) {
    print(stints[idx2, c("fullName", "affiliations")])
    stints[idx2, "affiliations"] <- "None"
  }

  # display non-affiliated stints
  idx2 <- idx[is.na(stints[idx, "affiliations"])]
  print(stints[idx2, c("fullName", "affiliations")])
}

# display non-affiliated stints overall
idx <- which(is.na(stints[, "affiliations"]))
print(stints[idx, c("fullName", "countries", "affiliations")])

# display existing affiliations (after update)
table(trimws(unlist(strsplit(stints[, "affiliations"], ";"))), useNA = "always")
print(sort(unique(trimws(unlist(strsplit(stints[, "affiliations"], ";"))))))
#
sort(unique(sapply(1:nrow(stints), function(i) paste0(stints[i, "countries"], "--", stints[i, "affiliations"]))))


# normalize competition names
compets <- trimws(unlist(strsplit(stints[, "competitions"], ";")))
print(sort(unique(compets)))
sort(table(compets, useNA = "always"))




########################################################################
# numeric: check formating
# "homeVenueCapacities"

caps <- trimws(unlist(strsplit(stints[, "homeVenueCapacities"], ";")))
sort(table(caps, useNA = "always"))
#which(grepl("Galway", stints[, "homeVenueCapacities"], ignore.case = TRUE))




########################################################################
# place names: nothing to do
# "homeVenueNames" "locations"

print(sort(unique(trimws(unlist(strsplit(stints[, "homeVenueNames"], ";"))))))
#
print(sort(unique(trimws(unlist(strsplit(stints[, "locations"], ";"))))))




########################################################################
# dates: check inception < termination
# "inceptionDate" "terminationDate"
stints[, "inceptionDate"] <- as.Date(stints[, "inceptionDate"])
stints[, "terminationDate"] <- as.Date(stints[, "terminationDate"])

tlog(2, "Missing inception dates: ", length(which(is.na(stints[, "inceptionDate"]))), "/", nrow(stints))

# test birth / death dates
head(sort(unique(stints[, "inceptionDate"])))
tail(sort(unique(stints[, "inceptionDate"])))
head(sort(unique(stints[, "terminationDate"])))
tail(sort(unique(stints[, "terminationDate"])))
idx <- which(stints[, "inceptionDate"] > stints[, "terminationDate"])
if (length(idx) > 0)
  print(stints[idx, ])




########################################################################
# urls: check unicity
# "wikipediaEn" "wikipediaFr" "wikipediaIt" "wikipediaEs" "wikipediaJa"

id_field <- "wikipediaEn"
tlog(2, "Processing ", id_field)
tt <- table(stints[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(stints[stints[, id_field] %in% duplicates, ])

id_field <- "wikipediaFr"
tlog(2, "Processing ", id_field)
tt <- table(stints[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(stints[stints[, id_field] %in% duplicates, ])

id_field <- "wikipediaIt"
tlog(2, "Processing ", id_field)
tt <- table(stints[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(stints[stints[, id_field] %in% duplicates, ])

id_field <- "wikipediaEs"
tlog(2, "Processing ", id_field)
tt <- table(stints[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(stints[stints[, id_field] %in% duplicates, ])

id_field <- "wikipediaJa"
tlog(2, "Processing ", id_field)
tt <- table(stints[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(stints[stints[, id_field] %in% duplicates, ])




########################################################################
# comments: nothing to do
# "comments"




########################################################################
# adjust column names
colnames(stints)[colnames(stints) == "allRugbyIds"]  <- "allRugbyId"
colnames(stints)[colnames(stints) == "googleKnowlIds"]  <- "googleKnowlId"





########################################################################
# summarize the table properties

# basic stats
summary(stints)

# a bit more advanced
skim(stints)

# full report
dfSummary(stints)
create_report(stints)




########################################################################
# record players table
tab_file <- file.path(data_folder, "stints_17.csv")
write.csv(stints, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# stop logging
end.rec.log()

# TODO
# - add missing values : dates, others ?
#   > use LLM to retrive them from WP?
# - compare stints and team/player dates
# - players: replace United Kingdom in country by one of the home nations
# - players with very long career
# - missing LAST end date: use average stint duration at team
