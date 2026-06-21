########################################################################
# Performs various checks and verification on the teams data table, in
# order to detect issues in the data.
#
# 06/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/check_teams.R")
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
start.rec.log("CheckTeams")




########################################################################
# load tables
tlog("Loading teams table")

data_folder <- file.path("data", "fusion")

teams <- read.csv(file.path(data_folder, "teams_08.csv"))
tlog(2, "Number of teams: ", nrow(teams))




########################################################################
# ids: check unicity
# "rugbyscopeId" "wikidataId" "allRugbyIds" "googleKnowlIds" "dbpediaId"
id_field <- "rugbyscopeId"
tlog(2, "Processing ", id_field)
tt <- table(teams[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(teams[teams[, id_field] %in% duplicates, ])

id_field <- "wikidataId"
tlog(2, "Processing ", id_field)
tt <- table(teams[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(teams[teams[, id_field] %in% duplicates, ])

id_field <- "allRugbyIds"
tlog(2, "Processing ", id_field)
tt <- table(teams[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(teams[teams[, id_field] %in% duplicates, ])

id_field <- "googleKnowlIds"
tlog(2, "Processing ", id_field)
tt <- table(teams[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(teams[teams[, id_field] %in% duplicates, ])

id_field <- "dbpediaId"
tlog(2, "Processing ", id_field)
tt <- table(teams[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(teams[teams[, id_field] %in% duplicates, ])




########################################################################
# alt names: remove redundant names and sort remaining names
# "fullName" "altNames"
split_altnames <- strsplit(teams[, "altNames"], ";", fixed = TRUE)
split_altnames <- lapply(split_altnames, function(an) if(all(is.na(an))) NA else trimws(an))
temp <- rep(NA, nrow(teams))
for (p in 1:nrow(teams)) {
  if (!all(is.na(split_altnames[[p]])))
    temp[p] <- paste0(sort(setdiff(split_altnames[[p]], teams[p, "fullName"])), collapse = "; ")
}

# verification
idx <- which(teams[, "altNames"] != temp)
print(length(idx))
if (length(idx) > 0)
  for (i in idx) print(c(teams[i, "fullName"], teams[i, "altNames"], temp[i]))

# replace "" by NA
temp[temp == ""] <- NA
# update table
teams[, "altNames"] <- temp




########################################################################
# categories: verify normalization
# "type" "countries" "competitions" "tier"

# check names vs. types
print(sort(unique(teams[, "type"])))
idx <- which(teams[, "type"] == "National senior team")
print(teams[idx, c("fullName", "tier")])
print(as.matrix(sort(teams[idx, "fullName"]), ncol = 1))


# normalize country names
print(sort(unique(trimws(unlist(strsplit(teams[, "countries"], ";"))))))
# check teams without country
idx <- which(is.na(teams[, "countries"]))
print(teams[idx, c("rugbyscopeId", "fullName", "countries")])


# check tier values
print(sort(unique(teams[, "tier"])))


########################################################################
# affiliation: complement missing affiliations
# "affiliations"

# display existing affiliations
table(trimws(unlist(strsplit(teams[, "affiliations"], ";"))), useNA = "always")
print(sort(unique(trimws(unlist(strsplit(teams[, "affiliations"], ";"))))))

# JUST DID: complemented all missing countries
# > now must list all countries and complement list of federations below

map <- c(
  "Argentina" = "Argentine Rugby Union",
  "Australia" = "Rugby Australia",
  "Belgium" = "Belgian Rugby Federation",
  "Bosnia and Herzegovina" = "Rugby Union of Bosnia and Herzegovina",
  "Canada" = "Rugby Canada",
  "Croatia" = "Croatian Rugby Federation",
  "Cyprus" = "Cyprus Rugby Federation",
  "Czechia" = "Czech Rugby Union",
  "England" = "Rugby Football Union",
  "Fiji" = "Fiji Rugby Union",
  "Finland" = "Finnish Rugby Federation",
  "France" = "French Rugby Federation",
  "Greece" = "Hellenic Rugby Federation",
  "Hong Kong" = "Hong Kong Rugby Union",
  "Ireland" = "Irish Rugby Football Union",
  "Israel" = "Rugby Israel",
  "Italy" = "Italian Rugby Federation",
  "Japan" = "Japan Rugby Football Union",
  "Kenya" = "Kenya Rugby Football Union",
  "Lebanon" = "Lebanese Rugby Union Federation",
  "Malta" = "Malta Rugby Football Union",
  "Montenegro" = "Montenegrin Rugby Union",
  "Morocco" = "Royal Moroccan Rugby Federation",
  "Nepal" = "Nepal Rugby Association",
  "Netherlands" = "Dutch Rugby Union",
  "New Zealand" = "New Zealand Rugby",
  "Norway" = "Norwegian Rugby Union",
  "Poland" = "Polish Rugby Union",
  "Romania" = "Romanian Rugby Federation",
  "Russia" = "Rugby Union of Russia",
  "Samoa" = "Rugby Samoa",
  "Scotland" = "Scottish Rugby Union",
  "South Africa" = "South African Rugby Union",
  "South Korea" = "Korea Rugby Union",
  "Spain" = "Spanish Rugby Federation",
  "Sri Lanka" = "Sri Lanka Rugby",
  "Taiwan" = "Taiwan Rugby Football Union",
  "Tanzania" = "Tanzania Rugby Football Union",
  "Tonga" = "Tonga Rugby Union",
  "Uganda" = "Uganda Rugby Football Union",
  "Ukraine" = "Ukraine Rugby Union",
  "United Arab Emirates" = "United Arab Emirates Rugby Federation",
  "U.S.A." = "USA Rugby",
  "Wales" = "Welsh Rugby Union",
  "Zimbabwe" = "Zimbabwe Rugby Union"
)
for (i in 1:length(map)) {
  country <- names(map)[i]
  federation_name <- map[i]

  idx <- which(teams[, "countries"] == country & is.na(teams[, "affiliations"]))
  table(teams[idx, "affiliations"], useNA = "always")
  table(teams[idx, "type"], useNA = "always")
  print(teams[idx, c("fullName", "affiliations")])

  # handle clubs
  idx2 <- idx[teams[idx, "type"] == "Club/franchise team"]
  if (length(idx2) > 0) {
    print(teams[idx2, c("fullName", "affiliations")])
    teams[idx2, "affiliations"] <- federation_name
  }

  # handle national teams
  idx2 <- idx[grepl("national", teams[idx, "type"], ignore.case = TRUE) & grepl(country, teams[idx, "fullName"], ignore.case = TRUE)]
  if (length(idx2) > 0) {
    print(teams[idx2, c("fullName", "affiliations")])
    teams[idx2, "affiliations"] <- federation_name
  }

  # handle regional teams
  idx2 <- idx[teams[idx, "type"] == "Regional team"]
  if (length(idx2) > 0) {
    print(teams[idx2, c("fullName", "affiliations")])
    teams[idx2, "affiliations"] <- federation_name
  }
  
  # display non-affiliated teams
  idx2 <- idx[is.na(teams[idx, "affiliations"])]
  print(teams[idx2, c("fullName", "affiliations")])
}
stop("STOP HERE")

# display non-affiliated teams overall
idx <- which(is.na(teams[, "affiliations"]))
print(teams[idx, c("fullName", "countries", "affiliations")])

federation_name <- "Spanish Rugby Federation"
country <- "Spain"
idx <- which(teams[, "countries"] == country)
table(teams[idx, "affiliations"], useNA = "always")
table(teams[idx, "type"], useNA = "always")
print(teams[idx, c("fullName", "affiliations")])
# handle clubs
idx2 <- idx[teams[idx, "type"] == "Club/franchise team"]
print(teams[idx2, c("fullName", "affiliations")])
teams[idx2, "affiliations"] <- federation_name
# handle national teams
idx2 <- idx[grepl("national", teams[idx, "type"], ignore.case = TRUE) & grepl(country, teams[idx, "fullName"], ignore.case = TRUE)]
print(teams[idx2, c("fullName", "affiliations")])
teams[idx2, "affiliations"] <- federation_name
# handle regional teams
idx2 <- idx[teams[idx, "type"] == "Regional team"]
print(teams[idx2, c("fullName", "affiliations")])
teams[idx2, "affiliations"] <- federation_name
# display non-affiliated teams
idx2 <- idx[is.na(teams[idx, "affiliations"])]
print(teams[idx2, c("fullName", "affiliations")])

# TODO deal with unassigned national teams



# normalize competition names
print(sort(unique(trimws(unlist(strsplit(teams[, "competitions"], ";"))))))





########################################################################
# numeric: check formating
# "homeVenueCapacities"




########################################################################
# place names: nothing to do
# "homeVenueNames" "locations"




########################################################################
# dates: check inception < termination
# "inceptionDate" "terminationDate"




########################################################################
# urls: check unicity
# "wikipediaEn" "wikipediaFr" "wikipediaIt" "wikipediaEs" "wikipediaJa"




########################################################################
# comments: nothing to do
# "comments"




########################################################################
# summarize the table properties

# basic stats
summary(teams)

# a bit more advanced
skim(teams)

# full report
dfSummary(teams)
#create_report(teams)




########################################################################
# record players table
tab_file <- file.path(data_folder, "teams_09.csv")
write.csv(teams, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# stop logging
end.rec.log()

# TODO
# - convert dates into dates
# - pseudo-countries like tahiti and so on ? > main country, but possibly local Union
# - add missing values : dates, others ?




















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
print(sort(unique(trimws(unlist(strsplit(players[, "citizenships"], ";"))))))
print(sort(unique(trimws(unlist(strsplit(players[, "sportCountries"], ";"))))))




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
