########################################################################
# Performs various checks and verification on the teams data table, in
# order to detect issues in the data.
#
# 06/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/fusion/check_teams.R")
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
res_folder <- file.path("data", "references", "union")

teams <- read.csv(file.path(data_folder, "teams_09.csv"))
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

# presence of WD ids in the wrong column
for (i in (1:ncol(teams))[-2]) {
  tlog(2, "Processing column ", colnames(teams)[i])
  idx <- which(grepl("Q\\d", teams[, i]))
  which(length(idx) > 0)
    tlog(4, paste0(idx, collapse = ", "))
}

# check multiple ids
which(grepl(";", teams[, "allRugbyIds"], fixed = TRUE))
which(grepl(";", teams[, "googleKnowlIds"], fixed = TRUE))




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

# check if non-latin names as fullnames
idx <- which(stri_detect_regex(teams[, "fullName"], "[^\\p{Latin}\\p{Common}\\p{Inherited}]"))
print(unique(teams[idx, "firstName"]))




########################################################################
# categories: verify normalization
# "type" "countries" "competitions" "tier"

# check names vs. types
print(sort(unique(teams[, "type"])))
idx <- which(teams[, "type"] == "National senior team")
print(teams[idx, c("fullName", "tier")])
print(as.matrix(sort(teams[idx, "fullName"]), ncol = 1))


# normalize country names
countries <- trimws(unlist(strsplit(teams[, "countries"], ";")))
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
#check teams without country
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

# read affiliation map
tmp <- read.csv(file.path(res_folder, "_governing_bodies.csv"))
map <- tmp[, "GoverningBody"]
names(map) <- tmp[, "Country"]
# apply to missing values
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

  # handle military teams
  idx2 <- idx[teams[idx, "type"] == "Military/police team"]
  if (length(idx2) > 0) {
    print(teams[idx2, c("fullName", "affiliations")])
    teams[idx2, "affiliations"] <- "None"
  }

  # display non-affiliated teams
  idx2 <- idx[is.na(teams[idx, "affiliations"])]
  print(teams[idx2, c("fullName", "affiliations")])
}

# display non-affiliated teams overall
idx <- which(is.na(teams[, "affiliations"]))
print(teams[idx, c("fullName", "countries", "affiliations")])

# display existing affiliations (after update)
table(trimws(unlist(strsplit(teams[, "affiliations"], ";"))), useNA = "always")
print(sort(unique(trimws(unlist(strsplit(teams[, "affiliations"], ";"))))))
#
sort(unique(sapply(1:nrow(teams), function(i) paste0(teams[i, "countries"], "--", teams[i, "affiliations"]))))


# normalize competition names
compets <- trimws(unlist(strsplit(teams[, "competitions"], ";")))
print(sort(unique(compets)))
sort(table(compets, useNA = "always"))




########################################################################
# numeric: check formating
# "homeVenueCapacities"

caps <- trimws(unlist(strsplit(teams[, "homeVenueCapacities"], ";")))
sort(table(caps, useNA = "always"))
#which(grepl("Galway", teams[, "homeVenueCapacities"], ignore.case = TRUE))




########################################################################
# place names: nothing to do
# "homeVenueNames" "locations"

print(sort(unique(trimws(unlist(strsplit(teams[, "homeVenueNames"], ";"))))))
#
print(sort(unique(trimws(unlist(strsplit(teams[, "locations"], ";"))))))




########################################################################
# dates: check inception < termination
# "inceptionDate" "terminationDate"
teams[, "inceptionDate"] <- as.Date(teams[, "inceptionDate"])
teams[, "terminationDate"] <- as.Date(teams[, "terminationDate"])

tlog(2, "Missing inception dates: ", length(which(is.na(teams[, "inceptionDate"]))), "/", nrow(teams))

# test inception / termination dates
head(sort(unique(teams[, "inceptionDate"])))
tail(sort(unique(teams[, "inceptionDate"])))
head(sort(unique(teams[, "terminationDate"])))
tail(sort(unique(teams[, "terminationDate"])))
idx <- which(teams[, "inceptionDate"] > teams[, "terminationDate"])
if (length(idx) > 0)
  print(teams[idx, ])




########################################################################
# urls: check unicity
# "wikipediaEn" "wikipediaFr" "wikipediaIt" "wikipediaEs" "wikipediaJa"

id_field <- "wikipediaEn"
tlog(2, "Processing ", id_field)
tt <- table(teams[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(teams[teams[, id_field] %in% duplicates, ])

id_field <- "wikipediaFr"
tlog(2, "Processing ", id_field)
tt <- table(teams[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(teams[teams[, id_field] %in% duplicates, ])

id_field <- "wikipediaIt"
tlog(2, "Processing ", id_field)
tt <- table(teams[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(teams[teams[, id_field] %in% duplicates, ])

id_field <- "wikipediaEs"
tlog(2, "Processing ", id_field)
tt <- table(teams[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(teams[teams[, id_field] %in% duplicates, ])

id_field <- "wikipediaJa"
tlog(2, "Processing ", id_field)
tt <- table(teams[, id_field])
duplicates <- names(tt[tt > 1])
if (length(duplicates) > 0)
  print(teams[teams[, id_field] %in% duplicates, ])




########################################################################
# comments: nothing to do
# "comments"




########################################################################
# adjust column names
colnames(teams)[colnames(teams) == "allRugbyIds"]  <- "allRugbyId"
colnames(teams)[colnames(teams) == "googleKnowlIds"]  <- "googleKnowlId"





########################################################################
# summarize the table properties

# basic stats
summary(teams)

# a bit more advanced
skim(teams)

# full report
dfSummary(teams)
create_report(teams)




########################################################################
# record players table
tab_file <- file.path(data_folder, "teams_09.csv")
write.csv(teams, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# stop logging
end.rec.log()
