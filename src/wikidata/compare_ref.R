# Compare the teams retrieved from Wikidata to those in the manually
# curated lists.
#
# Vincent Labatut
# 01/2025
########################################################################
library(stringi)




########################################################################
# paths
table_folder <- file.path("data", "wikidata", "tables")
ref_folder <- file.path("data", "references")




########################################################################
# Normalizes the specified club names by removing diacritics and case.
#
# names: original names.
#
# returns: list of normalized names.
########################################################################
normalize_names <- function(names) {
  # remove all diacritics
  result <- stri_trans_general(str = names, id = "Latin-ASCII")
  # switch to uppercase
  result <- toupper(result)

  return (result)
}





########################################################################
# load data table
teams <- read.csv(file.path(table_folder, "all_teams_descr.csv"))
cat("Raw number of teams:", nrow(teams), "\n")




########################################################################
# load reference tables
countries <- c("AR", "AU", "EN", "FJ", "FR", "IE", "IT", "JP", "NZ", "SC", "WA", "ZA")
country_refs <- list()
for (country in countries) {
  file <- file.path(ref_folder, paste0(country, "_teams.csv"))
  country_refs[[country]] <- read.csv(file)
  cat("Reading ", country, ": ", nrow(country_refs[[country]]), " teams\n", sep = "")
  # write.csv(x = country_refs[[country]], file = file, row.names = FALSE, fileEncoding = "UTF-8")
}




########################################################################
for (country in countries) {
  cat("Processing ", country, "\n", sep = "")

  # get info
  ref_names <- normalize_names(country_refs[[country]][, "Name"])
  tiers <- country_refs[[country]][, "Tier"]
  theor_hits <- length(which(country_refs[[country]][, "WD"] != "Missing"))
  wd_names <- normalize_names(teams[, "clubLabel"])

  # match names
  idx <- match(ref_names, wd_names)
  identified <- !is.na(idx)
  hits <- length(which(identified))

  # display stats
  cat("Actual hits: ", hits, "/", length(ref_names), "\n", sep = "")
  cat("Theoretical hits: ", theor_hits, "/", length(ref_names), "\n", sep = "")
  print(table(tiers, identified))
  # print(ref_names[!identified])
}




#########################################################
# Number of clubs by country according to WP
# https://en.wikipedia.org/wiki/List_of_rugby_union_playing_countries
#########################################################
# Argentina:     420
# Australia:     767
# England:      1809
# Fiji:          490
# France:       1798
# Ireland:       221
# Italy:         784
# Japan:        1522
# New Zealand:   600
# Scotland:      251
# South Africa: 1526
# Wales:         250
# Note: this probably includes a bunch of clubs out of the standard pyramid (e.g. corporate clubs in France)
#########################################################

