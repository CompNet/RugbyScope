########################################################################
# Compare the teams retrieved from Wikidata to those in the manually
# curated lists. This script computes various stats, and was used
# when elaborating the reference team lists, especially to detect
# dupplicates and insert WD ids in the reference tables.
#
# Vincent Labatut
# 01/2025
########################################################################
library("stringi")
library("dplyr")

source("src/common/logging.R")
source("src/common/norm_teams.R")




########################################################################
# paths
table_folder <- file.path("data", "wikidata", "tables")
ref_folder <- file.path("data", "references")




########################################################################
# load data table
teams <- read.csv(file.path(table_folder, "all_teams_descr.csv"))
cat("Raw number of teams:", nrow(teams), "\n")




########################################################################
# load reference tables
all_teams <- NA
countries <- c("AR", "AU", "EN", "FJ", "FR", "IE", "IT", "JP", "NZ", "SC", "WA", "ZA")
country_refs <- list()
for (country in countries) {
  tab_file <- file.path(ref_folder, paste0(country, "_teams.csv"))

  # # debug
  # country_refs[[country]] <- read.csv(tab_file, sep = "\t")
  # write.csv(x = country_refs[[country]], file = tab_file, row.names = FALSE, fileEncoding = "UTF-8")

  country_refs[[country]] <- read.csv(tab_file)
  country_refs[[country]] <- country_refs[[country]] %>% mutate(across(where(is.character), ~ na_if(., "")))
  cat("Reading ", country, ": ", nrow(country_refs[[country]]), " teams\n", sep = "")

  if (all(is.na(all_teams)))
    all_teams <- country_refs[[country]]
  else
    all_teams <- rbind(all_teams, country_refs[[country]])
}

# check that the same WD id is not used several times in the reference table
tlog(0, "Checking duplicate WD ids in reference table:")
print(which(table(all_teams[, "wikidataId"]) > 1))




########################################################################
for (country in countries) {
  cat("Processing ", country, "\n", sep = "")

  # get info
  ref_names <- normalize_names(country_refs[[country]][, "name"], level = 1)
  tiers <- country_refs[[country]][, "tier"]
  theor_hits <- length(which(!is.na(country_refs[[country]][, "wikidataId"])))
  wd_names <- normalize_names(teams[, "clubLabel"], level = 1)

  # match names
  idx <- match(ref_names, wd_names)
  identified <- !is.na(idx)
  hits <- length(which(identified))


# TODO match alt names too
# use approximate matching to increase hits
# use already matched teams to restrict comparison to open cases
# use two steps: if not found after the first, remove generic rugby parts and re-check

# use ref table info to complete our tables: division, affiliation, country...

# check that the excel data do not contain several times the same WikidataID
# check whether clubs retrieved from WD are all present in the ref

# retrieve alt names from wikidata
# https://stackoverflow.com/questions/46850562/how-to-query-wikidata-for-also-known-as

  # display stats
  cat("Actual hits: ", hits, "/", length(ref_names), "\n", sep = "")
  cat("Theoretical hits: ", theor_hits, "/", length(ref_names), "\n", sep = "")
  print(table(tiers, identified))
  # print(ref_names[!identified])

  # record detailed list
  tab <- cbind(country_refs[[country]], teams[idx, c("clubId", "clubLabel")])
  tab_file <- file.path(table_folder, paste0(country, "_teams_matches.csv"))
  write.csv(tab, tab_file, row.names = FALSE)

  # display misses
  missed <- is.na(idx)
  print(cbind(country_refs[[country]][missed, "name"], country_refs[[country]][missed, "wikidataId"]))
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

dpb_table_folder <- file.path("data", "dbpedia", "tables")
tab_file <- file.path(dpb_table_folder, "fusion_teams.csv")
teams <- read.csv(tab_file)
wd_names1 <- teams[, "fullName"]
wd_names2 <- teams[, "altNames"]
# ref_names <- country_refs[[country]][, "name"]
ref_names <- all_teams[, "name"]

# merge wd names in a single list
wd_names <- sapply(1:length(wd_names1), function(i) {
  if (is.na(wd_names2[i]))
    wd_names1[i]
  else
    paste0(wd_names1[i], "; ", wd_names2[i])
})

# match WD into reference
result <- match_names(src_names = wd_names, tgt_names = ref_names)
ll <- sapply(result, length)
idx <- which(ll > 1)
for (i in idx) {
  print(teams[i, ])
  print(all_teams[result[[i]], ])
  print("----------------")
}
