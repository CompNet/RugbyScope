########################################################################
# Compare the teams retrieved from Wikidata/DBpedia to those in the
# manually curated lists. This script computes various stats, and was
# used when elaborating the reference team lists, especially to detect
# dupplicates and insert WD ids in the reference tables.
# Finallt, this script merges the reference list of teams into the 
# Wikidata/DBpedia team table.
#
# Vincent Labatut
# 01/2025
#
# setwd("C:/Users/Vincent/eclipse/workspaces/Test/RugbyScope/RugbyScope")
########################################################################
# Number of clubs by country according to WP
# https://en.wikipedia.org/wiki/List_of_rugby_union_playing_countries
#
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
#
# Note: this probably includes a bunch of clubs out of the standard pyramid (e.g. corporate clubs in France)
########################################################################
library("stringi")
library("dplyr")

source("src/common/logging.R")
source("src/common/norm_teams.R")




########################################################################
# paths
dpb_table_folder <- file.path("data", "dbpedia", "tables")
wd_table_folder <- file.path("data", "wikidata", "tables")
ref_folder <- file.path("data", "references")




########################################################################
# load WD team table
wd_teams <- read.csv(file.path(wd_table_folder, "all_teams_descr.csv"))
cat("Raw number of teams in WD table:", nrow(wd_teams), "\n")

# load merged data table
fusion_file <- file.path(dpb_table_folder, "fusion_teams_wd-dbp.csv")
teams <- read.csv(fusion_file)
cat("Raw number of teams in merged table:", nrow(teams), "\n")




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
# compare WD table to reference: used once, to complement reference table with WD ids

# for (country in countries) {
#   cat("Processing ", country, "\n", sep = "")

#   # get info
#   ref_names <- normalize_names(country_refs[[country]][, "name"], level = 1)
#   tiers <- country_refs[[country]][, "tier"]
#   theor_hits <- length(which(!is.na(country_refs[[country]][, "wikidataId"])))
#   wd_names <- normalize_names(wd_teams[, "teamLabel"], level = 1)

#   # match names
#   idx <- match(ref_names, wd_names)
#   identified <- !is.na(idx)
#   hits <- length(which(identified))

#   # display stats
#   cat("Actual hits: ", hits, "/", length(ref_names), "\n", sep = "")
#   cat("Theoretical hits: ", theor_hits, "/", length(ref_names), "\n", sep = "")
#   print(table(tiers, identified))
#   # print(ref_names[!identified])

#   # record detailed list
#   tab <- cbind(country_refs[[country]], wd_teams[idx, c("teamId", "teamLabel")])
#   tab_file <- file.path(wd_table_folder, paste0(country, "_teams_matches.csv"))
#   write.csv(tab, tab_file, row.names = FALSE)

#   # display misses
#   missed <- is.na(idx)
#   print(cbind(country_refs[[country]][missed, "name"], country_refs[[country]][missed, "wikidataId"]))
# }




########################################################################
# match WD teams into reference using names
# goal: finding missing WD ids in reference table

# get ref names
# ref_names <- country_refs[[country]][, "name"]
ref_names <- all_teams[, "name"]

# get main and alt WD names
wd_names1 <- teams[, "fullName"]
wd_names2 <- teams[, "altNames"]
# merge WD names in a single list
wd_names <- sapply(1:length(wd_names1), function(i) {
  if (is.na(wd_names2[i]))
    wd_names1[i]
  else
    paste0(wd_names1[i], "; ", wd_names2[i])
})

# match WD names into reference names
ref_idx <- which(is.na(all_teams[, "wikidataId"]))
wd_idx <- which(is.na(match(teams[, "wikidataId"], all_teams[-ref_idx, "wikidataId"])))
result <- match_names(src_names = wd_names[wd_idx], tgt_names = ref_names[ref_idx])

# remove known homonyms, according to reference
for (i in 1:length(result)) {
  # get candidate matches
  if (is.list(result))
    cands <- result[[i]]
  else
    cands <- result[i]

  # check if declared homonyms
  if (!all(is.na(cands))) {
    cands2 <- c()
    for (j in 1:length(cands)) {
      homonyms <- all_teams[ref_idx[cands[j]], "homonyms"]
      homonyms <- unlist(strsplit(homonyms, "; "))
      if (!(teams[wd_idx[i], "wikidataId"] %in% homonyms))
        cands2 <- c(cands2, cands[j])
    }
    if (length(cands2) == 0)
      cands2 <- NA

    # update candidates
    if (is.list(result))
      result[[i]] <- cands2
    else
      result[i] <- cands2
  }
}

# display WD teams matching several reference teams
tlog("WD teams matching several reference teams:")
ll <- sapply(result, length)
idx <- which(ll > 1)
if (length(idx) > 0) {
  for (i in idx) {
    print(teams[wd_idx[i], ])
    print(all_teams[ref_idx[result[[i]]], ])
    print("----------------")
  }
}

# display WD teams matching reference team by name, but WD id missing in reference
tlog("WD teams matching reference team by name, but WD id missing in reference:")
result <- sapply(result, function(x) x[1])
idx <- which(!is.na(result))
print(cbind(teams[wd_idx[idx], c("wikidataId", "fullName")], all_teams[ref_idx[result[idx]], ]))




########################################################################
# add missing column to store club national tier
idx <- which(colnames(teams) == "competitions")
teams <- cbind(teams[, 1:idx], rep(NA, nrow(teams)), teams[, (idx + 1):ncol(teams)])
colnames(teams)[idx + 1] <- "tier"

# complement merged list with reference info: country, competition, tier
map <- c()
map["countries"] <- "country"
map["competitions"] <- "division"
map["tier"] <- "tier"
# loop over teams to copy DBP data
idx <- match(teams[, "wikidataId"], all_teams[, "wikidataId"])
for (c in 1:length(idx)) {
  tlog(4, "Processing team ", c, "/", length(idx))
  # check that there is a match between the tables
  if (!is.na(idx[c])) {
    for (col in names(map)) {
      val0 <- teams[c, col]
      val1 <- all_teams[idx[c], map[col]]
      vals <- val0
      if (is.na(val0))
        vals <- val1
      else if (!is.na(val1)) {
        vals <- strsplit(val0, "; ")[[1]]
        vals <- union(vals, val1)
        vals <- paste(vals, collapse = "; ")
      }
      teams[c, col] <- vals
    }
  }
}

# clean competition names
map <- c()
map["All-Ireland League; All-Ireland League - Division 1A"] <- "All-Ireland League - Division 1A"
map["All-Ireland League; All-Ireland League - Division 1B"] <- "All-Ireland League - Division 1B"
map["All-Ireland League; All-Ireland League - Division 2C"] <- "All-Ireland League - Division 2C"
map["Championnat Fédéral Nationale; Nationale 1"] <- "Nationale 1"
map["Cuyo Rugby Union; Unión de Rugby de Cuyo"] <- "Unión de Rugby de Cuyo"
map["Fédérale 2; Fédérale 1 - Poule 1"] <- "Fédérale 1 - Poule 1"
map["Fédérale 2; Fédérale 2 - Poule 5"] <- "Fédérale 2 - Poule 5"
map["French rugby union regional 1 championship; Rugby union regional Auvergne-Rhône-Alpes league; Régionale 2 - Ligue Auvergne-Rhône-Alpes"] <- "French rugby union regional 1 championship; Régionale 2 - Ligue Auvergne-Rhône-Alpes"
map["Serie C; Serie C Emilia Romagna"] <- "Serie C Emilia Romagna"
map["Serie C; Serie C Lazio"] <- "Serie C Lazio"
map["Serie C; Serie C Veneto"] <- "Serie C Veneto"
map["Top East League; Top East League Group B"] <- "Top East League Group B"
map["Top West; Top West League Group A"] <- "Top West League Group A"
map["Top West; Top West League Group B"] <- "Top West League Group B"
map["URBA Top 12"] <- "Top 12 de la URBA"
map["URBA Top 12; Top 12 de la URBA"] <- "Top 12 de la URBA"
for (m in 1:length(map)) {
  mm <- names(map)[m]
  idx <- which(teams[, "competitions"] == mm)
  if (length(idx) > 0)
    teams[idx, "competitions"] <- map[mm]
}
# idx <- which(grepl("; ", teams[, "competitions"], fixed = TRUE))
# print(table(teams[-idx, "competitions"]))
# print(table(teams[idx, "competitions"]))

# clean country names
map <- c()
map["مونتينيجرو"] <- "Montenegro"
map["United Kingdom; England"] <- "England"
map["United Kingdom; Ireland"] <- "Ireland"
map["United Kingdom; Scotland"] <- "Scotland"
map["United Kingdom; Wales"] <- "Wales"
for (m in 1:length(map)) {
  mm <- names(map)[m]
  idx <- which(teams[, "countries"] == mm)
  if (length(idx) > 0)
    teams[idx, "countries"] <- map[mm]
}
# idx <- which(grepl("; ", teams[, "countries"], fixed = TRUE))
# print(table(teams[-idx, "countries"]))
# print(table(teams[idx, "countries"]))




########################################################################
# which WD teams do not appear in the reference

# identify these teams
idx <- which(is.na(match(teams[, "wikidataId"], all_teams[, "wikidataId"])))
# record as CSV to check later
write.csv(teams[idx, ], file.path(wd_table_folder, "teams_missing_in_reference.csv"))




########################################################################
# add missing reference teams into the merged team table

# check: teams in the ref list, with a WD id, but not in the WD/DBP table (?)
idx0 <- which(!is.na(all_teams[, "wikidataId"]))
idx <- which(is.na(match(all_teams[idx0, "wikidataId"], teams[, "wikidataId"])))
if (length(idx) > 0) {
  tlog("Teams possessing a WD id in the reference list, but not found in the merged table")
  print(all_teams[idx0[idx], ])
}

# teams from the reference table without a WD id
idx <- which(is.na(all_teams[, "wikidataId"]))
tlog("Number of teams in the reference list not matched in the merged table: ", length(idx), "/", nrow(all_teams))

# add them into the merged table
addendum <- all_teams[idx, -which(colnames(all_teams) == "homonyms")]
cn <- c("fullName", "tier", "competitions", "countries", "wikidataId")
missing_cols <- setdiff(colnames(teams), cn)
addendum <- cbind(addendum, matrix(NA, nrow = nrow(addendum), ncol = length(missing_cols)))
colnames(addendum) <- c(cn, missing_cols)
compl_teams <- rbind(teams, addendum[, colnames(teams)])

# insert new id (internal) to account for missing WD ids
compl_teams <- cbind(1:nrow(compl_teams), compl_teams)
colnames(compl_teams)[1] <- "rubyscopeId"

# record as a new CSV file
tab.file <- file.path(wd_table_folder, "fusion_teams_wd-dbp-ref.csv")
tlog(2, "Recording as a CSV file: \"", tab.file, "\"")
write.csv(compl_teams, tab.file, row.names = FALSE)
