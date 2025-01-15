# Script designed to merge the tables retrieved from DBpedia
# into those retrieved from Wikidata. We give priority to the
# WD information, which appears to be much more reliable.
#
# Vincent Labatut
# 12/2024
########################################################################




########################################################################
# paths
dpb_table_folder <- file.path("data", "dbpedia", "tables")
wd_table_folder <- file.path("data", "wikidata", "tables")




########################################################################
# load DBpedia teals
teams_dbp <- read.csv(file.path(dpb_table_folder, "all_teams_descr.csv"))
cat("Raw number of DPB teams:", nrow(teams_dbp), "\n")

# load DBpedia players
players_dbp <- read.csv(file.path(dpb_table_folder, "all_players_descr.csv"))
cat("Raw number of DPB players:", nrow(players_dbp), "\n")

# normalize rugby positions
source("src/dbpedia/clean_tables.R")
all_positions <- get_clean_positions(players_dbp)
players_dbp[, "positions"] <- all_positions




########################################################################
# load Wikidata teams
teams_wd <- read.csv(file.path(wd_table_folder, "all_teams_descr.csv"))
cat("Raw number of WD teams:", nrow(teams_wd), "\n")

# load Wikidata players
players_wd <- read.csv(file.path(wd_table_folder, "all_players_descr.csv"))
cat("Raw number of WD players:", nrow(players_wd), "\n")

# normalize rugby positions
source("src/wikidata/clean_tables.R")
all_positions <- get_clean_positions(players_wd)
players_wd[, "positionLabels"] <- all_positions

# normalize countries
all_countries <- get_clean_countries(players_wd)
players_wd[, ""] <- all_countries
# TODO : change function to separate normalization from merging country/sport in WD




########################################################################
# identify teams in the DBP table without a WD id
hits <- which(!is.na(teams_dbp[, "wikidataId"]))
cat("DBP teams with a WD Id: ", length(hits), "/", nrow(teams_dbp), "\n", sep = "")
idx <- which(is.na(teams_dbp[, "wikidataId"]))
# export as CSV for later use
write.csv(teams_dbp[idx, ], file.path(dpb_table_folder, "comparison_teams_noid.csv"), row.names = FALSE)
# >>> manual examination reveals that these entries are either not rugby union teams
#     or that these teams have duplicates in DBP

# identify teams with a WD id that are not in our WD table (so, in theory, not rugby teams)
idx <- match(teams_dbp[hits, "wikidataId"], teams_wd[, "clubId"])
cat("DBP teams found in the WD table: ", length(which(!is.na(idx))), "/", length(hits), "\n", sep = "")
# print(teams_dbp[hits[which(is.na(idx))], "wikidataId"])
# export as CSV for later use
write.csv(teams_dbp[hits[which(is.na(idx))], ], file.path(dpb_table_folder, "comparison_teams_nomatch.csv"), row.names = FALSE)
# >>> we get a bunch of entities that are not rugby union teams
#     some are rugby league, or rugby union management organization,
#     or even have absolutely nothing to do with rugby.
#     we manually checked all cases, and fixed the incorrect ones in WD




########################################################################
# identify players in the DBP table without a WD id
hits <- which(!is.na(players_dbp[, "wikidataId"]))
cat("DBP players with a WD Id: ", length(hits), "/", nrow(players_dbp), "\n", sep = "")
idx <- which(is.na(players_dbp[, "wikidataId"]))
# export as CSV for later use
write.csv(players_dbp[idx, ], file.path(dpb_table_folder, "comparison_players_noid.csv"), row.names = FALSE)
# >>> manual examination reveals that many of these entries are indeed rugby union
#     players, but that they are duplicates (due to several name variants) and/or
#     actually present in the WD table. A lot of entities are also not rugby players,
#     and even not persons.

idx <- match(players_dbp[hits, "wikidataId"], players_wd[, "playerId"])
cat("DBP players found in the WD table: ", length(which(!is.na(idx))), "/", length(hits), "\n", sep = "")
# print(players_dbp[hits[is.na(idx)], "wikidataId"])
# >>> lot of females, rugby league players, and rugby union players not tied to any club
# export as CSV for later use
write.csv(players_dbp[hits[which(is.na(idx))], ], file.path(dpb_table_folder, "comparison_players_nomatch.csv"), row.names = FALSE)
#     so, mainly false positives in DBP

# temporary tests
# which(players_wd[, "playerId"] == "Q24874273")
# cbind(players_dbp[hits[!is.na(idx)], "wikidataId"], players_wd[idx[!is.na(idx)], "playerId"])
# cbind(players_dbp[hits[is.na(idx)], "wikidataId"], players_wd[idx[is.na(idx)], "playerId"])




########################################################################
# merging the player tables: trust the Wikidata data first, then complete
# with DBpedia content when WD is empty.
players <- players_wd

# clean birth dates
idx <- which(!is.na(players[, "dobMax"]) & is.na(players[, "dobFormat"]))
if (length(idx) > 0)
  players[idx, "dobMax"] <- NA

# clean death dates
idx <- which(!is.na(players[, "dodMax"]) & is.na(players[, "dodFormat"]))
if (length(idx) > 0)
  players[idx, "dodMax"] <- NA

# possibly add a new column for the DBP id
if (!("altNames" %in% colnames(players))) {
  players <- cbind(players[, 1:4], rep(NA, nrow(players)), players[, 5:ncol(players)])
  colnames(players)[5] <- "altNames"
}

# same for the alternative names
if (!("dbpediaId" %in% colnames(players))) {
  players <- cbind(players, rep(NA, nrow(players)))
  colnames(players)[ncol(players)] <- "dbpediaId"
}

# match WD players in DBP
idx <- match(players[, "playerId"], players_dbp[, "wikidataId"])

# specific merge for the names
# TODO only keep alt names that are not already matching fullname

# specific merge for weight/height
# TODO if several in WD, use the closest to DBP ?

# specific merge for countries
# TODO break multiple names in WD and DBP, only add the ones not already present

# TODO check that date format is the same in DBP, otherwise convert



# map SBP columns to WD columns
map <- c()
map["dbpediaId"] <- "player"
map["playerLabel"] <- "fullNames"
map["dobMax"] <- "birthDates"
map["pobLabels"] <- "birthPlaces"
map["dodMax"] <- "deathDates"
map["podLabels"] <- "deathPlaces"
map["masses"] <- "weights"
map["heights"] <- "heights"
map["positionLabels"] <- "positions"
map["playerId"] <- "wikidataId"

# loop over players to copy DBP data
for (p in 1:length(idx)) {
  # check that there is a match between the tables
  if (!is.na(idx[p])) {
    # get the empty cells in WD
    cols_wd <- colnames(players)[is.na(players[p, ])]
    cols_wd <- intersect(cols_wd, names(map))
    # copy DBP data into the WD table
    if (length(cols_wd) > 0) {
      cols_dbp <- map[cols_wd]
      players[p, cols_wd] <- players_dbp[idx[p], cols_dbp]
    }
  }
}

# remove supefluous columns
rem_cols <- c("dobFormat", "dodFormat")
idx <- which(colnames(players) %in% rem_cols)
players <- players[, -idx]

# rename certain columns
map <- c()
map["wikidataId"] <- "playerId"
map["fullName"] <- "playerLabel"
map["firstName"] <- "firstnameLabels"
map["lastName"] <- "lastnameLabels"
map["sex"] <- "sexLabel"
map["birthDate"] <- "dobMax"
map["birthPlaces"] <- "pobLabels"
map["deathDate"] <- "dodMax"
map["deathPlaces"] <- "podLabels"
map["citizenships"] <- "citizenshipLabels"
map["sportCountries"] <- "sportCountryLabels"
map["positions"] <- "positionLabels"
map["weights"] <- "masses"
idx <- match(map, colnames(players))
colnames(players)[idx] <- names(map)

# sort by WD id value
ids <- players[, "wikidataId"]
ids <- as.integer(substr(ids, start = 2, stop = nchar(ids)))
players <- players[order(ids), ]

# record as a new CSV file
write.csv(players, file.path(dpb_table_folder, "fusion_players.csv"), row.names = FALSE)
