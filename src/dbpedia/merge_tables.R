# Script designed to merge the tables retrieved from DBpedia
# into those retrieved from Wikidata.
#
# Vincent Labatut
# 12/2024
########################################################################




########################################################################
# paths
table_folder <- file.path("data", "dbpedia", "tables")
wd_table_folder <- file.path("data", "wikidata", "tables")




########################################################################
# load DBpedia tables
teams_dbp <- read.csv(file.path(table_folder, "all_teams_descr.csv"))
cat("Raw number of DPB teams:", nrow(teams_dbp), "\n")

players_dbp <- read.csv(file.path(table_folder, "all_players_descr.csv"))
cat("Raw number of DPB players:", nrow(players_dbp), "\n")




########################################################################
# load Wikidata tables
teams_wd <- read.csv(file.path(wd_table_folder, "all_teams_descr.csv"))
cat("Raw number of WD teams:", nrow(teams_wd), "\n")

players_wd <- read.csv(file.path(wd_table_folder, "all_players_descr.csv"))
cat("Raw number of WD players:", nrow(players_wd), "\n")




########################################################################
# identify players in the DBP list but not the WD one

hits <- which(!is.na(teams_dbp[, "wikidataId"]))
cat("DBP teams with a WD Id: ", length(hits), "/", nrow(teams_dbp), "\n", sep = "")
idx <- match(teams_dbp[hits, "wikidataId"], teams_wd[, "clubId"])
cat("DBP teams found in the WD table: ", length(which(!is.na(idx))), "/", length(hits), "\n", sep = "")
print(teams_dbp[hits[which(is.na(idx))], "wikidataId"])
# >>> we get a bunch of entities that have absolutely nothing to do with rugby
#     manually checked all cases, and fixed the incorrect ones in WD

hits <- which(!is.na(players_dbp[, "wikidataId"]))
cat("DBP players with a WD Id: ", length(hits), "/", nrow(players_dbp), "\n", sep = "")
idx <- match(players_dbp[hits, "wikidataId"], players_wd[, "playerId"])
cat("DBP players found in the WD table: ", length(which(!is.na(idx))), "/", length(hits), "\n", sep = "")
print(players_dbp[hits[is.na(idx)], "wikidataId"])
# >>> lot of females, rugby league players, and rugby union players not tied to any club
#     so, mainly false positives in DBP

# temporary tests
# which(players_wd[, "playerId"] == "Q24874273")
# cbind(players_dbp[hits[!is.na(idx)], "wikidataId"], players_wd[idx[!is.na(idx)], "playerId"])
# cbind(players_dbp[hits[is.na(idx)], "wikidataId"], players_wd[idx[is.na(idx)], "playerId"])




########################################################################
# merging the player tables: trust the Wikidata data first, then complete 
# with DBpedia content when empty.

# possibly add a new column for the DBP id
if (!("dbpediaId" %in% colnames(players_wd))) {
  players_wd <- cbind(players_wd, rep(NA, nrow(players_wd)))
  colnames(players_wd)[ncol(players_wd)] <- "dbpediaId"
}

# map SBP columns to WD columns
map <- c()
map["player"] <- "dbpediaId"
map["fullNames"] <- "playerLabel"
map["birthDates"] <- "dobFormat"
map["birthPlaces"] <- "pobLabels"
map["deathDates"] <- "dodFormat"
map["deathPlaces"] <- "podLabels"
map["weights"] <- "masses"
map["heights"] <- "heights"
map["positions"] <- "positionLabels"
map["wikidataId"] <- "playerId"

# match WD players in DBP
idx <- match(players_wd[hits, "playerId"], players_dbp[, "wikidataId"])

# loop over players
for (p in 1:length(idx)) {
  if (!is.na(idx[p])) {
    cols <- which(is.na(players_wd[p,]))
    if (length(cols) > 0) {

    }
  }
}
