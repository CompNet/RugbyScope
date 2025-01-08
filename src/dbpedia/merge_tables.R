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
# query WD to check whether they are actual rugby union players ?

hits <- which(!is.na(teams_dbp[, "wikidataId"]))
cat("DBP teams with a WD Id: ", length(hits), "/", nrow(teams_dbp), "\n", sep = "")
idx <- match(teams_dbp[hits, "wikidataId"], teams_wd[, "clubId"])
cat("DBP teams found in the WD table: ", length(which(!is.na(idx))), "/", length(hits), "\n", sep = "")
print(teams_dbp[hits[which(is.na(idx))], "wikidataId"])

hits <- which(!is.na(players_dbp[, "wikidataId"]))
cat("DBP players with a WD Id: ", length(hits), "/", nrow(players_dbp), "\n", sep = "")
idx <- match(players_dbp[hits, "wikidataId"], players_wd[, "playerId"])
cat("DBP players found in the WD table: ", length(which(!is.na(idx))), "/", length(hits), "\n", sep = "")
