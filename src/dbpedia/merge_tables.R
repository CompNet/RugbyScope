# Script designed to merge the tables retrieved from DBpedia
# into those retrieved from Wikidata.
#
# Vincent Labatut
# 12/2024
########################################################################




########################################################################
# paths
table_folder <- file.path("data", "dbpedia", "tables")




########################################################################
# load data tables
teams <- read.csv(file.path(table_folder, "all_teams_descr.csv"))
cat("Raw number of teams:", nrow(teams), "\n")

players <- read.csv(file.path(table_folder, "all_players_descr.csv"))
cat("Raw number of players:", nrow(players), "\n")

careers <- read.csv(file.path(table_folder, "all_players_careers.csv"))
cat("Raw number of career steps:", nrow(careers), "\n")




########################################################################
# identify players in the DBP list but not the WD one
# query WD to check whether they are actual rugby union players ?

hits <- which(!is.na(teams[, "wikidataId"]))
cat("WD Ids: ", length(hits), "/", nrow(teams), "\n", sep = "")

hits <- which(!is.na(players[, "wikidataId"]))
cat("WD Ids: ", length(hits), "/", nrow(players), "\n", sep = "")
