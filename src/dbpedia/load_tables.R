# Loads the two tables (players, teams) retrieved from DBpedia,
# and remove the information considered as useless.
#
# Vincent Labatut
# 01/2025
########################################################################
source("src/dbpedia/clean_tables.R")



########################################################################
# paths
table_folder <- file.path("data", "dbpedia", "tables")




########################################################################
# load data tables
teams_dpb <- read.csv(file.path(table_folder, "all_teams_descr.csv"))
cat("Raw number of teams:", nrow(teams_dpb), "\n")

players_dpb <- read.csv(file.path(table_folder, "all_players_descr.csv"))
cat("Raw number of players:", nrow(players_dpb), "\n")




########################################################################
# clean player data

# normalize rugby positions
all_positions <- get_clean_positions(players_dpb)
players_dpb[, "positions"] <- all_positions




########################################################################
# clean team data
