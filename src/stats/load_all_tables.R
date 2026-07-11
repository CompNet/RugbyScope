########################################################################
# Simple script that mutualizes the loading of all data tables, used
# later to compute and plot various statistics.
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/stats/load_all_tables.R")
########################################################################




########################################################################
# load tables
tlog("Loading data tables")

data_folder <- file.path("data")

players <- read.csv(file.path(data_folder, "players.csv"))
tlog(2, "Number of players: ", nrow(players))

teams <- read.csv(file.path(data_folder, "teams.csv"))
tlog(2, "Number of teams: ", nrow(teams))

stints <- read.csv(file.path(data_folder, "stints.csv"))
tlog(2, "Number of stints: ", nrow(stints))
