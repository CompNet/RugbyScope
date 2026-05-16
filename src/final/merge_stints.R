########################################################################
# The data contain many similar and redundant stints. This script tries
# to merge them efficiently.
#
# 05/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/merge_stints.R")
########################################################################
library("stringi")
library("stringr")
library("dplyr")
library("httr")
library("jsonlite")
library("magrittr")

source("src/common/logging.R")
source("src/common/norm_names.R")
source("src/common/norm_teams.R")




########################################################################
# start logging
start.rec.log("Cleaning")




########################################################################
# paths
data_folder <- file.path("data")




########################################################################
# load previously cleaned tables
tlog("Loading cleaned tables")

teams <- read.csv(file.path(data_folder, "teams_01.csv"))
# teams <- read.csv(file.path(data_folder, "teams_01.csv"))
tlog(2, "Number of teams: ", nrow(teams))

players <- read.csv(file.path(data_folder, "players_01.csv"))
tlog(2, "Number of players: ", nrow(players))

stints <- read.csv(file.path(data_folder, "stints_01.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# verifications in teams table





########################################################################
# stop logging
end.rec.log()

# TODO
# - why not including the stint type (junior, senior, etc.) in the table?
# - if >0 pts but 0 matches, then matches should be NA

# stats
# - compare evolution of number of player/team/stint *by data source*
