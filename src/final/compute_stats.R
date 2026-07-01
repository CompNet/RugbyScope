########################################################################
# Compute basic stats and generate plots based on the merged and cleaned
# tables.
#
# 06/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/compute_stats.R")
########################################################################
library("stringi")
library("stringr")
library("dplyr")
library("httr")
library("jsonlite")
library("magrittr")
library("skimr")
library("DataExplorer")
library("summarytools")

source("src/common/logging.R")




########################################################################
# start logging
start.rec.log("ComputeStats")




########################################################################
# load tables
tlog("Loading data tables")

data_folder <- file.path("data", "fusion")

players <- read.csv(file.path(data_folder, "players_11.csv"))
tlog(2, "Number of players: ", nrow(players))

teams <- read.csv(file.path(data_folder, "teams_09.csv"))
tlog(2, "Number of teams: ", nrow(teams))

stints <- read.csv(file.path(data_folder, "stints_18_raw.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# check the age of players during yout stints
# TODO



########################################################################
# stop logging
end.rec.log()
