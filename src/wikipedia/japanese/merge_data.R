########################################################################
# Loads the clean Japanese Wikipedia tables and merge them into our tables.
#
# 02/2025 Vincent Labatut
########################################################################
library("stringi")
library("stringr")
library("dplyr")
library("httr")
library("jsonlite")

source("src/common/logging.R")
source("src/common/norm_teams.R")




########################################################################
# load WP JA tables
tlog("Loading Wikipedia JA tables")
folder <- file.path("data", "wikipedia", "japanese")

players <- read.csv(file.path(folder, "players.csv"))
tlog(2, "Raw number of players: ", nrow(players))
players <- players %>% mutate(across(where(is.character), ~ na_if(., "")))

careers <- read.csv(file.path(folder, "careers.csv"))
tlog(2, "Raw number of career steps: ", nrow(careers))
careers <- careers %>% mutate(across(where(is.character), ~ na_if(., "")))





########################################################################
# TODO