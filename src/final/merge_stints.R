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
start.rec.log("MergingRedundantStints")




########################################################################
# paths
data_folder <- file.path("data")




########################################################################
# load previously merged tables
tlog("Loading cleaned tables")

teams <- read.csv(file.path(data_folder, "teams_01.csv"))
tlog(2, "Number of teams: ", nrow(teams))

players <- read.csv(file.path(data_folder, "players_01.csv"))
tlog(2, "Number of players: ", nrow(players))

stints <- read.csv(file.path(data_folder, "stints_01.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# merge stints with identical dates
tlog("Merging stints with identical dates")

# loop over players
tlog(2, "Looping over players")
merged_stints <- 0
rem_marked <- c()
for (p in 1:nrow(players)) {
  player_id <- players[p, "wikidataId"]
#  tlog(4, "Processing player ", player_id, " (", p, "/", nrow(players), ")")

  # retrieve the player's stints
  idx <- which(stints[, "playerId"] == player_id)
  player_stints <- stints[idx, ]
  player_teams <- sort(unique(player_stints[, "teamRsId"]))

  # loop over the palyer's teams
  for (t in player_teams) {
#    tlog(6, "Processing team ", t, " (", teams[teams[, "rugbyscopeId"] == t, "fullName"], ")")
    idx2 <- which(player_stints[, "teamRsId"] == t)
    team_stints <- player_stints[idx2, ]

    # look for identical stints
    if (nrow(team_stints) > 1) {
      for (s1 in 1:(nrow(team_stints) - 1)) {
        for (s2 in (s1 + 1):nrow(team_stints)) {
          start1 <- team_stints[s1, "startYear"]
          end1 <- team_stints[s1, "endYear"]
          start2 <- team_stints[s2, "startYear"]
          end2 <- team_stints[s2, "endYear"]
          
          if (((is.na(start1) && is.na(start2)) || (!is.na(start1) && !is.na(start2) && start1 == start2))
              && ((is.na(end1) && is.na(end2)) || (!is.na(end1) && !is.na(end2) && end1 == end2))) {
            tlog(2, "Identical stints detected:")
            print(team_stints[c(s1, s2), ])
            
            # merge stats in the first stint
            # matches played
            mp1 <- team_stints[s1, "matchesPlayed"]
            mp2 <- team_stints[s2, "matchesPlayed"]
            if (is.na(mp1))
              team_stints[s1, "matchesPlayed"] <- mp2
            else
              team_stints[s1, "matchesPlayed"] <- max(mp1, mp2, na.rm = TRUE)
            # points scored
            ps1 <- team_stints[s1, "pointsScored"]
            ps2 <- team_stints[s2, "pointsScored"]
            if (is.na(mp1))
              team_stints[s1, "pointsScored"] <- ps2
            else
              team_stints[s1, "pointsScored"] <- max(ps1, ps2, na.rm = TRUE)
            # data source
            ds1 <- trimws(unlist(strsplit(team_stints[s1, "dataSource"], ";")))
            ds2 <- trimws(unlist(strsplit(team_stints[s2, "dataSource"], ";")))
            team_stints[s1, "dataSource"] <- paste0(sort(unique(union(ds1, ds2))), collapse = "; ")
            
            # mark the second stint for removal (good enough, as there are no complicated cases)
            rem_marked <- c(rem_marked, idx[idx2[s2]])
            
            merged_stints <- merged_stints + 1
          }
        }
      }
    }
  }
}
tlog(2, "Number of stints before merging: ", nrow(stints))

# remove the marked rows
if (length(rem_marked) > 0)
  stints <- stints[-rem_marked, ]
# display result
tlog(2, "Number of identical stints merged: ", merged_stints)
tlog(2, "Number of stints after merging: ", nrow(stints))




########################################################################
# merge stints with incomplete but matching dates
tlog("Merging stints with incomplete but matching dates")


# s1==NA
#   e1==NA
#     s2==NA
#       e2==NA (4)
#         TRUE
#       e2!=NA (3)
#         TRUE
#     s2!=NA
#       e2==NA (3)
#         TRUE
#       e2!=NA (2)
#         TRUE
#   e1!=NA
#     s2==NA
#       e2==NA (3)
#         TRUE
#       e2!=NA (2)
#         e1==e2 ?
#     s2!=NA
#       e2==NA (2)
#         e1 >= s2 ?
#       e2!=NA (1)
#         e1 == e2 ?
# s1!=NA
#   e1==NA
#     s2==NA
#       e2==NA (3)
#         TRUE
#       e2!=NA (2)
#         s1 <= e2
#     s2!=NA
#       e2==NA (2)
#         s1==s2 ?
#       e2!=NA (1)
#         s1 == s2 ?
#   e1!=NA
#     s2==NA
#       e2==NA (2)
#         TRUE
#       e2!=NA (1)
#         e1 == e2 ?
#     s2!=NA
#       e2==NA (1)
#         s1 == s2 ?
#       e2!=NA (0)
#        s1 == s2 && e1 == e2 ?






########################################################################
# list players with conflicting stints
tlog("Identify players with conflicting stints")

# loop over players
tlog(2, "Looping over players")
player_conflicts <- c()
for (p in 1:nrow(players)) {
  player_id <- players[p, "wikidataId"]
  tlog(4, "Processing player ", player_id, " (", p, "/", nrow(players), ")")
  found_conflict <- FALSE

  # retrieve the player's stints
  idx <- which(stints[, "playerId"] == player_id)
  player_stints <- stints[idx, ]
  player_teams <- sort(unique(player_stints[, "teamRsId"]))

  # loop over teams
  for (t in player_teams) {
    tlog(6, "Processing team ", t, " (", teams[teams[, "rugbyscopeId"] == t, "fullName"], ")")
    idx2 <- which(player_stints[, "teamRsId"] == t)
    team_stints <- player_stints[idx2, ]

    # look for conflicting stints
    if (nrow(team_stints) > 1) {
      for (s1 in 1:(nrow(team_stints) - 1)) {
        #tlog(8, "s1: ", s1)
        for (s2 in (s1 + 1):nrow(team_stints)) {
          #tlog(10, "s2: ", s2)
          start1 <- team_stints[s1, "startYear"]
          end1 <- team_stints[s1, "endYear"]
          start2 <- team_stints[s2, "startYear"]
          end2 <- team_stints[s2, "endYear"]
          if (is.na(start1)) {
            if (is.na(end1)) {
              if (is.na(start2)) {
                if (is.na(end2)) {
                  conf <- FALSE
                } else {
                  conf <- FALSE
                }
              } else {
                if (is.na(end2)) {
                  conf <- FALSE
                } else {
                  conf <- FALSE
                }
              }
            } else {
              if (is.na(start2)) {
                if (is.na(end2)) {
                  conf <- FALSE
                } else {
                  conf <- end1 == end2
                }
              } else {
                if (is.na(end2)) {
                  conf <- FALSE
                } else {
                  conf <- end1 >= start2 && end1 <= end2
                }
              }
            }
          } else {
            if (is.na(end1)) {
              if (is.na(start2)) {
                if (is.na(end2)) {
                  conf <- FALSE
                } else {
                  conf <- FALSE
                }
              } else {
                if (is.na(end2)) {
                  conf <- start1 == start2
                } else {
                  conf <- start1 >= start2 && start1 <= end2
                }
              }
            } else {
              if (is.na(start2)) {
                if (is.na(end2)) {
                  conf <- FALSE
                } else {
                  conf <- end2 >= start1 && end2 <= end1
                }
              } else {
                if (is.na(end2)) {
                  conf <- start2 >= start1 && start2 <= end1
                } else {
                  conf <- start1 >= start2 && start1 <= end2 || start2 >= start1 && start2 <= end1
                }
              }
            }
          }

          if (conf) {
            tlog(2, "Conflict detected:")
            print(team_stints[c(s1, s2), ])
            found_conflict <- TRUE
          }
        }
      }
    }
  }

  if (found_conflict) {
    player_conflicts <- c(player_conflicts, player_id)
  }
}
print(player_conflicts)

# same stats but different dates
# > keep majority dates?
# > also take into account other date-conflicting stints
# "Q7571","Patricio Albacete","Q646070",101,"Argentina national rugby union team",2003,2013,57,5,"WD; frWP; itWP; esWP"
# "Q7571","Patricio Albacete","Q646070",101,"Argentina national rugby union team",2003,2014,57,5,"enWP"

# different stats and different dates
# > keep majority stints?
# > if no majority, use largest stats?
# > use conflicting stints to decide?
# "Q26037","Quade Cooper","Q1368281",253,"Queensland Reds",2006,2015,109,768,"enWP"
# "Q26037","Quade Cooper","Q1368281",253,"Queensland Reds",2006,2017,119,846,"jaWP"

# s1==NA
#   e1==NA
#     s2==NA
#       e2==NA (4)
#         FALSE
#       e2!=NA (3)
#         FALSE
#     s2!=NA
#       e2==NA (3)
#         FALSE
#       e2!=NA (2)
#         FALSE
#   e1!=NA
#     s2==NA
#       e2==NA (3)
#         FALSE
#       e2!=NA (2)
#         e1==e2 ?
#     s2!=NA
#       e2==NA (2)
#         FALSE
#       e2!=NA (1)
#         e1 in [s2, e2] ?
# s1!=NA
#   e1==NA
#     s2==NA
#       e2==NA (3)
#         FALSE
#       e2!=NA (2)
#         FALSE
#     s2!=NA
#       e2==NA (2)
#         s1==s2 ?
#       e2!=NA (1)
#         s1 in [s2, e2] ?
#   e1!=NA
#     s2==NA
#       e2==NA (2)
#         FALSE
#       e2!=NA (1)
#         e2 in [s1, e1] ?
#     s2!=NA
#       e2==NA (1)
#         s2 in [s1, e1] ?
#       e2!=NA (0)
#        s1 in [s2, e2] or s2 in [s1, e1] ?


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
# verifications
# - why not including the stint type (junior, senior, etc.) in the table?
# - check start date <= end date in stints

# merge stints
# - if >0 pts but 0 matches, then matches should be NA

# stats
# - compare evolution of number of player/team/stint *by data source*
