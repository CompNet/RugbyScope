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
source("src/final/fun_stints.R")




########################################################################
# start logging
start.rec.log("MergingRedundantStints")




########################################################################
# paths
data_folder <- file.path("data", "fusion")




########################################################################
# load previously merged tables
tlog("Loading cleaned tables")

teams <- read.csv(file.path(data_folder, "teams_08.csv"))
tlog(2, "Number of teams: ", nrow(teams))

players <- read.csv(file.path(data_folder, "players_08.csv"))
tlog(2, "Number of players: ", nrow(players))

stints <- read.csv(file.path(data_folder, "stints_12.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# quick stat fixes

# impossible to have played zero matches while having scored some points >> make it NA matches
idx <- which(!is.na(stints[, "matchesPlayed"]) & stints[, "matchesPlayed"]==0 & !is.na(stints[, "pointsScored"]) & stints[, "pointsScored"]!=0)
if (length(idx) > 0)
  stints[idx, "matchesPlayed"] <- NA
tlog("Fixed 'zero played matches but has scored points' issue in ", length(idx), " stints")

# if zero matches and zero points, then it probably means no stats >> make both NAs
idx <- which(!is.na(stints[, "matchesPlayed"]) & stints[, "matchesPlayed"]==0 & !is.na(stints[, "pointsScored"]) & stints[, "pointsScored"]==0)
if (length(idx) > 0) {
  stints[idx, "matchesPlayed"] <- NA
  stints[idx, "pointsScored"] <- NA
}
tlog("Fixed 'zero played matches / zero scored points' in ", length(idx), " stints")




########################################################################
# merge stints at the same team with identical dates
#source("src/final/merge_stints_identical.R")

# merge stints at the same team with nested dates
source("src/final/merge_stints_nested.R")




# ########################################################################
# # merge stints with incomplete but matching dates
# tlog("Merging stints with incomplete but matching dates")
# # NOTE: there is no such case remaining, only incompatible stints

# # loop over players
# tlog(2, "Looping over players")
# merged_stints <- 0
# rem_marked <- c()
# for (p in 1:nrow(players)) {
#   player_id <- players[p, "wikidataId"]
# #  tlog(4, "Processing player ", player_id, " (", p, "/", nrow(players), ")")

#   # retrieve the player's stints
#   idx <- which(stints[, "playerId"] == player_id)
#   player_stints <- stints[idx, ]
#   player_teams <- sort(unique(player_stints[, "teamRsId"]))

#   # loop over teams
#   for (t in player_teams) {
# #    tlog(6, "Processing team ", t, " (", teams[teams[, "rugbyscopeId"] == t, "fullName"], ")")
#     idx2 <- which(player_stints[, "teamRsId"] == t)
#     team_stints <- player_stints[idx2, ]

#     # look for compatible stints
#     if (nrow(team_stints) > 1) {
#       for (s1 in 1:(nrow(team_stints) - 1)) {
#         #tlog(8, "s1: ", s1)
#         for (s2 in (s1 + 1):nrow(team_stints)) {
#           #tlog(10, "s2: ", s2)
#           # check that the 2nd stint is not already marked for deletion
#           if (!(idx[idx2[s2]] %in% rem_marked)) {

#             start1 <- team_stints[s1, "startYear"]
#             end1 <- team_stints[s1, "endYear"]
#             start2 <- team_stints[s2, "startYear"]
#             end2 <- team_stints[s2, "endYear"]
#             if (is.na(start1)) {
#               if (is.na(end1)) {
#                 if (is.na(start2)) {
#                   if (is.na(end2)) {
#                     # s1==NA e1==NA s2==NA e2==NA
#                     conf <- TRUE
#                   } else {
#                     # s1==NA e1==NA s2==NA e2!=NA
#                     conf <- TRUE
#                   }
#                 } else {
#                   if (is.na(end2)) {
#                     # s1==NA e1==NA s2!=NA e2==NA
#                     conf <- TRUE
#                   } else {
#                     # s1==NA e1==NA s2!=NA e2!=NA
#                     conf <- TRUE
#                   }
#                 }
#               } else {
#                 if (is.na(start2)) {
#                   if (is.na(end2)) {
#                     # s1==NA e1!=NA s2==NA e2==NA
#                     conf <- TRUE
#                   } else {
#                     # s1==NA e1!=NA s2==NA e2!=NA
#                     conf <- end1 == end2
#                   }
#                 } else {
#                   if (is.na(end2)) {
#                     # s1==NA e1!=NA s2!=NA e2==NA
#                     conf <- end1 >= start2
#                   } else {
#                     # s1==NA e1!=NA s2!=NA e2!=NA
#                     conf <- end1 == end2
#                   }
#                 }
#               }
#             } else {
#               if (is.na(end1)) {
#                 if (is.na(start2)) {
#                   if (is.na(end2)) {
#                     # s1!=NA e1==NA s2==NA e2==NA
#                     conf <- TRUE
#                   } else {
#                     # s1!=NA e1==NA s2==NA e2!=NA
#                     conf <- start1 <= end2
#                   }
#                 } else {
#                   if (is.na(end2)) {
#                     # s1!=NA e1==NA s2!=NA e2==NA
#                     conf <- start1 == start2
#                   } else {
#                     # s1!=NA e1==NA s2!=NA e2!=NA
#                     conf <- start1 == start2
#                   }
#                 }
#               } else {
#                 if (is.na(start2)) {
#                   if (is.na(end2)) {
#                     # s1!=NA e1!=NA s2==NA e2==NA
#                     conf <- TRUE
#                   } else {
#                     # s1!=NA e1!=NA s2==NA e2!=NA
#                     conf <- end1 == end2
#                   }
#                 } else {
#                   if (is.na(end2)) {
#                     # s1!=NA e1!=NA s2!=NA e2==NA
#                     conf <- start1 == start2
#                   } else {
#                     # s1!=NA e1!=NA s2!=NA e2!=NA
#                     conf <- start1 == start2 && end1 == end2
#                   }
#                 }
#               }
#             }

#             if (conf) {
#               tlog(10, "Compatible stints detected:")
#               print(team_stints[c(s1, s2), ])

#               # merge dates in the first stint
#               # start year
#               sy1 <- team_stints[s1, "startYear"]
#               sy2 <- team_stints[s2, "startYear"]
#               if (is.na(sy1))
#                 stints[idx[idx2[s1]], "startYear"] <- sy2
#               # end year
#               ey1 <- team_stints[s1, "endYear"]
#               ey2 <- team_stints[s2, "endYear"]
#               if (is.na(ey1))
#                 stints[idx[idx2[s1]], "endYear"] <- ey2
              
#               # merge stats in the first stint
#               stints[idx[idx2[s1]], ] <- merge_stint_stats(team_stints[s1, ], team_stints[s2, ], mode = "max")
#               tlog(10, "Merged stint:")
#               print(stints[idx[idx2[s1]], ])

#               # mark the second stint for removal
#               rem_marked <- c(rem_marked, idx[idx2[s2]])
              
#               merged_stints <- merged_stints + 1
#             }
#           }
#         }
#       }
#     }
#   }
# }
# tlog(2, "Number of stints before merging: ", nrow(stints))

# # remove the marked rows
# rem_marked <- unique(rem_marked)
# if (length(rem_marked) > 0)
#   stints <- stints[-rem_marked, ]
# # display result
# tlog(2, "Number of compatible stints merged: ", merged_stints)
# tlog(2, "Number of stints after merging: ", nrow(stints))

# # s1==NA
# #   e1==NA
# #     s2==NA
# #       e2==NA (4)
# #         TRUE
# #       e2!=NA (3)
# #         TRUE
# #     s2!=NA
# #       e2==NA (3)
# #         TRUE
# #       e2!=NA (2)
# #         TRUE
# #   e1!=NA
# #     s2==NA
# #       e2==NA (3)
# #         TRUE
# #       e2!=NA (2)
# #         e1==e2 ?
# #     s2!=NA
# #       e2==NA (2)
# #         e1 >= s2 ?
# #       e2!=NA (1)
# #         e1 == e2 ?
# # s1!=NA
# #   e1==NA
# #     s2==NA
# #       e2==NA (3)
# #         TRUE
# #       e2!=NA (2)
# #         s1 <= e2
# #     s2!=NA
# #       e2==NA (2)
# #         s1 == s2 ?
# #       e2!=NA (1)
# #         s1 == s2 ?
# #   e1!=NA
# #     s2==NA
# #       e2==NA (2)
# #         TRUE
# #       e2!=NA (1)
# #         e1 == e2 ?
# #     s2!=NA
# #       e2==NA (1)
# #         s1 == s2 ?
# #       e2!=NA (0)
# #        s1 == s2 && e1 == e2 ?




########################################################################
# merge consecutive stints at the same team
tlog("Merging exactly consecutive stints at the same team")

# loop over players
tlog(2, "Looping over players")
merged_stints <- 0
conflict_stints <- 0
rem_marked <- c()
for (p in 1:nrow(players)) {
  player_id <- players[p, "wikidataId"]
#  tlog(4, "Processing player ", player_id, " (", p, "/", nrow(players), ")")

  # retrieve the player's stints
  idx <- which(stints[, "playerId"] == player_id)
  player_stints <- stints[idx, ]
  player_teams <- sort(unique(player_stints[, "teamRsId"]))

  # loop over the player's teams
  for (t in player_teams) {
#    tlog(6, "Processing team ", t, " (", teams[teams[, "rugbyscopeId"] == t, "fullName"], ")")
    idx2 <- which(player_stints[, "teamRsId"] == t)
    team_stints <- player_stints[idx2, ]

    # look for consecutive stints
    if (nrow(team_stints) > 1) {
      for (s1 in 1:(nrow(team_stints) - 1)) {
        if (!(idx[idx2[s1]] %in% rem_marked)) {
          start1 <- team_stints[s1, "startYear"]
          end1 <- team_stints[s1, "endYear"]

          if (!is.na(end1)) {
            iii <- (s1 + 1):nrow(team_stints)
            idx3 <- s1 + which(!is.na(team_stints[iii, "startYear"]) & team_stints[iii, "startYear"] == end1)
            # ignore rows already marked for deletion
            idx3 <- idx3[!(idx[idx2[idx3]] %in% rem_marked)]

            # single match
            if (length(idx3) == 1) {
              tlog(8, "One match detected:")
              print(team_stints[c(s1, idx3), ])

              # update end date in first stint
              stints[idx[idx2[s1]], "endYear"] <- team_stints[idx3, "endYear"]

              # add 2nd stint stats to 1st stint
              mp1 <- team_stints[s1, "matchesPlayed"]
              mp2 <- team_stints[idx3, "matchesPlayed"]
              ps1 <- team_stints[s1, "pointsScored"]
              ps2 <- team_stints[idx3, "pointsScored"]
              if (all(!is.na(c(mp1, ps1, mp2, ps2))) && mp1 == mp2 && ps1 == ps2) {
                # duplicate of the same stint: do nothing stat-wise
                conflict_stints <- conflict_stints + 1
              } else {
                if (!is.na(mp1)) {
                  if (!is.na(mp2))
                    stints[idx[idx2[s1]], "matchesPlayed"] <- mp1 + mp2
                } else
                    stints[idx[idx2[s1]], "matchesPlayed"] <- mp2
                if (!is.na(ps1)) {
                  if (!is.na(ps2))
                    stints[idx[idx2[s1]], "pointsScored"] <- ps1 + ps2
                } else
                    stints[idx[idx2[s1]], "pointsScored"] <- ps2
              }

              # display updated stint
              tlog(10, "Merged stint:")
              print(stints[idx[idx2[s1]], ])

              # mark the second stint for removal (good enough, as there are no complicated cases)
              rem_marked <- c(rem_marked, idx[idx2[idx3]])
              
              merged_stints <- merged_stints + 1

            # several matches
            } else if (length(idx3) > 1) {
              tlog(8, "Several matches detected:")
              print(team_stints[c(s1, idx3), ])
            }
          }
        }
      }
    }
  }
}
tlog(2, "Number of stints before merging: ", nrow(stints))  # only 22 pairs of stints are concerned

# remove the marked rows
if (length(rem_marked) > 0)
  stints <- stints[-rem_marked, ]
# display result
tlog(2, "Number of consecutive stints merged: ", merged_stints)
tlog(2, "Number of stints after merging: ", nrow(stints))
tlog(2, "Number of conflicts detected: ", conflict_stints)
stints0 <- stints; end.rec.log(); stop()




# TODO
# what about merging overlapping stints at the same club ?
# then the pb becomes: adjusting the bounds of conflicting stints
# or should we consider each WP version separately?




########################################################################
# merge stints with incomplete or only approximately matching dates
tlog("Merging stints with incomplete/approximately matching dates")
tolerance <- 1    # tolerance (in years) when comparing periods

# loop over players
tlog(2, "Looping over players")
merged_stints <- 0
rem_marked <- c()
for (p in 1:nrow(players)) {
  player_id <- players[p, "wikidataId"]
  tlog(4, "Processing player ", player_id, " (", p, "/", nrow(players), ")")

  # retrieve the player's stints
  idx <- which(stints[, "playerId"] == player_id)
  player_stints <- stints[idx, ]
  player_teams <- sort(unique(player_stints[, "teamRsId"]))

  # loop over teams
  for (t in player_teams) {
#    tlog(6, "Processing team ", t, " (", teams[teams[, "rugbyscopeId"] == t, "fullName"], ")")
    idx2 <- which(player_stints[, "teamRsId"] == t)
    team_stints <- player_stints[idx2, ]

    # look for compatible stints
    if (nrow(team_stints) > 1) {
      for (s1 in 1:(nrow(team_stints) - 1)) {
        #tlog(8, "s1: ", s1)
        # check that the 1st stint is not already marked for deletion
        if (!(idx[idx2[s1]] %in% rem_marked)) {
          # loop over the remaining stints
          for (s2 in (s1 + 1):nrow(team_stints)) {
            #tlog(10, "s2: ", s2)
            # check that the 2nd stint is not already marked for deletion
            if (!(idx[idx2[s2]] %in% rem_marked)) {

              start1 <- team_stints[s1, "startYear"]
              end1 <- team_stints[s1, "endYear"]
              start2 <- team_stints[s2, "startYear"]
              end2 <- team_stints[s2, "endYear"]
              if (is.na(start1)) {
                if (is.na(end1)) {
                  if (is.na(start2)) {
                    if (is.na(end2)) {
                      # s1==NA e1==NA s2==NA e2==NA
                      conf <- TRUE
                    } else {
                      # s1==NA e1==NA s2==NA e2!=NA
                      conf <- TRUE
                    }
                  } else {
                    if (is.na(end2)) {
                      # s1==NA e1==NA s2!=NA e2==NA
                      conf <- TRUE
                    } else {
                      # s1==NA e1==NA s2!=NA e2!=NA
                      conf <- TRUE
                    }
                  }
                } else {
                  if (is.na(start2)) {
                    if (is.na(end2)) {
                      # s1==NA e1!=NA s2==NA e2==NA
                      conf <- TRUE
                    } else {
                      # s1==NA e1!=NA s2==NA e2!=NA
                      conf <- abs(end1 - end2) <= tolerance
                    }
                  } else {
                    if (is.na(end2)) {
                      # s1==NA e1!=NA s2!=NA e2==NA
                      conf <- end1 >= start2
                    } else {
                      # s1==NA e1!=NA s2!=NA e2!=NA
                      conf <- abs(end1 - end2) <= tolerance && end1 >= start2 && end1 <= end2
                    }
                  }
                }
              } else {
                if (is.na(end1)) {
                  if (is.na(start2)) {
                    if (is.na(end2)) {
                      # s1!=NA e1==NA s2==NA e2==NA
                      conf <- TRUE
                    } else {
                      # s1!=NA e1==NA s2==NA e2!=NA
                      conf <- start1 <= end2
                    }
                  } else {
                    if (is.na(end2)) {
                      # s1!=NA e1==NA s2!=NA e2==NA
                      conf <- abs(start1 - start2) <= tolerance
                    } else {
                      # s1!=NA e1==NA s2!=NA e2!=NA
                      conf <- abs(start1 - start2) <= tolerance && start1 >= start2 && start1 <= end2
                    }
                  }
                } else {
                  if (is.na(start2)) {
                    if (is.na(end2)) {
                      # s1!=NA e1!=NA s2==NA e2==NA
                      conf <- TRUE
                    } else {
                      # s1!=NA e1!=NA s2==NA e2!=NA
                      conf <- abs(end1 - end2) <= tolerance && end2 >= start1 && end2 <= start1
                    }
                  } else {
                    if (is.na(end2)) {
                      # s1!=NA e1!=NA s2!=NA e2==NA
                      conf <- abs(start1 - start2) <= tolerance && start2 >= start1 && start2 <= end1
                    } else {
                      # s1!=NA e1!=NA s2!=NA e2!=NA
                      conf <- abs(start1 - start2) <= tolerance && abs(end1 - end2) <= tolerance &&
                                (start1 >= start2 && start1 <= end2 || end1 >= start2 && end1 <= end2)
                    }
                  }
                }
              }

              if (conf) {
                tlog(12, "Similar stints detected:")
                print(team_stints[c(s1, s2), ])

                mp1 <- team_stints[s1, "matchesPlayed"]
                mp2 <- team_stints[s2, "matchesPlayed"]
                ps1 <- team_stints[s1, "pointsScored"]
                ps2 <- team_stints[s2, "pointsScored"]

                # we merge into the stint that has stats, or that has the largest stats
                # if same stats or not stats at all: do not merge

                # no stat at all
                merge_flag <- FALSE
                if (all(is.na(c(mp1, mp2, ps1, ps2)))) {
                  # do nothing
                  merge_flag <- FALSE
                # first stint better
                } else if (((!is.na(mp1) && is.na(mp2)) || (!is.na(mp1) && !is.na(mp2) && mp1 > mp2)) || ((!is.na(ps1) && is.na(ps2)) || (!is.na(ps1) && !is.na(ps2) && ps1 > ps2))) {
                  # merge into first stint
                  s_tgt <- s1
                  s_rem <- s2
                  merge_flag <- TRUE
                # second stint better
                } else if (((!is.na(mp2) && is.na(mp1)) || (!is.na(mp1) && !is.na(mp2) && mp2 > mp1)) || ((!is.na(ps2) && is.na(ps1)) || (!is.na(ps1) && !is.na(ps2) && ps2 > ps1))) {
                  # merge into second stint
                  s_tgt <- s2
                  s_rem <- s1
                  merge_flag <- TRUE
                # other situations
                } else {
                  # do nothing
                  merge_flag <- FALSE
                }

                if (merge_flag) {
                  # merge dates in the first stint
                  # start year
                  sy1 <- team_stints[s_tgt, "startYear"]
                  sy2 <- team_stints[s_rem, "startYear"]
                  if (is.na(sy1))
                    stints[idx[idx2[s1]], "startYear"] <- sy2
                  else
                    stints[idx[idx2[s1]], "startYear"] <- sy1
                  # end year
                  ey1 <- team_stints[s_tgt, "endYear"]
                  ey2 <- team_stints[s_rem, "endYear"]
                  if (is.na(ey1))
                    stints[idx[idx2[s1]], "endYear"] <- ey2
                  else
                    stints[idx[idx2[s1]], "endYear"] <- ey1
                  
                  # merge stats in the first stint
                  stints[idx[idx2[s1]], ] <- merge_stint_stats(team_stints[s_tgt, ], team_stints[s_rem, ], mode = "max")
                  tlog(12, "Merged stint:")
                  print(stints[idx[idx2[s1]], ])

                  # mark the second stint for removal
                  rem_marked <- c(rem_marked, idx[idx2[s2]])
                  
                  merged_stints <- merged_stints + 1
                }
              }
            }
          }
        }
      }
    }
  }
}
tlog(2, "Number of stints before merging: ", nrow(stints))

# remove the marked rows
rem_marked <- unique(rem_marked)
if (length(rem_marked) > 0)
  stints <- stints[-rem_marked, ]
# display result
tlog(2, "Number of compatible stints merged: ", merged_stints)
tlog(2, "Number of stints after merging: ", nrow(stints))

# similar dates, no stats at all:
# keep majority stint, based on sources?

# similar dates, one stint with stats the other without : use the stint that has stats
# "Q3195748","Kevin Maggs","Q917973",162,"Bristol Bears",1993,1998,95,25,"itWP; esWP; enWP"
# "Q3195748","Kevin Maggs","Q917973",162,"Bristol Bears",1996,1999,NA,NA,"WD; frWP"

# similar dates, identical stats
# > keep majority dates?
# > also take into account other date-conflicting stints
# "Q7571","Patricio Albacete","Q646070",101,"Argentina national rugby union team",2003,2013,57,5,"WD; frWP; itWP; esWP"
# "Q7571","Patricio Albacete","Q646070",101,"Argentina national rugby union team",2003,2014,57,5,"enWP"

# similar dates, different stats
# > keep majority stints?
# > if no majority, use largest stats?
# > use conflicting stints to decide?
# "Q26037","Quade Cooper","Q1368281",253,"Queensland Reds",2006,2015,109,768,"enWP"
# "Q26037","Quade Cooper","Q1368281",253,"Queensland Reds",2006,2017,119,846,"jaWP"




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
  # keep only club stints to look for conflicts
  if (length(player_teams) > 0) {
    team_types <- teams[match(player_teams, teams[, "rugbyscopeId"]), "type"]
    player_teams <- player_teams[team_types == "Club/franchise team"]
  }

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
                  # s1==NA e1==NA s2==NA e2==NA
                  conf <- FALSE
                } else {
                  # s1==NA e1==NA s2==NA e2!=NA
                  conf <- FALSE
                }
              } else {
                if (is.na(end2)) {
                  # s1==NA e1==NA s2!=NA e2==NA
                  conf <- FALSE
                } else {
                  # s1==NA e1==NA s2!=NA e2!=NA
                  conf <- FALSE
                }
              }
            } else {
              if (is.na(start2)) {
                if (is.na(end2)) {
                  # s1==NA e1!=NA s2==NA e2==NA
                  conf <- FALSE
                } else {
                  # s1==NA e1!=NA s2==NA e2!=NA
                  conf <- end1 == end2
                }
              } else {
                if (is.na(end2)) {
                  # s1==NA e1!=NA s2!=NA e2==NA
                  conf <- FALSE
                } else {
                  # s1==NA e1!=NA s2!=NA e2!=NA
                  conf <- end1 >= start2 && end1 <= end2
                }
              }
            }
          } else {
            if (is.na(end1)) {
              if (is.na(start2)) {
                if (is.na(end2)) {
                  # s1!=NA e1==NA s2==NA e2==NA
                  conf <- FALSE
                } else {
                  # s1!=NA e1==NA s2==NA e2!=NA
                  conf <- FALSE
                }
              } else {
                if (is.na(end2)) {
                  # s1!=NA e1==NA s2!=NA e2==NA
                  conf <- start1 == start2
                } else {
                  # s1!=NA e1==NA s2!=NA e2!=NA
                  conf <- start1 >= start2 && start1 <= end2
                }
              }
            } else {
              if (is.na(start2)) {
                if (is.na(end2)) {
                  # s1!=NA e1!=NA s2==NA e2==NA
                  conf <- FALSE
                } else {
                  # s1!=NA e1!=NA s2==NA e2!=NA
                  conf <- end2 >= start1 && end2 <= end1
                }
              } else {
                if (is.na(end2)) {
                  # s1!=NA e1!=NA s2!=NA e2==NA
                  conf <- start2 >= start1 && start2 <= end1
                } else {
                  # s1!=NA e1!=NA s2!=NA e2!=NA
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

idx <- which(stints[, "playerId"] %in% player_conflicts)
conf_stints <- stints[idx, ]
tab.file <- file.path(data_folder, "stints_11_conflicts.csv")
write.csv(conf_stints, tab.file, row.names = FALSE, fileEncoding = "UTF-8")
ok_stints <- stints[-idx, ]
tab.file <- file.path(data_folder, "stints_11_ok.csv")
write.csv(ok_stints, tab.file, row.names = FALSE, fileEncoding = "UTF-8")

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
# verifications in teams table





########################################################################
# record the updated table

# tab.file <- file.path(data_folder, "stints_02.csv")
# write.csv(stints, tab.file, row.names = FALSE, fileEncoding = "UTF-8")




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


# principle :
# - for a given player, extract the stints for each source
# - fix each source separately
# - compare them to find the best source
# - then match the stints from the other sources to these best stints
# - if matched: merge to existing stint
# - if not : add extra stint

# best sources :
# - most stints?
# - covers the longer time span?

# 1) focus only on one source, get the stints of a player
# 2) detect issues in these stints, fix them
# 3) compare stings from WP versions : if no disagreement, merge
# 4) if disagreement, start from the earliest stint
# 5) if several, take the one from the source with the most stints
# 6) if the next stint overlaps, fix its start year



# "Q18455","Ben Tune","Q1368281",253,"Queensland Reds",1995,2007,112,155,"WD; frWP; esWP"
# "Q18455","Ben Tune","Q1368281",253,"Queensland Reds",1996,2007,84,155,"esWP; enWP"
# > same stint, but one year difference at start

# "Q29880","Ben Botica","Q104855944",2904,"North Harbour Rugby Union Team",2009,2011,19,168,"frWP"
# "Q29880","Ben Botica","Q104855944",2904,"North Harbour Rugby Union Team",2009,2012,17,168,"enWP"
# > same stint, but one year difference at end

# > check stint begore / after to determine the correct date

# "Q72185","Manu Tuilagi","Q1071691",204,"Leicester Tigers",2009,2020,128,205,"WD; jaWP; frWP; enWP"
# "Q72185","Manu Tuilagi","Q1071691",204,"Leicester Tigers",2010,2020,95,145,"itWP; esWP"

# other possibility : use largest stats

# "Q31193","Ben Smith","Q104856109",2911,"Otago Rugby Union Team",2007,2012,59,75,"jaWP; frWP; enWP"
# "Q31193","Ben Smith","Q104856109",2911,"Otago Rugby Union Team",2007,2016,37,50,"itWP"
# "Q31193","Ben Smith","Q104856109",2911,"Otago Rugby Union Team",2008,2009,NA,NA,"esWP"     >> this one should be merged in one of the others beforehand
# very different stints: can't see how to solve that automatically

# algo if one year difference
# 1) check if stint before: use end year to select compatible start year
#    if several stints before: should be solved beforehand
# 2) if no stint before (= first stint), then compare stats and use option with largest stats
# 3) if no stat, use majority stint in terms of sources
# 4) of none of that... deal with it manually?




# TODO
###############################
# - DATABASE
#   - notes:
#     - we should have kept stint order and loan indications
#   - misc:
#     > function to add back stint types (amateur, provincial, junior, senior, etc.)
#       - classify stints based on the WP categories (provincial, pro, etc.)
#     - write an algo to split appropriately loans and such, and list the cases that could not be split properly?
#     - handle frWP stints at the start of career: often wrong
#       > check players with very long career spans, probably this type of problem
#       > and/or players with big gaps between stints
#     - look for stints of the form xxx-2013 vs 2014-xxxx (same team)
#     - handles cases where there are stints without dates and the first dated stints starts >18yo
#   - final:
#     - look for exact duplicates
#     - check all categorical variables (in particular sources)
#     - clean (unique) + order sources (and other multiple fields)
#   - complement data:
#     - remove stints and parts of stints before 18 yo (without changing stats)
#     - complement missing U18 (and other youth teams) dates using player's age
#     - and do the opposite: complement missing birthdate using Uxx stints
#     - use other sources to complement missing dates (esp. NA-NA in career start ?) https://www.allrugby.com/
###############################
# - NET EXTRACTION
#   - complement missing end years by leveraging present start years
#   - complement missing dates/ split depending on loans, etc.
#     note : overlapping stints at clubs = loans
#            but there are also just simultaneous membership
#   - possibly put 2025 or 2026 for the latest stint ?
#   - merge junior/senior consecutive stints with the same team
###############################


########################################################################
# list players with conflicting stints
tlog("Identify players with a single stint")

# loop over players
tlog(2, "Looping over players")
selected_players <- c()
for (p in 1:nrow(players)) {
  player_id <- players[p, "wikidataId"]
  tlog(4, "Processing player ", player_id, " (", p, "/", nrow(players), ")")

  # retrieve the player's stints
  idx <- which(stints[, "playerId"] == player_id)
  player_stints <- stints[idx, ]
  player_teams <- sort(unique(player_stints[, "teamRsId"]))
  team_types <- teams[match(player_teams, teams[, "rugbyscopeId"]), "type"]

  if (nrow(player_stints) > 0) {
    # focus on a specific situation
    # if (length(player_teams) <= 2) {
      # 2 stints without dates
      #if (nrow(player_stints)==2 && (all(is.na(player_stints[1, c("startYear", "endYear")])) || all(is.na(player_stints[2, c("startYear", "endYear")]))))
      # one national team stint
      #if (any(grepl("National", team_types, fixed = TRUE)))
      # overlapping periods without NA
      # if (all(!is.na(unlist(player_stints[, c("startYear", "endYear")]))) && 
      #   (player_stints[1, "startYear"] >= player_stints[2, "startYear"] && player_stints[1, "startYear"] <= player_stints[2, "endYear"] ||
      #   player_stints[2, "startYear"] >= player_stints[1, "startYear"] && player_stints[2, "startYear"] <= player_stints[1, "endYear"]))
      # overlapping periods with NA
      # if (!is.na(player_stints[1, "endYear"]) && !is.na(player_stints[2, "startYear"]) && 
      #     (player_stints[1, "endYear"] >= player_stints[2, "startYear"] &&
      #      (is.na(player_stints[2, "endYear"]) || player_stints[1, "startYear"] <= player_stints[2, "endYear"])))
        # selected_players <- c(selected_players, player_id)
    # }

    if (nrow(player_stints) > 1) {
      for (i in 1:(nrow(player_stints)-1)) {
        team1 <- player_stints[i, "teamRsId"]
        start1 <- player_stints[i, "startYear"]
        end1 <- player_stints[i, "endYear"]
        mp1 <- player_stints[i, "matchesPlayed"]
        ps1 <- player_stints[i, "pointsScored"]
        
        # # same date and stats #but different teams
        # if((!is.na(start1) || !is.na(start2)) && (!is.na(mp1) || !is.na(ps1)) ) {
        #   for (j in (i+1):nrow(player_stints)) {
        #     team2 <- player_stints[j, "teamRsId"]
        #     start2 <- player_stints[j, "startYear"]
        #     end2 <- player_stints[j, "endYear"]
        #     mp2 <- player_stints[j, "matchesPlayed"]
        #     ps2 <- player_stints[j, "pointsScored"]
        #
        #     if((is.na(start1) && is.na(start2) || !is.na(start1) && !is.na(start2) && start1==start2) &&
        #       (is.na(end1) && is.na(end2) || !is.na(end1) && !is.na(end2) && end1==end2) &&
        #       (is.na(mp1) && is.na(mp2) || !is.na(mp1) && !is.na(mp2) && mp1==mp2) &&
        #       (is.na(ps1) && is.na(ps2) || !is.na(ps1) && !is.na(ps2) && ps1==ps2)) {
        #       selected_players <- c(selected_players, player_id)
        #       # cat("i =", i, "-", "j =", j, "\n") # debug
        #     }
        #   }

        # two stints with same team, start year present, but no end year
        for (j in (i+1):nrow(player_stints)) {
          team2 <- player_stints[j, "teamRsId"]
          start2 <- player_stints[j, "startYear"]
          end2 <- player_stints[j, "endYear"]
          mp2 <- player_stints[j, "matchesPlayed"]
          ps2 <- player_stints[j, "pointsScored"]
          if (!is.na(start1) && !is.na(start2) && is.na(end1) && is.na(end2) && team1 == team2 || 
              is.na(start1) && is.na(start2) && !is.na(end1) && !is.na(end2) && team1 == team2 ) {
            selected_players <- c(selected_players, player_id)
            cat("i =", i, "-", "j =", j, "\n") # debug
          }
        }

      }
    }
  }
}
print(selected_players)

idx <- which(stints[, "playerId"] %in% selected_players)
conf_stints <- stints[idx, ]
tab.file <- file.path(data_folder, "stints_12_selected.csv")
write.csv(conf_stints, tab.file, row.names = FALSE, fileEncoding = "UTF-8")
ok_stints <- stints[-idx, ]
tab.file <- file.path(data_folder, "stints_12_notselected.csv")
write.csv(ok_stints, tab.file, row.names = FALSE, fileEncoding = "UTF-8")

# order stints
stints <- order_stints(stints)
tab.file <- file.path(data_folder, "stints_13.csv")
write.csv(stints, tab.file, row.names = FALSE, fileEncoding = "UTF-8")
