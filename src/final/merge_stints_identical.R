########################################################################
# The data contain many similar and redundant stints. This script tries
# to merge them efficiently.
#
# 05/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/merge_stints_identical.R")
########################################################################




########################################################################
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

  # loop over the player's teams
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
            tlog(8, "Identical stints detected:")
            print(team_stints[c(s1, s2), ])
            
            # merge stats in the first stint
            stints[idx[idx2[s1]], ] <- merge_stint_stats(team_stints[s1, ], team_stints[s2, ], mode = "max")
            tlog(8, "Merged stint:")
            print(stints[idx[idx2[s1]], ])

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
# > only 22 pairs of stints are concerned

# remove the marked rows
if (length(rem_marked) > 0)
  stints <- stints[-rem_marked, ]
# display result
tlog(2, "Number of identical stints merged: ", merged_stints)
tlog(2, "Number of stints after merging: ", nrow(stints))
stints0 <- stints




########################################################################
# record table
tab.file <- file.path(data_folder, "stints_02.csv")
write.csv(stints, tab.file, row.names = FALSE, fileEncoding = "UTF-8")
