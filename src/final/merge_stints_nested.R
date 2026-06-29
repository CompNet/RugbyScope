########################################################################
# The data contain many similar and redundant stints. This script tries
# to merge them efficiently.
#
# 05/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/merge_stints_identical.R")
########################################################################
source("src/final/fun_stints.R")



########################################################################
# merge nested stints at the same team
tlog("Merging stints contained in other stints at the same team")

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

    # look for inclusions
    if (nrow(team_stints) > 1) {
      for (s1 in 1:(nrow(team_stints) - 1)) {
        if (!(idx[idx2[s1]] %in% rem_marked)) {
          start1 <- team_stints[s1, "startYear"]
          end1 <- team_stints[s1, "endYear"]

          if (!is.na(start1) && !is.na(end1)) {
            iii <- (s1 + 1):nrow(team_stints)
            idx3 <- s1 + which(!is.na(team_stints[iii, "startYear"]) & team_stints[iii, "startYear"] > start1 &
                              !is.na(team_stints[iii, "endYear"]) & team_stints[iii, "endYear"] < end1)
            # ignore rows already marked for deletion
            idx3 <- idx3[!(idx[idx2[idx3]] %in% rem_marked)]

            # single match
            if (length(idx3) == 1) {
              tlog(8, "One match detected:")
              print(team_stints[c(s1, idx3), ])

              # no date update in this case

              # merge stats the regular way
              stints[idx[idx2[s1]], ] <- merge_stint_stats(team_stints[s1, ], team_stints[idx3, ], mode = "max")

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

  #readline(prompt="Press [enter] to continue")
}
tlog(2, "Number of stints before merging: ", nrow(stints))

# remove the marked rows
if (length(rem_marked) > 0)
  stints <- stints[-rem_marked, ]
# display result
tlog(2, "Number of nested stints merged: ", merged_stints)
tlog(2, "Number of stints after merging: ", nrow(stints))
tlog(2, "Number of conflicts detected: ", conflict_stints)




########################################################################
# record table
tab.file <- file.path(data_folder, "stints_03.csv")
write.csv(stints, tab.file, row.names = FALSE, fileEncoding = "UTF-8")
