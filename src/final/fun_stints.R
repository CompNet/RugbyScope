########################################################################
# Functions used to merge redundant stints.
#
# Vincent Labatut
# 05/2026
########################################################################




########################################################################
# For the specified player, extracts the stint sequence of each source.
#
# player_id: WD id of the player.
# stints: full stint table.
#
# returns: list of stint sequences, each one corresponding to a distinct source.
########################################################################
retrieve_stints_by_source <- function(player_id, stints) {
  idx <- which(stints[, "playerId"] == player_id)
  player_stints <- stints[idx, ]

  # identify the sources for this player
  sources <- sort(unique(unlist(strsplit(player_stints[, "dataSource"], "; "))))

  # retrieve stints by source
  result <- list()
  for (source in sources) {
    idx <- which(grepl(source, player_stints[, "dataSource"], fixed = TRUE))
    result[[source]] <- player_stints[idx, ]
  }

  return(result)
}
#retrieve_stints_by_source("Q26037",stints)




########################################################################
# Takes the stints of a player, and merges those with the exact same
# dates and team.
#
# player_stints: table containing the stints of a specific player.
#
# returns: same table, but with merged identical stints.
########################################################################
merge_stints_identical <- function(player_stints) {
  player_teams <- sort(unique(player_stints[, "teamRsId"]))

  # loop over teams
  merged_stints <- 0
  rem_marked <- c()
  for (team_id in player_teams) {
#    tlog(6, "Processing team ", team_id, " (", teams[teams[, "rugbyscopeId"] == team_id, "fullName"], ")")

    # retrieve team stints
    idx <- which(player_stints[, "teamRsId"] == team_id)
    team_stints <- player_stints[idx, ]

    # look for identical dates
    if (nrow(team_stints) > 1) {
      for (s1 in 1:(nrow(team_stints) - 1)) {
        if (!(idx[s1] %in% rem_marked)) {
          for (s2 in (s1 + 1):nrow(team_stints)) {
            if (!(idx[s2] %in% rem_marked)) {
              start1 <- team_stints[s1, "startYear"]
              end1 <- team_stints[s1, "endYear"]
              start2 <- team_stints[s2, "startYear"]
              end2 <- team_stints[s2, "endYear"]
            
              if (((is.na(start1) && is.na(start2)) || (!is.na(start1) && !is.na(start2) && start1 == start2))
                && ((is.na(end1) && is.na(end2)) || (!is.na(end1) && !is.na(end2) && end1 == end2))) {
                tlog(8, "Identical stints detected:")
                print(team_stints[c(s1, s2), ])
                
                # merge stats in the first stint
                stints[idx[idx2[s1]], ] <- merge_stint_stats(team_stints[s1, ], team_stints[s2, ])
                tlog(8, "Merged stint:")
                print(stints[idx[idx2[s1]], ])

                # mark the second stint for removal
                rem_marked <- c(rem_marked, idx[s2])
                
                merged_stints <- merged_stints + 1
              }
            }
          }
        }
      }
    }
  }

  # remove the marked rows
  if (length(rem_marked) > 0)
    stints <- stints[-rem_marked, ]

  return(stints)
}



    # stints <- merge_stints_nested(stints)
    # stints <- merge_stints_consecutive(stints)
    # stints <- merge_stints_overlapping(stints)




########################################################################
# Cleans the stints in the list, which correspond to the stint sequences 
# of a given player according to various data sources.
#
# stint_list: list of stint sequences (one for each data source).
#
# returns: same list, but cleaned.
########################################################################
clean_stints_by_source <- function (seq_list) {
  result <- list()
  sources <- names(stints)

  # loop over data sources
  for (source in sources) {
    stints <- seq_list[[source]]
    res <- stints[-(1:nrow(stints)), ]

    stints <- merge_stints_identical(stints)
    stints <- merge_stints_nested(stints)
    stints <- merge_stints_consecutive(stints)
    stints <- merge_stints_overlapping(stints)

    result[[source]] <- stints
  }

  return(result)
}




########################################################################
# Identifies the best data source for the specified player. We compare the
# stint sequence according to all sources, and keep the best one according
# to the following criteria (by decreasing order of importance):
# - total number of covered years
# - 
# We assume there is no redundant stints, source-wise.
# 
# seq_list: list of player's stint sequences, on for each data source.
# 
# returns: name of the best source.
########################################################################
retrieve_stints_by_source <- function(seq_list) {
}
