########################################################################
# Functions used to merge redundant stints.
#
# Vincent Labatut
# 05/2026
########################################################################




########################################################################
# Merges the stats of two stints, taking their values into account
# (including the presence of NAs).
#
# s1: first stint to merge, as a row in the stint table.
# s2: second stint to merge, as a row in the stint table.
# mode: whether to take the max or the sum of the values.
#
# returns: resulting merged stint.
########################################################################
merge_stint_stats <- function(s1, s2, mode) {
  # init result
  res <- s1

  # matches played
  mp1 <- s1[, "matchesPlayed"]
  mp2 <- s2[, "matchesPlayed"]
  if (is.na(mp1))
    res[, "matchesPlayed"] <- mp2
  else {
    if (mode == "max")
      res[, "matchesPlayed"] <- max(mp1, mp2, na.rm = TRUE)
    else if (mode =="sum")
      res[, "matchesPlayed"] <- mp1 + mp2
  }

  # points scored
  ps1 <- s1[, "pointsScored"]
  ps2 <- s2[, "pointsScored"]
  if (is.na(ps1))
    res[, "pointsScored"] <- ps2
  else {
    if (mode == "max")
      res[, "pointsScored"] <- max(ps1, ps2, na.rm = TRUE)
    else if (mode =="sum")
      res[, "pointsScored"] <- ps1 + ps2
  }

  # data source
  ds1 <- trimws(unlist(strsplit(s1[, "dataSource"], ";")))
  ds2 <- trimws(unlist(strsplit(s2[, "dataSource"], ";")))
  res[, "dataSource"] <- paste0(sort(unique(union(ds1, ds2))), collapse = "; ")

  return(res)
}




########################################################################
# For the specified player, extracts the stint sequence of each data 
#source (Wikidata, EN Wikipedia, FR Wikipedia, etc.).
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
#### test
#retrieve_stints_by_source("Q26037",stints)




########################################################################
# Takes the stints of a player, and merges those with the exact same
# dates and team.
#
# player_stints: table containing the stints of a specific player.
#
# returns: same table, but with merged identical stints. And some stats.
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
                player_stints[idx[s1], ] <- merge_stint_stats(team_stints[s1, ], team_stints[s2, ], mode = "max")
                tlog(8, "Merged stint:")
                print(player_stints[idx[s1], ])

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
    player_stints <- player_stints[-rem_marked, ]

  result <- list(player_stints=player_stints, merged_stints=merged_stints)
  return(result)
}
#### test
#seq_list <- retrieve_stints_by_source("Q26037", stints)
#merge_stints_identical(seq_list$enWP)
#print("---------------------------------")
#merge_stints_identical(rbind(seq_list$enWP, seq_list$enWP[1, ]))




########################################################################
# Takes the stints of a player, detect those that are nested (i.e. same
# team and one stint's time period included in the other's) and merges
# them.
#
# player_stints: table containing the stints of a specific player.
#
# returns: same table, but with merged nested stints. And some stats.
########################################################################
merge_stints_nested <- function(player_stints) {
  player_teams <- sort(unique(player_stints[, "teamRsId"]))

  # loop over teams
  merged_stints <- 0
  conflict_stints <- 0
  rem_marked <- c()
  for (team_id in player_teams) {
#    tlog(6, "Processing team ", team_id, " (", teams[teams[, "rugbyscopeId"] == team_id, "fullName"], ")")

    # retrieve team stints
    idx <- which(player_stints[, "teamRsId"] == team_id)
    team_stints <- player_stints[idx, ]

    # look for inclusions
    if (nrow(team_stints) > 1) {
      for (s1 in 1:(nrow(team_stints) - 1)) {
        if (!(idx[s1] %in% rem_marked)) {
          start1 <- team_stints[s1, "startYear"]
          end1 <- team_stints[s1, "endYear"]

          if (!is.na(start1) && !is.na(end1)) {
            iii <- (s1 + 1):nrow(team_stints)
            idx2 <- s1 + which(!is.na(team_stints[iii, "startYear"]) & team_stints[iii, "startYear"] >= start1 &
                              !is.na(team_stints[iii, "endYear"]) & team_stints[iii, "endYear"] <= end1)
            # ignore rows already marked for deletion
            idx2 <- idx2[!(idx[idx2] %in% rem_marked)]

            # single match
            if (length(idx2) == 1) {
              tlog(8, "One match detected:")
              print(team_stints[c(s1, idx2), ])

              # no date update in this case

              # max-merge stats the regular way
              player_stints[idx[s1], ] <- merge_stint_stats(team_stints[s1, ], team_stints[idx2, ], mode = "max")

              # display updated stint
              tlog(10, "Merged stint:")
              print(player_stints[idx[s1], ])

              # mark the second stint for removal (good enough, as there are no complicated cases)
              rem_marked <- c(rem_marked, idx[idx2])
              
              merged_stints <- merged_stints + 1

            # several matches
            } else if (length(idx2) > 1) {
              tlog(8, "Several matches detected:")
              print(team_stints[c(s1, idx2), ])
              conflict_stints <- conflict_stints + 1
            }
          }
        }
      }
    }
  }

  # remove the marked rows
  if (length(rem_marked) > 0)
    player_stints <- player_stints[-rem_marked, ]

  result <- list(player_stints=player_stints, merged_stints=merged_stints, conflict_stints=conflict_stints)
  return(result)
}
#### test
#seq_list <- retrieve_stints_by_source("Q26037", stints)
#merge_stints_nested(seq_list$enWP)
#print("---------------------------------")
#tmp <- rbind(seq_list$enWP, seq_list$enWP[1, ])
#tmp[nrow(tmp), "startYear"] <- 2010
#merge_stints_nested(tmp)




########################################################################
# Merges the directly consecutive stints of a player, i.e. same team and
# same end1 / start2 year. The stats are added.
#
# player_stints: table containing the stints of a specific player.
#
# returns: same table, but with merged consecutive stints. And some stats.
########################################################################
merge_stints_consecutive <- function(player_stints) {
  player_teams <- sort(unique(player_stints[, "teamRsId"]))

  # loop over teams
  merged_stints <- 0
  conflict_stints <- 0
  rem_marked <- c()
  for (team_id in player_teams) {
#    tlog(6, "Processing team ", team_id, " (", teams[teams[, "rugbyscopeId"] == team_id, "fullName"], ")")

    # retrieve team stints
    idx <- which(player_stints[, "teamRsId"] == team_id)
    team_stints <- player_stints[idx, ]

    # look for consecutive stints
    if (nrow(team_stints) > 1) {
      for (s1 in 1:(nrow(team_stints) - 1)) {
        if (!(idx[s1] %in% rem_marked)) {
          start1 <- team_stints[s1, "startYear"]
          end1 <- team_stints[s1, "endYear"]

          if (!is.na(end1)) {
            iii <- (s1 + 1):nrow(team_stints)
            idx2 <- s1 + which(!is.na(team_stints[iii, "startYear"]) & team_stints[iii, "startYear"] == end1)
            # ignore rows already marked for deletion
            idx2 <- idx2[!(idx[idx2] %in% rem_marked)]

            # single match
            if (length(idx2) == 1) {
              tlog(8, "One match detected:")
              print(team_stints[c(s1, idx2), ])

              # update end date in first stint
              team_stints[s1, "endYear"] <- team_stints[idx2, "endYear"]

              # possibly add 2nd stint stats to 1st stint
              mp1 <- team_stints[s1, "matchesPlayed"]
              mp2 <- team_stints[idx2, "matchesPlayed"]
              ps1 <- team_stints[s1, "pointsScored"]
              ps2 <- team_stints[idx2, "pointsScored"]
              if (all(!is.na(c(mp1, ps1, mp2, ps2))) && mp1 == mp2 && ps1 == ps2)
                # likely a duplicate of the first stint: do nothing stats-wise
                player_stints[idx[s1], ] <- team_stints[s1, ]
              else
                player_stints[idx[s1], ] <- merge_stint_stats(team_stints[s1, ], team_stints[idx2, ], mode = "sum")

              # display updated stint
              tlog(10, "Merged stint:")
              print(player_stints[idx[s1], ])

              # mark the second stint for removal (good enough, as there are no complicated cases)
              rem_marked <- c(rem_marked, idx[idx2])
              
              merged_stints <- merged_stints + 1

            # several matches
            } else if (length(idx2) > 1) {
              tlog(8, "Several matches detected:")
              print(team_stints[c(s1, idx2), ])
              conflict_stints <- conflict_stints + 1
            }
          }
        }
      }
    }
  }

  # remove the marked rows
  if (length(rem_marked) > 0)
    player_stints <- player_stints[-rem_marked, ]

  result <- list(player_stints=player_stints, merged_stints=merged_stints, conflict_stints=conflict_stints)
  return(result)
}
#### test
#seq_list <- retrieve_stints_by_source("Q26037", stints)
#merge_stints_consecutive(seq_list$enWP)
#print("---------------------------------")
#tmp <- rbind(seq_list$enWP, seq_list$enWP[1, ])
#tmp[nrow(tmp), "startYear"] <- 2015
#tmp[nrow(tmp), "endYear"] <- 2016
#merge_stints_consecutive(tmp)




########################################################################
# For a given player, merges overlapping stints at the same club. We keep
# the max stats.
#
# player_stints: table containing the stints of a specific player.
#
# returns: same table, but with merged overlapping stints. And some stats.
########################################################################
merge_stints_overlapping <- function(player_stints) {
  player_teams <- sort(unique(player_stints[, "teamRsId"]))

  # loop over teams
  merged_stints <- 0
  conflict_stints <- 0
  rem_marked <- c()
  for (team_id in player_teams) {
#    tlog(6, "Processing team ", team_id, " (", teams[teams[, "rugbyscopeId"] == team_id, "fullName"], ")")

    # retrieve team stints
    idx <- which(player_stints[, "teamRsId"] == team_id)
    team_stints <- player_stints[idx, ]

    # look for overlapping stints
    if (nrow(team_stints) > 1) {
      for (s1 in 1:(nrow(team_stints) - 1)) {
        if (!(idx[s1] %in% rem_marked)) {
          start1 <- team_stints[s1, "startYear"]
          end1 <- team_stints[s1, "endYear"]

          if (!is.na(end1)) {
            iii <- (s1 + 1):nrow(team_stints)
            idx2 <- s1 + which(!is.na(team_stints[iii, "startYear"]) & team_stints[iii, "startYear"] <= end1)
            # ignore rows already marked for deletion
            idx2 <- idx2[!(idx[idx2] %in% rem_marked)]

            # single match
            if (length(idx2) == 1) {
              tlog(8, "One match detected:")
              print(team_stints[c(s1, idx2), ])

              # update end date in first stint
              team_stints[s1, "endYear"] <- team_stints[idx2, "endYear"]

              # combine stats
              player_stints[idx[s1], ] <- merge_stint_stats(team_stints[s1, ], team_stints[idx2, ], mode = "max")

              # display updated stint
              tlog(10, "Merged stint:")
              print(player_stints[idx[s1], ])

              # mark the second stint for removal (good enough, as there are no complicated cases)
              rem_marked <- c(rem_marked, idx[idx2])
              
              merged_stints <- merged_stints + 1

            # several matches
            } else if (length(idx2) > 1) {
              tlog(8, "Several matches detected:")
              print(team_stints[c(s1, idx2), ])
              conflict_stints <- conflict_stints + 1
            }
          }
        }
      }
    }
  }

  # remove the marked rows
  if (length(rem_marked) > 0)
    player_stints <- player_stints[-rem_marked, ]

  result <- list(player_stints=player_stints, merged_stints=merged_stints, conflict_stints=conflict_stints)
  return(result)
}
### test
seq_list <- retrieve_stints_by_source("Q26037", stints)
merge_stints_overlapping(seq_list$enWP)
print("---------------------------------")
tmp <- rbind(seq_list$enWP, seq_list$enWP[1, ])
tmp[nrow(tmp), "startYear"] <- 2013
tmp[nrow(tmp), "endYear"] <- 2016
merge_stints_overlapping(tmp)
stop()




########################################################################
# Cleans the stints in the list, which correspond to the stint sequences 
# of a given player according to various data sources.
#
# stint_list: list of stint sequences (one for each data source).
#
# returns: same list, but cleaned. And some stats.
########################################################################
clean_stints_by_source <- function(seq_list) {
  res_lst <- list()
  sources <- names(seq_list)

  step_names <- c("identical", "nested", "consecutive", "overlapping")
  merged_stints <- rep(0, length(step_names))
  conflict_stints <- rep(0, length(step_names))
  names(merged_stints) <- names(conflict_stints) <- step_names

  # loop over data sources
  for (source in sources) {
    player_stints <- seq_list[[source]]

    # merge identical stints
    tmp <- merge_stints_identical(player_stints)
    player_stints <- tmp$player_stints
    merged_stints["identical"] <- merged_stints["identical"] + tmp$merged_stints

    # merge nested stints
    tmp <- merge_stints_nested(player_stints)
    player_stints <- tmp$player_stints
    merged_stints["nested"] <- merged_stints["nested"] + tmp$merged_stints
    conflict_stints["nested"] <- conflict_stints["nested"] + tmp$conflict_stints

    # merge consecutive stints
    tmp <- merge_stints_consecutive(player_stints)
    player_stints <- tmp$player_stints
    merged_stints["consecutive"] <- merged_stints["consecutive"] + tmp$merged_stints
    conflict_stints["consecutive"] <- conflict_stints["consecutive"] + tmp$conflict_stints

    # merge overlapping stints
    tmp <- merge_stints_overlapping(player_stints)
    player_stints <- tmp$player_stints
    merged_stints["overlapping"] <- merged_stints["overlapping"] + tmp$merged_stints
    conflict_stints["overlapping"] <- conflict_stints["overlapping"] + tmp$conflict_stints

    res_lst[[source]] <- player_stints
  }

  result <- list(res_lst, merged_stints=merged_stints, conflict_stints=conflict_stints)
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
identify_best_source <- function(seq_list) {
  # TODO
}
