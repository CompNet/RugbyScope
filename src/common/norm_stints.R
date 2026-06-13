########################################################################
# Functions used to normalize certain fields that describe rugby union
# stints. This processing is generic, i.e. not tied to a specific data
# source.
#
# Vincent Labatut
# 05/2026
########################################################################
library("stringi")

source("src/common/logging.R")




########################################################################
# Takes a stint table, loops over players and 1) detects overlapping stints
# 2) merges them if compatible. Here, compatible means :
# - same team
# - same period, or 
#   year1-year2 vs. NA-year2 or year1-NA, or
#   NA-year vs. NA-year, or
#   NA-year1 vs year2-NA with year1 >= year3, or
#   year-NA vs. year-NA, or
#   year1-NA vs. NA-year2 with year1 <= year2
#
# stints: stint table.
#
# returns: updated stint table.
########################################################################
merge_overlapping_stints_strict <- function(stints) {
  tlog(2, "Merging overlapping stints")

  # init result table
  result <- stints[-(1:nrow(stints)),]

  # get player list
  player_ids <- sort(unique(stints[, "playerId"]))

  for (p in 1:length(player_ids)) {
    player_id <- player_ids[p]
    tlog(4, "Processing player ", player_id, " (", p, "/", length(player_ids), ")")
    
    # retrieve existing stints for this player
    idx <- which(stints[, "playerId"] == player_id)
    idx2 <- c()
    idx3 <- c()
    if (length(idx) > 1) {
      for (s in 1:(length(idx)-1)) {
        team_id <- stints[idx[s], "teamRsId"]
        finished <- FALSE

        # compare teams
        idx1 <- idx[-(1:s)]
        idx2 <- idx1[stints[idx1, "teamRsId"] == team_id]
        idx3 <- c()
        if (length(idx2) > 0) {
          start_year <- stints[idx[s], "startYear"]
          end_year <- stints[idx[s], "endYear"]
          matches_played <- stints[idx[s], "matchesPlayed"]
          points_scored <- stints[idx[s], "pointsScored"]
          data_sources <- stints[idx[s], "dataSource"]
          update <- FALSE

          #### debug
          print(stints[idx[s], ])
          print(stints[idx2, ])
          tlog(6, stints[idx[s], "startYear"], "-", stints[idx[s], "endYear"])
          for (z in idx2)
            tlog(8, stints[z, "startYear"], "-", stints[z, "endYear"])

          # compare the start/end years to find a compatible stint
          if (is.na(start_year)) {
            if (is.na(end_year)) {
              # both start and end src years are NA: cannot be reliably matched to an existing stint
              # > nothing more to do
              finished <- TRUE
            } else {
              # start src year is NA and end src year is not: try to match only the latter to existing stints
              idx3 <- idx2[stints[idx2, "endYear"] == end_year]
              if (length(idx3) > 0) {
                # if at least one match: remove NAs
                if (any(!is.na(idx3))) {
                  idx3 <- idx3[!is.na(idx3)]
                # otherwise, all existing stints have an NA end year
                } else {
                  # we check the start year of the existing stints, and keep those with NAs or anterior years
                  idx3 <- idx2[(is.na(stints[idx2, "startYear"]) | stints[idx2, "startYear"] <= end_year) &
                            is.na(stints[idx2, "endYear"])]

                  # if there's only one of them, it will be merged with the src stint
                  # if not, then there's an issue because we can't choose between them
                }
              }
            }
          } else {
            if (is.na(end_year)) {
              # start src year is not NA but end src year is: try to match only the former to existing stints
              idx3 <- idx2[stints[idx2, "startYear"] == start_year]
              if (length(idx3) > 0) {
                # if at least one match: remove NAs
                if (any(!is.na(idx3))) {
                  idx3 <- idx3[!is.na(idx3)]
                # otherwise, all existing stints have an NA start year
                } else {
                  # we check the end year of the existing stints, and keep those with NAs or posterior years
                  idx3 <- idx2[is.na(stints[idx2, "startYear"]) &
                        (is.na(stints[idx2, "endYear"]) | stints[idx2, "endYear"] >= start_year)]
                  # if there's only one of them, it will be merged with the src stint
                  # if not, then there's an issue because we can't choose between them
                }
              }
            } else {
              # both start and end src years are non-NA: try matching both to existing stints
              idx3 <- idx2[stints[idx2, "startYear"] == start_year & stints[idx2, "endYear"] == end_year]
              if (length(idx3) > 0) {
                # if at least one complete match (both years): remove NAs
                if (any(!is.na(idx3))) {
                  idx3 <- idx3[!is.na(idx3)]
                # otherwise, all existing stints have an NA start and/or end year
                } else {
                  # we keep only existing stints with one year matching the src stint
                  idx3 <- idx2[(is.na(stints[idx2, "startYear"]) | stints[idx2, "startYear"] == start_year) & 
                        (is.na(stints[idx2, "endYear"])   | stints[idx2, "endYear"] == end_year)]
                }
              }
            }
          }

          # if there's a single match: possibly update stats
          if (!finished && length(idx3) == 1) {
            finished <- TRUE
            update <- TRUE
          }
          # if several matches: problem
          if (!finished && length(idx3) > 1) {
            finished <- TRUE
            tlog(6, "ERROR: Found several matching stints, which should not be possible")
            print(stints[idx[s], ])
            print(stints[idx3, ])
            update <- FALSE #stop("ERROR")
          }
    
          # possibly update stats
          if (update) {
            # debug
            tlog(6, "Updating the table")
        
            # we assume that the info already present is more reliable
            # and update only the NA fields from the merged table
            if (is.na(stints[idx3, "startYear"]))
              stints[idx3, "startYear"] <- start_year
            if (is.na(stints[idx3, "endYear"]))
              stints[idx3, "endYear"] <- end_year
            if (is.na(stints[idx3, "matchesPlayed"]) || !is.na(matches_played) && stints[idx3, "matchesPlayed"] < matches_played)
              stints[idx3, "matchesPlayed"] <- matches_played
            if (is.na(stints[idx3, "pointsScored"]) || !is.na(points_scored) && stints[idx3, "pointsScored"] < points_scored)
              stints[idx3, "pointsScored"] <- points_scored
            
            # update data sources
            ds1 <- trimws(unlist(strsplit(data_sources, ";")))
            ds2 <- trimws(unlist(strsplit(stints[idx3, "dataSource"], ";")))
            stints[idx3, "dataSource"] <- paste0(union(ds1, ds2), collapse = "; ")

            # debug
            print(stints[idx3, ])
          }
        }

        # if no similar stint, add new row to result table
        if (!finished) {
          # debug
          #tlog(6, "No matching stint: creating a new one")

          result <- rbind(result, stints[idx[s], ])
        }
      }

      # debug
      #readline(prompt="Press [enter] to continue")
    }

    # copy last stint
    result <- rbind(result, stints[idx[length(idx)], ])
  }
  
  # sort result table
  ids <- as.integer(substring(result[, "playerId"], first = 2, last = nchar(result[, "playerId"])))
  result <- result[order(ids, result[, "startYear"], result[, "endYear"], result[, "teamName"]), ]

  return(result)
}

#### test
# #setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# #source("src/common/norm_stints.R")

# start.rec.log("StintMerge")

# fusion_folder <- file.path("data", "fusion")
# fus_stints <- read.csv(file.path(fusion_folder, "stints_04_eswp.csv"))

# stints <- merge_overlapping_stints_strict(fus_stints)

# tab.file <- file.path(fusion_folder, "stints_04_eswp_v1.csv")
# write.csv(stints, tab.file, row.names = FALSE, fileEncoding = "UTF-8")

# end.rec.log()
####




########################################################################
# Order stints based on dates, and player and team names.
#
# stints: stint table.
#
# returns: ordered stint table.
########################################################################
order_stints <- function(stints) {
  # player ids
  player_ids <- stints[, "playerId"]
  player_ids <- as.integer(substr(player_ids, start = 2, stop = nchar(player_ids)))

  # start years
  start_years <- as.integer(stints[, "startYear"])
  start_years[is.na(start_years)] <- 1000

  # end years
  end_years <- as.integer(stints[, "endYear"])
  end_years[is.na(end_years)] <- 9999
  end_years <- -end_years # we want the largest stints first

  # team names
  team_names <- stints[, "teamName"]

  # order stints
  idx <- order(player_ids, start_years, end_years, team_names)
  stints <- stints[idx, ]

  return(stints)
}
# TODO
# - if start=end, not a loan but put before the longer stint
# - would be nice to put invitational and national stints at the end
