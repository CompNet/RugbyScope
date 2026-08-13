########################################################################
# Removes stints occurring under 18-yo, based on birth date.
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/fusion/remove_minor_stints.R")
########################################################################
source("src/common/logging.R")




########################################################################
# start logging
start.rec.log("RemoveMinorStints")




########################################################################
# load tables
source("src/stats/load_all_tables.R")

#data_folder <- file.path("data", "fusion")




########################################################################
tlog("Looping over players")

# loop over players
rem_flag <- c()
change_nbr <- 0
for (p in 1:nrow(players)) {
  player_id <- players[p, "wikidataId"]
  changes <- FALSE
#   if (p %% 1000 == 0)
    tlog(2, "------------------- Processing player ", player_id, " (", p, "/", nrow(players), ")")

  # we can only work if we know the birth year
  if (!is.na(players[p, "birthDate"])) {
    # compute majority year
    birth_year <- as.integer(format(players[p, "birthDate"], "%Y"))
    majority_year <- birth_year + 18
    tlog(4, "Majority year: ", majority_year)

    # process stints
    idx <- which(stints[, "playerId"] == player_id)

    # end year equal or before majority year => minor-age stint, should be removed
    idx2 <- idx[!is.na(stints[idx, "endYear"]) & stints[idx, "endYear"] <= majority_year]
    if (length(idx2) > 0) {
      tlog(4, "Removing the following stints:")
      print(stints[idx2, ])
      rem_flag <- c(rem_flag, idx2)
      changes <- TRUE
      change_nbr <- change_nbr + length(idx2)
    }

    # start year strictly before, and end year strictly after majority year => replace start year by majority year
    idx2 <- idx[!is.na(stints[idx, "startYear"]) & !is.na(stints[idx, "endYear"]) & stints[idx, "startYear"] < majority_year & majority_year < stints[idx, "endYear"]]
    if (length(idx2) > 0) {
      tlog(4, "Shortening existing stints:")
      print(stints[idx2, ])
      stints[idx2, "startYear"] <- majority_year
      tlog(4, "After having been shortened:")
      print(stints[idx2, ])
      changes <- TRUE
      change_nbr <- change_nbr + length(idx2)
    }

    # missing start year, and end year strictly after majority year => use majority year for start year
    idx2 <- idx[is.na(stints[idx, "startYear"]) & !is.na(stints[idx, "endYear"]) & majority_year < stints[idx, "endYear"]]
    if (length(idx2) > 0) {
      # ## debug phase
      # tlog(4, "Rare occurrence:")
      # print(stints[idx2, ])
      # tlog(4, "Rest of the stints:")
      # print(stints[idx, ])
      # readline("Press enter to continue")
      # changes <- TRUE

      tlog(2, "Setting missing start dates:")
      print(stints[idx2, ])
      stints[idx2, "startYear"] <- majority_year
      tlog(2, "After modification:")
      print(stints[idx2, ])
      changes <- TRUE
      change_nbr <- change_nbr + length(idx2)
    }

    # missing end year, and start year strictly before majority year => replace start year by majority year
    idx2 <- idx[!is.na(stints[idx, "startYear"]) & is.na(stints[idx, "endYear"]) & stints[idx, "startYear"] < majority_year]
    if (length(idx2) > 0) {
      # ## debug phase
      # tlog(4, "Rare occurrence:")
      # print(stints[idx2, ])
      # tlog(4, "Rest of the stints:")
      # print(stints[idx, ])
      # readline("Press enter to continue")
      # changes <- TRUE
      # change_nbr <- change_nbr + length(idx2)
      
        tlog(4, "Shortening existing stints:")
      print(stints[idx2, ])
      stints[idx2, "startYear"] <- majority_year
      tlog(4, "After having been shortened:")
      print(stints[idx2, ])
      changes <- TRUE
      change_nbr <- change_nbr + length(idx2)
    }
  } else {
    tlog(4, "No birth date available")
  }

#   if (changes)
#     readline("Press enter to continue")
}

# remove flagged stints
stints <- stints[-rem_flag, ]
tlog(2, "Number of stints removed or modified: ", length(rem_flag)) # 3,753 / 96,751 removals (4%)

# total number of modifications
tlog(2, "Number of modifications: ", change_nbr)  # 7,074 / 96,751 modifications (7%)




########################################################################
# record stint table
#tab.file <- file.path(data_folder, "stints_25_rm-minor-stints.csv")
tab.file <- file.path(data_folder, "stints_major.csv")
write.csv(stints, tab.file, row.names = FALSE, fileEncoding = "UTF-8")

# stop logging
end.rec.log()
