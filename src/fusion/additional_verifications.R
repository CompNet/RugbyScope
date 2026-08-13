########################################################################
# Performs additional verifications on stints.
#
# 08/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/fusion/additional_verifications.R")
########################################################################
source("src/common/logging.R")




########################################################################
# start logging
start.rec.log("AddVerifs")




########################################################################
# load tables
source("src/stats/load_all_tables.R")

data_folder <- file.path("data", "fusion")




########################################################################
# check barbarian stint dates

tlog("Checking Barbarian stints")
team_id <- "Q807749"
idx0 <- which(stints[, "teamWdId"] == team_id)

idx <- which(is.na(stints[idx0, "startYear"]) | is.na(stints[idx0, "endYear"]))
print(idx)
tlog(2, "Number of Barbarian stints with missing years: ", length(idx))




########################################################################
# check B&I Lions stint dates

tlog("Checking B&I Lions stints")
team_id <- "Q733600"
idx0 <- which(stints[, "teamWdId"] == team_id)

idx <- which(is.na(stints[idx0, "startYear"]) | is.na(stints[idx0, "endYear"]))
print(idx)
tlog(2, "Number of B&I Lions stints with missing years: ", length(idx))
print(stints[idx0[idx], ])

idx <- which(stints[idx0, "endYear"] - stints[idx0, "startYear"] > 1)
print(idx)
tlog(2, "Number of B&I Lions stints longer than a year: ", length(idx))
print(stints[idx0[idx], ])

# note: after verifications, touring every 4 years is quite recent, so no need to check that




########################################################################
# check ZA provinces stint types

tlog("Checking ZA provinces stint types")
team_ids <- c("Q885646", "Q1170721", "Q949807", "Q3046571", "Q266149", "Q1210146", "Q3116637", "Q1170716", "Q744636", "Q1777760", "Q1170708", "Q1170732", "Q1170726", "Q794910")
idx0 <- which(stints[, "teamWdId"] %in% team_ids)

#idx <- which((!is.na(stints[idx0, "type"]) & stints[idx0, "type"] == "Regional") & 
#             (!is.na(stints[idx0, "startYear"]) & stints[idx0, "startYear"] < 1995 | !is.na(stints[idx0, "endYear"]) & stints[idx0, "endYear"] <= 1995))
idx <- which((is.na(stints[idx0, "type"]) | stints[idx0, "type"] != "Regional") & 
             (!is.na(stints[idx0, "startYear"]) & stints[idx0, "startYear"] < 1995 | !is.na(stints[idx0, "endYear"]) & stints[idx0, "endYear"] <= 1995))
print(idx)
tlog(2, "Number of ZA stints with incorrect type: ", length(idx))
print(head(stints[idx0[idx], ]))

# note: after verification, there are more "amateur" than "regional" stints, so let's keep it like this




########################################################################
# # record stint table
# tab.file <- file.path(data_folder, "stints_25_additional-adjustments.csv")
# write.csv(stints, tab.file, row.names = FALSE, fileEncoding = "UTF-8")

# stop logging
end.rec.log()
