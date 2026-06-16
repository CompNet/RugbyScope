########################################################################
# Add (back) the stint types, which for some reason were discarded during
# the earlier data processing steps.
#
# 06/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/final/add_stint_types.R")
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
start.rec.log("AddStintTypes")




########################################################################
# paths
data_folder <- file.path("data", "fusion")

wp_folder <- file.path("data", "wikipedia")




########################################################################
# load WP stint tables

# stints
en_stints <- read.csv(file.path(wp_folder, "english", "stints.csv"))
fr_stints <- read.csv(file.path(wp_folder, "french", "stints.csv"))
it_stints <- read.csv(file.path(wp_folder, "italian", "stints.csv"))
ja_stints <- read.csv(file.path(wp_folder, "japanese", "stints.csv"))
# es_stints <- read.csv(file.path(wp_folder, "spanish", "stints.csv"))

# teams
en_teams <- read.csv(file.path(wp_folder, "english", "teams.csv"))
fr_teams <- read.csv(file.path(wp_folder, "french", "teams.csv"))
it_teams <- read.csv(file.path(wp_folder, "italian", "teams.csv"))
ja_teams <- read.csv(file.path(wp_folder, "japanese", "teams.csv"))
# es_teams <- read.csv(file.path(wp_folder, "spanish", "teams.csv"))




########################################################################
# load previously merged tables
tlog("Loading cleaned tables")

teams <- read.csv(file.path(data_folder, "teams_08.csv"))
tlog(2, "Number of teams: ", nrow(teams))

players <- read.csv(file.path(data_folder, "players_08.csv"))
tlog(2, "Number of players: ", nrow(players))

stints <- read.csv(file.path(data_folder, "stints_14.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# normalize stint types

# english
tlog("English version;")
print(table(en_stints[, "stintType"]))
en_map <- c(
  "amateur" = "Amateur",
  "Amateur team(s)" = "Amateur",
  "international" = "International",
  "International career" = "International",
  "senior_club" = "Senior",
  "Youth career" = "Amateur"
)
en_stints[, "stintType"] <- en_map[en_stints[, "stintType"]]
print(table(en_stints[, "stintType"]))

# french
tlog("French version;")
print(table(fr_stints[, "stintType"]))
fr_map <- c(
  "International" = "International",
  "Senior" = "Senior",
  "Senir" = "Senior",
  "Youth" = "Amateur"
)
fr_stints[, "stintType"] <- fr_map[fr_stints[, "stintType"]]
print(table(fr_stints[, "stintType"]))

# italian
tlog("Italian version;")
print(table(it_stints[, "stintType"]))
it_map <- c(
  "International" = "International",
  "Regional" = "Regional",
  "Senior" = "Senior",
  "Youth" = "Amateur"
)
it_stints[, "stintType"] <- it_map[it_stints[, "stintType"]]
print(table(it_stints[, "stintType"]))

# japanese
tlog("Japanese version;")
print(table(ja_stints[, "stintType"]))
ja_map <- c(
  "Amateur" = "Amateur",
  "International" = "International",
  "Regional" = "Regional",
  "Senior" = "Senior",
  "Youth" = "Amateur"
)
ja_stints[, "stintType"] <- ja_map[ja_stints[, "stintType"]]
print(table(ja_stints[,"stintType"]))

# spanish
#tlog("Spanish version;")
#print(table(es_stints[,"stintType"]))

# put everything in convenient lists
all_stints <- list(enWP=en_stints, frWP=fr_stints, itWP=it_stints, jaWP=ja_stints)
all_teams <- list(enWP=en_teams, frWP=fr_teams, itWP=it_teams, jaWP=ja_teams)




########################################################################
# handle regional teams
# [x] Rugby Football Union Team
# [x] Rugby Football Union
# [x] RFU Team
# [x] Rugby Union
# [x] Rugby Union Team
# [x] District
# [x] Country
# [x] XV
# [x] Division
# [x] Unión
# [x] County
# [x] Combined
#idx <- which(teams[, "type"] != "Regional team" & grepl("XV", teams[,"fullName"], ignore.case = FALSE, fixe = TRUE))
#print(teams[idx, c("rugbyscopeId","wikidataId","fullName","type","altNames")])
# fix incorrect stint types
conv <- match(stints[, "teamRsId"], teams[, "rugbyscopeId"])
idx <- which(stints[, "type"] != "Regional" & teams[conv, "type"] == "Regional team")
print(head(stints[idx, ]))
stints[idx, "type"] <- "Regional"


# handle national teams
#idx <- which(grepl("national", teams[, "type"], ignore.case = TRUE, fixed = TRUE) & grepl("national", teams[,"fullName"], ignore.case = FALSE, fixe = TRUE))
#print(teams[idx, c("fullName")])
# fix incorrect stint types
conv <- match(stints[, "teamRsId"], teams[, "rugbyscopeId"])
idx <- which(stints[, "type"] != "International" & grepl("National", teams[conv, "type"], ignore.case = TRUE, fixed = TRUE))
print(head(stints[idx, ]))
print(sort(unique(stints[idx, "teamName"])))
stints[idx, "type"] <- "International"




########################################################################
# match merged stints to WP stints

# add type column to the stints table
if(!"type" %in% colnames(stints))
  stints <- data.frame(stints[, 1:2], type=rep(NA, nrow(stints)), stints[, 3:ncol(stints)])

# open log files for debugging
con_mult <- file(file.path(data_folder, "stints_13_multiple.csv"), open = "w")
# con_none <- file(file.path(data_folder, "stints_13_unmatched.csv"), open = "w")

nbr_missed <- 0
nbr_multiple <- 0
fus_types <- c()
concerned_players <- c()
for (s in 2800:nrow(stints)) {
  tlog(2, "Processing stint ", s, "/", nrow(stints))

  # retrive data sources for this stint
  player_id <- stints[s, "playerId"]
  #sources <- setdiff(sort(unique(trimws(unlist(strsplit(stints[s, "dataSource"], ";"))))), "WD")
  sources <- names(all_stints)

  # retrieve stint types from original stint tables
  original_types <- c()
  plyr_matches <- list()
  for(source in sources) {
    # get the relevant tables
    old_stints <- all_stints[[source]]
    old_teams <- all_teams[[source]]

    # retrieve the team id
    team_id <- stints[s, "teamRsId"]
    tids <- which(old_teams[, "rugbyscopeId"] == team_id)

    if (length(tids) > 0) {
      old_team_wps <- old_teams[tids, "teamWP"]
      old_team_names <- old_teams[tids, "altNames"]

      # match stint to original tables
      idx <- which(old_stints[, "origWdId"] == stints[s, "playerId"])
      idx2 <- c()
      for (t in 1:length(tids)) {
        idx0 <- idx
        # compare WP URL
        if (!is.na(old_team_wps[t]))
          idx0 <- idx0[!is.na(old_stints[idx0, "teamWP"]) & old_stints[idx0, "teamWP"] == old_team_wps[t] | 
                      is.na(old_stints[idx0, "teamWP"]) & grepl(pattern = toupper(old_team_names[t]), x = toupper(old_stints[idx0, "teamName"]), fixed = TRUE)]
        else
          idx0 <- idx0[grepl(pattern = toupper(old_team_names[t]), x = toupper(old_stints[idx0, "teamName"]), fixed = TRUE)]
        
        # compare dates
          if (length(idx0) > 0) {
            if (is.na(stints[s, "startYear"])) {
              if (is.na(stints[s, "endYear"])) {
                idx0 <- idx0[is.na(old_stints[idx0, "startYear"]) & is.na(old_stints[idx0, "endYear"])]
              } else {
                idx0 <- idx0[is.na(old_stints[idx0, "startYear"])]
              }
            } else {
              if (is.na(stints[s, "endYear"])) {
                idx0 <- idx0[is.na(old_stints[idx0, "endYear"])]
              } else {
                idx0 <- idx0[(old_stints[idx0, "startYear"] >= stints[s, "startYear"] & old_stints[idx0, "startYear"] <= stints[s, "endYear"]) |
                             (old_stints[idx0, "endYear"] >= stints[s, "startYear"] & old_stints[idx0, "endYear"] <= stints[s, "endYear"])]
              }
            }
            # idx0 <- idx0[(is.na(old_stints[idx0, "startYear"]) & is.na(stints[s, "startYear"]) | !is.na(old_stints[idx0, "startYear"]) & !is.na(stints[s, "startYear"]) & old_stints[idx0, "startYear"] == stints[s, "startYear"]) |
            #              (is.na(old_stints[idx0, "endYear"]) & is.na(stints[s, "endYear"]) | !is.na(old_stints[idx0, "endYear"]) & !is.na(stints[s, "endYear"]) & old_stints[idx0, "endYear"] == stints[s, "endYear"])]
            idx2 <- c(idx2, idx0)
        }
      }
      idx <- sort(unique(idx2))
      plyr_matches[[source]] <- idx

      # get stint type
      if (length(idx) > 0)
        original_types <- c(original_types, old_stints[idx, "stintType"])
    }
  }
  original_types <- sort(unique(original_types))
  
  # # update stint table
  # if (length(original_types) > 0)
  #   stints[s, "type"] <- paste0(original_types, collapse = "; ")

  # # debug: no match
  # if (length(original_types) == 0) {
  #   # log issue
  #   nbr_missed <- nbr_missed + 1
  #   tlog(4, "Unmatched stint:")
  #   print(stints[s, ])
  #
  #   # export for debug
  #   concerned_players <- c(concerned_players, player_id)
  #   # write current stint's player's stints
  #   ps <- which(stints[, "playerId"] == player_id)
  #   for(r in ps) 
  #     writeLines(paste0(if(r == s) "***" else "", '"', paste0(stints[r, ], collapse='","'), '"'), con_none)
  #   writeLines("=========================", con_none)
  #   # write no-match source data
  #   for (source in sources) {
  #     old_stints <- all_stints[[source]]
  #     ps <- which(old_stints[, "origWdId"] == player_id)
  #     for(r in ps) 
  #       writeLines(paste0(paste0(old_stints[r, ], collapse='","'), '"'), con_none)
  #     writeLines(paste0("====================AAAA=", source), con_none)
  #   }
  #   writeLines("", con_none)  # player separator
  #
  # # debug: multiple matches
  # } else if (length(original_types) > 1) {
if (stints[s, "type"] %in% c("Amateur; Senior", "Amateur; Regional; Senior")) {
    # log issue
    nbr_multiple <- nbr_multiple + 1
    tlog(4, "Several matches: ", paste0(original_types, collapse = "; "))

    # # export for debug
    # concerned_players <- c(concerned_players, player_id)
    # # write current stint's player's stints
    # ps <- which(stints[, "playerId"] == player_id)
    # for(r in ps) 
    #   writeLines(paste0(if(r == s) "***" else "", '"', paste0(stints[r, ], collapse='","'), '"'), con_mult)
    # writeLines("=========================", con_mult)
    # # write conflicting source data
    # for (source in sources) {
    #   old_stints <- all_stints[[source]]
    #   ps <- which(old_stints[, "origWdId"] == player_id)
    #   for(r in ps) 
    #     writeLines(paste0(if(r %in% plyr_matches[[source]]) "***" else "", '"', paste0(old_stints[r, ], collapse='","'), '"'), con_mult)
    #   writeLines(paste0("====================AAAA=", source), con_mult)
    # }
    # writeLines("", con_mult)  # player separator

    # instead: ask the user to solve the issue and automatically split the concerned stint
    # show current stint's player's stints
    ps <- which(stints[, "playerId"] == player_id)
    for(r in ps) 
      tlog(4, paste0(if(r == s) "***" else "", '"', paste0(stints[r, ], collapse='","'), '"'))
    tlog(4, "=========================")
    # show conflicting source data
    for (source in sources) {
      old_stints <- all_stints[[source]]
      ps <- which(old_stints[, "origWdId"] == player_id)
      for(r in ps) 
        tlog(4, paste0(if(r %in% plyr_matches[[source]]) "***" else "", '"', paste0(old_stints[r, ], collapse='","'), '"'))
      tlog(4, paste0("====================AAAA=", source))
    }
    #readline("Type enter to solve the issue")
    answer <- readline("Enter the split year, or nothing to ignore the issue: ")
    if (answer != "") {
      # not splitting
      if (answer == "a") {
        stints[s, "type"] <- "Amateur"
        print(stints[s, ])
      } else if (answer == "s") {
        stints[s, "type"] <- "Senior"
        print(stints[s, ])
      } else {
        split_year <- as.integer(answer)
        senior_stint <- stints[s, ]
        # correct amateur stint
        stints[s, "type"] <- "Amateur"
        stints[s, "endYear"] <- split_year
        stints[s, "matchesPlayed"] <- NA
        stints[s, "pointsScored"] <- NA
        # add senior stint
        senior_stint[1, "type"] <- "Senior"
        senior_stint[1, "startYear"] <- split_year
        stints <- rbind(stints, senior_stint)
        # print for verifications
        print(stints[c(s, nrow(stints)),])
      }
      readline("Press enter to continue")
    }
  }

  # readline("Type enter to continue")
}
concerned_players <- sort(unique(concerned_players, player_id))
tlog("Number of stints that could not be matched: ", nbr_missed)
tlog("Number of stints matched to several original stints: ", nbr_multiple)

close(con_mult)
# close(con_none)

idx <- which(!(stints[, "playerId"] %in% concerned_players))
ok_stints <- stints[idx, ]
tab.file <- file.path(data_folder, "stints_13_ok.csv")
write.csv(ok_stints, tab.file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# stop logging
end.rec.log()











########################################################################
# look for perfect duplicates among stints
rem_flag <- c()
concerned_players <- c()
pids <- sort(unique(stints[, "playerId"]))
for (p in 1:length(pids)) {
  if (p %% 1000 == 0)
    tlog(2, "Processing player ", p, "/", length(pids))
  pid <- pids[p]
  player_stints <- which(stints[, "playerId"] == pid)
  if (length(player_stints) > 1) {
    for (s1 in 1:(length(player_stints) - 1)) {
      stint1 <- stints[player_stints[s1], ]
      stint1[1, is.na(stint1[1, ])] <- "NA"
      for (s2 in (s1 + 1):length(player_stints)) {
        stint2 <- stints[player_stints[s2], ]
        stint2[1, is.na(stint2[1, ])] <- "NA"
        if (all(stint1[1, ] == stint2[1, ])) {
          tlog(4, "------------------")
          print(stints[player_stints[c(s1, s2)], ])
          rem_flag <- c(rem_flag, player_stints[s2])
          concerned_players <- c(concerned_players, pid)
        }
      }
    }
  }
}
rem_flag <- sort(unique(rem_flag))
print(length(rem_flag))
concerned_players <- sort(unique(concerned_players))
print(length(concerned_players))














########################################################################
# handle pre-profesionnalism stints: should be amateur, not senior
switch_year <- 1995

#####################
# principle : no "senior" stint before 1995, then it means more or less pro after 1995
# algorithm: depends on the country, as they differ in how they switched to professionalism
#####################
# FR/EN/IT/JP:
# - <1995: all clubs "amateur"
# - >=1995: certain clubs switched to pro ("senior")
# NZ:
# - provinces: always "regional"
# - clubs: always "amateur"
# - franchises: always "senior"
# IE/WA/SC/AU :
# - clubs: always "amateur"
# - provinces/franchises: "regional" <1995 then "senior" >=1995
# ZA:
# - clubs: always "amateur"
# - provinces: "regional" <1995 then "senior" >=1995
# FJ/AR : everything "amateur" except certain teams:
# - FJ: Fijian Drua
# - AR: Jaguares
#####################

#### case: <1995 - <1995
idx <- which(!is.na(stints[, "startYear"]) & stints[, "startYear"] < switch_year & !is.na(stints[, "endYear"]) & stints[, "endYear"] < switch_year)
table(stints[idx, "type"])
sort(names(table(stints[idx, "startYear"])))
sort(names(table(stints[idx, "endYear"])))
# show specific cases
#idx2 <- idx[!is.na(stints[idx, "type"]) & stints[idx, "type"] == "Amateur; Regional; Senior"]
#print(stints[idx2, ])
#
#idx2 <- idx[!is.na(stints[idx, "type"]) & stints[idx, "type"] == "Amateur; Regional"]
#print(stints[idx2, ])
#
#idx2 <- idx[!is.na(stints[idx, "type"]) & stints[idx, "type"] == "International; Regional; Senior"]
#print(stints[idx2, ])
# clean stint types
map <- c(
  "Amateur; Senior" = "Amateur",
  "International; Senior" = "International",
  "Senior" = "Amateur"
)
for (m in 1:length(map)) {
  idx2 <- idx[!is.na(stints[idx, "type"]) & stints[idx, "type"] == names(map)[m]]
  print(stints[idx2, "type"])
  stints[idx2, "type"] <- map[m]
}

#### case: NA - <1995
idx <- which(is.na(stints[, "startYear"]) & !is.na(stints[, "endYear"]) & stints[, "endYear"] < switch_year)
table(stints[idx, "type"])
sort(names(table(stints[idx, "startYear"])))
sort(names(table(stints[idx, "endYear"])))
# clean stint types
map <- c(
  "Amateur; Senior" = "Amateur",
  "Senior" = "Amateur"
)
for (m in 1:length(map)) {
  idx2 <- idx[!is.na(stints[idx, "type"]) & stints[idx, "type"] == names(map)[m]]
  print(stints[idx2, "type"])
  stints[idx2, "type"] <- map[m]
}

#### case: <1995 - >=1995
idx <- which(!is.na(stints[, "startYear"]) & stints[, "startYear"] < switch_year & !is.na(stints[, "endYear"]) & stints[, "endYear"] > switch_year)
table(stints[idx, "type"])
sort(names(table(stints[idx, "startYear"])))
sort(names(table(stints[idx, "endYear"])))
years <- apply(stints[idx, c("startYear", "endYear")], 1, function(row) paste0(row, collapse = "-"))
table(years)
# show specific cases
idx2 <- idx[!is.na(stints[idx, "type"]) & stints[idx, "type"] == "Amateur; Senior"]
print(stints[idx2, ])
print(sort(unique(stints[idx2, "teamName"])))
# clean stint types
map <- c(
  "Senior" = "Amateur; Senior"
)
for (m in 1:length(map)) {
  idx2 <- idx[!is.na(stints[idx, "type"]) & stints[idx, "type"] == names(map)[m]]
  print(stints[idx2, "type"])
  stints[idx2, "type"] <- map[m]
}
# split around 1995: pre=>amateur, post=> senior
idx2 <- idx[!is.na(stints[idx, "type"]) & stints[idx, "type"] == "Amateur; Senior"]
print(stints[idx2, "type"])
head(stints[idx2, ])
print(sort(unique(stints[idx2, "teamName"])))
#### export list of teams to be annotated and used later
#tids <- sort(unique(stints[idx2, "teamRsId"]))
#tab <- teams[teams[, "rugbyscopeId"] %in% tids, c("rugbyscopeId", "fullName", "countries")]
#tab <- tab[order(tab[, "countries"], tab[, "rugbyscopeId"]), ]
#write.csv(cbind(tab, rep(NA, nrow(tab))), file.path(data_folder, "team_list.csv"), row.names = FALSE)
####
map <- read.csv(file = file.path(data_folder, "team_list.csv"))
# use the map to split stints
for (t in 1:nrow(map)) {
  tid <- map[t, "rugbyscopeId"]
  before <- map[t, "before"]
  after <- map[t, "after"]
  tlog(2, "Processing stint ", t, "/", nrow(map), " (", before, "-", after, ")")

  idx3 <- idx2[stints[idx2, "teamRsId"] == tid]
  if (length(idx3) > 0) {
    if (before == after) {
      print(stints[idx3, ])
      stints[idx3, "type"] <- before
      print(stints[idx3, ])
    } else {
      for (s in idx3) {
        new_stint <- stints[s, ]
        duration <- stints[s, "endYear"] - stints[s, "startYear"]
        matches_played <- stints[s, "matchesPlayed"]
        points_scored <- stints[s, "pointsScored"]
        # fix old stint
        stints[s, "type"] <- before
        stints[s, "endYear"] <- switch_year
        if (!is.na(matches_played))
          stints[s, "matchesPlayed"] <- round(matches_played / duration * (stints[s, "endYear"] - stints[s, "startYear"]))
        if (!is.na(points_scored))
          stints[s, "pointsScored"] <- round(points_scored / duration * (stints[s, "endYear"] - stints[s, "startYear"]))
        # add extra stint
        new_stint[1, "type"] <- after
        new_stint[1, "startYear"] <- switch_year
        if (!is.na(matches_played))
          new_stint[1, "matchesPlayed"] <- round(matches_played / duration * (new_stint[1, "endYear"] - new_stint[1, "startYear"]))
        if (!is.na(points_scored))
          new_stint[1, "pointsScored"] <- round(points_scored / duration * (new_stint[1, "endYear"] - new_stint[1, "startYear"]))
        stints <- rbind(stints, new_stint)
        # print for verifications
        print(stints[c(s, nrow(stints)),])
      }
    }
  }

  # readline("Type enter to continue")
}

#### case: <1995 - NA
idx <- which(!is.na(stints[, "startYear"]) & stints[, "startYear"] < switch_year & is.na(stints[, "endYear"]))
table(stints[idx, "type"])
sort(names(table(stints[idx, "startYear"])))
sort(names(table(stints[idx, "endYear"])))
# display all stints (not that many)
print(stints[idx, ])

# TODO

# show specific cases
idx2 <- idx[!is.na(stints[idx, "type"]) & stints[idx, "type"] == "Amateur; Regional; Senior"]
print(stints[idx2, ])
#
idx2 <- idx[!is.na(stints[idx, "type"]) & stints[idx, "type"] == "Amateur; Regional"]
print(stints[idx2, ])
#
idx2 <- idx[!is.na(stints[idx, "type"]) & stints[idx, "type"] == "International; Regional; Senior"]
print(stints[idx2, ])
# clean stint types
map <- c(
  "Amateur; Senior" = "Amateur",
  "International; Senior" = "International",
  "Senior" = "Amateur"
)
for (m in 1:length(map)) {
  idx2 <- idx[!is.na(stints[idx, "type"]) & stints[idx, "type"] == names(map)[m]]
  print(stints[idx2, "type"])
  stints[idx2, "type"] <- map[m]
}
