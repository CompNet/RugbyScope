########################################################################
# Loads the clean French Wikipedia tables and inserts them into our
# merged tables.
#
# 03/2025 Vincent Labatut
########################################################################
library("stringi")
library("stringr")
library("dplyr")
library("httr")
library("jsonlite")
library("magrittr")
library("polyglotr")

source("src/common/logging.R")
source("src/common/norm_names.R")
source("src/common/norm_teams.R")




########################################################################
# paths
wp_folder <- file.path("data", "wikipedia", "french")
#
fusion_folder <- file.path("data", "fusion")




########################################################################
# start logging
start.rec.log("IntegrationFrWP")




########################################################################
# load previously merged tables
tlog("Loading merged tables")

fus_teams <- read.csv(file.path(fusion_folder, "teams_03_jawp.csv"))
tlog(2, "Raw number of teams: ", nrow(fus_teams))

fus_players <- read.csv(file.path(fusion_folder, "players_02_jawp.csv"))
tlog(2, "Raw number of players: ", nrow(fus_players))

fus_stints <- read.csv(file.path(fusion_folder, "stints_01_wd-jawp.csv"))
tlog(2, "Raw number of stints: ", nrow(fus_stints))




########################################################################
# load FR WP tables
tlog("Loading Wikipedia FR tables")

wp_players <- read.csv(file.path(wp_folder, "players.csv"))
tlog(2, "Raw number of players: ", nrow(wp_players))
wp_players <- wp_players %>% mutate(across(where(is.character), ~ na_if(., "")))

wp_stints <- read.csv(file.path(wp_folder, "stints.csv"))
tlog(2, "Raw number of stints: ", nrow(wp_stints))
wp_stints <- wp_stints %>% mutate(across(where(is.character), ~ na_if(., "")))




########################################################################
# remove stints without a team
idx <- which(is.na(wp_stints[, "teamName"]))
if (length(idx) > 0)
  wp_stints <- wp_stints[-idx, ]
tlog(2, "Removed ", length(idx), " stints without a team")




# ########################################################################
# # merge players
# tlog("Merging players")

# # convert dob and dod into proper dates
# wp_players[, "birthDate"] %<>%  as.Date()
# wp_players[, "deathDate"] %<>%  as.Date()
# fus_players[, "birthDate"] %<>%  as.Date()
# fus_players[, "deathDate"] %<>%  as.Date()

# # match players from WP to the merged list
# idx <- match(wp_players[, "wikidataId"], fus_players[, "wikidataId"])
# tlog(2, "Successful player matches: ", length(which(!is.na(idx))), "/", nrow(wp_players))

# # insert WP info if field is empty in the merged table
# map <- c()  # merged <- wp
# map["birthDate"] <- "birthDate"
# map["birthPlaces"] <- "birthPlace"
# map["deathDate"] <- "deathDate"
# map["deathPlaces"] <- "deathPlace"
# map["fullName"] <- "fullName"
# map["heights"] <- "height"
# map["positions"] <- "positions"
# tlog(2, "Merging regular fields")
# total_changes <- rep(0, length(map))
# names(total_changes) <- names(map)
# for (p in 1:nrow(wp_players)) {
#   if (p %% 100 == 0)
#     tlog(4, "Processing player ", p, "/", nrow(wp_players))
#   filled_wp_cols <- which(!is.na(wp_players[p, map]))
#   empty_fus_cols <- which(is.na(fus_players[idx[p], names(map)]))
#   cols <- intersect(filled_wp_cols, empty_fus_cols)
#   if (length(cols) > 0) {
#     fus_players[idx[p], names(map)[cols]] <- wp_players[p, map[cols]]
#     total_changes[names(map)[cols]] <- total_changes[names(map)[cols]] + rep(1, length(cols))
#   }
# }
# tlog(2, "Total numbers of changes: ", sum(total_changes))
# tlog(4, "Fields: ", paste0(total_changes, collapse = ", "))
# print(total_changes)

# # only keep WP name as alt name, and only if it does not match current fullname
# tlog(2, "Copying WP names into alt name list")
# total_changes <- c(total_changes, 0)
# names(total_changes)[length(total_changes)] <- "altNames"
# full_names <- fus_players[, "fullName"]
# alt_names <- strsplit(fus_players[, "altNames"], "; ")
# fr_names <- wp_players[, "frName"]
# # loop over players to copy WP data
# for (p in 1:length(idx)) {
#   if (!is.na(fr_names[p]) && fr_names[p] != full_names[idx[p]]) {
#     # possibly complement list of alt names
#     if (all(is.na(alt_names[[idx[p]]])))
#       a_names <- fr_names[p]
#     else
#       a_names <- union(alt_names[[idx[p]]], fr_names[p])
    
#     # update in table
#     alt_names_new <- paste(a_names, collapse = "; ")
#     if (is.na(fus_players[idx[p], "altNames"]) || alt_names_new != fus_players[idx[p], "altNames"]) {
#       total_changes["altNames"] <- total_changes["altNames"] + 1
#       tlog(4, "(", full_names[idx[p]], ") ", fus_players[idx[p], "altNames"], " => ", alt_names_new)
#     }
#     fus_players[idx[p], "altNames"] <- alt_names_new
#   }
# }
# idx <- which(fus_players[, "altNames"] == "NA")
# fus_players[idx, "altNames"] <- NA
# tlog(2, "Total numbers of changes: ", sum(total_changes))
# tlog(4, "Fields: ", paste0(total_changes, collapse = ", "))
# print(total_changes)

# # record merged table as a new CSV file
# tab.file <- file.path(fusion_folder, "players_03_frwp.csv")
# tlog(2, "Recording as a CSV file: \"", tab.file, "\"")
# write.csv(fus_players, tab.file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# extract team table from stints
tlog("Extract team table from stints")
removed_teams <- c()

# clean team names
wp_stints[, "teamName"] <- gsub("\\[(\\d+|[a-z]+)\\]", "", wp_stints[, "teamName"], fixed = FALSE)
wp_stints[, "teamName"] <- gsub("→", "", wp_stints[, "teamName"], fixed = TRUE)
wp_stints[, "teamName"] <- gsub("(prêt)", "", wp_stints[, "teamName"], fixed = TRUE)
wp_stints[, "teamName"] <- trimws(wp_stints[, "teamName"])

# load url conversion map (to fix certain errors in the original data)
temp <- read.csv(file.path(wp_folder, "maps", "url2url.csv"))
map_urls <- temp[, "newUrl"]
names(map_urls) <- temp[, "oldUrl"]

# init table
cn <- c("rugbyscopeId", "altNames", "teamWP")
wp_teams <- data.frame(matrix(NA, nrow = 0, ncol = length(cn)))
colnames(wp_teams) <- cn

# populate table with unique names/urls
tlog(2, "Populate team table with unique names/urls")
alt_names <- list()
for (r in 1:nrow(wp_stints)) {
  team_name <- wp_stints[r, "teamName"]
  team_url <- wp_stints[r, "teamWP"]

  # no associated url
  if (is.na(team_url)) {
    # search by team name
    idx <- c()
    if (length(alt_names) > 0)
      idx <- which(sapply(alt_names, function(an) team_name %in% an))
    # none found: new entry
    if (length(idx) == 0) {
      wp_teams <- rbind(wp_teams, c(NA, NA, NA))
      colnames(wp_teams) <- cn
      alt_names <- c(alt_names, list(team_name))
    }
    # otherwise: nothing to do
  # name and url both available
  } else {
    # search by team url
    if (team_url %in% names(map_urls))
      team_url <- map_urls[team_url]
    idx <- which(wp_teams[, "teamWP"] == team_url)
    # none found: search by team name
    if (length(idx) == 0) {
      idx <- c()
      if (length(alt_names) > 0)
        idx <- which(sapply(alt_names, function(an) team_name %in% an))
      # found none, or some with different urls: create new entry
      if (length(idx) == 0 || all(!is.na(wp_teams[idx, "teamWP"]))) {
        wp_teams <- rbind(wp_teams, c(NA, NA, team_url))
        colnames(wp_teams) <- cn
        alt_names <- c(alt_names, list(team_name))
      # otherwise: update existing entry
      } else {
        idx0 <- which(is.na(wp_teams[idx, "teamWP"]))
        wp_teams[idx[idx0], "teamWP"] <- team_url
      }
    # update existing entry
    } else {
      nn <- union(alt_names[[idx]], team_name)
      alt_names[[idx]] <- nn
    }
  }
# print(alt_names)
# readline(prompt="Press [enter] to continue")
# if(any(sapply(alt_names,length) > 1))
#   {print(r);print(alt_names[sapply(alt_names,length) > 1])}
# if(r==192)
# stop("")
}
tlog(4, "Found ", nrow(wp_teams), " unique teams")
wp_teams[, "altNames"] <- sapply(alt_names, function(an) paste0(an, collapse = "; "))
#### debug: take a look at teams with multiple names, some are associated to very generic url
#### and should not be merged (ex. NZ url associated to NZ, U20 NZ, U21 NZ...)
#idx <- which(grepl(";", wp_teams[, "altNames"], fixed = TRUE) & !is.na(wp_teams[, "teamWP"]))
#tab <- cbind(wp_teams[idx, ], paste0("http://ja.wikipedia.org/wiki/", wp_teams[idx, "teamWP"]))
#colnames(tab)[ncol(tab)] <- "fullUrl"
#tab <- cbind(rep(NA, nrow(tab)), tab)
#colnames(tab)[1] <- "translation"
#for (r in 1:nrow(tab)) {
#   tlog(4, "Translating name ", r, "/", nrow(tab))
#   orig <- tab[r, "altNames"]
#   go_on <- TRUE
#   while (go_on) {
#     response <- tryCatch({create_translation_table(words = orig, languages = "en")}, error = function(e) {tlog("Server error: ", e$message); NA})
#     if (all(is.na(response))) {
#       Sys.sleep(2)
#       tlog("Server error: retrying")
#     } else {
#       go_on <- FALSE
#       tab[r, "translation"] <- response[1, "en"]
#     }
#   }
#}
#write.csv(tab, file.path(wp_folder, "multiple_names.csv"), row.names = FALSE, fileEncoding = "UTF-8")
#### we use the above file to manually correct stints.csv by disambiguating URLs

    idx <- which(sapply(alt_names,length)>1)
    wp_teams <- wp_teams[idx,]


#### debug: record team table for visualizing
tab.file <- file.path(wp_folder, "teams.csv")
tlog(2, "Recording as a CSV file: \"", tab.file, "\"")
write.csv(wp_teams, tab.file, row.names = FALSE, fileEncoding = "UTF-8")
#### "Otago","[^O\d\?-]
#### "[^S\d][^"]*","Stade_fran%C3%A7ais_Paris_rugby

#### debug: check teams with the same name but a different URL (some URLs are incorrect)
#dup_names <- names(which(table(wp_teams[, "altNames"]) > 1))
#tab <- wp_teams[-(1:nrow(wp_teams)), ]
#for (dupp_name in dup_names) {
#  idx <- which(wp_teams[, "altNames"] == dupp_name)
#  tab <- rbind(tab, wp_teams[idx, ])
#}
#write.csv(tab, file.path(wp_folder, "duplicate_names.csv"), row.names = FALSE, fileEncoding = "UTF-8")
#### the above code was used to define the url2url.csv map, allowing to solve specific cases of
#### the same team being associated to several distinct URLs. the map associates an incorrect url
#### to a correct one, and the substitution is made in the loop that builds the wp_teams table
#### Note: a few duplicates remain, but these are highschools, which are removed later

# we first focus on teams possessing a URL, as they are easier to match
# match teams using WP URLs
tlog(2, "Match WP teams to merged table based on urls")
non_na <- which(!is.na(wp_teams[, "teamWP"]))
tlog(4, "Found ", length(non_na), "/", nrow(wp_teams), " WP teams  with a URL")
unique_urls <- trimws(wp_teams[non_na, "teamWP"])
unique_urls <- unique_urls[!grepl("redlink=1", unique_urls, fixed = TRUE)]
unique_urls <- unique_urls[!startsWith(unique_urls, "#")]
matches <- cbind(match(unique_urls, fus_teams[, "wikipediaEn"]), match(unique_urls, fus_teams[, "wikipediaFr"]), match(unique_urls, fus_teams[, "wikipediaIt"]), match(unique_urls, fus_teams[, "wikipediaEs"]), match(unique_urls, fus_teams[, "wikipediaJa"]))
mm <- apply(matches, 1, function(row) {
  res <- unique(row[!is.na(row)])
  if (length(res) == 0)
    res <- NA
  return(res)
})
idx <- which(!is.na(mm))
tlog(4, "Could match directly ", length(idx), "/", length(unique_urls), " non-NA URLs to entries in merged table")
wp_teams[non_na, "rugbyscopeId"] <- fus_teams[mm, "rugbyscopeId"]
#length(which(!is.na(wp_teams[, "rugbyscopeId"])))
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")

# use the URLs to retrieve from WP the English names of unmatched teams
tlog(2, "Retrieving missing English team names")
idx <- which(!is.na(wp_teams[, "teamWP"]) & is.na(wp_teams[, "rugbyscopeId"]))
failed <- c()
for (i in 1:length(idx)) {
  team_url <- trimws(wp_teams[idx[i], "teamWP"])
  # if(!grepl("redlink=1", team_url, fixed = TRUE) && !startsWith(team_url, "#")) {
  tlog(4, "Retrieving translation for \"", team_url, "\" (", i, "/", length(idx), ")")
  title <- get_english_title(team_url, lang = "fr")
  #tlog(6, "Result: ", title)
  if (is.null(title))
    failed <- c(failed, team_url)
  else
    alt_names[[idx[i]]] <- union(alt_names[[idx[i]]], list(title))
  # Sys.sleep(1)
}
tlog(4, "Could retrieve the name associated to ", (length(idx) - length(failed)), "/", length(idx), " unmatched teams possessing a URL")
tlog(4, "Still missing the name associated to ", length(failed), "/", length(idx), " unmatched teams possessing a URL")
wp_teams[, "altNames"] <- sapply(alt_names, function(an) paste0(an, collapse = "; "))
#### debug
#write.csv(failed, file.path(wp_folder, "no-name_urls.csv"), row.names = FALSE, fileEncoding = "UTF-8")
#### at this stage, we use the above file to define manually the url2id.csv and url2name.csv map files
#### each no-name url must be associated to an id (if team present in merged table) or a WP page title (otherwise)

# use url2id map to match the remaining cases based on their URL
tlog(2, "Handle remaining teams possessing a URL, using manually constituted url2id map")
temp <- read.csv(file.path(wp_folder, "maps", "url2id.csv"))
map_ids <- temp[, "teamId"]
names(map_ids) <- temp[, "url"]
wp_idx <- match(names(map_ids), wp_teams[, "teamWP"])
# handle teams present in merged table: just update id in WP table
fus_idx <- match(as.integer(map_ids), fus_teams[, "rugbyscopeId"])
ii <- which(!is.na(wp_idx) & !is.na(fus_idx))
wp_teams[wp_idx[ii], "rugbyscopeId"] <- fus_teams[fus_idx[ii], "rugbyscopeId"]
tlog(4, "Could match directly ", length(ii), " teams based on ids")
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")
# handle teams absent from merged table: create them
temp <- read.csv(file.path(wp_folder, "maps", "id2name.csv"))
map_names <- temp[, "fullName"]
names(map_names) <- temp[, "wikidataId"]
supp <- fus_teams[-(1:nrow(fus_teams)), ]
rsid <- max(fus_teams[, "rugbyscopeId"]) + 1
r <- 1
del_rows <- c()
for (i in 1:length(map_ids)) {
  if (is.na(fus_idx[i])) {
    # if high school team: mark for removal
    if (grepl("([Hh]igh|[sS]econdary) [sS]chool", map_names[map_ids[i]], fixed = FALSE)) {
      del_rows <- c(del_rows, wp_idx[i])
    # otherwise: insert in merged table
    } else {
      supp[r, "rugbyscopeId"] <- rsid
      wp_teams[wp_idx[i], "rugbyscopeId"] <- rsid
      rsid <- rsid + 1
      supp[r, "wikipediaFr"] <- names(map_ids)[i]
      supp[r, "wikidataId"] <- map_ids[i]
      supp[r, "countries"] <- "France"
      supp[r, "fullName"] <- map_names[map_ids[i]]
      supp[r, "altNames"] <- wp_teams[matches[i], "altNames"]
      r <- r + 1
    }
  }
}
removed_teams <- c(removed_teams, wp_teams[del_rows, "altNames"])
wp_teams <- wp_teams[-del_rows, ]
tlog(4, "Removed ", length(del_rows), " highschool team from the WP table")
fus_teams <- rbind(fus_teams, supp)
tlog(4, "Created ", nrow(supp), " new teams in the merged table")
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")

# use url2name map to match the remaining cases based on their URL
tlog(2, "Handle remaining teams possessing a URL, using their English name")
temp <- read.csv(file.path(wp_folder, "maps", "url2name.csv"))
map_names <- temp[, "fullName"]
names(map_names) <- temp[, "url"]
# remove high schools etc.
idx <- which(grepl("([Hh]igh|[sS]econdary) [sS]chool", map_names, fixed = FALSE) | map_names == "Samurai Seven")
rows <- which(wp_teams[, "teamWP"] %in% names(map_names[idx]))
#print(wp_teams[rows, "rugbyscopeId"])
map_names <- map_names[-idx]
tlog(4, "Removed ", length(rows), " high school teams from the WP table")
removed_teams <- c(removed_teams, wp_teams[rows, "altNames"])
wp_teams <- wp_teams[-rows, ]
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")
# add new entries to merged table
wp_idx <- match(names(map_names), wp_teams[, "teamWP"])
supp <- fus_teams[-(1:nrow(fus_teams)), ]
rsid <- max(fus_teams[, "rugbyscopeId"]) + 1
r <- 1
for (i in 1:length(map_names)) {
  #tlog(4, "Processing row ", r, "/", length(map_names))
  supp[r, "rugbyscopeId"] <- rsid
  wp_teams[wp_idx[i], "rugbyscopeId"] <- rsid
  rsid <- rsid + 1
  supp[r, "wikipediaFr"] <- names(map_names)[i]
  supp[r, "countries"] <- "France"
  supp[r, "fullName"] <- map_names[i]
  supp[r, "altNames"] <- wp_teams[wp_idx[i], "altNames"]
  r <- r + 1
}
fus_teams <- rbind(fus_teams, supp)
tlog(4, "Had to create ", nrow(supp), " new teams in merged table")
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")

#### debug: check that the failed teams from before (for which no English name could 
#### be directly retrieved) are now matched to a team in the merged team table
#idx <- match(failed, wp_teams[, "teamWP"])
#idx <- idx[which(!is.na(idx))]
#length(which(is.na(wp_teams[idx, "rugbyscopeId"])))
#### the value displayed above should be zero: all teams have been either removed or matched

## from now on, we work with the unmatched teams that have an English name and a URL

# first, remove some superfluous teams
tlog(2, "Handle remaining teams using their English name")
# remove high school teams and others
idx <- which(grepl("([Hh]igh|[sS]econdary) [sS]chool", wp_teams[, "altNames"], fixed = FALSE))
removed_teams <- c(removed_teams, wp_teams[idx, "altNames"])
wp_teams <- wp_teams[-idx, ]
tlog(4, "Removed ", length(idx), " highschool teams")
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")
# remove sevens team
idx <- which(grepl("\\b[Ss]evens\\b", wp_teams[, "altNames"], fixed = FALSE))
removed_teams <- c(removed_teams, wp_teams[idx, "altNames"])
wp_teams <- wp_teams[-idx, ]
tlog(4, "Removed ", length(idx), " rugby sevens teams")
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")

# then match WP names in merged table
# combine merged team names in a single list
fus_names1 <- fus_teams[, "fullName"]
fus_names2 <- fus_teams[, "altNames"]
fus_names <- sapply(1:length(fus_names1), function(i) {
  if (is.na(fus_names2[i]))
    fus_names1[i]
  else
    paste0(fus_names1[i], "; ", fus_names2[i])
})
# search WP names in merged table
#fus_idx <- which(!(fus_teams[, "rugbyscopeId"] %in% wp_teams[, "rugbyscopeId"]))
wp_idx <- which(is.na(wp_teams[, "rugbyscopeId"]) & !is.na(wp_teams[, "teamWP"]))
result <- match_team_names(src_names = wp_teams[wp_idx, "altNames"], tgt_names = fus_names)
#### debug
# handle multiple matching case (note: there should be none)
#idx <- which(sapply(result, length) > 1)
##for (i in idx) result[[i]] <- result[[i]][1]
##result <- unlist(result)
# use matches to update WP team table
idx <- which(!is.na(result))
tlog(4, "Could match ", length(idx), " teams based on English name")
#### debug (check matches)
#tab <- cbind(wp_teams[wp_idx[idx], "altNames"], fus_names[result[idx]], fus_teams[result[idx], "rugbyscopeId"], wp_teams[wp_idx[idx], "teamWP"])
#write.csv(tab, file.path(wp_folder, "successful_matches.csv"), row.names = FALSE, fileEncoding = "UTF-8")
wp_teams[wp_idx[idx], "rugbyscopeId"] <- fus_teams[result[idx], "rugbyscopeId"]
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")

# focusing on the remaining teams with a URL but not matched
wp_idx <- wp_idx[-idx]
tab <- wp_teams[wp_idx, ]
#### debug
#write.csv(tab, file.path(wp_folder, "unmatched_teams.csv"), row.names = FALSE, fileEncoding = "UTF-8")
#### at this stage, we use the above file to determine whether some teams can be matched manually
#### if it is the case: we add them to url2ids.csv (and we apply the above process again)
#### the rest will be considered as new teams: created and inserted in the merged table (see below)

# add the remaining unmatched teams possessing a URL as new teams in the merged table
supp <- fus_teams[-(1:nrow(fus_teams)), ]
rsid <- max(fus_teams[, "rugbyscopeId"]) + 1
r <- 1
for (i in 1:nrow(tab)) {
  #tlog(4, "Processing row ", r, "/", length(map_names))
  supp[r, "rugbyscopeId"] <- rsid
  wp_teams[wp_idx[i], "rugbyscopeId"] <- rsid
  rsid <- rsid + 1
  supp[r, "wikipediaFr"] <- tab[i, "teamWP"]
  supp[r, "countries"] <- "France"
  names <- strsplit(tab[i, "altNames"], "; ")[[1]]
  en_name <- NA
  fr_names <- c()
  for (name in names) {
    if (grepl("[A-Za-z]+", name, fixed = FALSE) && is.na(en_name))
      en_name <- name
    else
      fr_names <- c(fr_names, name)
  }
  supp[r, "fullName"] <- en_name
  supp[r, "altNames"] <- paste0(fr_names, collapse = "; ")
  r <- r + 1
}
# update certain teams' countries based on name2country.csv map
temp <- read.csv(file.path(wp_folder, "maps", "name2country.csv"))
map_ctry <- temp[, "country"]
names(map_ctry) <- temp[, "teamName"]
idx <- which(!is.na(match(supp[, "fullName"], names(map_ctry))))
for (team in names(map_ctry))
  supp[which(supp[, "fullName"] == team), "countries"] <- map_ctry[team]
# add to merged table
fus_teams <- rbind(fus_teams, supp)
tlog(4, "Had to create ", nrow(supp), " new teams in merged table")
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")

#### debug: remaining unmatched teams that have a URL
#idx1 <- which(is.na(wp_teams[, "rugbyscopeId"]))
#idx2 <- which(!is.na(wp_teams[, "teamWP"]))
#idx <- intersect(idx1, idx2)
#write.csv(wp_teams[idx, c("altNames", "teamWP")], file.path(wp_folder, "unnamed_urls.csv"), row.names = FALSE, fileEncoding = "UTF-8")
# at this stage, all WP teams with a URL should have been matched
# and therefore, the above file should be empty

## we now switch to names only, as the remaining WP teams do not have a URL

# use manually defined map to match certain teams
tlog(2, "Use manually defined map to match certain teams based on their french name")
temp <- read.csv(file.path(wp_folder, "maps", "name2id.csv"))
map_names <- temp[, "rugbyscopeId"]
names(map_names) <- temp[, "frName"]
# remove teams associated with name NA
idx <- match(names(map_names), wp_teams[, "altNames"])
idx_rem <- which(is.na(map_names))
if (length(idx_rem) > 0) {
  removed_teams <- c(removed_teams, wp_teams[idx[idx_rem], "altNames"])
  wp_teams <- wp_teams[-(idx[idx_rem]), ]
  map_names <- map_names[-idx_rem]
}
# update the other teams
idx <- match(names(map_names), wp_teams[, "altNames"])
wp_teams[idx, "rugbyscopeId"] <- map_names
#### entries of name2id.csv not used anymore (keeping them just in case)
# "Auckland Institute of Technology","オークランド工科大学","6911"
# "Moseley","モーズリー","26"
# "Nice","ニース","276"
# "North Harbor","ノース・ハーバー","2904"
# "Plymouth","プリマス","589"
# "Shangwu","尚武",NA
# "Viadana","ヴィアダーナ","329"

# translate all remaining names to english using polyglotr
idx_noid <- which(is.na(wp_teams[, "rugbyscopeId"]))                      # 422
en_names <- c()
fr_names <- c()
for (r in 1:length(idx_noid)) {
  tlog(4, "Translating name ", r, "/", length(idx_noid))
  orig <- wp_teams[idx_noid[r], "altNames"]

  # send to server
  go_on <- TRUE
  while (go_on) {
    response <- tryCatch({create_translation_table(words = orig, languages = "en")}, error = function(e) {tlog("Server error: ", e$message); NA})
    if (all(is.na(response))) {
      Sys.sleep(2)
      tlog("Server error: retrying")
    } else {
      go_on <- FALSE
      translation <- response[1, "en"]
    }
  }

  tlog(6, "\"", orig, "\" >> \"", translation, "\"")
  en_names <- c(en_names, translation)
  fr_names <- c(fr_names, orig)
  #Sys.sleep(1)
}
unique_names <- sort(unique(en_names))                                    # 383
#### debug: export translated names, for visualization
#tab <- cbind(unique_names, wp_teams[idx_noid[match(unique_names, en_names)], "altNames"], rep("", length(unique_names)))
#colnames(tab) <- c("fullName", "fr_names", "teamId")
#write.csv(tab, file.path(wp_folder, "remaining_names.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# filter out remaining high schools
idx_uhs <- which(grepl("[Hh]igh [Ss]chool", unique_names, fixed = FALSE)) # 167
idx_ehs <- which(en_names %in% unique_names[idx_uhs])                     # 181
unique_names <- unique_names[-idx_uhs]                                    # 210
en_names <- en_names[-idx_ehs]                                            # 229
fr_names <- fr_names[-idx_ehs]                                            # 229
removed_teams <- c(removed_teams, wp_teams[idx_noid[idx_ehs], "altNames"])
wp_teams <- wp_teams[-(idx_noid[idx_ehs]), ]                              # 1172
idx_noid <- which(is.na(wp_teams[, "rugbyscopeId"]))                      # 229

# try matching the teams by name
result <- match_team_names(src_names = unique_names, tgt_names = fus_names)
#### debug: check names with several matches
#idx <- which(sapply(result, length) > 1)
#for (i in idx) {
#   print(unique_names[i])
#   print(fus_teams[result[[i]], "fullName"])
#   ii <- which(en_names == unique_names[i])
#   team <- wp_teams[idx_noid[ii], "altNames"]
#   ii <- match(team, wp_stints[, "teamName"])
#   print(wp_stints[ii, ])
#   print("-------------------")
#}
#### the above loop is used to detect cases of multiple matching
#### and solve them manually by adding them to the map in file name2id.csv

# at this stage, we assume there is no multiple matches
idx_um <- which(!is.na(result))
idx_em <- which(en_names %in% unique_names[idx_um])
map <- match(en_names[idx_em], unique_names[idx_um])
tlog(4, "Could match ", length(idx_um), " teams based on English name")
#### debug (check matches)
#tab <- cbind(wp_teams[idx_noid[idx_em], "altNames"], fr_names[idx_em], en_names[idx_em], fus_names[result[idx_um]][map], fus_teams[result[idx_um][map], "rugbyscopeId"], wp_teams[idx_noid[idx_em], "teamWP"])
#colnames(tab) <- c("altNames", "frName", "enName", "fullName", "rugbyscopeId", "teamWp")
#write.csv(tab, file.path(wp_folder, "successful_matches.csv"), row.names = FALSE, fileEncoding = "UTF-8")
#### this file is used to visually verify that the matches obtained above are correct
#### errors can be corrected by complementing the name2id.csv file used before, to force manual matching
wp_teams[idx_noid[idx_em], "rugbyscopeId"] <- fus_teams[result[idx_um][map], "rugbyscopeId"]
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")

#### debug
#idx_um <- which(is.na(result))
#idx_em <- which(en_names %in% unique_names[idx_um])
#idx_plyr <- match(fr_names[idx_em], wp_stints[, "teamName"])
#tab <- cbind(en_names[idx_em], fr_names[idx_em], rep(NA, length(idx_em)), wp_stints[idx_plyr, "wpPage"])
#colnames(tab) <- c("fullName", "frName", "rugbyscopeId", "playerWP")
#idx <- order(en_names[idx_em])
#write.csv(tab[idx,], file.path(wp_folder, "unmatched_teams.csv"), row.names = FALSE, fileEncoding = "UTF-8")
#### at this stage, it was not possible to match the remaining teams automatically
#### therefore, we exported the list of remaining unmatched teams so that they could be matched manually
#### through the map already defined in file name2id.csv (and complemetented based on the above list)
#### the rest will be considered as new teams: created and inserted in the merged table (see above)

# reading new teams listed manually in the new_teams.csv
tab <- read.csv(file.path(wp_folder, "maps", "new_teams.csv"))
supp <- fus_teams[-(1:nrow(fus_teams)), ]
rsid <- max(fus_teams[, "rugbyscopeId"]) + 1
r <- 1
for (r in 1:nrow(tab)) {
  # update temp table
  supp[r, "rugbyscopeId"] <- rsid
  supp[r, "countries"] <- tab[r, "countries"]
  supp[r, "fullName"] <- tab[r, "fullName"]
  supp[r, "altNames"] <- tab[r, "altNames"]
  supp[r, "type"] <- tab[r, "type"]
  # update WP team table
  fr_name <- tab[r, "altNames"]
  idx <- which(wp_teams[, "altNames"] == fr_name)
  if (length(idx) == 0)
    stop("Could not find entry ", r, " (", fr_name, ")")
  wp_teams[idx, "rugbyscopeId"] <- rsid
  rsid <- rsid + 1
  r <- r + 1
}
# add to merged table
fus_teams <- rbind(fus_teams, supp)
tlog(4, "Had to create ", nrow(supp), " new teams in merged table")
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")

#### debug: check the remaining unmatched teams (should be empty)
#idx <- which(is.na(wp_teams[, "rugbyscopeId"]))
#write.csv(tab[idx,], file.path(wp_folder, "unmatched_teams.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# add merged table name to FR entry, for visual verification
idx <- match(wp_teams[, "rugbyscopeId"], fus_teams[, "rugbyscopeId"])
wp_teams[, "fusionName"] <- fus_teams[idx, "fullName"]

#### Debug: list entries from name2id.csv that refer to teams missing from the original 
#### merged table and added during the previous steps of this script
#idx <- which(wp_teams[, "rugbyscopeId"] == "TODO")
#print(wp_teams[idx, ])
#### we used this list to complement name2id.csv appropriately

#### debug: check RS id duplicates (several WP teams with the same id)
#idx <- as.integer(names(which(table(wp_teams[, "rugbyscopeId"]) > 1)))
#for (i in idx) {
#  print("------------------------")
#  ii <- which(wp_teams[, "rugbyscopeId"] == i)
#  print(wp_teams[ii, ])
#}
#### there are many cases: it is expected, there are many slightly different french names

# record final WP team table as a new CSV file, for verification
tab.file <- file.path(wp_folder, "teams.csv")
tlog(2, "Recording as a CSV file: \"", tab.file, "\"")
write.csv(wp_teams, tab.file, row.names = FALSE, fileEncoding = "UTF-8")

# complement alternative names in merged table, using french names
tlog("Complement alternative names in merged team table")
idx <- match(wp_teams[, "rugbyscopeId"], fus_teams[, "rugbyscopeId"])
fus_full_names <- fus_teams[idx, "fullName"]
wp_alt_names <- strsplit(wp_teams[, "altNames"], ";")
for (r in 1:length(idx)) {
  # original alt names
  ofn <- fus_full_names[r]
  oan <- trimws(strsplit(fus_teams[idx[r], "altNames"], ";")[[1]])
  if (all(is.na(oan)))
    oan <- c()
  # wp altnames
  wan <- trimws(wp_alt_names[[r]])
  ii <- which(sapply(wan, function(w) grepl("[\u3040-\u309f\u30a0-\u30ff\u4e00-\u9faf\u3400-\u4dbf]+", w, fixed = FALSE))) # check if the string contains at least two french characters
  # combine altnames
  an <- setdiff(union(oan, wan[ii]), ofn)
  if (all(is.na(an)) || length(an) == 0)
    san <- NA
  else
    san <- paste0(an, collapse = "; ")
  if (!is.na(san) & !is.na(fus_teams[idx[r], "altNames"]) & san != fus_teams[idx[r], "altNames"] | !is.na(san) & is.na(fus_teams[idx[r], "altNames"])) {
    tlog(2, "Changed name for team ", r, "/", length(idx), ":")
    tlog(4, "Original: ", fus_teams[idx[r], "altNames"])
    tlog(4, "Revised:  ", san)
    fus_teams[idx[r], "altNames"] <- san
  }
}

# record merged table as a new CSV file
tab.file <- file.path(fusion_folder, "teams_04_frwp.csv")
tlog(2, "Recording as a CSV file: \"", tab.file, "\"")
write.csv(fus_teams, tab.file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# merge stints
tlog("Merging stints")
removed_teams <- unique(trimws(unlist(strsplit(removed_teams, ";"))))
#### debug: directly load the file to bypass all previous processing
#wp_teams  <- read.csv(file.path(wp_folder, "teams.csv"))
#fus_teams <- read.csv(file.path(fusion_folder, "teams_04_frwp.csv"))
#### debug: reload data table for quick testing
#wp_stints <- read.csv(file.path(wp_folder, "stints.csv"))
#wp_stints <- wp_stints %>% mutate(across(where(is.character), ~ na_if(., "")))
#wp_stints[, "teamName"] <- gsub("（英語版）", "", wp_stints[, "teamName"], fixed = TRUE)    # "english version" (of a team name)
#wp_stints[, "teamName"] <- gsub("（フランス語版）", "", wp_stints[, "teamName"], fixed = TRUE) # "french version"
#idx <- which(is.na(wp_stints[, "teamName"]))
#if (length(idx) > 0)
#   wp_stints <- wp_stints[-idx, ]

# reorder stint table to respect wikidataId / names
ids <- fus_stints[, "playerId"]
ids <- as.integer(substr(ids, start = 2, stop = nchar(ids)))
idx <- order(ids, fus_stints[, "playerName"], fus_stints[, "startYear"], fus_stints[, "endYear"], fus_stints[, "teamName"])
fus_stints <- fus_stints[idx, ]

# connect WP stint to WP team tables (and so merged teams)
tlog(2, "Matching stint teams to team table")
wp_stints <- cbind(wp_stints, rep(NA, nrow(wp_stints)))
colnames(wp_stints)(ncol(wp_stints)) <- "rugbyscopeId"
del_raws <- c()
alt_names <- strsplit(wp_teams[, "altNames"], ";")
alt_names <- lapply(alt_names, trimws)
for (r in 1:nrow(wp_stints)) {
  team_name <- wp_stints[r, "teamName"]
  tlog(4, "Processing stint ", r, "/", nrow(wp_stints), " (", team_name, ")")
  # possibly ignore the team if it is in the ignore list
  if (team_name %in% removed_teams)
    del_raws <- c(del_raws, r)
  else {
    # try to match using exact comparison
    idx <- which(sapply(alt_names, function(names) all(team_name == names)))
    if (length(idx) == 0)
      # try to match using one of the names
      idx <- which(sapply(alt_names, function(names) team_name %in% names))
    # multiple matches
    if (length(idx) > 1) {
      ids <- wp_teams[idx, "rugbyscopeId"]
      # if they all agree on the merged team match, no pb
      if (all(ids[1] == ids[-1]))
        idx <- idx[1]
      # otherwise, use URL to disambiguate
      else {
        team_url <- wp_stints[r, "teamWP"]
        if (!is.na(team_url)) {
          idx <- idx[which(wp_teams[idx, "teamWP"] == team_url)]
          # if no match at all
          if (length(idx) == 0) {
              print(team_name)
              print(team_url)
              print(wp_teams[idx, ])
              stop("No match, could not find the URL: ", r)
          # if multiple matches again
          } else if (length(idx) > 1) {
            ids <- wp_teams[idx, "rugbyscopeId"]
            # if they all agree on the merged team match, no pb
            if (all(ids[1] == ids[-1]))
              idx <- idx[1]
            # otherwise, error
            else {
              print(team_name)
              print(wp_teams[idx, ])
              stop("Too many matches: ", r)
            }
          }
        # no url to compare 
        } else {
          print(team_name)
          print(wp_teams[idx, ])
          stop("Too many matches, no URL to disambiguate: ", r)
        }
      }
    } else if (length(idx) == 0) {
      print(team_name)
      print(wp_teams[idx, ])
      stop("No match, could not find the name: ", r)
    }
    # get id
    wp_stints[r, "rugbyscopeId"] <- wp_teams[idx, "rugbyscopeId"]
  }
}

# delete rows corresponding to removed teams
tlog(2, "Delete ", length(del_raws), "/", nrow(wp_stints), " stints corresponding to ", length(removed_teams), " removed teams")
wp_stints <- wp_stints[-del_raws, ]

# add new column in stint table, corresponding to RS id
rs_ids <- match(fus_stints[, "teamId"], fus_teams[, "wikidataId"])
fus_stints <- cbind(fus_stints[, 1:3], rs_ids, fus_stints[, 4:ncol(fus_stints)])
colnames(fus_stints)[3:4] <- c("teamWdId", "teamRsId")
# add new column to indicate the data source of each stint
fus_stints <- cbind(fus_stints, rep("WD", nrow(fus_stints)))
colnames(fus_stints)[ncol(fus_stints)] <- "dataSource"

# insert WP stint in merged table
for (r in 1:nrow(wp_stints)) {
  player_id <- wp_stints[r, "origWdId"]
  tlog(4, "Processing stint ", r, "/", nrow(wp_stints), " (player ", player_id, ")")
  team_id <- wp_stints[r, "rugbyscopeId"]
  found <- FALSE
# print(wp_stints[r, ])

  # retrieve existing stints for this player
  idx <- which(fus_stints[, "playerId"] == player_id)
  if (length(idx) > 0) {
    # compare teams
    idx2 <- idx[fus_stints[idx, "teamRsId"] == team_id]
    if (length(idx2) > 0) {
      start_year <- wp_stints[r, "startYear"]
      end_year <- wp_stints[r, "endYear"]
      matches_played <- wp_stints[r, "matchesPlayed"]
      points_scored <- wp_stints[r, "pointsScored"]
      update <- FALSE

      if (is.na(start_year)) {
        if (is.na(end_year)) {
          # both start and end years contain NA > cannot be reliably matched to an existing stint
          # > nothing more to do
          found <- TRUE
        } else {
          # start year is NA and end year is not: try to match only the latter
          idx3 <- idx2[fus_stints[idx2, "endYear"] == end_year]
          if (length(idx3) == 1 && is.na(idx3)) {
            idx3 <- idx2[is.na(fus_stints[idx2, "endYear"]) | fus_stints[idx2, "endYear"] == end_year]
            # both start and end years contain NA > cannot be reliably matched to an existing stint
            found <- TRUE
          }
        }
      } else {
        if (is.na(end_year)) {
          # start year is not NA but end year is: try to match only the former
          idx3 <- idx2[fus_stints[idx2, "startYear"] == start_year]
          if (length(idx3) == 1 && is.na(idx3)) {
            idx3 <- idx2[is.na(fus_stints[idx2, "startYear"]) | fus_stints[idx2, "startYear"] == start_year]
            # both start and end years contain NA > cannot be reliably matched to an existing stint
            found <- TRUE
          }
        } else {
          # both start and end years are non-NA: try matching both
          idx3 <- idx2[fus_stints[idx2, "startYear"] == start_year & fus_stints[idx2, "endYear"] == end_year]
          # remove NA (ie merged table with NA start and/or end year) only if several matches
          if (length(idx3) > 1)
            idx3 <- idx3[-is.na(idx)]
          else if (length(idx3) == 1 && is.na(idx3))
            idx3 <- idx2[is.na(fus_stints[idx2, "startYear"]) | fus_stints[idx2, "startYear"] == start_year & 
                         is.na(fus_stints[idx2, "endYear"]) | fus_stints[idx2, "endYear"] == end_year]
          # # if no match at all: consider missing years in merged table
          # if (length(idx3) == 0)
          #   idx3 <- idx2[fus_stints[idx2, "startYear"] == start_year & is.na(fus_stints[idx2, "endYear"])]
          # if (length(idx3) == 0)
          #   idx3 <- idx2[is.na(fus_stints[idx2, "startYear"]) & fus_stints[idx2, "endYear"] == end_year]
        }
      }

      # if a single match: possibly update stats
      if (!found && length(idx3) == 1) {
        found <- TRUE
        update <- TRUE
      }
      # if several matches: problem
      if (!found && length(idx3) > 1) {
        found <- TRUE
        tlog(6, "Found several matching stints, which is not normal")
        print(wp_stints[r, ])
        print(fus_stints[idx3, ])
        stop("ERROR")
      }
      
      # possibly update stats
      if (update) {
# print(fus_stints[idx3, ])
        # we assume that the info already present is more reliable
        # and update only the NA fields from the merted table
        if (is.na(fus_stints[idx3, "startYear"]))
          fus_stints[idx3, "startYear"] <- start_year
        if (is.na(fus_stints[idx3, "endYear"]))
          fus_stints[idx3, "endYear"] <- end_year
        if (is.na(fus_stints[idx3, "matchesPlayed"]))
          fus_stints[idx3, "matchesPlayed"] <- matches_played
        if (is.na(fus_stints[idx3, "pointsScored"]))
          fus_stints[idx3, "pointsScored"] <- points_scored
        # add WP as source if agreement on any non-NA field 
        changes <- fus_stints[idx3, c("startYear", "endYear", "pointsScored", "pointsScored")] != c(start_year, end_year, matches_played, points_scored)
        if (!all(is.na(changes)) && any(changes, na.rm = TRUE))
          fus_stints[idx3, "dataSource"] <- paste0(fus_stints[idx3, "dataSource"], "; frWP")
      }
    }
  }

  # if no similar stint, add to merged table
  if (!found) {
    rr <- nrow(fus_stints) + 1
    fus_stints[rr, "playerId"] <- player_id
    fus_stints[rr, "playerName"] <- fus_players[fus_players[, "wikidataId"] == player_id, "fullName"]
    fus_stints[rr, "teamWdId"] <- fus_teams[fus_teams[, "rugbyscopeId"] == team_id, "wikidataId"]
    fus_stints[rr, "teamRsId"] <- team_id
    fus_stints[rr, "teamName"] <- fus_teams[fus_teams[, "rugbyscopeId"] == team_id, "fullName"]
    fus_stints[rr, "startYear"] <- wp_stints[r, "startYear"]
    fus_stints[rr, "endYear"] <- wp_stints[r, "endYear"]
    fus_stints[rr, "matchesPlayed"] <- wp_stints[r, "matchesPlayed"]
    fus_stints[rr, "pointsScored"] <- wp_stints[r, "pointsScored"]
    fus_stints[rr, "dataSource"] <- "frWP"
  }
}

# reorder stint table to respect wikidataId / names
ids <- fus_stints[, "playerId"]
ids <- as.integer(substr(ids, start = 2, stop = nchar(ids)))
idx <- order(ids, fus_stints[, "playerName"], fus_stints[, "startYear"], fus_stints[, "endYear"], fus_stints[, "teamName"])
fus_stints <- fus_stints[idx, ]

# record as a new CSV file
fus_stints[,"teamRsId"] <- as.integer(fus_stints[,"teamRsId"])
tab.file <- file.path(fusion_folder, "stints_02_frwp.csv")
tlog(2, "Recording as a CSV file: \"", tab.file, "\"")
write.csv(fus_stints, tab.file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# stop logging
end.rec.log()
