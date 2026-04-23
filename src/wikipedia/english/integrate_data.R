########################################################################
# Loads the clean English Wikipedia tables and inserts them into our
# merged tables.
#
# 05/2025 Vincent Labatut
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




########################################################################
# paths
wp_folder <- file.path("data", "wikipedia", "english")
#
fusion_folder <- file.path("data", "fusion")
#
ref_folder <- file.path("data", "references", "league")



########################################################################
# start logging
start.rec.log("IntegrationEnWP")




########################################################################
# load previously merged tables
tlog("Loading merged tables")

fus_teams <- read.csv(file.path(fusion_folder, "teams_06_eswp.csv"))
tlog(2, "Raw number of teams: ", nrow(fus_teams))

fus_players <- read.csv(file.path(fusion_folder, "players_05_eswp.csv"))
tlog(2, "Raw number of players: ", nrow(fus_players))

fus_stints <- read.csv(file.path(fusion_folder, "stints_04_eswp.csv"))
tlog(2, "Raw number of stints: ", nrow(fus_stints))




########################################################################
# load EN WP tables
tlog("Loading Wikipedia EN tables")

wp_players <- read.csv(file.path(wp_folder, "players.csv"))
tlog(2, "Raw number of players: ", nrow(wp_players))
wp_players <- wp_players %>% mutate(across(where(is.character), ~ na_if(., "")))

wp_stints <- read.csv(file.path(wp_folder, "stints.csv"))
tlog(2, "Raw number of stints: ", nrow(wp_stints))
wp_stints <- wp_stints %>% mutate(across(where(is.character), ~ na_if(., "")))




########################################################################
# merge players
tlog("Merging players")

# convert dob and dod into proper dates
wp_players[, "birthDate"] %<>%  as.Date()
wp_players[, "deathDate"] %<>%  as.Date()
fus_players[, "birthDate"] %<>%  as.Date()
fus_players[, "deathDate"] %<>%  as.Date()

# merge country and place of birth/death
wp_players[, "birthPlace"] <- sapply(1:nrow(wp_players), function(i) {
  if (is.na(wp_players[i, "birthCountry"]))
    return(wp_players[i, "birthPlace"])
  else
    return(paste0(wp_players[i, "birthPlace"], "; ", wp_players[i, "birthCountry"]))
})
wp_players[, "deathPlace"] <- sapply(1:nrow(wp_players), function(i) {
  if (is.na(wp_players[i, "deathCountry"]))
    return(wp_players[i, "deathPlace"])
  else
    return(paste0(wp_players[i, "deathPlace"], "; ", wp_players[i, "deathCountry"]))
})

# normalize player names case
wp_players[, "enName"] <- str_to_title(wp_players[, "enName"])

# match players from WP to the merged list
idx <- match(wp_players[, "wikidataId"], fus_players[, "wikidataId"])
tlog(2, "Successful player matches: ", length(which(!is.na(idx))), "/", nrow(wp_players))

# insert WP info if field is empty in the merged table
map <- c()  # merged <- wp
map["birthDate"] <- "birthDate"
map["birthPlaces"] <- "birthPlace"
map["deathDate"] <- "deathDate"
map["deathPlaces"] <- "deathPlace"
map["fullName"] <- "fullName"
map["heights"] <- "height"
map["weights"] <- "weight"
map["positions"] <- "positions"
tlog(2, "Merging regular fields")
total_changes <- rep(0, length(map))
names(total_changes) <- names(map)
for (p in 1:nrow(wp_players)) {
  if (p %% 1000 == 0)
    tlog(4, "Processing player ", p, "/", nrow(wp_players))
  filled_wp_cols <- which(!is.na(wp_players[p, map]))
  empty_fus_cols <- which(is.na(fus_players[idx[p], names(map)]))
  cols <- intersect(filled_wp_cols, empty_fus_cols)
  if (length(cols) > 0) {
    fus_players[idx[p], names(map)[cols]] <- wp_players[p, map[cols]]
    total_changes[names(map)[cols]] <- total_changes[names(map)[cols]] + rep(1, length(cols))
  }
}
tlog(2, "Total numbers of changes: ", sum(total_changes))
tlog(4, "Fields: ", paste0(total_changes, collapse = ", "))
print(total_changes)

# only keep WP name as alt name, and only if it does not match current fullname
tlog(2, "Copying WP names into alt name list")
total_changes <- c(total_changes, 0)
names(total_changes)[length(total_changes)] <- "altNames"
full_names <- fus_players[, "fullName"]
alt_names <- strsplit(fus_players[, "altNames"], "; ")
en_names <- wp_players[, "enName"]
# loop over players to copy WP data
for (p in 1:length(idx)) {
  if (!is.na(en_names[p]) && str_to_upper(en_names[p]) != str_to_upper(full_names[idx[p]])) {
    # possibly complement list of alt names
    if (all(is.na(alt_names[[idx[p]]])))
      a_names <- en_names[p]
    else {
#      a_names <- union(alt_names[[idx[p]]], en_names[p])
      a_names <- c(alt_names[[idx[p]]], en_names[p])
      a_names <- a_names[!duplicated(str_to_upper(a_names))]
    }
    
    # update in table
    alt_names_new <- paste(a_names, collapse = "; ")
    if (is.na(fus_players[idx[p], "altNames"]) || str_to_upper(alt_names_new) != str_to_upper(fus_players[idx[p], "altNames"])) {
      total_changes["altNames"] <- total_changes["altNames"] + 1
      tlog(4, "(", full_names[idx[p]], ") ", fus_players[idx[p], "altNames"], " => ", alt_names_new)
    }
    fus_players[idx[p], "altNames"] <- alt_names_new
  }
}
idx <- which(fus_players[, "altNames"] == "NA")
if (length(idx) > 0)
  fus_players[idx, "altNames"] <- NA
tlog(2, "Total numbers of changes: ", sum(total_changes))
tlog(4, "Fields: ", paste0(total_changes, collapse = ", "))
print(total_changes)
#head(sort(wp_players[, "enName"]),n=40)

############################### 
############################### 
# -  D did not retrive stints with no date at all
# - why not including the stint type (junior, senior, etc.) in the table?
############################### 
############################### 


# record merged table as a new CSV file
tab.file <- file.path(fusion_folder, "players_06_enwp.csv")
tlog(2, "Recording as a CSV file: \"", tab.file, "\"")
write.csv(fus_players, tab.file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# extract team table from stints
tlog("Extract team table from stints")
removed_teams <- c()

# TODO TEMPORARILY remove stints with missing team names
idx <- which(wp_stints[, "teamName"] %in% c("(d/r)", "(medical joker)", "(loan)", "(on loan)", "(permit)", "(trial)", "(amateur)", "[", "]", "1", "2", "3", "4", "5"))
if (length(idx) > 0)
  wp_stints <- wp_stints[-idx, ]

# clean team names
wp_stints[, "teamName"] <- gsub("→", "", wp_stints[, "teamName"], fixed = TRUE)
#
# wp_stints[, "teamName"] <- gsub("\\[(\\d+|[a-z]+)\\]", "", wp_stints[, "teamName"], fixed = FALSE)
wp_stints[, "teamName"] <- gsub("-+>", "", wp_stints[, "teamName"], fixed = FALSE)
wp_stints[, "teamName"] <- gsub(" ?\\(amateur\\)$", "", wp_stints[, "teamName"], fixed = FALSE)
wp_stints[, "teamName"] <- gsub(" ?\\(guest\\)$", "", wp_stints[, "teamName"], fixed = FALSE)
wp_stints[, "teamName"] <- gsub(" ?\\(training squad\\)$", "", wp_stints[, "teamName"], fixed = FALSE)
wp_stints[, "teamName"] <- gsub(" ?\\(trial\\)$", "", wp_stints[, "teamName"], fixed = FALSE)
wp_stints[, "teamName"] <- gsub(" ?\\(dual-registration\\)$", "", wp_stints[, "teamName"], fixed = FALSE)
wp_stints[, "teamName"] <- gsub(" ?\\(player-coach\\)$", "", wp_stints[, "teamName"], fixed = FALSE)
wp_stints[, "teamName"] <- gsub(" ?\\(d[/\\-]r\\)$", "", wp_stints[, "teamName"], fixed = FALSE)
wp_stints[, "teamName"] <- gsub(" ?- on loan$", "", wp_stints[, "teamName"], fixed = FALSE)
wp_stints[, "teamName"] <- trimws(wp_stints[, "teamName"])
idx <- which(wp_stints[, "teamName"] %in% c("?", "", "NA"))
if (length(idx) > 0)
  wp_stints[idx, "teamName"] <- NA
#### debug: show team list
#head(sort(unique(wp_stints[, "teamName"])),100)
#tail(sort(unique(wp_stints[, "teamName"])),100)
####

# insert new teams based on the manually curated list new_teams
tlog(2, "Import manually curated additional teams and insert into merged team table")
temp <- read.csv(file.path(wp_folder, "maps", "new_teams.csv"))
max_id <- max(fus_teams[, "rugbyscopeId"])
temp <- cbind(max_id:(max_id + nrow(temp) - 1) + 1, temp)
colnames(temp)[1] <- "rugbyscopeId"
fus_teams <- rbind(fus_teams, temp)

# fix url problems based on the manually curated url2url map
tlog(2, "fix url problems based on the manually curated url2url map")
temp <- read.csv(file.path(wp_folder, "maps", "url2url.csv"))
map_urls <- temp[, "newUrl"]
names(map_urls) <- temp[, "oldUrl"]
for (i in 1:length(map_urls)) {
  old_url <- names(map_urls[i])
  new_url <- map_urls[i]
  idx <- which(wp_stints[, "teamWP"] == old_url)
  if (length(idx) == 0)
    tlog(4, "WARNING: did not find any team with URL ", old_url)
  else
    wp_stints[idx, "teamWP"] <- new_url
}

# remove stints without a team
idx <- which(is.na(wp_stints[, "teamName"]))
if (length(idx) > 0)
  wp_stints <- wp_stints[-idx, ]
tlog(2, "Removed ", length(idx), " stints without a team")

# init WP team table
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
}
tlog(4, "Found ", nrow(wp_teams), " unique teams")
wp_teams[, "altNames"] <- sapply(alt_names, function(an) paste0(an, collapse = "; "))
#### debug: take a look at teams with multiple names, some are associated to very generic url
#### and should not be merged (ex. NZ url associated to NZ, U20 NZ, U21 NZ...)
#idx <- which(sapply(alt_names, length) > 1)
#tab <- wp_teams[idx,]
#tab.file <- file.path(wp_folder, "duplicate_urls.csv")
#tlog(2, "Recording as a CSV file: \"", tab.file, "\"")
#write.csv(tab, tab.file, row.names = FALSE, fileEncoding = "UTF-8")
#### "Otago","[^O\d\?-]
#### "[^S\d][^"]*","Stade_fran%C3%A7ais_Paris_rugby
#### we use the above file to manually correct stints.csv by disambiguating URLs

#### debug: check teams with the same name but a different URL (some URLs are incorrect)
#dup_names <- names(which(table(wp_teams[, "altNames"]) > 1))
#tab <- wp_teams[-(1:nrow(wp_teams)), ]
#for (dupp_name in dup_names) {
# idx <- which(wp_teams[, "altNames"] == dupp_name)
# tab <- rbind(tab, wp_teams[idx, ])
#}
#write.csv(tab, file.path(wp_folder, "duplicate_names.csv"), row.names = FALSE, fileEncoding = "UTF-8")
#### the above code was used to define the url2url.csv map, allowing to solve specific cases of
#### the same team being associated to several distinct URLs. the map associates an incorrect url
#### to a correct one, and the substitution is made in the loop that builds the wp_teams table
#### Note: a few duplicates remain, but they correspond to teams that are removed later (highschools, rugby league, etc.)

# remove sevens teams
tlog(2, "Removing rugby sevens teams")
del_rows <- which(grepl("\\b[sS]evens?\\b", wp_teams[, "altNames"], fixed = FALSE) | grepl("[\\W_/,.][sS]evens?[\\W_/,.]", wp_teams[, "teamWP"], fixed = FALSE) |
                  grepl("\\b7s?\\b", wp_teams[, "altNames"], fixed = FALSE) | grepl("[\\W_/,.]7s[\\W_/,.]", wp_teams[, "teamWP"], fixed = FALSE) |
                  grepl("\\bVII\\b", wp_teams[, "altNames"], fixed = FALSE, ignore.case = TRUE) | grepl("[\\W_/,.]VII[\\W_/,.]", wp_teams[, "teamWP"], fixed = FALSE, ignore.case = TRUE))
#write.csv(wp_teams[del_rows, ], file.path(wp_folder, "rugby_sevens_teams.csv"), row.names = FALSE, fileEncoding = "UTF-8")
removed_teams <- c(removed_teams, wp_teams[del_rows, "altNames"])
wp_teams <- wp_teams[-del_rows, ]
tlog(4, "Removed ", length(del_rows), " rugby sevens teams from the WP table")
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")

# remove rugby league teams
tlog(2, "Removing rugby league teams")
#### debug: code used to detect rugby league teams (first approximation)
#idx <- which(grepl("\\b[Ll]eague\\b", wp_teams[, "altNames"], fixed = FALSE) | grepl("[\\W_/,.][Ll]eague[\\W_/,.]", wp_teams[, "teamWP"], fixed = FALSE))
#tab <- wp_teams[idx, ]  # 420 teams detected
#idx <- order(tab[, "altNames"])
#write.csv(tab[idx, -1], file.path(ref_folder, "rugby_league_teams.csv"), row.names = FALSE, fileEncoding = "UTF-8")
#### this list was then completed manually and used below
# use the rugby league urls
list_rleague <- read.csv(file.path(ref_folder, "team_urls.csv"))
del_rows <- which(wp_teams[, "teamWP"] %in% list_rleague[, "url"])
removed_teams <- c(removed_teams, wp_teams[del_rows, "altNames"])
wp_teams <- wp_teams[-del_rows, ]
tlog(4, "Removed ", length(del_rows), " rugby league teams from the WP table, based on their URL")
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")
# use the rugby league names
list_rleague <- read.csv(file.path(ref_folder, "team_names.csv"))
del_rows <- which(wp_teams[, "altNames"] %in% tolower(list_rleague[, "name"]))
if (length(del_rows) > 0) {
  removed_teams <- c(removed_teams, wp_teams[del_rows, "altNames"])
  wp_teams <- wp_teams[-del_rows, ]
}
tlog(4, "Removed ", length(del_rows), " rugby league teams from the WP table, based on their name")
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")

# remove junior teams (highschool, U17, etc.)
tlog(2, "Removing junior teams")
del_rows <- which(grepl("\\bhigh ?school\\b", wp_teams[, "altNames"], fixed = FALSE, ignore.case = TRUE) |
                  grepl("\\bgrammar\\b", wp_teams[, "altNames"], fixed = FALSE, ignore.case = TRUE) |
                  grepl("\\bunder[ -]1[3-7]\\b", wp_teams[, "altNames"], fixed = FALSE, ignore.case = TRUE) |
                  grepl("\\bacademy\\b", wp_teams[, "altNames"], fixed = FALSE, ignore.case = TRUE))                # >>>> TODO check that on definitive data
#write.csv(wp_teams[del_rows, ], file.path(wp_folder, "junior_teams.csv"), row.names = FALSE, fileEncoding = "UTF-8")
removed_teams <- c(removed_teams, wp_teams[del_rows, "altNames"])
wp_teams <- wp_teams[-del_rows, ]
tlog(4, "Removed ", length(del_rows), " highschool teams from the WP table")
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")

# remove women's teams
del_rows <- which(grepl("\\bwallaroos?\\b", wp_teams[, "altNames"], fixed = FALSE, ignore.case = TRUE))                # >>>> TODO check that on definitive data
#write.csv(wp_teams[del_rows, ], file.path(wp_folder, "women_teams.csv"), row.names = FALSE, fileEncoding = "UTF-8")
removed_teams <- c(removed_teams, wp_teams[del_rows, "altNames"])
wp_teams <- wp_teams[-del_rows, ]
tlog(4, "Removed ", length(del_rows), " women's teams from the WP table")
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")

# we first focus on teams possessing a URL, as they are easier to match

# match teams using WP URLs
tlog(2, "Match WP teams to merged table based on urls")
non_na <- which(!is.na(wp_teams[, "teamWP"]))
tlog(4, "Found ", length(non_na), "/", nrow(wp_teams), " WP teams with a URL")
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
#### debug: record the list of unmatched teams with a URL
#idx <- which(is.na(mm))
#write.csv(unique_urls[idx], file.path(wp_folder, "unmatched_urls.csv"), row.names = FALSE, fileEncoding = "UTF-8")
#### we use the above file to define manually the url2url map (used above), url2id map (used below) and bew_teams list

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
                    # handle teams marked for deletion (NA id) in url2id
                    ii <- which(!is.na(wp_idx) & is.na(fus_idx))
                    del_rows <- wp_idx[ii]
                    removed_teams <- c(removed_teams, wp_teams[del_rows, "altNames"])
                    wp_teams <- wp_teams[-del_rows, ]
                    tlog(4, "Removed ", length(del_rows), " highschool teams or other similar teams from the WP table")
                    tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")

## we now switch to names only, as the remaining WP teams do not have a URL

# use manually defined map to match certain teams
tlog(2, "Use manually defined map to match certain teams based on their name")
temp <- read.csv(file.path(wp_folder, "maps", "name2id.csv"))
map_names <- temp[, "rugbyscopeId"]
names(map_names) <- temp[, "fullName"]
# remove teams associated with value NA in the map
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
tlog(4, "Matched ", length(idx), " WP teams")
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")

# now trying to match teams by name
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
wp_idx <- which(is.na(wp_teams[, "rugbyscopeId"]))
result <- match_team_names(src_names = wp_teams[wp_idx, "altNames"], tgt_names = fus_names)
#### debug: check names with several matches
#idx <- which(sapply(result, length) > 1)
#for (i in idx) {
#  print(wp_teams[wp_idx[i], ])
#  print(fus_teams[result[[i]], ])
#  print("-------------------")
#}
#### the above loop is used to detect cases of multiple matching
# use matches to update WP team table
                                                                                result <- sapply(result, function(x) x[1])
idx <- which(!is.na(result))
tlog(4, "Could match ", length(idx), " teams based on name only")
#### debug (check matches)
#tab <- cbind(wp_teams[wp_idx[idx], "altNames"], fus_names[result[idx]], fus_teams[result[idx], "rugbyscopeId"], wp_teams[wp_idx[idx], "teamWP"])
#colnames(tab) <- c("wpName", "fusName", "rugbyscopeId", "teamWP")
#write.csv(tab, file.path(wp_folder, "successful_matches.csv"), row.names = FALSE, fileEncoding = "UTF-8")
#### the above file is meant for visual inspection and verification
wp_teams[wp_idx[idx], "rugbyscopeId"] <- fus_teams[result[idx], "rugbyscopeId"]
tlog(6, "Remaining: ", length(which(is.na(wp_teams[, "rugbyscopeId"]))), "/", nrow(wp_teams), " WP teams to match")

#### debug: export the list of unmatched teams
#idx <- which(is.na(wp_teams[, "rugbyscopeId"]))
#tab <- wp_teams[idx, c("altNames", "rugbyscopeId")]
#idx <- order(tab[, "altNames"])
#tab <- tab[idx, ]
#colnames(tab)[1] <- "teamName"
#write.csv(tab, file.path(wp_folder, "unmatched_names.csv"), row.names = FALSE, fileEncoding = "UTF-8")
#### the produced file is used to complement existing maps

# add merged table name to EN entry, for visual verification
idx <- match(wp_teams[, "rugbyscopeId"], fus_teams[, "rugbyscopeId"])
wp_teams[, "fusionName"] <- fus_teams[idx, "fullName"]

#### debug: check RS id duplicates (several WP teams with the same id)
#idx <- as.integer(names(which(table(wp_teams[, "rugbyscopeId"]) > 1)))
#for (i in idx) {
#  print("------------------------")
#  ii <- which(wp_teams[, "rugbyscopeId"] == i)
#  print(wp_teams[ii, ])
#}
#### there are many cases: it is expected, there are many slightly different english names

# record final WP team table as a new CSV file, for verification
tab.file <- file.path(wp_folder, "teams.csv")
tlog(2, "Recording as a CSV file: \"", tab.file, "\"")
write.csv(wp_teams, tab.file, row.names = FALSE, fileEncoding = "UTF-8")

# complement alternative names in merged table, using english names
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
  # combine altnames
  an <- sort(setdiff(union(oan, wan), ofn))
  if (all(is.na(an)) || length(an) == 0)
    san <- NA
  else
    san <- paste0(an, collapse = "; ")
  if (!is.na(san) & !is.na(fus_teams[idx[r], "altNames"]) & san != fus_teams[idx[r], "altNames"] | !is.na(san) & is.na(fus_teams[idx[r], "altNames"])) {
    tlog(2, "Changed alt names for team ", ofn, " ", r, "/", length(idx), ":")
    tlog(4, "Original: ", fus_teams[idx[r], "altNames"])
    tlog(4, "Revised:  ", san)
    fus_teams[idx[r], "altNames"] <- san
  }
}

# record merged table as a new CSV file
tab.file <- file.path(fusion_folder, "teams_07_enwp.csv")
tlog(2, "Recording as a CSV file: \"", tab.file, "\"")
write.csv(fus_teams, tab.file, row.names = FALSE, fileEncoding = "UTF-8")

#### debug: look for teams with the same name in the *merged* team table
#result <- match_team_names(src_names = fus_names, tgt_names = fus_names)
## remove self-references
#for (i in 1:length(result)) {
#  res <- result[[i]]
#  res <- setdiff(res, i)
#  result[[i]] <- res
#}
## export to ease visual verification
#fileConn <- file(file.path(wp_folder, "potential_duplicates.txt"), open = "w")
#idx <- which(sapply(result, length) > 1)
#for (i in idx) {
# writeLines(paste0("Team ",i), fileConn)
# writeLines(paste0(fus_teams[i, ], collapse = ", "), fileConn)
# for (j in result[[i]])
#  writeLines(paste0(fus_teams[j, ], collapse = ", "), fileConn)
# writeLines("-------------------", fileConn)
#}
#close(fileConn)
####

#### debug: list WP teams sharing a name and having different ids
#### > this must be disambiguated before switching to players
#alt_names <- strsplit(wp_teams[, "altNames"], "; ")
#for (i in 1:(length(alt_names) - 1)) {
#  for (j in (i+1):length(alt_names)) {
#    common_names <- intersect(alt_names[[i]], alt_names[[j]])
#    if (length(common_names) > 0 && wp_teams[i, "rugbyscopeId"] != wp_teams[j, "rugbyscopeId"]) {
#      tlog(2, "Team ", i, " vs. ", j)
#      tlog(4, "Common name(s): ", paste0(common_names, collapse = ", "))
#      print(wp_teams[c(i,j), ])
#      print("----------------------")
#    }
#  }
#}
####

#### debug: check removed teams that share a name
#### with a team still in the list
#rem_names <- unique(trimws(unlist(strsplit(removed_teams, ";"))))
#alt_names <- strsplit(wp_teams[, "altNames"], "; ")
#for (i in 1:length(rem_names)) {
#  for (j in 1:length(alt_names)) {
#    rn <- rem_names[i]
#    an <- alt_names[[j]]
#    if (rn %in% an) {
#      idx <- which(wp_stints[, "teamName"] == rn)
#      tlog(2, "Problem with ", rn)
#      print(unique(wp_stints[idx, "teamWP"]))
#    }
#  }
#}
####




########################################################################
# merge stints
tlog("Merging stints")
removed_teams <- unique(trimws(unlist(strsplit(removed_teams, ";"))))

# connect WP stint to WP team tables (and so, to merged teams)
tlog(2, "Matching stint teams to team table")
wp_stints <- cbind(wp_stints, rep(NA, nrow(wp_stints)))
colnames(wp_stints)[ncol(wp_stints)] <- "rugbyscopeId"
del_rows <- c()
alt_names <- strsplit(wp_teams[, "altNames"], ";")
alt_names <- lapply(alt_names, trimws)
for (r in 1:nrow(wp_stints)) {
  team_name <- wp_stints[r, "teamName"]
  tlog(4, "Processing stint ", r, "/", nrow(wp_stints), " (", team_name, ")")
  # possibly ignore the team if it is in the ignore list
  if (team_name %in% removed_teams)
    del_rows <- c(del_rows, r)
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
          stop("Too many matches, and no URL to disambiguate: ", r)
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
tlog(2, "Deleted ", length(del_rows), "/", nrow(wp_stints), " stints corresponding to ", length(removed_teams), " removed teams")
#print(head(wp_stints[del_rows, ]))
wp_stints <- wp_stints[-del_rows, ]

# insert WP stints in merged table
for (r in 1:nrow(wp_stints)) {
  player_id <- wp_stints[r, "origWdId"]
  tlog(4, "Processing stint ", r, "/", nrow(wp_stints), " (player ", player_id, ")")
  team_id <- wp_stints[r, "rugbyscopeId"]
  finished <- FALSE

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

      # debug
      #print(wp_stints[r, ])
      #print(fus_stints[idx2, ])
      #tlog(6, wp_stints[r, "startYear"], "-", wp_stints[r, "endYear"])
      #for (z in idx2)
      #  tlog(8, fus_stints[z, "startYear"], "-", fus_stints[z, "endYear"])

      # compare the start/end years to find a compatible stint
      if (is.na(start_year)) {
        if (is.na(end_year)) {
          # both start and end WP years are NA: cannot be reliably matched to an existing stint
          # > nothing more to do
          finished <- TRUE
        } else {
          # start WP year is NA and end WP year is not: try to match only the latter to existing stints
          idx3 <- idx2[fus_stints[idx2, "endYear"] == end_year]
          if (length(idx3) > 0) {
            # if at least one match: remove NAs
            if (any(!is.na(idx3))) {
              idx3 <- idx3[!is.na(idx3)]
            # otherwise, all existing stints have an NA end year
            } else {
              # we check the start year of the existing stints, and keep those with NAs or anterior years
              idx3 <- idx2[(is.na(fus_stints[idx2, "startYear"]) | fus_stints[idx2, "startYear"] <= end_year) &
                            is.na(fus_stints[idx2, "endYear"])]

          # if there's only one of them, it will be merged with the WP stint
          # if not, then there's an issue because we can't choose between them
            }
          }
        }
      } else {
        if (is.na(end_year)) {
          # start WP year is not NA but end WP year is: try to match only the former to existing stints
          idx3 <- idx2[fus_stints[idx2, "startYear"] == start_year]
          if (length(idx3) > 0) {
            # if at least one match: remove NAs
            if (any(!is.na(idx3))) {
              idx3 <- idx3[!is.na(idx3)]
            # otherwise, all existing stints have an NA start year
            } else {
              # we check the end year of the existing stints, and keep those with NAs or posterior years
              idx3 <- idx2[is.na(fus_stints[idx2, "startYear"]) &
                          (is.na(fus_stints[idx2, "endYear"]) | fus_stints[idx2, "endYear"] >= start_year)]
          # if there's only one of them, it will be merged with the WP stint
          # if not, then there's an issue because we can't choose between them
            }
          }
        } else {
          # both start and end WP years are non-NA: try matching both to existing stints
          idx3 <- idx2[fus_stints[idx2, "startYear"] == start_year & fus_stints[idx2, "endYear"] == end_year]
          if (length(idx3) > 0) {
            # if at least one complete match (both years): remove NAs
            if (any(!is.na(idx3))) {
              idx3 <- idx3[!is.na(idx3)]
            # otherwise, all existing stints have an NA start and/or end year
            } else {
              # we keep only existing stints with one year matching the WP stint
              idx3 <- idx2[(is.na(fus_stints[idx2, "startYear"]) | fus_stints[idx2, "startYear"] == start_year) & 
                          (is.na(fus_stints[idx2, "endYear"])   | fus_stints[idx2, "endYear"] == end_year)]
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
        tlog(6, "Found several matching stints, which is not normal")
        print(wp_stints[r, ])
        print(fus_stints[idx3, ])
        stop("ERROR")
      }
      
      # possibly update stats
      if (update) {
        # debug
        #tlog(6, "Updating the table")
        
        # we assume that the info already present is more reliable
        # and update only the NA fields from the merged table
        if (is.na(fus_stints[idx3, "startYear"]))
          fus_stints[idx3, "startYear"] <- start_year
        if (is.na(fus_stints[idx3, "endYear"]))
          fus_stints[idx3, "endYear"] <- end_year
        if (is.na(fus_stints[idx3, "matchesPlayed"]) || !is.na(matches_played) && fus_stints[idx3, "matchesPlayed"] < matches_played)
          fus_stints[idx3, "matchesPlayed"] <- matches_played
        if (is.na(fus_stints[idx3, "pointsScored"]) || !is.na(points_scored) && fus_stints[idx3, "pointsScored"] < points_scored)
          fus_stints[idx3, "pointsScored"] <- points_scored
        # add WP as source if agreement on any non-NA field 
        agreements <- fus_stints[idx3, c("startYear", "endYear", "pointsScored", "pointsScored")] == c(start_year, end_year, matches_played, points_scored)
        if (!all(is.na(agreements)) && any(agreements, na.rm = TRUE))
          fus_stints[idx3, "dataSource"] <- paste0(fus_stints[idx3, "dataSource"], "; enWP")

        # debug
        #print(fus_stints[idx3, ])
      }
    }
  }

  # if no similar stint, add new row to merged table
  if (!finished) {
    # debug
    #tlog(6, "No matching stint: creating a new one")

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
    fus_stints[rr, "dataSource"] <- "enWP"
  }

  # debug
  #readline(prompt="Press [enter] to continue")
}

# clean both stat columns
fus_stints[, "matchesPlayed"] <- gsub("+", "", fus_stints[, "matchesPlayed"], fixed = TRUE)
fus_stints[, "matchesPlayed"] <- gsub("-", "", fus_stints[, "matchesPlayed"], fixed = TRUE)
fus_stints[, "matchesPlayed"] <- gsub("\\?+", "", fus_stints[, "matchesPlayed"], fixed = FALSE)
idx <- which(fus_stints[, "matchesPlayed"] == "")
if (length(idx) > 0)
  fus_stints[idx, "matchesPlayed"] <- NA
fus_stints[, "matchesPlayed"] <- as.integer(fus_stints[, "matchesPlayed"])
#sort(unique(fus_stints[, "matchesPlayed"]))
fus_stints[, "pointsScored"] <- gsub("+", "", fus_stints[, "pointsScored"], fixed = TRUE)
fus_stints[, "pointsScored"] <- gsub("-", "", fus_stints[, "pointsScored"], fixed = TRUE)
fus_stints[, "pointsScored"] <- gsub("\\?+", "", fus_stints[, "pointsScored"], fixed = FALSE)
fus_stints[, "pointsScored"] <- gsub(" ", "", fus_stints[, "pointsScored"], fixed = TRUE)
idx <- which(fus_stints[, "pointsScored"] == "")
if (length(idx) > 0)
  fus_stints[idx, "pointsScored"] <- NA
fus_stints[, "pointsScored"] <- as.integer(fus_stints[, "pointsScored"])
#sort(unique(fus_stints[, "pointsScored"]))

# reorder stint table to respect wikidataId / names
ids <- fus_stints[, "playerId"]
ids <- as.integer(substr(ids, start = 2, stop = nchar(ids)))
idx <- order(ids, fus_stints[, "playerName"], fus_stints[, "startYear"], fus_stints[, "endYear"], fus_stints[, "teamName"])
fus_stints <- fus_stints[idx, ]

# record as a new CSV file
fus_stints[, "teamRsId"] <- as.integer(fus_stints[,"teamRsId"])
tab.file <- file.path(fusion_folder, "stints_05_enwp.csv")
tlog(2, "Recording as a CSV file: \"", tab.file, "\"")
write.csv(fus_stints, tab.file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# stop logging
end.rec.log()



# club types
#  [1] "Club"                     "National senior team"
#  [3] "Combined team"            "National U20 team"
#  [5] "Invitational team"        "National U21 team"
#  [7] "National school team"     "National U18 team"
#  [9] "National U19 team"        "Regional team"
# [11] "National U16 team"        "National university team"
# [13] "National U17 team"        "National U23 team"
# [15] NA                         "National military team"
