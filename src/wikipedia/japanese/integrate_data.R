########################################################################
# Loads the clean Japanese Wikipedia tables and merge them into our tables.
#
# 02/2025 Vincent Labatut
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
wp_folder <- file.path("data", "wikipedia", "japanese")
#
fusion_folder <- file.path("data", "fusion")




########################################################################
# load previously merged tables
tlog("Loading our own tables")

our_teams <- read.csv(file.path(fusion_folder, "teams_02_ref.csv"))
tlog(2, "Raw number of teams: ", nrow(our_teams))

our_players <- read.csv(file.path(fusion_folder, "players_01_wd-dbp.csv"))
tlog(2, "Raw number of players: ", nrow(our_players))

our_careers <- read.csv(file.path("data", "wikidata", "tables", "careers.csv"))
tlog(2, "Raw number of career steps: ", nrow(our_careers))




########################################################################
# load WP JA tables
tlog("Loading Wikipedia JA tables")

wp_players <- read.csv(file.path(wp_folder, "players.csv"))
tlog(2, "Raw number of players: ", nrow(wp_players))
wp_players <- wp_players %>% mutate(across(where(is.character), ~ na_if(., "")))

wp_careers <- read.csv(file.path(wp_folder, "careers.csv"))
tlog(2, "Raw number of career steps: ", nrow(wp_careers))
wp_careers <- careers %>% mutate(across(where(is.character), ~ na_if(., "")))




########################################################################
# merge players
tlog("Merging players")

# convert dob and dod into proper dates
wp_players[, "birthDate"] %<>%  as.Date()
wp_players[, "deathDate"] %<>%  as.Date()
our_players[, "birthDate"] %<>%  as.Date()
our_players[, "deathDate"] %<>%  as.Date()

# match players from WP to our list
idx <- match(wp_players[, "wikidataId"], our_players[, "wikidataId"])
tlog(2, "Successful matches: ", length(which(!is.na(idx))), "/", nrow(wp_players))

# insert WP info if field is empty in our table
map <- c()  # ours <- wp
map["birthDate"] <- "birthDate"
map["birthPlaces"] <- "birthPlace"
map["deathDate"] <- "deathDate"
map["deathPlaces"] <- "deathPlace"
map["fullName"] <- "fullName"
map["weights"] <- "weight"
map["heights"] <- "height"
map["positions"] <- "positions"
tlog(2, "Merging regular fields")
total_changes <- rep(0, length(map))
names(total_changes) <- names(map)
for (p in 1:nrow(wp_players)) {
  if (p %% 100 == 0)
    tlog(4, "Processing player ", p, "/", nrow(wp_players))
  filled_wp_cols <- which(!is.na(wp_players[p, map]))
  empty_our_cols <- which(is.na(our_players[idx[p], names(map)]))
  cols <- intersect(filled_wp_cols, empty_our_cols)
  if (length(cols) > 0) {
    our_players[idx[p], names(map)[cols]] <- wp_players[p, map[cols]]
    total_changes[names(map)[cols]] <- total_changes[names(map)[cols]] + rep(1, length(cols))
  }
}
tlog(2, "Total numbers of changes: ", sum(total_changes))
tlog(4, "Fields: ", paste0(total_changes, collapse = ", "))
print(total_changes)

# only keep WP name as alt name, if it does not match current fullname
tlog(2, "Copying WP names into alt name list")
full_names <- our_players[, "fullName"]
alt_names <- strsplit(our_players[, "altNames"], "; ")
ja_names <- wp_players[, "jaName"]
# loop over players to copy WP data
for (p in 1:length(idx)) {
  if (!is.na(ja_names[p]) && ja_names[p] != full_names[idx[p]]) {
    if (all(is.na(alt_names[[idx[p]]])))
      a_names <- ja_names[p]
    else
      a_names <- union(alt_names[[idx[p]]], ja_names[p])
    our_players[p, "altNames"] <- paste(a_names, collapse = "; ")
  }
}
idx <- which(our_players[, "altNames"] == "NA")
our_players[idx, "altNames"] <- NA

# record as a new CSV file
tab.file <- file.path(fusion_folder, "players_02_ja-wp.csv")
tlog(2, "Recording as a CSV file: \"", tab.file, "\"")
write.csv(our_players, tab.file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# extract team table from career steps
tlog("Extract team table from career steps")

# init table
cn <- c("rugbyscopeId", "altNames", "teamWP")
wp_teams <- data.frame(matrix(NA, nrow = 0, ncol = length(cn)))
colnames(wp_teams) <- cn

# populate table with unique names/urls
tlog(2, "Populate team table with unique names/urls")
alt_names <- list()
for (r in 1:nrow(wp_careers)) {
  team_name <- wp_careers[r, "teamName"]
  team_url <- wp_careers[r, "teamWP"]
  # no associated url
  if (is.na(team_url)) {
    # search by team name
    idx <- which(sapply(alt_names, function(an) team_name %in% an))
    # new entry
    if (length(idx) == 0) {
      wp_teams <- rbind(wp_teams, c(NA, NA, NA))
      alt_names <- c(alt_names, list(team_name))
    }
  # name and url both available
  } else {
    idx <- which(wp_teams[, "teamWP"] == team_url)
    # new entry
    if (length(idx) == 0) {
      wp_teams <- rbind(wp_teams, c(NA, NA, team_url))
      colnames(wp_teams) <- cn
      alt_names <- c(alt_names, list(team_name))
    # update existing entry
    } else {
      nn <- union(alt_names[[idx]], team_name)
      alt_names[[idx]] <- nn
    }
  }
}
wp_teams[, "altNames"] <- sapply(alt_names, function(an) paste0(an, collapse = "; "))

# match teams using WP URLs
tlog(2, "Match teams to merged table based on urls")
non_na <- which(!is.na(wp_teams[, "teamWP"]))
unique_urls <- trimws(wp_teams[non_na, "teamWP"])
unique_urls <- unique_urls[!grepl("redlink=1", unique_urls, fixed = TRUE)]
unique_urls <- unique_urls[!startsWith(unique_urls, "#")]
unique_urls <- gsub("https://[a-z]{2}.wikipedia.org/wiki/", "", unique_urls, fixed = FALSE)
unique_urls <- gsub("/wiki/", "", unique_urls, fixed = TRUE)
matches <- cbind(match(unique_urls, our_teams[, "wikipediaEn"]), match(unique_urls, our_teams[, "wikipediaFr"]), match(unique_urls, our_teams[, "wikipediaIt"]), match(unique_urls, our_teams[, "wikipediaEs"]), match(unique_urls, our_teams[, "wikipediaJa"]))
mm <- apply(matches, 1, function(row) {
  res <- unique(row[!is.na(row)])
  if (length(res) == 0)
    res <- NA
  return(res)
})
idx <- which(!is.na(mm))
tlog(4, "Could match directly ", length(idx), "/", length(unique_urls), " non-NA URLs")
wp_teams[non_na, "rugbyscopeId"] <- our_teams[mm, "rubyscopeId"]
#length(which(!is.na(wp_teams[, "rugbyScopeId"])))

# get english names of unmatched teams (with url)
tlog(2, "Retrieving missing English team names")
idx <- which(!is.na(wp_teams[, "teamWP"]) & is.na(wp_teams[, "rugbyscopeId"]))
failed <- c()
for (i in 1:length(idx)) {
  team_url <- trimws(wp_teams[idx[i], "teamWP"])
  # if(!grepl("redlink=1", team_url, fixed = TRUE) && !startsWith(team_url, "#")) {
  tlog(4, "Retrieving translation for \"", team_url, "\" (", i, "/", length(idx), ")")
  title <- get_english_title(team_url, lang = "ja")
  tlog(6, "Result: ", title)
  if (is.null(title))
    failed <- c(failed, team_url)
  else
    alt_names[[idx[i]]] <- union(alt_names[[idx[i]]], list(team_name))
  # Sys.sleep(1)
}
# debug
write.csv(failed, file.path(wp_folder, "unnamed_urls.csv"), row.names = FALSE, fileEncoding = "UTF-8")
# handle the remaining cases manually
map_url["/wiki/%E3%83%96%E3%83%AB%E3%83%BC%E3%82%B7%E3%83%A3%E3%83%BC%E3%82%AF%E3%82%B9"] <- "Shimizu Koto Blue Sharks"



# match teams using names

# record as a new CSV file
tab.file <- file.path(wp_folder, "teams.csv")
tlog(2, "Recording as a CSV file: \"", tab.file, "\"")
write.csv(wp_teams, tab.file, row.names = FALSE, fileEncoding = "UTF-8")






########################################################################
# merge career steps
tlog("Merging career steps")

# [1] "origWdId"      "origName"      "jaName"        "wpPage"
# [5] "stepType"      "timePeriod"    "teamName"      "teamWP"
# [9] "matchesPlayed" "pointsScored"  "startYear"     "endYear"

# [1] "playerId"      "playerName"    "teamId"        "teamName"
# [5] "startYear"     "endYear"       "matchesPlayed" "pointsScored"









# get english names of teams
tlog(2, "Normalize team names")
all_urls <- c(careers[, "teamWP"])
all_urls <- strsplit(all_urls, "; ")
unique_urls <- sort(unique(trimws(unlist(all_urls))))
unique_urls <- unique_urls[!grepl("redlink=1", unique_urls, fixed = TRUE)]
unique_urls <- unique_urls[!startsWith(unique_urls, "#")]
# define conversion map for locations
tlog(4, "Building the conversion maps")
map_url <- c()
for (i in 1:length(unique_urls)) {
  unique_url <- unique_urls[i]
  tlog(6, "Retrieving translation for \"", unique_url, "\" (", i, "/", length(unique_urls), ")")
  title <- get_english_title(unique_url)
  tlog(8, "Result: ", title)
  if (is.null(title))
    title <- unique_url[i]
  map_url[unique_urls[i]] <- title
}
# debug
tmp <- which(is.na(map_url))
write.csv(names(tmp), "temp.csv", row.names = FALSE)
map_url["https://it.wikipedia.org/wiki/CUS_Padova_Rugby"] <- "CUS Padova Rugby"
map_url["https://it.wikipedia.org/wiki/Rugby_Grande_Milano"] <- "Rugby Grande Milano"
map_url["https://it.wikipedia.org/wiki/Rugby_Milano"] <- "Rugby Milano"
map_url["https://it.wikipedia.org/wiki/Valsugana_Rugby_Padova"] <- "Valsugana Rugby Padova"
map_url["https://it.wikipedia.org/wiki/Verona_Rugby"] <- "Verona Rugby"
#
map_url["/wiki/%E3%81%A4%E3%81%8F%E3%81%B0%E7%A7%80%E8%8B%B1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- "Tsukuba Shuei High School"
map_url["/wiki/%E3%81%BF%E3%81%9A%E3%81%BB%E3%83%95%E3%82%A3%E3%83%8A%E3%83%B3%E3%82%B7%E3%83%A3%E3%83%AB%E3%82%B0%E3%83%AB%E3%83%BC%E3%83%97%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- "Mizuho Financial Group Rugby Club"
map_url["/wiki/%E3%82%A4%E3%83%BC%E3%82%B9%E3%82%BF%E3%83%B3%E3%83%BB%E3%83%97%E3%83%AD%E3%83%B4%E3%82%A3%E3%83%B3%E3%82%B9%E3%83%BB%E3%82%AD%E3%83%B3%E3%82%B0%E3%82%B9"] <- "Eastern Province Elephants"
map_url["/wiki/%E3%82%A6%E3%82%A7%E3%82%B9%E3%82%BF%E3%83%B3%E3%83%BB%E3%83%95%E3%82%A9%E3%83%BC%E3%82%B9_(%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC)"] <- "Western Force"
map_url["/wiki/%E3%82%AA%E3%83%BC%E3%83%AB%E3%83%96%E3%83%A9%E3%83%83%E3%82%AF%E3%82%B9"] <- ""
map_url["/wiki/%E3%82%AB%E3%83%B3%E3%82%BF%E3%83%99%E3%83%AA%E3%83%BC%E5%A4%A7%E5%AD%A6_(%E3%83%8B%E3%83%A5%E3%83%BC%E3%82%B8%E3%83%BC%E3%83%A9%E3%83%B3%E3%83%89)"] <- ""
map_url["/wiki/%E3%82%AB%E3%83%BC%E3%83%87%E3%82%A3%E3%83%95%E3%83%BB%E3%83%96%E3%83%AB%E3%83%BC%E3%82%BA"] <- ""
map_url["/wiki/%E3%82%AD%E3%83%A4%E3%83%8E%E3%83%B3%E3%82%A4%E3%83%BC%E3%82%B0%E3%83%AB%E3%82%B9"] <- ""
map_url["/wiki/%E3%82%AF%E3%82%A4%E3%83%BC%E3%83%B3%E3%82%BA%E3%83%A9%E3%83%B3%E3%83%89%E3%83%BB%E3%83%AC%E3%83%83%E3%82%BA"] <- ""
map_url["/wiki/%E3%82%AF%E3%82%A4%E3%83%BC%E3%83%B3%E3%82%BA%E3%83%A9%E3%83%B3%E3%83%89%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%82%AF%E3%83%A9%E3%83%96"] <- ""
map_url["/wiki/%E3%82%AF%E3%83%9C%E3%82%BF%E3%82%B9%E3%83%94%E3%82%A2%E3%83%BC%E3%82%BA"] <- ""
map_url["/wiki/%E3%82%AF%E3%83%A9%E3%82%A4%E3%82%B9%E3%83%88%E3%83%81%E3%83%A3%E3%83%BC%E3%83%81%E3%83%BB%E3%83%9C%E3%83%BC%E3%82%A4%E3%82%BA%E3%83%8F%E3%82%A4%E3%82%B9%E3%82%AF%E3%83%BC%E3%83%AB"] <- ""
map_url["/wiki/%E3%82%AF%E3%83%A9%E3%82%A4%E3%82%B9%E3%83%88%E3%83%81%E3%83%A3%E3%83%BC%E3%83%81%E7%94%B7%E5%AD%90%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E3%82%AF%E3%83%AA%E3%83%BC%E3%83%B3%E3%83%95%E3%82%A1%E3%82%A4%E3%82%BF%E3%83%BC%E3%82%BA"] <- ""
map_url["/wiki/%E3%82%AF%E3%83%AA%E3%83%BC%E3%83%B3%E3%83%95%E3%82%A1%E3%82%A4%E3%82%BF%E3%83%BC%E3%82%BA%E5%B1%B1%E6%A2%A8"] <- ""
map_url["/wiki/%E3%82%AF%E3%83%AB%E3%82%BB%E3%82%A4%E3%83%80%E3%83%BC%E3%82%BA"] <- ""
map_url["/wiki/%E3%82%B0%E3%83%AA%E3%83%BC%E3%82%AF%E3%82%A2%E3%82%BA"] <- ""
map_url["/wiki/%E3%82%B3%E3%83%BC%E3%83%8B%E3%83%83%E3%82%B7%E3%83%A5%E3%83%BB%E3%83%91%E3%82%B9%E3%83%86%E3%82%A3"] <- ""
map_url["/wiki/%E3%82%B5%E3%82%B6%E3%83%B3%E3%83%BB%E3%83%87%E3%82%A3%E3%82%B9%E3%83%88%E3%83%AA%E3%82%AF%E3%83%84"] <- ""
map_url["/wiki/%E3%82%B5%E3%83%A0%E3%83%A9%E3%82%A4%E3%82%BB%E3%83%96%E3%83%B3"] <- ""
map_url["/wiki/%E3%82%B5%E3%83%B3%E3%83%88%E3%83%AA%E3%83%BC%E3%82%B5%E3%83%B3%E3%82%B4%E3%83%AA%E3%82%A2%E3%82%B9"] <- ""
map_url["/wiki/%E3%82%B5%E3%83%B3%E3%83%88%E3%83%AA%E3%83%BC%E3%83%95%E3%83%BC%E3%82%BA%E3%82%B5%E3%83%B3%E3%83%87%E3%83%AB%E3%83%95%E3%82%A3%E3%82%B9"] <- ""
map_url["/wiki/%E3%82%B7%E3%83%89%E3%83%8B%E3%83%BC%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%82%AF%E3%83%A9%E3%83%96"] <- ""
map_url["/wiki/%E3%82%B8%E3%83%A3%E3%83%91%E3%83%B3%E3%82%BB%E3%83%9F%E3%82%B3%E3%83%B3%E3%83%80%E3%82%AF%E3%82%BF%E3%83%BC%E5%A4%A7%E5%88%86%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E3%82%B9%E3%82%BF%E3%83%83%E3%83%89%E3%83%BB%E3%83%95%E3%83%A9%E3%83%B3%E3%82%BB"] <- ""
map_url["/wiki/%E3%82%BB%E3%82%A4%E3%83%9C%E3%82%B9"] <- ""
map_url["/wiki/%E3%82%BB%E3%82%B3%E3%83%A0%E3%83%A9%E3%82%AC%E3%83%83%E3%83%84"] <- ""
map_url["/wiki/%E3%82%BB%E3%83%B3%E3%83%88%E3%83%A9%E3%83%AB%E3%83%BB%E3%83%81%E3%83%BC%E3%82%BF%E3%83%BC%E3%82%BA"] <- ""
map_url["/wiki/%E3%82%BC%E3%83%96%E3%83%AC"] <- ""
map_url["/wiki/%E3%83%86%E3%82%A3%E3%83%9F%E3%82%B7%E3%83%A7%E3%82%A2%E3%83%A9%E3%83%BB%E3%82%B5%E3%83%A9%E3%82%BB%E3%83%B3%E3%82%BA"] <- ""
map_url["/wiki/%E3%83%88%E3%83%A8%E3%82%BF%E8%87%AA%E5%8B%95%E8%BB%8A%E3%83%B4%E3%82%A7%E3%83%AB%E3%83%96%E3%83%AA%E3%83%83%E3%83%84"] <- ""
map_url["/wiki/%E3%83%88%E3%83%A8%E3%82%BF%E8%87%AA%E5%8B%95%E8%BB%8A%E3%83%B4%E3%82%A7%E3%83%AB%E3%83%96%E3%83%AA%E3%83%83%E3%83%84/wiki/%E6%97%A5%E6%9C%AC"] <- ""
map_url["/wiki/%E3%83%8A%E3%82%BF%E3%83%BC%E3%83%AB%E3%83%BB%E3%82%B7%E3%83%A3%E3%83%BC%E3%82%AF%E3%82%B9"] <- ""
map_url["/wiki/%E3%83%8E%E3%83%BC%E3%82%B9%E3%82%A2%E3%82%B8%E3%82%A2%E5%A4%A7%E5%AD%A6%E6%98%8E%E6%A1%9C%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E3%83%8F%E3%82%A4%E3%83%A9%E3%83%B3%E3%83%80%E3%83%BC%E3%82%BA"] <- ""
map_url["/wiki/%E3%83%8F%E3%82%B0%E3%82%A2%E3%83%AC%E3%82%B9XV"] <- ""
map_url["/wiki/%E3%83%8F%E3%83%BC%E3%82%AD%E3%83%A5%E3%83%AA%E3%83%BC%E3%82%BA_(%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC)"] <- ""
map_url["/wiki/%E3%83%91%E3%83%8A%E3%82%BD%E3%83%8B%E3%83%83%E3%82%AF%E3%83%AF%E3%82%A4%E3%83%AB%E3%83%89%E3%83%8A%E3%82%A4%E3%83%84"] <- ""
map_url["/wiki/%E3%83%91%E3%83%8A%E3%82%BD%E3%83%8B%E3%83%83%E3%82%AF_%E3%83%AF%E3%82%A4%E3%83%AB%E3%83%89%E3%83%8A%E3%82%A4%E3%83%84"] <- ""
map_url["/wiki/%E3%83%91%E3%83%B3%E3%83%91%E3%82%B9XV"] <- ""
map_url["/wiki/%E3%83%96%E3%83%AA%E3%83%A5%E3%83%83%E3%82%BB%E3%83%AB%E3%83%BB%E3%83%87%E3%83%B4%E3%82%A3%E3%83%AB%E3%82%BA"] <- ""
map_url["/wiki/%E3%83%96%E3%83%AB%E3%83%BC%E3%82%B7%E3%83%A3%E3%83%BC%E3%82%AF%E3%82%B9"] <- ""
map_url["/wiki/%E3%83%96%E3%83%AB%E3%83%BC%E3%82%B9_(%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC)"] <- ""
map_url["/wiki/%E3%83%99%E3%83%8D%E3%83%88%E3%83%B3%E3%83%BB%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC"] <- ""
map_url["/wiki/%E3%83%9A%E3%83%88%E3%83%A9%E3%83%AB%E3%82%AB%E3%83%BB%E3%83%91%E3%83%89%E3%83%B4%E3%82%A1"] <- ""
map_url["/wiki/%E3%83%9B%E3%83%B3%E3%83%80%E3%83%92%E3%83%BC%E3%83%88"] <- ""
map_url["/wiki/%E3%83%9E%E3%83%84%E3%83%80%E3%83%96%E3%83%AB%E3%83%BC%E3%82%BA%E3%83%BC%E3%83%9E%E3%83%BC%E3%82%BA"] <- ""
map_url["/wiki/%E3%83%A4%E3%82%AF%E3%83%AB%E3%83%88%E3%83%AC%E3%83%93%E3%83%B3%E3%82%BA"] <- ""
map_url["/wiki/%E3%83%A4%E3%82%AF%E3%83%AB%E3%83%88%E3%83%AC%E3%83%93%E3%83%B3%E3%82%BA%E6%88%B8%E7%94%B0"] <- ""
map_url["/wiki/%E3%83%A4%E3%83%9E%E3%83%8F%E7%99%BA%E5%8B%95%E6%A9%9F%E3%82%B8%E3%83%A5%E3%83%93%E3%83%AD"] <- ""
map_url["/wiki/%E3%83%A9%E3%82%A4%E3%82%AA%E3%83%B3%E3%83%95%E3%82%A1%E3%83%B3%E3%82%B0%E3%82%B9"] <- ""
map_url["/wiki/%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%9F%93%E5%9B%BD%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BCATL"] <- ""
map_url["/wiki/%E3%83%A9%E3%82%B7%E3%83%B392_(%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC)"] <- ""
map_url["/wiki/%E3%83%A9%E3%83%B3%E3%83%89%E3%82%A6%E3%82%A3%E3%83%83%E3%82%AFDRUFC"] <- ""
map_url["/wiki/%E3%83%AA%E3%82%B3%E3%83%BC%E3%82%B8%E3%83%A3%E3%83%91%E3%83%B3%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E3%83%AA%E3%82%B3%E3%83%BC%E3%83%96%E3%83%A9%E3%83%83%E3%82%AF%E3%83%A9%E3%83%A0%E3%82%BA"] <- ""
map_url["/wiki/%E3%83%AA%E3%83%B3%E3%83%95%E3%82%A3%E3%83%BC%E3%83%AB%E3%83%89%E3%83%BB%E3%82%AB%E3%83%AC%E3%83%83%E3%82%B8"] <- ""
map_url["/wiki/%E3%83%AB%E3%83%AA%E3%83%BC%E3%83%AD%E7%A6%8F%E5%B2%A1"] <- ""
map_url["/wiki/%E3%83%AD%E3%83%B3%E3%83%89%E3%83%B3%E3%83%BB%E3%83%AF%E3%82%B9%E3%83%97%E3%82%B9"] <- ""
map_url["/wiki/%E3%83%AF%E3%82%BB%E3%83%80%E3%82%AF%E3%83%A9%E3%83%96TOP_RUSHERS"] <- ""
map_url["/wiki/%E3%83%AF%E3%83%A9%E3%82%BF%E3%83%BC%E3%82%BA_(%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC)"] <- ""
map_url["/wiki/%E4%B8%89%E7%94%B0%E5%B8%82%E7%AB%8B%E3%81%91%E3%82%84%E3%81%8D%E5%8F%B0%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%B8%89%E8%8F%B1%E8%87%AA%E5%8B%95%E8%BB%8A%E4%BA%AC%E9%83%BD%E3%83%AC%E3%83%83%E3%83%89%E3%82%A8%E3%83%9C%E3%83%AA%E3%83%A5%E3%83%BC%E3%82%B7%E3%83%A7%E3%83%B3%E3%82%BA"] <- ""
map_url["/wiki/%E4%B8%89%E8%8F%B1%E9%87%8D%E5%B7%A5%E9%95%B7%E5%B4%8E%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E4%B8%89%E9%87%8D%E7%9C%8C%E7%AB%8B%E5%90%8D%E5%BC%B5%E8%A5%BF%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%B8%89%E9%87%8D%E7%9C%8C%E7%AB%8B%E5%9B%9B%E6%97%A5%E5%B8%82%E8%BE%B2%E8%8A%B8%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%B8%89%E9%87%8D%E7%9C%8C%E7%AB%8B%E5%BF%97%E6%91%A9%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%B8%89%E9%87%8D%E7%9C%8C%E7%AB%8B%E6%9C%9D%E6%98%8E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%B8%89%E9%87%8D%E7%9C%8C%E7%AB%8B%E6%9D%BE%E9%98%AA%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%B8%8A%E5%AE%AE%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%B8%8A%E5%AE%AE%E5%A4%AA%E5%AD%90%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%B8%AD%E5%A4%AE%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E4%B8%AD%E5%A4%AE%E5%B8%82%E7%AB%8B%E7%94%B0%E5%AF%8C%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%B8%AD%E6%9D%91%E5%AD%A6%E5%9C%92%E4%B8%89%E9%99%BD%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%B8%AD%E9%83%A8%E5%A4%A7%E5%AD%A6%E6%98%A5%E6%97%A5%E4%B8%98%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%B8%B8%E5%92%8C%E9%81%8B%E8%BC%B8%E6%A9%9F%E9%96%A2%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E4%B8%B8%E5%92%8C%E9%81%8B%E8%BC%B8%E6%A9%9F%E9%96%A2AZ-MOMOTARO%27S"] <- ""
map_url["/wiki/%E4%B9%9D%E5%B7%9E%E5%85%B1%E7%AB%8B%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E4%B9%9D%E5%B7%9E%E5%9B%BD%E9%9A%9B%E5%A4%A7%E5%AD%A6%E4%BB%98%E5%B1%9E%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%B9%9D%E5%B7%9E%E5%AD%A6%E9%99%A2%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%B9%9D%E5%B7%9E%E7%94%A3%E6%A5%AD%E5%A4%A7%E5%AD%A6%E4%BB%98%E5%B1%9E%E4%B9%9D%E5%B7%9E%E7%94%A3%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%B9%9D%E5%B7%9E%E7%94%A3%E6%A5%AD%E5%A4%A7%E5%AD%A6%E4%BB%98%E5%B1%9E%E4%B9%9D%E5%B7%9E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%BA%AC%E9%83%BD%E5%85%88%E7%AB%AF%E7%A7%91%E5%AD%A6%E5%A4%A7%E5%AD%A6%E4%BB%98%E5%B1%9E%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%BA%AC%E9%83%BD%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E4%BA%AC%E9%83%BD%E5%B8%82%E5%BD%B9%E6%89%80"] <- ""
map_url["/wiki/%E4%BA%AC%E9%83%BD%E5%B8%82%E7%AB%8B%E4%BA%AC%E9%83%BD%E5%B7%A5%E5%AD%A6%E9%99%A2%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%BA%AC%E9%83%BD%E5%B8%82%E7%AB%8B%E4%BC%8F%E8%A6%8B%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%BA%AC%E9%83%BD%E5%BA%9C%E7%AB%8B%E4%BA%80%E5%B2%A1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%BA%AC%E9%83%BD%E5%BA%9C%E7%AB%8B%E6%9D%B1%E5%AE%87%E6%B2%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%BA%AC%E9%83%BD%E5%BA%9C%E7%AB%8B%E6%B4%9B%E5%8C%97%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%99%84%E5%B1%9E%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%BA%AC%E9%83%BD%E6%88%90%E7%AB%A0%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%BA%AC%E9%83%BD%E7%94%A3%E6%A5%AD%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E4%BB%99%E5%8F%B0%E5%B8%82%E7%AB%8B%E4%BB%99%E5%8F%B0%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%BB%99%E5%8F%B0%E8%82%B2%E8%8B%B1%E5%AD%A6%E5%9C%92%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%BC%8A%E5%8B%A2%E4%B8%B9%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E4%BD%90%E8%B3%80%E7%9C%8C%E7%AB%8B%E4%BD%90%E8%B3%80%E6%9D%B1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%BD%90%E9%87%8E%E6%97%A5%E6%9C%AC%E5%A4%A7%E5%AD%A6%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%BD%9C%E6%96%B0%E5%AD%A6%E9%99%A2%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%BF%9D%E5%96%84%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E4%BF%AE%E5%BE%B3%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%85%89%E6%B3%89%E3%82%AB%E3%83%88%E3%83%AA%E3%83%83%E3%82%AF%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%85%AC%E6%96%87%E5%9B%BD%E9%9A%9B%E5%AD%A6%E5%9C%92%E4%B8%AD%E7%AD%89%E9%83%A8%E3%83%BB%E9%AB%98%E7%AD%89%E9%83%A8"] <- ""
map_url["/wiki/%E5%85%AD%E7%94%B2%E3%83%95%E3%82%A1%E3%82%A4%E3%83%86%E3%82%A3%E3%83%B3%E3%82%B0%E3%83%96%E3%83%AB"] <- ""
map_url["/wiki/%E5%85%B5%E5%BA%AB%E7%9C%8C%E7%AB%8B%E5%85%B5%E5%BA%AB%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%85%B5%E5%BA%AB%E7%9C%8C%E7%AB%8B%E5%A7%AB%E8%B7%AF%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%85%B5%E5%BA%AB%E7%9C%8C%E7%AB%8B%E6%98%8E%E7%9F%B3%E6%B8%85%E6%B0%B4%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%85%B5%E5%BA%AB%E7%9C%8C%E7%AB%8B%E6%9D%B1%E6%92%AD%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%85%B5%E5%BA%AB%E7%9C%8C%E7%AB%8B%E7%A5%9E%E6%88%B8%E7%94%B2%E5%8C%97%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%85%B5%E5%BA%AB%E7%9C%8C%E7%AB%8B%E9%B3%B4%E5%B0%BE%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%87%B8%E7%89%88%E5%8D%B0%E5%88%B7"] <- ""
map_url["/wiki/%E5%87%BD%E9%A4%A8%E5%A4%A7%E5%AD%A6%E4%BB%98%E5%B1%9E%E6%9C%89%E6%96%97%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%8C%97%E6%B5%B7%E9%81%93%E3%83%90%E3%83%BC%E3%83%90%E3%83%AA%E3%82%A2%E3%83%B3%E3%82%BA"] <- ""
map_url["/wiki/%E5%8C%97%E6%B5%B7%E9%81%93%E4%B8%83%E9%A3%AF%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%8C%97%E6%B5%B7%E9%81%93%E4%B8%AD%E6%A8%99%E6%B4%A5%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%8C%97%E6%B5%B7%E9%81%93%E5%87%BD%E9%A4%A8%E5%8C%97%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%8C%97%E6%B5%B7%E9%81%93%E5%87%BD%E9%A4%A8%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%8C%97%E6%B5%B7%E9%81%93%E6%97%AD%E5%B7%9D%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%8C%97%E6%B5%B7%E9%81%93%E7%BE%8E%E5%B9%8C%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%8C%97%E6%B5%B7%E9%81%93%E8%8A%A6%E5%88%A5%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%8C%97%E6%B5%B7%E9%81%93%E9%81%A0%E8%BB%BD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%8C%97%E8%B6%8A%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%8D%83%E8%91%89%E7%9C%8C%E7%AB%8B%E4%BD%90%E5%80%89%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%8D%83%E8%91%89%E7%B5%8C%E6%B8%88%E5%A4%A7%E5%AD%A6%E9%99%84%E5%B1%9E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%90%8C%E5%BF%97%E7%A4%BE%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%90%8C%E5%BF%97%E7%A4%BE%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E5%90%8C%E5%BF%97%E7%A4%BE%E9%A6%99%E9%87%8C%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%90%8D%E5%8F%A4%E5%B1%8B%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%90%8D%E5%8F%A4%E5%B1%8B%E5%B8%82%E7%AB%8B%E5%BE%A1%E7%94%B0%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%90%8D%E5%8F%A4%E5%B1%8B%E5%B8%82%E7%AB%8B%E8%A5%BF%E9%99%B5%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%90%8D%E8%AD%B7%E9%AB%98%E6%A0%A1"] <- ""
map_url["/wiki/%E5%90%91%E4%B8%8A%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%92%8C%E6%AD%8C%E5%B1%B1%E7%9C%8C%E7%AB%8B%E5%92%8C%E6%AD%8C%E5%B1%B1%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%92%8C%E6%AD%8C%E5%B1%B1%E7%9C%8C%E7%AB%8B%E7%86%8A%E9%87%8E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%95%93%E5%85%89%E5%AD%A6%E5%9C%92%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%9B%BD%E5%A3%AB%E8%88%98%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E5%9C%8B%E5%AD%B8%E9%99%A2%E5%A4%A7%E5%AD%B8%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E5%9C%8B%E5%AD%B8%E9%99%A2%E5%A4%A7%E5%AD%B8%E4%B9%85%E6%88%91%E5%B1%B1%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%9C%8B%E5%AD%B8%E9%99%A2%E5%A4%A7%E5%AD%B8%E6%A0%83%E6%9C%A8%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%9C%9F%E4%BD%90%E5%A1%BE%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%9F%8E%E5%8C%97%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%9F%BC%E7%8E%89%E3%83%AF%E3%82%A4%E3%83%AB%E3%83%89%E3%83%8A%E3%82%A4%E3%83%84"] <- ""
map_url["/wiki/%E5%9F%BC%E7%8E%89%E7%9C%8C%E7%AB%8B%E5%AF%84%E5%B1%85%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%9F%BC%E7%8E%89%E7%9C%8C%E7%AB%8B%E6%89%80%E6%B2%A2%E5%8C%97%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%9F%BC%E7%8E%89%E7%9C%8C%E7%AB%8B%E6%9C%9D%E9%9C%9E%E8%A5%BF%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%9F%BC%E7%8E%89%E7%9C%8C%E7%AB%8B%E6%B5%A6%E5%92%8C%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%9F%BC%E7%8E%89%E7%9C%8C%E7%AB%8B%E6%B7%B1%E8%B0%B7%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%9F%BC%E7%8E%89%E7%9C%8C%E7%AB%8B%E7%86%8A%E8%B0%B7%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%9F%BC%E7%8E%89%E7%9C%8C%E7%AB%8B%E7%86%8A%E8%B0%B7%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%9F%BC%E7%8E%89%E7%9C%8C%E7%AB%8B%E8%8D%89%E5%8A%A0%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A0%B1%E5%BE%B3%E5%AD%A6%E5%9C%92%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E5%88%86%E6%9D%B1%E6%98%8E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E5%90%91%E9%99%BD%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E5%88%86%E7%9C%8C%E7%AB%8B%E5%A4%A7%E5%88%86%E8%88%9E%E9%B6%B4%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E5%88%86%E7%9C%8C%E7%AB%8B%E6%A3%AE%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E5%88%86%E7%9C%8C%E7%AB%8B%E6%B5%B7%E6%B4%8B%E7%A7%91%E5%AD%A6%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E5%88%86%E7%9C%8C%E7%AB%8B%E7%94%B1%E5%B8%83%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E5%95%86%E5%AD%A6%E5%9C%92%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E6%9D%B1%E6%96%87%E5%8C%96%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E5%A4%A7%E6%9D%B1%E6%96%87%E5%8C%96%E5%A4%A7%E5%AD%A6%E7%AC%AC%E4%B8%80%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E6%B4%A5%E5%B8%82%E7%AB%8B%E7%80%AC%E7%94%B0%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E4%BD%93%E8%82%B2%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%B8%82%E7%AB%8B%E6%B1%8E%E6%84%9B%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%B8%82%E7%AB%8B%E9%83%BD%E5%B3%B6%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%BA%9C%E7%AB%8B%E4%B8%89%E5%B3%B6%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%BA%9C%E7%AB%8B%E5%88%80%E6%A0%B9%E5%B1%B1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%BA%9C%E7%AB%8B%E5%8D%83%E9%87%8C%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%BA%9C%E7%AB%8B%E5%9B%9B%E6%A2%9D%E7%95%B7%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%BA%9C%E7%AB%8B%E5%B3%B6%E6%9C%AC%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%BA%9C%E7%AB%8B%E5%B8%83%E6%96%BD%E5%B7%A5%E7%A7%91%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%BA%9C%E7%AB%8B%E6%91%82%E6%B4%A5%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%BA%9C%E7%AB%8B%E6%B7%80%E5%B7%9D%E5%B7%A5%E7%A7%91%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%BA%9C%E7%AB%8B%E6%B8%8B%E8%B0%B7%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%BA%9C%E7%AB%8B%E7%94%9F%E9%87%8E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%BA%9C%E7%AB%8B%E7%B7%91%E9%A2%A8%E5%86%A0%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%BA%9C%E7%AB%8B%E8%8C%A8%E6%9C%A8%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%BA%9C%E7%AB%8B%E9%87%91%E5%B2%A1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E5%BA%9C%E8%AD%A6%E5%AF%9F%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E6%95%99%E5%93%A1%E5%9B%A3"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E6%9C%9D%E9%AE%AE%E9%AB%98%E7%B4%9A%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E7%94%A3%E6%A5%AD%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E5%A4%A7%E9%98%AA%E7%94%A3%E6%A5%AD%E5%A4%A7%E5%AD%A6%E9%99%84%E5%B1%9E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A9%E7%90%86%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E5%A4%A9%E7%90%86%E6%95%99%E6%A0%A1%E5%AD%A6%E5%9C%92%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%A9%E7%90%86%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A4%AA%E6%88%90%E5%AD%A6%E9%99%A2%E5%A4%A7%E5%AD%A6%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A5%88%E8%89%AF%E7%9C%8C%E7%AB%8B%E5%90%89%E9%87%8E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A5%88%E8%89%AF%E7%9C%8C%E7%AB%8B%E5%BE%A1%E6%89%80%E5%AE%9F%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%A5%88%E8%89%AF%E7%9C%8C%E7%AB%8B%E5%BE%A1%E6%89%80%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%AD%A6%E7%BF%92%E9%99%A2%E4%B8%AD%E3%83%BB%E9%AB%98%E7%AD%89%E7%A7%91"] <- ""
map_url["/wiki/%E5%AD%A6%E7%BF%92%E9%99%A2%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E5%AE%89%E5%B7%9D%E9%9B%BB%E6%A9%9F%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E5%AE%AE%E5%9F%8E%E7%9C%8C%E4%BD%90%E6%B2%BC%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%AE%AE%E5%9F%8E%E7%9C%8C%E6%B0%97%E4%BB%99%E6%B2%BC%E5%90%91%E6%B4%8B%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%AE%AE%E5%9F%8E%E7%9C%8C%E6%B0%B4%E7%94%A3%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%AE%AE%E5%B4%8E%E7%9C%8C%E7%AB%8B%E4%BD%90%E5%9C%9F%E5%8E%9F%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%AE%AE%E5%B4%8E%E7%9C%8C%E7%AB%8B%E5%AE%AE%E5%B4%8E%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%AE%AE%E5%B4%8E%E7%9C%8C%E7%AB%8B%E5%AE%AE%E5%B4%8E%E8%A5%BF%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%99%84%E5%B1%9E%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%AE%AE%E5%B4%8E%E7%9C%8C%E7%AB%8B%E5%BB%B6%E5%B2%A1%E6%98%9F%E9%9B%B2%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%AE%AE%E5%B4%8E%E7%9C%8C%E7%AB%8B%E5%BB%B6%E5%B2%A1%E6%9D%B1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%AE%AE%E5%B4%8E%E7%9C%8C%E7%AB%8B%E6%97%A5%E5%90%91%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%AE%AE%E5%B4%8E%E7%9C%8C%E7%AB%8B%E9%83%BD%E5%9F%8E%E6%B3%89%E3%83%B6%E4%B8%98%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%99%84%E5%B1%9E%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%AE%AE%E5%B4%8E%E7%9C%8C%E7%AB%8B%E9%AB%98%E9%8D%8B%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%AF%8C%E5%B1%B1%E7%9C%8C%E7%AB%8B%E5%AF%8C%E5%B1%B1%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%AF%8C%E5%B1%B1%E7%AC%AC%E4%B8%80%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B0%82%E4%BF%AE%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E5%B0%BC%E5%B4%8E%E5%B8%82%E7%AB%8B%E5%B0%BC%E5%B4%8E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B0%BE%E9%81%93%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B1%A5%E6%AD%A3%E7%A4%BE%E5%AD%A6%E5%9C%92%E8%B1%8A%E4%B8%AD%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E5%B1%A5%E6%AD%A3%E7%A4%BE%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B1%B1%E5%8F%A3%E7%9C%8C%E7%AB%8B%E5%A4%A7%E6%B4%A5%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B1%B1%E5%8F%A3%E7%9C%8C%E7%AB%8B%E5%AE%87%E9%83%A8%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B1%B1%E5%8F%A3%E7%9C%8C%E7%AB%8B%E8%90%A9%E5%95%86%E5%B7%A5%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B1%B1%E5%BD%A2%E7%9C%8C%E7%AB%8B%E5%B1%B1%E5%BD%A2%E4%B8%AD%E5%A4%AE%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B1%B1%E5%BD%A2%E7%9C%8C%E7%AB%8B%E9%85%92%E7%94%B0%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B1%B1%E6%A2%A8%E5%AD%A6%E9%99%A2%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E5%B1%B1%E6%A2%A8%E7%9C%8C%E7%AB%8B%E6%97%A5%E5%B7%9D%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B1%B1%E6%A2%A8%E7%9C%8C%E7%AB%8B%E6%A1%82%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B1%B1%E6%A2%A8%E7%9C%8C%E7%AB%8B%E7%94%B2%E5%BA%9C%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B2%90%E9%98%9C%E7%9C%8C%E7%AB%8B%E5%B2%90%E9%98%9C%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B2%A1%E5%B1%B1%E7%9C%8C%E7%AB%8B%E5%80%89%E6%95%B7%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B2%A1%E5%B1%B1%E7%9C%8C%E7%AB%8B%E5%B2%A1%E5%B1%B1%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B2%A1%E5%B1%B1%E7%9C%8C%E7%AB%8B%E5%B2%A1%E5%B1%B1%E6%9C%9D%E6%97%A5%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B2%A1%E5%B1%B1%E7%9C%8C%E9%AB%98%E6%A2%81%E6%97%A5%E6%96%B0%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B2%A1%E5%B4%8E%E5%9F%8E%E8%A5%BF%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B2%A1%E5%B4%8E%E5%B8%82%E7%AB%8B%E7%94%B2%E5%B1%B1%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B2%A9%E6%89%8B%E7%9C%8C%E7%AB%8B%E4%B8%8D%E6%9D%A5%E6%96%B9%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B2%A9%E6%89%8B%E7%9C%8C%E7%AB%8B%E5%AE%AE%E5%8F%A4%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B2%A9%E6%89%8B%E7%9C%8C%E7%AB%8B%E7%9B%9B%E5%B2%A1%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B2%A9%E6%89%8B%E7%9C%8C%E7%AB%8B%E9%87%9C%E7%9F%B3%E5%95%86%E5%B7%A5%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B2%A9%E6%89%8B%E7%9C%8C%E7%AB%8B%E9%87%9C%E7%9F%B3%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B2%A9%E6%89%8B%E7%9C%8C%E7%AB%8B%E9%BB%92%E6%B2%A2%E5%B0%BB%E5%8C%97%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B2%A9%E6%89%8B%E7%9C%8C%E7%AB%8B%E9%BB%92%E6%B2%A2%E5%B0%BB%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B3%B6%E6%A0%B9%E7%9C%8C%E7%AB%8B%E5%87%BA%E9%9B%B2%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B3%B6%E6%B4%A5%E8%A3%BD%E4%BD%9C%E6%89%80%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E5%B4%87%E5%BE%B3%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B4%87%E5%BE%B3%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B7%9D%E8%B6%8A%E6%9D%B1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B8%9D%E4%BA%AC%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B8%9D%E4%BA%AC%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E5%B8%B8%E7%B7%8F%E5%AD%A6%E9%99%A2%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B8%B8%E7%BF%94%E5%95%93%E5%85%89%E5%AD%A6%E5%9C%92%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B8%B8%E7%BF%94%E5%AD%A6%E5%9C%92%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%B8%B8%E7%BF%94%E5%AD%A6%E5%9C%92%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%BA%83%E5%B3%B6%E7%9C%8C%E7%AB%8B%E5%AE%89%E8%8A%B8%E5%8D%97%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%BA%83%E5%B3%B6%E7%9C%8C%E7%AB%8B%E5%B0%BE%E9%81%93%E5%95%86%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%BA%83%E5%B3%B6%E7%9C%8C%E7%AB%8B%E5%BA%83%E5%B3%B6%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%BA%83%E5%B3%B6%E7%9C%8C%E7%AB%8B%E7%A6%8F%E5%B1%B1%E8%AA%A0%E4%B9%8B%E9%A4%A8%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%BA%83%E5%B3%B6%E7%9C%8C%E7%AB%8B%E7%AB%B9%E5%8E%9F%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%BB%B6%E4%B8%96%E5%A4%A7%E5%AD%A6"] <- ""
map_url["/wiki/%E5%BE%A1%E6%89%80%E5%AE%9F%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%BE%B3%E5%B3%B6%E7%9C%8C%E7%AB%8B%E3%81%A4%E3%82%8B%E3%81%8E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%BE%B3%E5%B3%B6%E7%9C%8C%E7%AB%8B%E5%BE%B3%E5%B3%B6%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%BE%B3%E5%B3%B6%E7%9C%8C%E7%AB%8B%E8%84%87%E7%94%BA%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%BE%B3%E5%B3%B6%E7%9C%8C%E7%AB%8B%E8%B2%9E%E5%85%89%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E5%BF%97%E5%AD%A6%E9%A4%A8%E4%B8%AD%E7%AD%89%E9%83%A8%E3%83%BB%E9%AB%98%E7%AD%89%E9%83%A8_(%E5%8D%83%E8%91%89%E7%9C%8C)"] <- ""
map_url["/wiki/%E5%BF%97%E5%AD%B8%E9%A4%A8%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%84%9B%E5%AA%9B%E7%9C%8C%E7%AB%8B%E5%8C%97%E6%9D%A1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%84%9B%E5%AA%9B%E7%9C%8C%E7%AB%8B%E8%A5%BF%E6%9D%A1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%84%9B%E7%9F%A5%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%84%9B%E7%9F%A5%E6%95%99%E5%93%A1%E3%82%AF%E3%83%A9%E3%83%96"] <- ""
map_url["/wiki/%E6%84%9B%E7%9F%A5%E7%9C%8C%E7%AB%8B%E4%B8%89%E5%A5%BD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%84%9B%E7%9F%A5%E7%9C%8C%E7%AB%8B%E4%B8%AD%E6%9D%91%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%84%9B%E7%9F%A5%E7%9C%8C%E7%AB%8B%E5%8D%83%E7%A8%AE%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%84%9B%E7%9F%A5%E7%9C%8C%E7%AB%8B%E6%97%AD%E9%87%8E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%84%9B%E7%9F%A5%E7%9C%8C%E7%AB%8B%E6%98%8E%E5%92%8C%E9%AB%98%E6%A0%A1"] <- ""
map_url["/wiki/%E6%84%9B%E7%9F%A5%E7%9C%8C%E7%AB%8B%E6%98%AD%E5%92%8C%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%84%9B%E7%9F%A5%E7%9C%8C%E7%AB%8B%E8%B1%8A%E6%98%8E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%85%B6%E6%87%89%E7%BE%A9%E5%A1%BE%E9%AB%94%E8%82%B2%E6%9C%83%E8%B9%B4%E7%90%83%E9%83%A8"] <- ""
map_url["/wiki/%E6%85%B6%E7%86%99%E5%A4%A7%E5%AD%A6"] <- ""
map_url["/wiki/%E6%88%90%E5%9F%8E%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%88%90%E8%B9%8A%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%88%90%E8%B9%8A%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%8B%93%E6%AE%96%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%91%82%E5%8D%97%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%95%A6%E8%B3%80%E5%9B%BD%E9%9A%9B%E4%BB%A4%E5%92%8C%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%96%B0%E6%BD%9F%E7%9C%8C%E7%AB%8B%E5%B7%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%96%B0%E6%BD%9F%E7%9C%8C%E7%AB%8B%E6%96%B0%E6%BD%9F%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%96%B0%E6%BD%9F%E7%9C%8C%E7%AB%8B%E6%96%B0%E7%99%BA%E7%94%B0%E8%BE%B2%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%96%B0%E7%94%B0%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%97%A5%E6%96%B0%E8%A3%BD%E9%8B%BC"] <- ""
map_url["/wiki/%E6%97%A5%E6%9C%AC%E4%BD%93%E8%82%B2%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%97%A5%E6%9C%AC%E4%BD%93%E8%82%B2%E5%A4%A7%E5%AD%A6%E6%9F%8F%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%97%A5%E6%9C%AC%E4%BD%93%E8%82%B2%E5%A4%A7%E5%AD%A6%E8%8D%8F%E5%8E%9F%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%97%A5%E6%9C%AC%E5%9B%BD%E5%9C%9F%E9%96%8B%E7%99%BA"] <- ""
map_url["/wiki/%E6%97%A5%E6%9C%AC%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%97%A5%E6%9C%AC%E5%A4%A7%E5%AD%A6%E5%B7%A5%E5%AD%A6%E9%83%A8%E3%83%BB%E5%A4%A7%E5%AD%A6%E9%99%A2%E5%B7%A5%E5%AD%A6%E7%A0%94%E7%A9%B6%E7%A7%91"] <- ""
map_url["/wiki/%E6%97%A5%E6%9C%AC%E5%A4%A7%E5%AD%A6%E8%97%A4%E6%B2%A2%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%97%A5%E6%9C%AC%E5%A4%A7%E5%AD%A6%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%97%A5%E6%9C%AC%E6%96%87%E7%90%86%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%97%A5%E6%9C%AC%E6%96%87%E7%90%86%E5%A4%A7%E5%AD%A6%E9%99%84%E5%B1%9E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%97%A5%E6%9C%AC%E8%88%AA%E7%A9%BA%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E7%9F%B3%E5%B7%9D"] <- ""
map_url["/wiki/%E6%97%A5%E6%9C%AC%E8%A3%BD%E9%89%84%E5%85%AB%E5%B9%A1%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%97%A5%E6%9C%ACIBM%E3%83%93%E3%83%83%E3%82%B0%E3%83%96%E3%83%AB%E3%83%BC"] <- ""
map_url["/wiki/%E6%97%A5%E7%AB%8B%E8%A3%BD%E4%BD%9C%E6%89%80%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%97%A5%E9%87%8E%E8%87%AA%E5%8B%95%E8%BB%8A%E3%83%AC%E3%83%83%E3%83%89%E3%83%89%E3%83%AB%E3%83%95%E3%82%A3%E3%83%B3%E3%82%BA"] <- ""
map_url["/wiki/%E6%97%A9%E7%A8%B2%E7%94%B0%E5%A4%A7%E5%AD%A6%E7%B3%BB%E5%B1%9E%E6%97%A9%E7%A8%B2%E7%94%B0%E5%AE%9F%E6%A5%AD%E5%AD%A6%E6%A0%A1%E5%88%9D%E7%AD%89%E9%83%A8%E3%83%BB%E4%B8%AD%E7%AD%89%E9%83%A8%E3%83%BB%E9%AB%98%E7%AD%89%E9%83%A8"] <- ""
map_url["/wiki/%E6%97%A9%E7%A8%B2%E7%94%B0%E6%91%82%E9%99%B5%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%97%A9%E7%A8%B2%E7%94%B0%E6%91%82%E9%99%B5%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%98%8C%E5%B9%B3%E4%B8%AD%E5%AD%A6%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%98%8C%E5%B9%B3%E9%AB%98%E6%A0%A1"] <- ""
map_url["/wiki/%E6%98%8E%E5%92%8C%E7%9C%8C%E5%A4%AE%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%98%8E%E6%B2%BB%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%98%8E%E6%B2%BB%E5%A4%A7%E5%AD%A6%E4%BB%98%E5%B1%9E%E4%B8%AD%E9%87%8E%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%98%8E%E6%B2%BB%E5%A4%A7%E5%AD%A6%E4%BB%98%E5%B1%9E%E5%85%AB%E7%8E%8B%E5%AD%90%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%98%8E%E6%B2%BB%E5%AD%A6%E9%99%A2%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E6%98%8E%E6%B2%BB%E5%AD%A6%E9%99%A2%E6%9D%B1%E6%9D%91%E5%B1%B1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%98%8E%E6%B2%BB%E5%AD%A6%E9%99%A2%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%98%8E%E6%B2%BB%E5%AE%89%E7%94%B0%E7%94%9F%E5%91%BD%E3%83%9B%E3%83%BC%E3%83%AA%E3%83%BC%E3%82%BA"] <- ""
map_url["/wiki/%E6%9C%9D%E6%97%A5%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%9C%9D%E9%AE%AE%E5%A4%A7%E5%AD%A6%E6%A0%A1%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%9C%AD%E5%B9%8C%E5%B1%B1%E3%81%AE%E6%89%8B%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%B1%E4%BA%AC%E3%82%AC%E3%82%B9%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%9D%B1%E4%BA%AC%E5%A4%A7%E5%AD%A6%E9%81%8B%E5%8B%95%E4%BC%9A%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%9D%B1%E4%BA%AC%E8%BE%B2%E6%A5%AD%E5%A4%A7%E5%AD%A6%E7%AC%AC%E4%B8%80%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E4%B8%AD%E7%AD%89%E9%83%A8"] <- ""
map_url["/wiki/%E6%9D%B1%E4%BA%AC%E8%BE%B2%E6%A5%AD%E5%A4%A7%E5%AD%A6%E7%AC%AC%E4%BA%8C%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%B1%E4%BA%AC%E9%83%BD%E7%AB%8B%E5%9B%BD%E7%AB%8B%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%B1%E4%BA%AC%E9%83%BD%E7%AB%8B%E5%A4%A7%E6%B3%89%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%99%84%E5%B1%9E%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%B1%E4%BA%AC%E9%83%BD%E7%AB%8B%E5%B0%8F%E5%B1%B1%E5%8F%B0%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%B1%E4%BA%AC%E9%83%BD%E7%AB%8B%E5%BA%9C%E4%B8%AD%E8%A5%BF%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%B1%E5%A4%A7%E9%98%AA%E5%A4%A7%E5%AD%A6%E6%9F%8F%E5%8E%9F%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%B1%E5%A4%A7%E9%98%AA%E5%B8%82%E7%AB%8B%E6%97%A5%E6%96%B0%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%B1%E5%A4%A7%E9%98%AA%E5%B8%82%E7%AB%8B%E8%8B%B1%E7%94%B0%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%B1%E5%B1%B1%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%B1%E6%97%A5%E6%9C%AC%E9%9B%BB%E4%BF%A1%E9%9B%BB%E8%A9%B1"] <- ""
map_url["/wiki/%E6%9D%B1%E6%B4%8B%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%9D%B1%E6%B4%8B%E5%A4%A7%E5%AD%A6%E9%99%84%E5%B1%9E%E7%89%9B%E4%B9%85%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%B1%E6%B4%8B%E5%A4%A7%E5%AD%A6%E9%99%84%E5%B1%9E%E7%89%9B%E4%B9%85%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%B1%E6%B5%B7%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%9D%B1%E6%B5%B7%E5%A4%A7%E5%AD%A6%E4%BB%98%E5%B1%9E%E4%BB%B0%E6%98%9F%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%B1%E6%B5%B7%E5%A4%A7%E5%AD%A6%E4%BB%98%E5%B1%9E%E4%BB%B0%E6%98%9F%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E4%B8%AD%E7%AD%89%E9%83%A8"] <- ""
map_url["/wiki/%E6%9D%B1%E6%B5%B7%E5%A4%A7%E5%AD%A6%E4%BB%98%E5%B1%9E%E5%A4%A7%E9%98%AA%E4%BB%B0%E6%98%9F%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E4%B8%AD%E7%AD%89%E9%83%A8"] <- ""
map_url["/wiki/%E6%9D%B1%E6%B5%B7%E5%A4%A7%E5%AD%A6%E4%BB%98%E5%B1%9E%E7%9B%B8%E6%A8%A1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E4%B8%AD%E7%AD%89%E9%83%A8"] <- ""
map_url["/wiki/%E6%9D%B1%E6%B5%B7%E5%A4%A7%E5%AD%A6%E4%BB%98%E5%B1%9E%E7%A6%8F%E5%B2%A1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%B1%E6%B5%B7%E5%A4%A7%E5%AD%A6%E4%BB%98%E5%B1%9E%E9%9D%99%E5%B2%A1%E7%BF%94%E6%B4%8B%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E4%B8%AD%E7%AD%89%E9%83%A8"] <- ""
map_url["/wiki/%E6%9D%B1%E6%B5%B7%E5%A4%A7%E5%AD%A6%E4%BD%93%E8%82%B2%E4%BC%9A%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%83%95%E3%83%83%E3%83%88%E3%83%9C%E3%83%BC%E3%83%AB%E9%83%A8"] <- ""
map_url["/wiki/%E6%9D%B1%E6%B5%B7%E5%A4%A7%E5%AD%A6%E6%B9%98%E5%8D%97%E6%A0%A1%E8%88%8E%E4%BD%93%E8%82%B2%E4%BC%9A%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%83%95%E3%83%83%E3%83%88%E3%83%9C%E3%83%BC%E3%83%AB%E9%83%A8"] <- ""
map_url["/wiki/%E6%9D%B1%E6%B5%B7%E5%A4%A7%E5%AD%A6%E8%8F%85%E7%94%9F%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E4%B8%AD%E7%AD%89%E9%83%A8"] <- ""
map_url["/wiki/%E6%9D%B1%E7%A6%8F%E5%B2%A1%E8%87%AA%E5%BD%8A%E9%A4%A8%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E6%9D%B1%E7%A6%8F%E5%B2%A1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%B1%E8%8A%9D%E3%83%96%E3%83%AC%E3%82%A4%E3%83%96%E3%83%AB%E3%83%BC%E3%83%91%E3%82%B9"] <- ""
map_url["/wiki/%E6%9D%B1%E8%8A%9D%E5%A4%A7%E5%88%86%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%9D%B1%E8%8A%9D%E9%9D%92%E6%A2%85%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%9D%BE%E5%B1%B1%E8%81%96%E9%99%B5%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%BE%E6%88%B8%E5%B8%82%E7%AB%8B%E6%9D%BE%E6%88%B8%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9D%BE%E9%9F%BB%E5%AD%A6%E5%9C%92%E7%A6%8F%E5%B3%B6%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9E%9A%E6%96%B9%E5%B8%82%E7%AB%8B%E7%AC%AC%E4%B8%89%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%9E%9A%E6%96%B9%E5%B8%82%E7%AB%8B%E8%B9%89%E8%B7%8E%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%A0%83%E6%9C%A8%E7%9C%8C%E7%AB%8B%E4%BD%90%E9%87%8E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%99%84%E5%B1%9E%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%A0%84%E5%BE%B3%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%A0%97%E7%94%B0%E5%B7%A5%E6%A5%AD%E3%82%A6%E3%82%A9%E3%83%BC%E3%82%BF%E3%83%BC%E3%82%AC%E3%83%83%E3%82%B7%E3%83%A5"] <- ""
map_url["/wiki/%E6%A1%90%E7%94%9F%E5%A4%A7%E5%AD%A6%E9%99%84%E5%B1%9E%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E6%A1%90%E7%94%9F%E7%AC%AC%E4%B8%80%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%A1%90%E8%94%AD%E5%AD%A6%E5%9C%92%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%A1%90%E8%94%AD%E5%AD%A6%E5%9C%92%E4%B8%AD%E7%AD%89%E6%95%99%E8%82%B2%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%A1%90%E8%94%AD%E5%AD%A6%E5%9C%92%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%A8%B9%E5%BE%B3%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%AD%A3%E6%99%BA%E6%B7%B1%E8%B0%B7%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%B2%96%E7%B8%84%E7%9C%8C%E7%AB%8B%E3%82%B3%E3%82%B6%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%B2%96%E7%B8%84%E7%9C%8C%E7%AB%8B%E5%90%8D%E8%AD%B7%E5%95%86%E5%B7%A5%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%B2%96%E7%B8%84%E7%9C%8C%E7%AB%8B%E5%90%8D%E8%AD%B7%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%B2%96%E7%B8%84%E7%9C%8C%E7%AB%8B%E5%AE%AE%E5%8F%A4%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%B2%96%E7%B8%84%E7%9C%8C%E7%AB%8B%E7%9F%B3%E5%B7%9D%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%B3%95%E6%94%BF%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%B3%95%E6%94%BF%E5%A4%A7%E5%AD%A6%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%B3%95%E6%94%BF%E5%A4%A7%E5%AD%A6%E7%AC%AC%E4%BA%8C%E4%B8%AD%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%B3%95%E6%94%BF%E5%A4%A7%E5%AD%A6%E7%AC%AC%E4%BA%8C%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%B3%95%E6%94%BF%E7%AC%AC%E4%BA%8C%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%B5%81%E9%80%9A%E7%B5%8C%E6%B8%88%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E6%B5%81%E9%80%9A%E7%B5%8C%E6%B8%88%E5%A4%A7%E5%AD%A6%E4%BB%98%E5%B1%9E%E6%9F%8F%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%B5%A6%E5%AE%89D-Rocks"] <- ""
map_url["/wiki/%E6%B5%AA%E9%80%9F%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%B5%B7%E6%98%9F%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1_(%E9%95%B7%E5%B4%8E%E7%9C%8C)"] <- ""
map_url["/wiki/%E6%B8%85%E7%9C%9F%E5%AD%A6%E5%9C%92%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%B8%85%E7%9C%9F%E5%AD%A6%E5%9C%92%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%B9%98%E5%8D%97%E5%AD%A6%E5%9C%92%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%BB%8B%E8%B3%80%E7%9C%8C%E7%AB%8B%E5%85%AB%E5%B9%A1%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%BB%9D%E5%B7%9D%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E6%BB%9D%E5%B7%9D%E7%AC%AC%E4%BA%8C%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%86%8A%E6%9C%AC%E7%9C%8C%E7%AB%8B%E6%B0%B7%E5%B7%9D%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%86%8A%E6%9C%AC%E7%9C%8C%E7%AB%8B%E7%86%8A%E6%9C%AC%E5%8C%97%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%86%8A%E6%9C%AC%E7%9C%8C%E7%AB%8B%E7%86%8A%E6%9C%AC%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%86%8A%E6%9C%AC%E7%9C%8C%E7%AB%8B%E7%86%8A%E6%9C%AC%E8%A5%BF%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%86%8A%E6%9C%AC%E7%9C%8C%E7%AB%8B%E7%86%8A%E6%9C%AC%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%86%8A%E6%9C%AC%E7%9C%8C%E7%AB%8B%E8%8D%92%E5%B0%BE%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%8D%A8%E5%8D%94%E5%9F%BC%E7%8E%89%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%8E%89%E5%B7%9D%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E7%94%B2%E5%8D%97%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%99%BD%E9%B7%97%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E7%9B%AE%E9%BB%92%E5%AD%A6%E9%99%A2%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%9B%AE%E9%BB%92%E5%AD%A6%E9%99%A2%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%9C%9F%E5%92%8C%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%9F%B3%E5%B7%9D%E7%9C%8C%E7%AB%8B%E7%BE%BD%E5%92%8B%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%9F%B3%E5%B7%9D%E7%9C%8C%E7%AB%8B%E9%B6%B4%E6%9D%A5%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%9F%B3%E8%A6%8B%E6%99%BA%E7%BF%A0%E9%A4%A8%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A5%9E%E5%A5%88%E5%B7%9D%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E7%A5%9E%E5%A5%88%E5%B7%9D%E7%9C%8C%E7%AB%8B%E5%A4%A7%E7%A3%AF%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A5%9E%E5%A5%88%E5%B7%9D%E7%9C%8C%E7%AB%8B%E5%B7%9D%E5%B4%8E%E5%8C%97%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A5%9E%E5%A5%88%E5%B7%9D%E7%9C%8C%E7%AB%8B%E6%B9%98%E5%8D%97%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A5%9E%E5%A5%88%E5%B7%9D%E7%9C%8C%E7%AB%8B%E7%94%9F%E7%94%B0%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A5%9E%E5%A5%88%E5%B7%9D%E7%9C%8C%E7%AB%8B%E7%9B%B8%E6%A8%A1%E5%8F%B0%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A5%9E%E6%88%B8%E5%B8%82%E7%AB%8B%E7%A7%91%E5%AD%A6%E6%8A%80%E8%A1%93%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A5%9E%E6%88%B8%E6%9D%91%E9%87%8E%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A5%9E%E6%88%B8%E8%A3%BD%E9%8B%BC"] <- ""
map_url["/wiki/%E7%A5%9E%E6%88%B8%E8%A3%BD%E9%8B%BC%E3%82%B3%E3%83%99%E3%83%AB%E3%82%B3%E3%82%B9%E3%83%86%E3%82%A3%E3%83%BC%E3%83%A9%E3%83%BC%E3%82%BA"] <- ""
map_url["/wiki/%E7%A5%9E%E6%88%B8%E8%A3%BD%E9%8B%BC%E3%82%B3%E3%83%99%E3%83%AB%E3%82%B3%E3%82%B9%E3%83%86%E3%82%A3%E3%83%BC%E3%83%A9%E3%83%BC%E3%82%BA/wiki/%E6%97%A5%E6%9C%AC"] <- ""
map_url["/wiki/%E7%A6%8F%E4%BA%95%E7%9C%8C%E7%AB%8B%E8%8B%A5%E7%8B%AD%E6%9D%B1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B2%A1%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B2%A1%E5%B7%A5%E6%A5%AD%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B2%A1%E5%B7%A5%E6%A5%AD%E5%A4%A7%E5%AD%A6%E9%99%84%E5%B1%9E%E5%9F%8E%E6%9D%B1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B2%A1%E5%B8%82%E7%AB%8B%E5%9F%8E%E5%8D%97%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B2%A1%E5%B8%82%E7%AB%8B%E7%A6%8F%E5%B2%A1%E8%A5%BF%E9%99%B5%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B2%A1%E7%9C%8C%E7%AB%8B%E5%85%AB%E5%B9%A1%E4%B8%AD%E5%A4%AE%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B2%A1%E7%9C%8C%E7%AB%8B%E5%85%AB%E5%B9%A1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B2%A1%E7%9C%8C%E7%AB%8B%E5%8C%97%E4%B9%9D%E5%B7%9E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B2%A1%E7%9C%8C%E7%AB%8B%E5%B0%8F%E5%80%89%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B2%A1%E7%9C%8C%E7%AB%8B%E6%9D%B1%E7%AD%91%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B2%A1%E7%9C%8C%E7%AB%8B%E7%A6%8F%E5%B2%A1%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B2%A1%E7%9C%8C%E7%AB%8B%E7%AD%91%E7%B4%AB%E4%B8%98%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B2%A1%E7%9C%8C%E7%AB%8B%E7%AD%91%E7%B4%AB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B2%A1%E7%9C%8C%E7%AB%8B%E8%BC%9D%E7%BF%94%E9%A4%A8%E4%B8%AD%E7%AD%89%E6%95%99%E8%82%B2%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B2%A1%E7%9C%8C%E7%AB%8B%E9%A6%99%E6%A4%8E%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B3%B6%E7%9C%8C%E7%AB%8B%E7%A3%90%E5%9F%8E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A6%8F%E5%B3%B6%E7%9C%8C%E7%AB%8B%E7%A6%8F%E5%B3%B6%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A7%8B%E7%94%B0%E3%83%8E%E3%83%BC%E3%82%B6%E3%83%B3%E3%83%96%E3%83%AC%E3%83%83%E3%83%84"] <- ""
map_url["/wiki/%E7%A7%8B%E7%94%B0%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%B0%82%E9%96%80%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A7%8B%E7%94%B0%E7%9C%8C%E7%AB%8B%E7%94%B7%E9%B9%BF%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A7%8B%E7%94%B0%E7%9C%8C%E7%AB%8B%E7%A7%8B%E7%94%B0%E4%B8%AD%E5%A4%AE%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A7%8B%E7%94%B0%E7%9C%8C%E7%AB%8B%E7%A7%8B%E7%94%B0%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%A7%8B%E7%94%B0%E7%9C%8C%E7%AB%8B%E9%87%91%E8%B6%B3%E8%BE%B2%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%AB%8B%E5%91%BD%E9%A4%A8%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%AB%8B%E5%91%BD%E9%A4%A8%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E7%AB%8B%E6%95%99%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E7%AB%8B%E6%AD%A3%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E7%AD%91%E6%B3%A2%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E7%AD%91%E7%B4%AB%E5%8F%B0%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%BE%A4%E9%A6%AC%E7%9C%8C%E7%AB%8B%E5%A4%AA%E7%94%B0%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%BE%A4%E9%A6%AC%E7%9C%8C%E7%AB%8B%E9%AB%98%E5%B4%8E%E5%95%86%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%BE%A4%E9%A6%AC%E7%9C%8C%E7%AB%8B%E9%AB%98%E5%B4%8E%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E7%BF%92%E5%BF%97%E9%87%8E%E8%87%AA%E8%A1%9B%E9%9A%8A%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E7%BF%92%E5%BF%97%E9%87%8E%E8%87%AA%E8%A1%9B%E9%9A%8APARATROOPS"] <- ""
map_url["/wiki/%E8%88%88%E5%9C%8B%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E8%88%B9%E6%A9%8B%E5%B8%82%E7%AB%8B%E8%88%B9%E6%A9%8B%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E8%8A%B1%E5%9C%92%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E8%8A%B1%E5%9C%92%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E8%8C%97%E6%BA%AA%E5%AD%A6%E5%9C%92%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E8%8C%A8%E5%9F%8E%E7%9C%8C%E7%AB%8B%E6%97%A5%E7%AB%8B%E7%AC%AC%E4%B8%80%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%99%84%E5%B1%9E%E4%B8%AD%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E8%8C%A8%E5%9F%8E%E7%9C%8C%E7%AB%8B%E7%A3%AF%E5%8E%9F%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E8%A5%BF%E5%8D%97%E5%AD%A6%E9%99%A2%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E8%B1%8A%E7%94%B0%E8%87%AA%E5%8B%95%E7%B9%94%E6%A9%9F%E3%82%B7%E3%83%A3%E3%83%88%E3%83%AB%E3%82%BA"] <- ""
map_url["/wiki/%E8%B1%8A%E7%94%B0%E9%80%9A%E5%95%86BLUE_WING"] <- ""
map_url["/wiki/%E8%BF%91%E7%95%BF%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E8%BF%91%E9%89%84%E3%83%A9%E3%82%A4%E3%83%8A%E3%83%BC%E3%82%BA"] <- ""
map_url["/wiki/%E9%83%BD%E5%9F%8E%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%87%91%E5%85%89%E8%97%A4%E8%94%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%87%9C%E7%9F%B3%E3%82%B7%E3%83%BC%E3%82%A6%E3%82%A7%E3%82%A4%E3%83%96%E3%82%B9"] <- ""
map_url["/wiki/%E9%95%B7%E5%B4%8E%E5%8D%97%E5%B1%B1%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%95%B7%E5%B4%8E%E7%9C%8C%E7%AB%8B%E8%AB%AB%E6%97%A9%E8%BE%B2%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%95%B7%E5%B4%8E%E7%9C%8C%E7%AB%8B%E9%95%B7%E5%B4%8E%E5%8C%97%E9%99%BD%E5%8F%B0%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%95%B7%E5%B4%8E%E7%9C%8C%E7%AB%8B%E9%95%B7%E5%B4%8E%E5%8C%97%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%95%B7%E5%B4%8E%E7%9C%8C%E7%AB%8B%E9%95%B7%E5%B4%8E%E6%9D%B1%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%95%B7%E5%B4%8E%E7%9C%8C%E7%AB%8B%E9%95%B7%E5%B4%8E%E9%B6%B4%E6%B4%8B%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%95%B7%E9%87%8E%E7%9C%8C%E5%B2%A1%E8%B0%B7%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%95%B7%E9%87%8E%E7%9C%8C%E9%A3%AF%E7%94%B0%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%96%8B%E5%BF%97%E5%9B%BD%E9%9A%9B%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%96%A2%E5%B8%82%E7%AB%8B%E9%96%A2%E5%95%86%E5%B7%A5%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%96%A2%E6%9D%B1%E5%AD%A6%E9%99%A2%E4%B8%AD%E5%AD%A6%E6%A0%A1%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%96%A2%E6%9D%B1%E5%AD%A6%E9%99%A2%E5%85%AD%E6%B5%A6%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%96%A2%E6%9D%B1%E5%AD%A6%E9%99%A2%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E9%96%A2%E8%A5%BF%E4%B8%B8%E5%92%8C%E3%83%AD%E3%82%B8%E3%82%B9%E3%83%86%E3%82%A3%E3%83%83%E3%82%AF%E3%82%B9%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E9%96%A2%E8%A5%BF%E5%89%B5%E4%BE%A1%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%96%A2%E8%A5%BF%E5%A4%A7%E5%80%89%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%96%A2%E8%A5%BF%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E9%96%A2%E8%A5%BF%E5%A4%A7%E5%AD%A6%E5%8C%97%E9%99%BD%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%96%A2%E8%A5%BF%E5%AD%A6%E9%99%A2%E4%B8%AD%E5%AD%A6%E9%83%A8%E3%83%BB%E9%AB%98%E7%AD%89%E9%83%A8"] <- ""
map_url["/wiki/%E9%96%A2%E8%A5%BF%E5%AD%A6%E9%99%A2%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E9%96%A2%E8%A5%BF%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%98%B2%E8%A1%9B%E5%A4%A7%E5%AD%A6%E6%A0%A1%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E9%9D%92%E5%B1%B1%E5%AD%A6%E9%99%A2%E4%B8%AD%E7%AD%89%E9%83%A8%E3%83%BB%E9%AB%98%E7%AD%89%E9%83%A8"] <- ""
map_url["/wiki/%E9%9D%92%E5%B1%B1%E5%AD%A6%E9%99%A2%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E9%9D%92%E6%A3%AE%E5%B1%B1%E7%94%B0%E4%B8%AD%E5%AD%A6%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%9D%92%E6%A3%AE%E7%9C%8C%E7%AB%8B%E4%B8%89%E6%9C%AC%E6%9C%A8%E8%BE%B2%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%9D%92%E6%A3%AE%E7%9C%8C%E7%AB%8B%E4%B8%89%E6%B2%A2%E5%95%86%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%9D%92%E6%A3%AE%E7%9C%8C%E7%AB%8B%E5%85%AB%E6%88%B8%E8%A5%BF%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%9D%92%E6%A3%AE%E7%9C%8C%E7%AB%8B%E5%85%AB%E6%88%B8%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%9D%92%E6%A3%AE%E7%9C%8C%E7%AB%8B%E5%BC%98%E5%89%8D%E5%AE%9F%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%9D%92%E6%A3%AE%E7%9C%8C%E7%AB%8B%E9%9D%92%E6%A3%AE%E5%8C%97%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%9D%92%E6%A3%AE%E7%9C%8C%E7%AB%8B%E9%9D%92%E6%A3%AE%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%9D%99%E5%B2%A1%E7%9C%8C%E7%AB%8B%E6%B5%9C%E6%9D%BE%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%9D%99%E5%B2%A1%E7%9C%8C%E7%AB%8B%E6%B8%85%E6%B0%B4%E5%8D%97%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%A6%99%E5%B7%9D%E7%9C%8C%E7%AB%8B%E9%AB%98%E6%9D%BE%E5%8C%97%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%AB%98%E5%B2%A1%E7%AC%AC%E4%B8%80%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%AB%98%E6%A0%A1%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E6%97%A5%E6%9C%AC%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/%E9%AB%98%E7%9F%A5%E4%B8%AD%E5%A4%AE%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%B9%BF%E5%85%90%E5%B3%B6%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/%E9%B9%BF%E5%85%90%E5%B3%B6%E5%AE%9F%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%B9%BF%E5%85%90%E5%B3%B6%E5%B8%82%E7%AB%8B%E9%B9%BF%E5%85%90%E5%B3%B6%E7%8E%89%E9%BE%8D%E4%B8%AD%E5%AD%A6%E6%A0%A1%E3%83%BB%E9%B9%BF%E5%85%90%E5%B3%B6%E7%8E%89%E9%BE%8D%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%B9%BF%E5%85%90%E5%B3%B6%E7%9C%8C%E7%AB%8B%E5%8A%A0%E6%B2%BB%E6%9C%A8%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%B9%BF%E5%85%90%E5%B3%B6%E7%9C%8C%E7%AB%8B%E5%A4%A7%E5%8F%A3%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%B9%BF%E5%85%90%E5%B3%B6%E7%9C%8C%E7%AB%8B%E5%A4%A7%E5%B3%B6%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%B9%BF%E5%85%90%E5%B3%B6%E7%9C%8C%E7%AB%8B%E9%B9%BF%E5%85%90%E5%B3%B6%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%B9%BF%E5%85%90%E5%B3%B6%E7%9C%8C%E7%AB%8B%E9%B9%BF%E5%B1%8B%E5%B7%A5%E6%A5%AD%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%B9%BF%E5%85%90%E5%B3%B6%E7%9C%8C%E7%AB%8B%E9%B9%BF%E5%B1%8B%E9%AB%98%E7%AD%89%E5%AD%A6%E6%A0%A1"] <- ""
map_url["/wiki/%E9%BE%8D%E8%B0%B7%E5%A4%A7%E5%AD%A6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/7%E4%BA%BA%E5%88%B6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E7%94%B7%E5%AD%90%E3%82%A2%E3%82%A4%E3%83%AB%E3%83%A9%E3%83%B3%E3%83%89%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/7%E4%BA%BA%E5%88%B6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E7%94%B7%E5%AD%90%E3%82%A6%E3%82%A7%E3%83%BC%E3%83%AB%E3%82%BA%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/7%E4%BA%BA%E5%88%B6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E7%94%B7%E5%AD%90%E3%82%B5%E3%83%A2%E3%82%A2%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/7%E4%BA%BA%E5%88%B6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E7%94%B7%E5%AD%90%E3%83%8B%E3%83%A5%E3%83%BC%E3%82%B8%E3%83%BC%E3%83%A9%E3%83%B3%E3%83%89%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/7%E4%BA%BA%E5%88%B6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E7%94%B7%E5%AD%90%E5%8D%97%E3%82%A2%E3%83%95%E3%83%AA%E3%82%AB%E5%85%B1%E5%92%8C%E5%9B%BD%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/7%E4%BA%BA%E5%88%B6%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E7%94%B7%E5%AD%90%E6%97%A5%E6%9C%AC%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/AS%E3%83%99%E3%82%B8%E3%82%A8%E3%83%BB%E3%82%A8%E3%83%AD%E3%83%BC"] <- ""
map_url["/wiki/ASM%E3%82%AF%E3%83%AC%E3%83%AB%E3%83%A2%E3%83%B3%E3%83%BB%E3%82%AA%E3%83%BC%E3%83%B4%E3%82%A7%E3%83%AB%E3%83%8B%E3%83%A5"] <- ""
map_url["/wiki/AZ-COM%E4%B8%B8%E5%92%8CMOMOTARO%27S"] <- ""
map_url["/wiki/BIG_BLUES"] <- ""
map_url["/wiki/CA%E3%82%B5%E3%83%B3%E3%83%BB%E3%82%A4%E3%82%B7%E3%83%89%E3%83%AD"] <- ""
map_url["/wiki/CA%E3%83%96%E3%83%AA%E3%83%BC%E3%83%B4"] <- ""
map_url["/wiki/CA%E3%83%AD%E3%82%B5%E3%83%AA%E3%82%AA"] <- ""
map_url["/wiki/CS%E3%83%87%E3%82%A3%E3%83%8A%E3%83%A2%E3%83%BB%E3%83%96%E3%82%AF%E3%83%AC%E3%82%B7%E3%83%A5%E3%83%86%E3%82%A3"] <- ""
map_url["/wiki/CS%E3%83%96%E3%83%AB%E3%82%B4%E3%83%AF%E3%83%B3%EF%BC%9D%E3%82%B8%E3%83%A3%E3%82%A4%E3%83%A6%E3%83%BC"] <- ""
map_url["/wiki/CSA%E3%82%B9%E3%83%86%E3%82%A2%E3%82%A6%E3%82%A2%E3%83%BB%E3%83%96%E3%82%AF%E3%83%AC%E3%82%B7%E3%83%A5%E3%83%86%E3%82%A3"] <- ""
map_url["/wiki/CSM%E3%82%B7%E3%83%A5%E3%83%86%E3%82%A3%E3%82%A4%E3%83%B3%E3%83%84%E3%82%A1%E3%83%BB%E3%83%90%E3%83%A4%E3%83%BB%E3%83%9E%E3%83%BC%E3%83%AC"] <- ""
map_url["/wiki/CSM%E3%83%96%E3%82%AB%E3%83%AC%E3%82%B9%E3%83%88"] <- ""
map_url["/wiki/EP%E3%82%AD%E3%83%B3%E3%82%B0%E3%82%B9"] <- ""
map_url["/wiki/FC%E3%82%AA%E3%83%BC%E3%82%B7%E3%83%A5%E3%83%BB%E3%82%B8%E3%82%A7%E3%83%BC%E3%83%AB"] <- ""
map_url["/wiki/FC%E3%82%B0%E3%83%AB%E3%83%8E%E3%83%BC%E3%83%96%E3%83%AB"] <- ""
map_url["/wiki/FC%E3%83%90%E3%83%AB%E3%82%BB%E3%83%AD%E3%83%8A_(%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC)"] <- ""
map_url["/wiki/JAPAN_XV"] <- ""
map_url["/wiki/JR%E4%B9%9D%E5%B7%9E%E3%82%B5%E3%83%B3%E3%83%80%E3%83%BC%E3%82%B9"] <- ""
map_url["/wiki/JR%E6%9D%B1%E6%97%A5%E6%9C%AC%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%83%A8"] <- ""
map_url["/wiki/JR%E8%A5%BF%E6%97%A5%E6%9C%AC%E3%83%AC%E3%82%A4%E3%83%A9%E3%83%BC%E3%82%BA"] <- ""
map_url["/wiki/LA%E3%82%AE%E3%83%AB%E3%83%86%E3%82%A3%E3%83%8B%E3%82%B9"] <- ""
map_url["/wiki/NEC%E3%82%B0%E3%83%AA%E3%83%BC%E3%83%B3%E3%83%AD%E3%82%B1%E3%83%83%E3%83%84"] <- ""
map_url["/wiki/NEC%E3%82%B0%E3%83%AA%E3%83%BC%E3%83%B3%E3%83%AD%E3%82%B1%E3%83%83%E3%83%84%E6%9D%B1%E8%91%9B"] <- ""
map_url["/wiki/New_Zealand_national_rugby_union_team"] <- ""
map_url["/wiki/NTT%E3%82%B3%E3%83%9F%E3%83%A5%E3%83%8B%E3%82%B1%E3%83%BC%E3%82%B7%E3%83%A7%E3%83%B3%E3%82%BA%E3%82%B7%E3%83%A3%E3%82%A4%E3%83%8B%E3%83%B3%E3%82%B0%E3%82%A2%E3%83%BC%E3%82%AF%E3%82%B9"] <- ""
map_url["/wiki/NTT%E3%82%B3%E3%83%9F%E3%83%A5%E3%83%8B%E3%82%B1%E3%83%BC%E3%82%B7%E3%83%A7%E3%83%B3%E3%82%BA_%E3%82%B7%E3%83%A3%E3%82%A4%E3%83%8B%E3%83%B3%E3%82%B0%E3%82%A2%E3%83%BC%E3%82%AF%E3%82%B9%E6%9D%B1%E4%BA%AC%E3%83%99%E3%82%A4%E6%B5%A6%E5%AE%89"] <- ""
map_url["/wiki/NTT%E3%83%89%E3%82%B3%E3%83%A2%E3%83%AC%E3%83%83%E3%83%89%E3%83%8F%E3%83%AA%E3%82%B1%E3%83%BC%E3%83%B3%E3%82%BA"] <- ""
map_url["/wiki/NTT%E3%83%89%E3%82%B3%E3%83%A2%E3%83%AC%E3%83%83%E3%83%89%E3%83%8F%E3%83%AA%E3%82%B1%E3%83%BC%E3%83%B3%E3%82%BA%E5%A4%A7%E9%98%AA"] <- ""
map_url["/wiki/RC%E3%82%A2%E3%82%A4%E3%82%A2%E3%83%BB%E3%82%AF%E3%82%BF%E3%82%A4%E3%82%B7"] <- ""
map_url["/wiki/RC%E3%83%88%E3%82%A5%E3%83%BC%E3%83%AD%E3%83%B3"] <- ""
map_url["/wiki/RC%E3%83%8A%E3%83%AB%E3%83%9C%E3%83%B3%E3%83%8C"] <- ""
map_url["/wiki/RC%E3%83%9E%E3%82%B7%E3%83%BC"] <- ""
map_url["/wiki/RC%E3%83%AD%E3%82%B3%E3%83%A2%E3%83%86%E3%82%A3%E3%83%B4%E3%82%A3%E3%83%BB%E3%83%88%E3%83%93%E3%83%AA%E3%82%B7"] <- ""
map_url["/wiki/RC%E3%83%B4%E3%82%A1%E3%83%B3%E3%83%8C"] <- ""
map_url["/wiki/RC_AIA%E3%82%AF%E3%82%BF%E3%82%A4%E3%82%B7"] <- ""
map_url["/wiki/SC%E3%82%A2%E3%83%AB%E3%83%93"] <- ""
map_url["/wiki/SCM%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%83%BB%E3%83%86%E3%82%A3%E3%83%9F%E3%82%B7%E3%83%A7%E3%82%A2%E3%83%A9"] <- ""
map_url["/wiki/SU%E3%82%A2%E3%82%B8%E3%83%A3%E3%83%B3"] <- ""
map_url["/wiki/SWD%E3%82%A4%E3%83%BC%E3%82%B0%E3%83%AB%E3%82%B9"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%82%A2%E3%82%A4%E3%83%AB%E3%83%A9%E3%83%B3%E3%83%89%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%82%A2%E3%83%A1%E3%83%AA%E3%82%AB%E5%90%88%E8%A1%86%E5%9B%BD%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%82%A2%E3%83%AB%E3%82%BC%E3%83%B3%E3%83%81%E3%83%B3%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%82%A4%E3%82%BF%E3%83%AA%E3%82%A2%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%82%A4%E3%83%B3%E3%82%B0%E3%83%A9%E3%83%B3%E3%83%89%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%82%A6%E3%82%A7%E3%83%BC%E3%83%AB%E3%82%BA%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%82%A6%E3%83%AB%E3%82%B0%E3%82%A2%E3%82%A4%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%82%AA%E3%83%BC%E3%82%B9%E3%83%88%E3%83%A9%E3%83%AA%E3%82%A2%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%82%AB%E3%83%8A%E3%83%80%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%82%B5%E3%83%A2%E3%82%A2%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%82%B8%E3%83%A7%E3%83%BC%E3%82%B8%E3%82%A2%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%82%B9%E3%82%B3%E3%83%83%E3%83%88%E3%83%A9%E3%83%B3%E3%83%89%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%83%81%E3%83%AA%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%83%88%E3%83%B3%E3%82%AC%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%83%8A%E3%83%9F%E3%83%93%E3%82%A2%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%83%8B%E3%83%A5%E3%83%BC%E3%82%B8%E3%83%BC%E3%83%A9%E3%83%B3%E3%83%89%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%83%95%E3%82%A3%E3%82%B8%E3%83%BC%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%83%95%E3%83%A9%E3%83%B3%E3%82%B9%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%83%9D%E3%83%AB%E3%83%88%E3%82%AC%E3%83%AB%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E3%83%AB%E3%83%BC%E3%83%9E%E3%83%8B%E3%82%A2%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E5%8D%97%E3%82%A2%E3%83%95%E3%83%AA%E3%82%AB%E5%85%B1%E5%92%8C%E5%9B%BD%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E6%97%A5%E6%9C%AC%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U-20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E9%A6%99%E6%B8%AF%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U17%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E6%97%A5%E6%9C%AC%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U19%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E6%97%A5%E6%9C%AC%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/U20%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC%E6%97%A5%E6%9C%AC%E4%BB%A3%E8%A1%A8"] <- ""
map_url["/wiki/UE%E3%82%B5%E3%83%B3%E3%83%9C%E3%82%A4%E3%82%A2%E3%83%8A"] <- ""
map_url["/wiki/US%E3%82%AB%E3%83%AB%E3%82%AB%E3%82%BD%E3%83%B3%E3%83%8C"] <- ""
map_url["/wiki/US%E3%82%B3%E3%83%AD%E3%83%9F%E3%82%A8"] <- ""
map_url["/wiki/US%E3%83%80%E3%82%AF%E3%82%B9"] <- ""
map_url["/wiki/US%E3%83%96%E3%83%AC%E3%83%83%E3%82%B5%E3%83%B3%E3%83%8C"] <- ""
map_url["/wiki/US%E3%83%A2%E3%83%B3%E3%83%88%E3%83%BC%E3%83%90%E3%83%B3"] <- ""
map_url["/wiki/USA%E3%83%9A%E3%83%AB%E3%83%94%E3%83%8B%E3%83%A3%E3%83%B3"] <- ""
map_url["/wiki/USON%E3%83%8C%E3%83%B4%E3%82%A7%E3%83%BC%E3%83%AB%E3%83%BB%E3%83%A9%E3%82%B0%E3%83%93%E3%83%BC"] <- ""
map_url["/wiki/VVA%E3%83%9D%E3%83%89%E3%83%AB%E3%82%B9%E3%82%AF"] <- ""
# translation map
map_ja <- c()
map_ja["アイルシャム"] <- "Aylsham"


# clean locations
tlog(4, "Substituting in the table")
cols <- c("birthPlace", "deathPlace")
for (col in cols) {
  tlog(6, "Normalizing \"", col, "\"")
  # split place names
  all_places <- players[, col]
  all_places <- gsub("\\[.+\\]", "", all_places, fixed = FALSE)
  all_places <- strsplit(all_places, "; ")
  # split place urls
  all_urls <- players[, paste0(col, "WP")]
  all_urls <- gsub("\\[.+\\]", "", all_urls, fixed = FALSE)
  all_urls <- strsplit(all_urls, "; ")

  # loop over table rows (ie players)
  for (p in 1:length(all_places)) {
    places <- all_places[[p]]
    urls <- all_urls[[p]]

    if (length(places) == 0) {
      places <- " "
    } else {
      # normalize place names
      for (url in names(map_url))
        places[urls == url] <- map_url[url]

      # translate remaining names
      for (ja_name in names(map_ja))
        places[places == ja_name] <- map_ja[ja_name]

      # remove duplicates
      places <- gsub(", ?", "; ", places, fixed = FALSE)
      places <- unique(unlist(strsplit(places, "; ")))
    }

    # update list
    all_places[[p]] <- places
  }

  # collapse to get strings again
  all_places <- sapply(all_places, function(places) paste0(places, collapse = "; "))
  all_places[all_places == "NA"] <- NA
  names(all_places) <- NULL
  players[, col] <- all_places
}
# debug
#all_places <- c(players[, "birthPlace"], players[, "deathPlace"])
#all_places <- gsub("\\[.+\\]", "", all_places, fixed = FALSE)
#all_places <- strsplit(all_places, "; ")
#all_places <- sort(unique(unlist(all_places)))
#print(tail(all_places))
# debug
#print(head(players[, c("birthPlace", "deathPlace")]))
