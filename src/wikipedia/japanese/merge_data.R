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
tlog(2, "Raw number of players: ", nrow(players))
wp_players <- wp_players %>% mutate(across(where(is.character), ~ na_if(., "")))

careers <- read.csv(file.path(wp_folder, "careers.csv"))
tlog(2, "Raw number of career steps: ", nrow(careers))
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
    if(p %% 100 == 0)
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

# alternative names are processed separately
# map["fullName"] <- "altNames"

# record as a new CSV file
tab.file <- file.path(fusion_folder, "players_02_ja-wp.csv")
tlog(2, "Recording as a CSV file: \"", tab.file, "\"")
write.csv(our_players, tab.file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# merge career steps
tlog("Merging career steps")
