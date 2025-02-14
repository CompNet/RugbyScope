########################################################################
# Functions used to compare the merged player table to a list of internationals
# semi-automatically retrieved from Wikipedia.
#
# Vincent Labatut
# 01/2025
########################################################################
source("src/common/logging.R")




########################################################################
# paths
dpb_table_folder <- file.path("data", "dbpedia", "tables")
wp_table_folder <- file.path("data", "wikipedia", "temp_vl")




########################################################################
# load Wikipedia list of internationals
tab_file <- file.path(wp_table_folder, "international_players_urls.csv")
tlog("Loading the Wikipedia list of players: \"", tab_file, "\"")
wp_players <- read.csv(tab_file)
tlog(2, "Number of players in the WP list: ", nrow(wp_players))
# write.csv(wp_players, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# keep only the name of the WP page (not the beginning of the URL)
urls <- wp_players[, "url"]
urls <- substr(urls, start = nchar("https://en.wikipedia.org/wiki/") + 1, stop = nchar(urls) + 1)
wp_players[, "url"] <- urls




########################################################################
# load merged player table
tab_file <- file.path(dpb_table_folder, "players_01_wd-dbp.csv")
tlog("Loading the merged player table: \"", tab_file, "\"")
fus_players <- read.csv(tab_file)
tlog(2, "Number of players in our table: ", nrow(fus_players))




########################################################################
tlog("Some stats regarding matching between the WP list and our table")

# match the list to our table based on WP URLs
no_wp <- which(is.na(wp_players[, "url"]))
tlog(2, "WP entries without a WP page: ", length(no_wp), "/", nrow(wp_players))
#
has_wp <- which(!is.na(wp_players[, "url"]))
idx <- match(wp_players[has_wp, "url"], fus_players[, "wikipediaEn"])
tlog(2, "WP entries with a WP page that match a player in our table: ", length(which(!is.na(idx))), "/", length(has_wp))
#
tlog(2, "List of the WP entries with a WP page not found in our table:")
print(wp_players[has_wp[is.na(idx)], ])
tab_file <- file.path(wp_table_folder, "internationals__missing_from_fusion.csv")
tlog(4, "Exporting as a CSV file in: ", tab_file)
write.csv(wp_players[has_wp[is.na(idx)], ], tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# same thing using names for the players without a WP page
wp_names <- normalize_player_names(wp_players[no_wp, "name"])
idx0 <- (1:nrow(fus_players))[-sort(unique(idx))]
fus_names <- normalize_player_names(fus_players[idx0, "fullName"])
mm <- match(wp_names, fus_names)
idx1 <- which(!is.na(mm))
tlog(2, "WP entries without a WP page that match a player's name in our table: ", length(idx1), "/", length(no_wp))
tab_file <- file.path(wp_table_folder, "internationals__matching_names_in_fusion.csv")
tlog(4, "Exporting as a CSV file in: ", tab_file)
tt <- cbind(wp_names[idx1], fus_players[idx0, "wikipediaEn"][mm[idx1]])
write.csv(tt, tab_file, row.names = FALSE, fileEncoding = "UTF-8")
