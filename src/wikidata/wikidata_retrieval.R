# Script designed to extract raw data from Wikidata.
#
# We had to break down the queries in small bits in order
# for them to run on the Wikidata server.
#
# The SPARQL queries are available in folder `queries/wikidata`.
#
# Note: this script runs correctly as of 22/12/2024, but that
# may not be true in the future, depending on the evolution of 
# Wikidata data. The CSV files produced by this scripts are 
# available in folder `out/wikidata.`
#
# Vincent Labatut
# 12/2024
########################################################################
library("readtext")
library("WikidataR")
library("dplyr")




########################################################################
# paths
query.folder <- file.path("queries")
out.folder <- file.path("out", "wikidata")




########################################################################
# extraction of player list
cat("Retrieving the list of player IDs from WD\n")

# load query file
query <- readtext(file.path(query.folder, "wd_players_list.sparql"))$text

# run query and get list of ids
player_ids <- query_wikidata(query)$playerId
cat("Number of player IDs retrieved:", length(player_ids), "\n")




########################################################################
# extraction of player information
cat("Retrieving the individual information of each player (may take a while)\n")
col_names <- c(
  "playerId", "playerLabel", 
  "firstnameLabels", "lastnameLabels", "sexLabel", 
  "dobMax", "dobFormat", "pobLabels", "dodMax", "dodFormat", "podLabels", 
  "citizenshipLabels", "sportCountryLabels", 
  "positionLabels", "careerStartYears", "careerEndYears", 
  "masses", "heights", 
  "ESPNscrumIDs", "AllRugbyIDs", "GoogleKnowlIDs", "ItsRugbyIDs", "RugbyDatabaseIDs",
  "articleEn", "articleFr", "articleEs", "articleJa"
)

# load query file
query <- readtext(file.path(query.folder, "wd_players_info.sparql"))$text
# remove the comments/spaces/newlines, otherwise the query is too long
query <- gsub("#[^\r\n]*[\r\n]+", "\n", query)
query <- gsub("  +", " ", query)
query <- gsub("[\r\n]+", "\n", query)
query <- gsub(" *[\r\n] *", "\n", query)

# init players table
players <- as.data.frame(matrix(NA, nrow=length(player_ids), ncol=length(col_names)))
colnames(players) <- col_names

# run query for each player
for (p in 1:length(player_ids)) {
  # get player ID
  player_id <- player_ids[p]
  cat("++++++++++++ Processing player ", player_id, " (", p, "/", length(player_ids), ")\n", sep="")

  # run query
  pl_query <- gsub("QQQQQQ", player_id, query, fixed = TRUE)
  row <- query_wikidata(pl_query)
  print.data.frame(row)
  if(nrow(row) > 1)
    stop(paste0("ERROR: several rows returned for one player (some field probably contains multiple values). Player ID= "), player_id)

  # add to table
  players[p, col_names] <- row[1, col_names]
}

# replacing empty strings by NAs
players <- players %>% mutate(across(where(is.character), ~ na_if(., "")))

# display a few details for verification
cat("Dimension of the players table:", paste(dim(players), collapse = ", "), "\n")
cat("Classes of the columns: ", paste(apply(players, 2, class), collapse = ", "), "\n")
cat("Top of the table:\n");
print.data.frame(players[1:10, ])

# export table as a CSV
write.csv(x = players, file = file.path(out.folder, "all_pro_players_descr.csv"), row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# extraction of team list
cat("Retrieving the list of team IDs from WD\n")

# load query file
query <- readtext(file.path(query.folder, "wd_teams_list.sparql"))$text

# run query and get list of ids
team_ids <- query_wikidata(query)$clubId
cat("Number of teams IDs retrieved:", length(team_ids), "\n")




########################################################################
# extraction of team information
cat("Retrieving the individual information of each team (may take a while)\n")
col_names <- c(
  "clubId", "clubLabel", "clubTypeLabel",
  "inceptionMax", "inceptionFormat",
  "terminationMax", "terminationFormat",
  "nickmaneLabels", "affiliationLabels",
  "countryLabels", "competitionLabels",
  "homeVenueLabels", "homeVenueCapacities", "locationLabels",
  "AllRugbyIDs", "GoogleKnowlIDs"
)

# load query file
query <- readtext(file.path(query.folder, "wd_teams_info.sparql"))$text
# remove the comments/spaces/newlines, otherwise the query is too long
query <- gsub("#[^\r\n]*[\r\n]+", "\n", query)
query <- gsub("  +", " ", query)
query <- gsub("[\r\n]+", "\n", query)
query <- gsub(" *[\r\n] *", "\n", query)

# init teams table
teams <- as.data.frame(matrix(NA, nrow=length(team_ids), ncol=length(col_names)))
colnames(teams) <- col_names

# run query for each team
for (t in 864:length(team_ids)) {
  # get team ID
  team_id <- team_ids[t]
  cat("++++++++++++ Processing team ", team_id, " (", t, "/", length(team_ids), ")\n", sep="")

  # run query
  tm_query <- gsub("QQQQQQ", team_id, query, fixed = TRUE)
  row <- query_wikidata(tm_query)
  print.data.frame(row)
  if(nrow(row) > 1)
    stop(paste0("ERROR: several rows returned for one team (some field probably contains multiple values). Team ID= "), player_id)

  # add to table
  teams[t, col_names] <- row[1, col_names]
}

# replacing empty strings by NAs
teams <- teams %>% mutate(across(where(is.character), ~ na_if(., "")))

# display a few details for verification
cat("Dimension of the teams table:", paste(dim(teams), collapse = ", "), "\n")
cat("Classes of the columns: ", paste(apply(teams, 2, class), collapse = ", "), "\n")
cat("Top of the table:\n")
print.data.frame(teams[1:10, ])

# export table as a CSV
write.csv(x = teams, file = file.path(out.folder, "all_pro_teams_descr.csv"), row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# extraction of player careers
cat("Retrieving players' careers (may take a while)\n")
col_names <- c(
  "playerId",
  "clubId",
  "startYear", "endYear",
  "played", "points"
)

# load query file
query <- readtext(file.path(query.folder, "wd_career_steps.sparql"))$text
# remove the comments/spaces/newlines, otherwise the query is too long
query <- gsub("#[^\r\n]*[\r\n]+", "\n", query)
query <- gsub("  +", " ", query)
query <- gsub("[\r\n]+", "\n", query)
query <- gsub(" *[\r\n] *", "\n", query)

# init careers table
careers <- as.data.frame(matrix(NA, nrow = 1, ncol=length(col_names)))
colnames(careers) <- col_names
careers <- careers[-1, , drop = FALSE]

# run query for each player
for (p in 1:length(player_ids)) {
  # get player ID
  player_id <- player_ids[p]
  cat("++++++++++++ Processing player ", player_id, " (", p, "/", length(player_ids), ")\n", sep="")

  # run query
  cr_query <- gsub("QQQQQQ", player_id, query, fixed = TRUE)
  row <- query_wikidata(cr_query)
  print.data.frame(row)

  # add to table
  careers <- rbind(careers, row[, col_names])
}

# replacing empty strings by NAs
careers <- careers %>% mutate(across(where(is.character), ~ na_if(., "")))

# display a few details for verification
cat("Dimension of the careers table:", paste(dim(careers), collapse = ", "), "\n")
cat("Classes of the columns: ", paste(apply(careers, 2, class), collapse = ", "), "\n")
cat("Top of the table:\n");
print.data.frame(careers[1:10, ])

# add player and team names
idx <- match(unlist(careers[, "playerId"]), players[, "playerId"])
plyr_names <- players[idx, "playerLabel"]
idx <- match(unlist(careers[, "clubId"]), teams[, "clubId"])
club_names <- teams[idx, "clubLabel"]
careers <- cbind(careers[, "playerId"], playerLabel = plyr_names, careers[, "clubId"], clubLabel = club_names, careers[,3:ncol(careers)])

# export table as a CSV
write.csv(x = careers, file = file.path(out.folder, "all_pro_players_careers.csv"), row.names = FALSE, fileEncoding = "UTF-8")
