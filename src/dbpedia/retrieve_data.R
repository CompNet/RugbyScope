# Script designed to extract raw data from DBpedia, and record
# them as tables.
#
# The SPARQL queries are available in folder `queries/dbpedia`.
#
# Note: this script runs correctly as of 22/12/2024, but that
# may not be true in the future, depending on the evolution of 
# DBpedia. The CSV files produced by this scripts are 
# available in folder `out/dbpedia/tables.`
#
# Vincent Labatut
# 12/2024
########################################################################
library("readtext")
library("httr2")
library("dplyr")




########################################################################
# paths
query_folder <- file.path("queries", "dbpedia")
table_folder <- file.path("data", "dbpedia", "tables")




########################################################################
# Takes a query and the name of a CSV file, process the query using
# DBpedia, and record the resulting table as a CSV file. The table is
# also returned by the function.
#
# query: the query to process.
# file: the CSV file to create.
#
# returns: the table corresponding to the query answer.
########################################################################
query_dbpedia <- function(query, file) {
  endpoint <- "https://dbpedia.org/sparql"

  # send the query
  req <- request(endpoint) |>
          req_url_query(query = query, format = "text/csv")
  response <- req_perform(req)

  # check the status code
  if (resp_status(response) == 200) {
    content <- resp_body_string(response)

    # write as CSV file
    fileConn <- file(file)
    writeLines(content, fileConn)
    close(fileConn)

    # read to return the content as a table
    result <- read.csv(file)

    # turn empty strings into NAs
    result <- result %>% mutate(across(where(is.character), ~ na_if(., "")))
  } else {
    # print error message if the request failed
    stop(paste("Error:", resp_status(response)))
  }

  return(result)
}




########################################################################
# extraction of player table
cat("Retrieving the list of players\n")

# load query file
query <- readtext(file.path(query_folder, "players_list.sparql"))$text

# run query and get the players
tab_file <- file.path(table_folder, "all_players_descr.csv")
players <- query_dbpedia(query, file = tab_file)

# display a few details for verification
cat("Dimension of the players table:", paste(dim(players), collapse = ", "), "\n")
cat("Classes of the columns: ", paste(apply(players, 2, class), collapse = ", "), "\n")
cat("Top of the table:\n");
print.data.frame(players[1:10, ])

# normalize certain columns
# TODO
# - merge "firstNames","givenNames","lastNames"
# - merge "fullName", "birthNames"? or keep both?
# - check dates are complete (no year alone)
# - remove html part of place names
# - remove units in weights/heights
# - merge "weightA","heightA","weightB","heightB"
# - normalize position
# - remove http from WD id
# - check if WD id always filled
# - filter out rugby league players (see names, WD)

# export table as a CSV
write.csv(x = players, file = file.path(table_folder, "all_players_descr.csv"), row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# extraction of team list
cat("Retrieving the list of team IDs from WD\n")

# load query file
query <- readtext(file.path(query_folder, "teams_list.sparql"))$text

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
query <- readtext(file.path(query_folder, "teams_info.sparql"))$text
# remove the comments/spaces/newlines, otherwise the query is too long
query <- gsub("#[^\r\n]*[\r\n]+", "\n", query)
query <- gsub("  +", " ", query)
query <- gsub("[\r\n]+", "\n", query)
query <- gsub(" *[\r\n] *", "\n", query)

# init teams table
teams <- as.data.frame(matrix(NA, nrow=length(team_ids), ncol=length(col_names)))
colnames(teams) <- col_names

# run query for each team
for (t in 1:length(team_ids)) {
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
write.csv(x = teams, file = file.path(table_folder, "all_teams_descr.csv"), row.names = FALSE, fileEncoding = "UTF-8")




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
query <- readtext(file.path(query_folder, "career_steps.sparql"))$text
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
write.csv(x = careers, file = file.path(table_folder, "all_players_careers.csv"), row.names = FALSE, fileEncoding = "UTF-8")
