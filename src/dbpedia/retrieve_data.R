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
cat("Retrieving the list of players from DBpedia\n")

# load query file
query <- readtext(file.path(query_folder, "players_list.sparql"))$text

# run query and get the players (must use a paginated access due to 10000 hit DBpedia limit)
tab_file <- file.path(table_folder, "all_players_descr.csv")
go_on <- TRUE
page_nbr <- 1
pg_limit <- 10000
players <- NA
while (go_on) {
  cat("Processing page ", page_nbr, "\n", sep = "")
  offset <- (page_nbr - 1) * pg_limit
  pg_query <- gsub("xxxxxx", offset, query, fixed = TRUE)
  page <- query_dbpedia(pg_query, file = tab_file)
  if (page_nbr == 1)
    players <- page
  else
    players <- rbind(players, page)
  go_on <- nrow(page) == pg_limit
  page_nbr <- page_nbr + 1
}
cat("Dimension of the players table:", paste(dim(players), collapse = ", "), "\n")

# normalize certain columns
# TODO
# - SIZE PB: 10 000 LIMITATION
# - merge "firstNames","givenNames","lastNames"
# - merge "fullName", "birthNames"? or keep both?
# - check dates are complete (no year alone)
# - remove html part of place names
# - remove units in weights/heights
# - merge "weightA","heightA","weightB","heightB"
# - normalize position
# - remove http from WD id
# - check if WD id always filled

# display a few details for verification
cat("Dimension of the players table:", paste(dim(players), collapse = ", "), "\n")
cat("Classes of the columns: ", paste(apply(players, 2, class), collapse = ", "), "\n")
cat("Top of the table:\n")
print.data.frame(players[1:10, ])

# export table as a CSV
write.csv(x = players, file = file.path(table_folder, "all_players_descr.csv"), row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# extraction of team list
cat("Retrieving the list of teams from DBpedia\n")

# load query file
query <- readtext(file.path(query_folder, "teams_list.sparql"))$text

# run query and get the players
tab_file <- file.path(table_folder, "all_teams_descr.csv")
teams <- query_dbpedia(query, file = tab_file)
cat("Dimension of the teams table:", paste(dim(teams), collapse = ", "), "\n")

# normalize certain columns
# TODO
# - merge names: "teamLabel", "teamNames", "teamNames2", "fullNames"
# - remove http from WD id
# - check if WD id always filled

idx <- which(grepl("season", teams[, "team"], fixed = TRUE) | grepl("Season", teams[, "team"], fixed = TRUE))
cat("Removing ", length(idx), " women teams from the table\n")
teams <- teams[-idx, ]
# >> do that in the query!

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
