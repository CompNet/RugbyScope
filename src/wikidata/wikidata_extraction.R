# Script designed to extract raw data from Wikidata.
#
# WARNING: In the end, we got HTTP error 500 (i.e. internal server error)
# while running some of these queries. So we directly ran them
# on the online interface to retrieve the data and exported them
# as CSV files: https://query.wikidata.org
#
# The SPARQL queries are available in folder `queries/wikidata`.
#
# Vincent Labatut
# 12/2024
########################################################################
library("readtext")
library("WikidataR")




########################################################################
# paths
query.folder <- file.path("queries")
out.folder <- file.path("out", "wikidata")




########################################################################
# complete extraction of pro players

# load query file
query <- readtext(file.path(query.folder, "wd_all_players.sparql"))$text

# querying WD with pagination
PAGE_SIZE <- 500
page <- 1
go_on <- TRUE
while (go_on) {
  cat("Processing page #", page, "\n", sep = "")
  page_data <- query_wikidata(#format="smart",
    paste0(query,
      "\nLIMIT ", PAGE_SIZE,
      "\nOFFSET ", (page - 1) * PAGE_SIZE
    )
  )
  page_data <- page_data[,-which(colnames(page_data)=="dobMax")]
  page_data <- page_data[,-which(colnames(page_data)=="dodMax")]

  if(page == 1) 
    players <- page
  else
    players <- rbind(players, page)
  page <- page + 1
  go_on <- nrow(page_data) == PAGE_SIZE
}

# export as CSV file
print(dim(players))
write.csv(players, file.path(out.folder, "all_pro_players_descr.csv"))
# players <- read.csv(file.path(out.folder, "all_pro_players_descr.csv"))
print(apply(players, 2, class))
print.data.frame(players[1:10, ])

# list players occurring several times
doubles <- names(which(table(players$player) > 1))
for (double in doubles) {
  idx <- which(players$player == double)
  print(idx)
  print.data.frame(players[idx, ])
  print(colnames(players)[which(players[idx[1], ] != players[idx[2],])])
}




########################################################################
# extraction of club/team information

# load query file
query <- readtext(file.path(query.folder, "wd_all_clubs.sparql"))$text

clubs <- query_wikidata(query)
write.csv(clubs, file.path(out.folder, "all_pro_clubs_descr.csv"))
# clubs <- read.csv(file.path(out.folder, "all_pro_clubs_descr.csv"))
print(apply(clubs, 2, class))
print.data.frame(clubs[1:10, ])

# list clubs occurring several times
doubles <- names(which(table(clubs$club) > 1))
for (double in doubles) {
  idx <- which(clubs$club == double)
  print(idx)
  print.data.frame(clubs[idx, ])
  print(colnames(clubs)[which(clubs[idx[1], ] != clubs[idx[2],])])
}

# TODO
# affiliation : check rugby organization
# some have several headquarters




########################################################################
# extraction of player careers

# load query file
query <- readtext(file.path(query.folder, "wd_all_careers.sparql"))$text

careers <- query_wikidata(query)
write.csv(careers, file.path(out.folder, "all_pro_players_careers.csv"))
# careers <- read.csv(file.path(out.folder, "all_pro_players_careers.csv"))
print(apply(careers, 2, class))
print.data.frame(careers[1:10, ])
