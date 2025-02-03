########################################################################
# Extracts the club network based on the data retrieved from Wikidata.
#
# Vertices represent clubs and directed edges represent player transfers
# between them. Edge weights correspond to the number of transfers.
#
# Vincent Labatut
# 12/2024
########################################################################
library("igraph")

source("src/common/logging.R")




########################################################################
# paths
net_folder <- file.path("data", "wikidata", "networks")




########################################################################
# load and clean data tables
source("src/wikidata/load_tables.R")




########################################################################
# extract club network
tlog("Extracting club transfer network")

# init edgelist table
el <- matrix(NA, nrow = 1, ncol = 2)
colnames(el) <- c("From", "To")
el <- el[-1, , drop = FALSE]
weights <- c()

# init last step variables
last_player <- filt_careers[1, "playerId"]
last_club <- filt_careers[1, "teamId"]
last_end <- filt_careers[1, "endYear"]
row <- 2

# loop over each career step
while(row <= nrow(filt_careers)) {
  tlog(2, "Processing career step ", row, "/", nrow(filt_careers))
  player_id <- filt_careers[row, "playerId"]
  club_id <- filt_careers[row, "teamId"]
  start_year <- filt_careers[row, "startYear"]
  end_year <- filt_careers[row, "endYear"]
  tlog(2, player_id, ", ", club_id)

  # next step of the previous player
  if (player_id == last_player) {
    # the new club must be different, and there must be no gap between both steps' dates
    if (last_club != club_id && (is.na(last_end) || start_year == last_end || start_year == (last_end + 1))) {
      idx <- which(el[, "From"] == last_club & el[, "To"] == club_id)
      if (length(idx) == 0) {
        el <- rbind(el, c(last_club, club_id))
        weights <- c(weights, 1)
      } else {
        weights[idx] <- weights[idx] + 1
      }
    }
  } else {
    # starting to process a different player
    last_player <- player_id
  }
  last_club <- club_id
  last_end <- end_year

  row <- row + 1
}

# init graph
g <- graph_from_edgelist(el, directed = TRUE)
E(g)$weight <- weights
idx <- match(V(g)$name, teams[, "teamId"])
V(g)$fullname <- teams[idx, "teamLabel"]
plot(g)




########################################################################
# insert individual information
tlog("Insert individual information")

# add main team information
idx <- match(V(g)$name, teams[, "teamId"])
V(g)$country <- teams[idx, "countryLabels"]
V(g)$competition <- teams[idx, "competitionLabels"]




########################################################################
# finalize the network

# print some stats
print(sort(table(V(g)$country)))
print(sort(table(V(g)$competition)))

# export as a graphml file
net_file <- file.path(net_folder, "transfers.graphml")
tlog("Recording graph in '", net_file, "'")
write.graph(g, file = net_file, format = "graphml")
