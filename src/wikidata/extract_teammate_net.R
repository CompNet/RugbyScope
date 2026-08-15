########################################################################
# Extracts the teammate network based on the data retrieved from Wikidata.
#
# Each vertex represents a player, and vertices are connected when the
# corresponding players have played together in the same team. The edges
# are undirected, and their weight correspond to the number of seasons
# (possibly *incomplete* seasons) spent together in the same team.
#
# Vincent Labatut
# 12/2024
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope/RugbyScope")
# source("src/wikidata/extract_teammate_net.R")
########################################################################
library("igraph")

source("src/common/logging.R")
source("src/wikidata/clean_tables.R")




########################################################################
# paths
net_folder <- file.path("data", "wikidata", "networks")




########################################################################
# load and clean data tables
source("src/wikidata/load_tables.R")




########################################################################
# extract player network

# init edgelist table
adj_mat <- matrix(0, nrow = nrow(players), ncol = nrow(players))
colnames(adj_mat) <- rownames(adj_mat) <- players[, "playerId"]

# loop over the first players
tlog.start.loop(0, (nrow(players) - 1), "Looping over pairs of players: player#1")
for (p1 in 1:(nrow(players) - 1)) {
  p1_id <- players[p1, "playerId"]
  tlog.loop(2, p1, "Processing player #1 ", p1_id, " ", p1, "/", (nrow(players) - 1), " (", players[p1, "playerLabel"], ")")
  
  # only process those with enough information
  idx1 <- which(filt_stints[, "playerId"] == p1_id)
  if (length(idx1) > 0) {
    w_team <- idx1[!is.na(filt_stints[idx1, "teamId"])]
    if (length(w_team) > 0) {
      
      # loop over the second player
      # tlog.start.loop(2, nrow(players) - (p1 + 1), "Looping over pairs of players: player#2")
      for (p2 in (p1 + 1):nrow(players)) {
        p2_id <- players[p2, "playerId"]
        # tlog.loop(2, p2 - p1, "Processing player #2 ", p2_id, " ", p2, "/", nrow(players) - (p1 + 1), " (", players[p2, "playerLabel"], ")")

        # only process those with enough information
        idx2 <- which(filt_stints[, "playerId"] == p2_id)
        if (length(idx2) > 0) {
          w_team <- idx2[!is.na(filt_stints[idx2, "teamId"])]
          if (length(w_team) > 0) {
            
            # compare the stints
            inter_teams <- intersect(filt_stints[idx1, "teamId"], filt_stints[idx2, "teamId"])
            for (inter_team in inter_teams) {
              # tlog(4, "Processing common team ",inter_team, " (", teams[which(teams[, "teamId"] == inter_team), "teamLabel"], ")")
              
              i1 <- which(filt_stints[idx1, "teamId"] == inter_team)
              i2 <- which(filt_stints[idx2, "teamId"] == inter_team)
              start1 <- filt_stints[idx1[i1], "startYear"]
              end1 <- filt_stints[idx1[i1], "endYear"]
              start2 <- filt_stints[idx2[i2], "startYear"]
              end2 <- filt_stints[idx2[i2], "endYear"]
              overlap <- min(end1, end2) - max(start1, start2) + 1
              # tlog(6, "Temporal overlap: [", start1, ";", end1, "] vs. [", start2, ";", end2, "] >> ", overlap, " years")
              if (overlap > 0) {
                adj_mat[p1, p2] <- adj_mat[p1, p2] + overlap
                adj_mat[p2, p1] <- adj_mat[p2, p1] + overlap
              }
            }
          }
        }
      }
    	# tlog.end.loop(2, "Completed loop player#2")
    }
  }
}
tlog.end.loop(0, "Completed loop player#1")

# init graph
g <- graph_from_adjacency_matrix(adjmatrix = adj_mat, mode = "undirected", weighted = TRUE, diag = FALSE)
tlog("Number of vertices: ", gorder(g))
tlog("Number of edges: ", gsize(g))




########################################################################
# adding individual information

# add names
idx <- match(V(g)$name, players[, "playerId"])
V(g)$fullname <- players[idx, "playerLabel"]
# plot(g)

# add main player information
all_nations <- get_merged_nations(players)
V(g)$nation <- all_nations[idx]
all_positions <- get_clean_positions(players)
V(g)$position <- all_positions[idx]
V(g)$composition <- players[idx, "positionLabels"]
V(g)$mass <- players[idx, "masses"]
V(g)$height <- players[idx, "heights"]




########################################################################
# finalizing the network

# remove isolates
deg <- degree(graph = g, mode = "all")
idx <- which(deg == 0)
g <- delete_vertices(graph = g, v = idx)
tlog("Removed ", length(idx), " isolates out of ", nrow(players), " vertices")

tlog("Number of vertices remaining: ", gorder(g))
tlog("Number of edges remaining: ", gsize(g))

# export as a graphml file
net_file <- file.path(net_folder, "teammates.graphml")
tlog("Recording graph in '", net_file, "'")
write.graph(g, file = net_file, format = "graphml")
