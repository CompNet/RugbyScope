# Extracts the teammate network based on the data retrieved from Wikidata.
# Each vertex represents a player, and vertices are connected when the
# corresponding players have played together in the same club. The edges
# are undirected, and their weight correspond to the number of seasons
# (possibly *incomplete* seasons) spent together in the same club.
#
# Vincent Labatut
# 12/2024
########################################################################
library("igraph")




########################################################################
# paths
out.folder <- file.path("out", "wikidata")




########################################################################
# load and clean data tables
source("src/wikidata/clean_tables.R")



########################################################################
# extract club network

# init edgelist table
adj_mat <- matrix(0, nrow = nrow(players), ncol = nrow(players))
colnames(adj_mat) <- rownames(adj_matrix) <- players[, "playerId"]

# loop over the first players
for(p1 in 1:(nrow(players) - 1)) {
  p1_id <- players[p1, "playerId"]
  cat("Processing player #1 ", p1_id, " ", p1, "/", (nrow(players) - 1), " (", players[p1, "playerLabel"], ")", "\n", sep = "")
  
  # only process those with enough information
  idx1 <- which(filt_careers[, "playerId"] == p1_id)
  if (length(idx1) > 0) {
    w_club <- idx1[!is.na(filt_careers[idx1, "clubId"])]
    if (length(w_club) > 0) {
      
      # loop over the second player
      for(p2 in (p1 + 1):nrow(players)) {
        p2_id <- players[p2, "playerId"]
        cat("..Processing player #2 ", p2_id, " ", p2, "/", nrow(players), " (", players[p2, "playerLabel"], ")", "\n", sep = "")

        # only process those with enough information
        idx2 <- which(filt_careers[, "playerId"] == p2_id)
        if (length(idx2) > 0) {
          w_club <- idx2[!is.na(filt_careers[idx2, "clubId"])]
          if (length(w_club) > 0) {
            
            # compare the career steps
            inter_clubs <- intersect(filt_careers[idx1, "clubId"], filt_careers[idx2, "clubId"])
            for (inter_club in inter_clubs) {
              cat("....Processing common club ",inter_club, " (", clubs[which(clubs[, "clubId"] == inter_club), "clubLabel"], ")\n", sep = "")
              
              i1 <- which(filt_careers[idx1, "clubId"] == inter_club)
              i2 <- which(filt_careers[idx2, "clubId"] == inter_club)
              start1 <- filt_careers[idx1[i1], "startYear"]
              end1 <- filt_careers[idx1[i1], "endYear"]
              start2 <- filt_careers[idx2[i2], "startYear"]
              end2 <- filt_careers[idx2[i2], "endYear"]
              overlap <- min(end1, end2) - max(start1, start2) + 1
              cat("......Temporal overlap: [", start1, ";", end1, "] vs. [", start2, ";", end2, "] >> ", overlap, " years\n", sep = "")
              if (overlap > 0) {
                adj_mat[p1, p2] <- adj_mat[p1, p2] + overlap
                adj_mat[p2, p1] <- adj_mat[p2, p1] + overlap
              }
            }
          }
        }
      }
    }
  }
}

# init graph
g <- graph_from_adjacency_matrix(adjmatrix = adj_mat, mode = "undirected", weighted = TRUE, diag = FALSE)
cat("Number of vertices: ", gorder(g), "\n", sep = "")
cat("Number of edges: ", gsize(g), "\n", sep = "")

# add names
idx <- match(V(g)$name, players[, "playerId"])
V(g)$fullname <- players[idx, "playerLabel"]
# plot(g)

# add main player information
V(g)$country <- players[idx, "countryLabels"]
V(g)$composition <- players[idx, "positionLabels"]
V(g)$mass <- players[idx, "masses"]
V(g)$height <- players[idx, "heights"]

# remove isolates
deg <- degree(graph = g, mode = "all")
idx <- which(deg == 0)
g <- delete_vertices(graph = g, v = idx)
cat("Removed ", length(idx), " isolates\n", sep = "")

# export as a graphml file
cat("Number of vertices remaining: ", gorder(g), "\n", sep = "")
cat("Number of edges remaining: ", gsize(g), "\n", sep = "")
write.graph(g, file = file.path(out.folder, "all_teammates.graphml"), format = "graphml")
