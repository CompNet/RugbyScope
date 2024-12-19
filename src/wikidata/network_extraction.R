# Extracts the team network based on the data retrieved from Wikidata.
#
# Vincent Labatut
# 12/2024
########################################################################
library("igraph")




########################################################################
# load data tables
players <- read.csv(file.path("out", "all_pro_players_descr.csv"))
careers <- read.csv(file.path("out", "all_pro_players_careers.csv"))
teams <- read.csv(file.path("out", "all_pro_teams_descr.csv"))




########################################################################
# extract club network

# init
idx <- which(is.na(careers$startYear))
el <- NA
filt_careers <- careers[-idx,]
current_player <- filt_careers[1, "player"]
last_club <- filt_careers[1, "clubLabel"]
row <- 2

# loop over each career step
while(row <= nrow(filt_careers)) {
  cat("Processing career step ", row, "/", nrow(filt_careers), "\n", sep="")
  cat(filt_careers$player[row], ", ", filt_careers$clubLabel[row], "\n", sep="")

  # next step of the previous player
  if (filt_careers$player[row] == current_player) {
    new_club <- filt_careers$clubLabel[row]
    if (last_club != new_club) {
      if (all(is.na(el))) {
        el <- matrix(c(last_club, new_club), nrow = 1, ncol = 2)
        weights <- 1
        colnames(el) <- c("From", "To")
      } else {
        idx <- which(el[,"From"] == last_club & el[,"To"] == new_club)
        if (length(idx) == 0) {
          el <- rbind(el, c(last_club, new_club))
          weights <- c(weights, 1)
        } else {
          weights[idx] <- weights[idx] + 1
        }
      }
      last_club <- new_club
    }
  } else {
    # starting to process a different player
    current_player <- filt_careers$player[row]
    last_club <- filt_careers$clubLabel[row]
  }
  row <- row + 1
}

# init graph
g <- graph_from_edgelist(el, directed=TRUE)
E(g)$weight <- weights
plot(g)

# export as a graphml file
write.graph(g, file = file.path("out", "all_pro_teams.graphml"), format = "graphml")

# TODO
# > rajouter une feuille pr les clubs
