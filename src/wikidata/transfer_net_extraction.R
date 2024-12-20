# Extracts the club network based on the data retrieved from Wikidata.
#
# Vincent Labatut
# 12/2024
########################################################################
library("igraph")
library("dplyr")




########################################################################
# paths
out.folder <- file.path("out","wikidata")




########################################################################
# load data tables
teams <- read.csv(file.path(out.folder, "all_pro_teams_descr.csv"))
cat("Raw number of teams:", nrow(teams), "\n")
teams <- teams %>% mutate(across(where(is.character), ~ na_if(.,"")))


players <- read.csv(file.path(out.folder, "all_pro_players_descr.csv"))
cat("Raw number of players:", nrow(players), "\n")

careers <- read.csv(file.path(out.folder, "all_pro_players_careers.csv"))
cat("Raw number of career steps:", nrow(careers), "\n")




########################################################################
# clean team data

# filter out national teams
idx <- which(teams[, "clubTypeLabel"] == "national rugby union team")
if (length(idx) > 0)
  clubs <- teams[-idx, ]
cat("Removed", length(idx), "national teams\n")

# filter out invitational teams (Barbarians et al.)
# note: Brussels Barbarians is a proper club
invitational_teams <- c("Q807749", "Q28223950", "Q2004853", "Q7015235", "Q7565434", "Q3071726", "Q65068423", "Q7435412", "Q1490464")
idx <- which(clubs[, "clubId"] %in% invitational_teams)
if (length(idx) > 0)
  clubs <- clubs[-idx, ]
cat("Removed", length(idx), "invitational teams\n")

# filter out combined teams (British & Irish Lions et al.)
combined_teams <- c("Q3651754", "Q624092", "Q733600", "Q5327644", "Q3606252", "Q247246", "Q3976615", "Q121190772")
idx <- which(clubs[, "clubId"] %in% combined_teams)
if (length(idx) > 0)
  clubs <- clubs[-idx, ]
cat("Removed", length(idx), "combined teams\n")

# # filter out clubs with no affiliation and competition
# idx <- which(is.na(clubs[, "affiliationLabel"]) & is.na(clubs[, "competitionLabel"]))
# if (length(idx) > 0)
#   clubs <- clubs[-idx, ]
# cat("Removed", length(idx), "clubs without affiliation and competition\n")

cat("Number of clubs remaining:", nrow(clubs), "\n")




########################################################################
# clean career data

# filer out career steps without a start date
# TODO




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
write.graph(g, file = file.path(out.folder, "all_pro_transfers.graphml"), format = "graphml")

# TODO
# > keep only rows with a start date
# > check prev end date and next start date are both present and consecutive
# > handle clubs vs. national selections / invitation clubs

