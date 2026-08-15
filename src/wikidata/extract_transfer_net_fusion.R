########################################################################
# Extracts the club network based on the merged data tables.
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
net_folder <- file.path("data", "networks")
fusion_folder <- file.path("data", "fusion")




########################################################################
# load data tables
tlog("Loading merged tables")

teams <- read.csv(file.path(fusion_folder, "teams_05_itwp.csv"))
tlog(2, "Raw number of teams: ", nrow(fus_teams))

players <- read.csv(file.path(fusion_folder, "players_04_itwp.csv"))
tlog(2, "Raw number of players: ", nrow(fus_players))

stints <- read.csv(file.path(fusion_folder, "stints_03_itwp.csv"))
tlog(2, "Raw number of stints: ", nrow(fus_stints))




########################################################################
# filter out irrelevant teams
tlog("Cleaning teams")
nbr <- nrow(teams)

# filter out national teams
nat_types <- c(
  "National military team",
  "National school team",
  "National senior team",
  "National U16 team",
  "National U17 team",
  "National U18 team",
  "National U19 team",
  "National U20 team",
  "National U21 team",
  "National U23 team",
  "National university team"
)
idx <- which(teams[, "type"] %in% nat_types)
if (length(idx) > 0)
  teams <- teams[-idx, ]
tlog(2, "Removed ", length(idx), " national teams")

# filter out other selections
sel_types <- c(
  "Invitational team",
  "Combined team",
  "Regional team"
)
idx <- which(teams[, "type"] %in% sel_types)
if (length(idx) > 0)
  teams <- teams[-idx, ]
tlog(2, "Removed ", length(idx), " other types of selections")

tlog(2, "Number of clubs remaining: ", nrow(teams), "/", nbr)




########################################################################
# filter out stints
nbr <- nrow(stints)

# filter out stints without a start date
idx <- which(is.na(stints[, "startYear"]))
stints <- stints[-idx, ]
tlog("Removed ", length(idx), " stints without start date")

# using the start year as the end year when it is missing (very raw approximation)
idx <- which(is.na(stints$endYear))
stints[idx, "endYear"] <- stints[idx, "startYear"]
tlog("Complemented ", length(idx), " missing end years (using the corresponding start year)")

# filter out stints related to clubs (now) absent from the list
idx <- which(!(stints[, "teamRsId"] %in% teams[, "rugbyscopeId"]))
stints <- stints[-idx, ]
tlog("Removed ", length(idx), " stints without club (or with filtered out team)")

tlog("Number of stints remaining: ", nrow(stints), "/", nbr)




########################################################################
# extract club network
tlog("Extracting club transfer network")

# init edgelist table
el <- matrix(NA, nrow = 1, ncol = 2)
colnames(el) <- c("From", "To")
el <- el[-1, , drop = FALSE]
weights <- c()

# init last stint variables
last_player <- stints[1, "playerId"]
last_club <- stints[1, "teamRsId"]
last_end <- stints[1, "endYear"]
row <- 2

# loop over each stint
while(row <= nrow(stints)) {
  tlog(2, "Processing stint ", row, "/", nrow(stints))
  player_id <- stints[row, "playerId"]
  club_id <- stints[row, "teamRsId"]
  start_year <- stints[row, "startYear"]
  end_year <- stints[row, "endYear"]
  tlog(2, player_id, ", ", club_id)

  # next stint of the previous player
  if (player_id == last_player) {
    # the new club must be different, and there must be no gap between both stints' dates
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
el <- cbind(as.character(el[, "From"]), as.character(el[, "To"]))
g <- graph_from_edgelist(el, directed = TRUE)
E(g)$weight <- weights
idx <- match(V(g)$name, teams[, "rugbyscopeId"])
V(g)$fullname <- teams[idx, "fullName"]
#plot(g)




########################################################################
# insert individual information
tlog("Insert individual information")

# add main team information
idx <- match(V(g)$name, teams[, "rugbyscopeId"])
V(g)$type <- teams[idx, "type"]
V(g)$nation <- teams[idx, "nations"]
V(g)$competition <- teams[idx, "competitions"]




########################################################################
# finalize the network

# print some stats
print(sort(table(V(g)$type)))
print(sort(table(V(g)$nation)))
print(sort(table(V(g)$competition)))

# export as a graphml file
net_file <- file.path(net_folder, "transfers.graphml")
tlog("Recording graph in '", net_file, "'")
write.graph(g, file = net_file, format = "graphml")
