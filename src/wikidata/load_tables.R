########################################################################
# Loads the three tables (players, teams, careers) retrieved from
# Wikidata, and remove the information considered as useless.
#
# Vincent Labatut
# 12/2024
########################################################################




########################################################################
# paths
table_folder <- file.path("data", "wikidata", "tables")




########################################################################
# load data tables
teams <- read.csv(file.path(table_folder, "all_teams_descr.csv"))
cat("Raw number of teams:", nrow(teams), "\n")

players <- read.csv(file.path(table_folder, "all_players_descr.csv"))
cat("Raw number of players:", nrow(players), "\n")

careers <- read.csv(file.path(table_folder, "all_players_careers.csv"))
cat("Raw number of career steps:", nrow(careers), "\n")




########################################################################
# clean team data
clubs <- teams

# # debug stuff
# idx <- which(grepl("^Q\\d+", teams[, "teamLabel"]))
# paste0("https://www.wikidata.org/wiki/", teams[idx, "teamId"])

# filter out national teams for specific world cups
idx <- which(grepl("world cup", clubs[, "teamLabel"], fixed = TRUE) | grepl("World Cup", clubs[, "teamLabel"], fixed = TRUE))
# clubs[idx, "teamLabel"]
# paste0("https://www.wikidata.org/wiki/", clubs[idx, "teamId"])
if (length(idx) > 0)
  clubs <- clubs[-idx, ]
cat("Removed", length(idx), "national teams tied to specific world cups\n")

# filter out national teams
idx <- which(clubs[, "teamTypeLabel"] %in% c("national rugby union team", "second national rugby union teams"))
if (length(idx) > 0)
  clubs <- clubs[-idx, ]
cat("Removed", length(idx), "national teams\n")

# filter out national youth teams
idx <- which(grepl("under", clubs[, "teamLabel"], fixed = TRUE) | grepl("Under", clubs[, "teamLabel"], fixed = TRUE))
if (length(idx) > 0)
  clubs <- clubs[-idx, ]
cat("Removed", length(idx), "national youth teams\n")

# filter out invitational teams (Barbarians et al.)
# note: Brussels Barbarians is a proper club
invitational_teams <- c("Q807749", "Q28223950", "Q2004853", "Q7015235", "Q7565434", "Q3071726", "Q65068423", "Q7435412", "Q1490464", "Q11298953", "Q2894383")
idx <- which(clubs[, "teamId"] %in% invitational_teams)
if (length(idx) > 0)
  clubs <- clubs[-idx, ]
cat("Removed", length(idx), "invitational teams\n")

# filter out combined teams (British & Irish Lions et al.)
combined_teams <- c("Q3651754", "Q624092", "Q733600", "Q5327644", "Q3606252", "Q247246", "Q3976615", "Q121190772")
idx <- which(clubs[, "teamId"] %in% combined_teams)
if (length(idx) > 0)
  clubs <- clubs[-idx, ]
cat("Removed", length(idx), "combined teams\n")

# filter out regional selections (NZ South Island, etc.)
regional_teams <- c("Q104649868", "Q16237227", "Q7057169", "Q85815139", "Q7565682", "Q7569050")
idx <- which(clubs[, "teamId"] %in% regional_teams)
if (length(idx) > 0)
  clubs <- clubs[-idx, ]
cat("Removed", length(idx), "regional teams\n")

# filter out armed force teams (Royal Air Force Rugby Union, etc.)
army_teams <- c("Q105561254", "Q7374556")
idx <- which(clubs[, "teamId"] %in% army_teams)
if (length(idx) > 0)
  clubs <- clubs[-idx, ]
cat("Removed", length(idx), "regional teams\n")

# filter out clubs with no affiliation and no competition
# this is an attempt to retain only pro clubs, but this condition is too strict
# idx <- which(is.na(clubs[, "affiliationLabels"]) & is.na(clubs[, "competitionLabels"]))
# if (length(idx) > 0)
#   clubs <- clubs[-idx, ]
# cat("Removed", length(idx), "clubs without affiliation and without competition\n")

cat("Number of clubs remaining:", nrow(clubs), "\n")




########################################################################
# clean career data
filt_careers <- careers

# filter out career steps without a start date
idx <- which(is.na(filt_careers$startYear))
filt_careers <- filt_careers[-idx, ]
cat("Removed", length(idx), "steps without start date\n")

# using the start year as the end year when it is missing
idx <- which(is.na(filt_careers$endYear))
filt_careers[idx, "endYear"] <- filt_careers[idx, "startYear"]
cat("Complemented", length(idx), "missing end year (using the start year)\n")

# filter out career steps related to clubs (now) absent from the list
idx <- which(!(filt_careers$teamId %in% clubs$teamId))
filt_careers <- filt_careers[-idx, ]
cat("Removed", length(idx), "steps without club (or with filtered out club)\n")

cat("Number of steps remaining:", nrow(filt_careers), "\n")
