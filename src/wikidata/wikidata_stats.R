# Plots some stats for the tables retrieved by the `wikidata_retrieva.R`
# script.
#
# Vincent Labatut
# 12/2024
########################################################################




########################################################################
# paths
out.folder <- file.path("out", "wikidata")




########################################################################
# load data tables
teams <- read.csv(file.path(out.folder, "all_teams_descr.csv"))
cat("Raw number of teams:", nrow(teams), "\n")

players <- read.csv(file.path(out.folder, "all_players_descr.csv"))
cat("Raw number of players:", nrow(players), "\n")

careers <- read.csv(file.path(out.folder, "all_players_careers.csv"))
cat("Raw number of career steps:", nrow(careers), "\n")




########################################################################
# number of players by start year of career

# init structures
years <- as.integer(players[, "careerStartYears"])
years <- years[!is.na(years)]

# compute stats
player_by_year <- sapply(sort(unique(years)), function(year) length(which(years == year)))

# produce plot
pdf(file.path(out.folder, "player_by_start-year.pdf"))
plot(
  x = sort(unique(years)),
  y = player_by_year,
  xlab = "Year", ylab = "Number of players",
  main = "Number of players by career year start",
  type = "l", col = "red"
)
dev.off()




########################################################################
# number of active players by year

# filter out career steps without a start or end date
idx <- which(is.na(careers$startYear) | is.na(careers$endYear))
filt_careers <- careers[-idx, ]

# init structures
start_years <- as.integer(filt_careers[, "startYear"])
start_years <- start_years[!is.na(start_years)]
end_years <- as.integer(filt_careers[, "endYear"])
end_years <- end_years[!is.na(end_years)]
start_year <- min(c(start_years, end_years))
end_year <- max(c(start_years, end_years))

# compute stats
player_by_year <- sapply(start_year:end_year, function(year) length(which(start_years <= year & end_years >= year)))
names(player_by_year) <- start_year:end_year

pdf(file.path(out.folder, "active-player_by_year.pdf"))
plot(
  x = start_year:end_year,
  y = player_by_year,
  xlab = "Year", ylab = "Number of players",
  main = "Number of active players by year",
  type = "l", col = "blue",
  log = "y"
)
dev.off()
