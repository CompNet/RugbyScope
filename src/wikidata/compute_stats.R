########################################################################
# Computes and plots the evolution of the number of players by career
# start date.
#
# Vincent Labatut
# 12/2024
########################################################################
source("src/common/colors.R")
source("src/wikidata/clean_tables.R")




########################################################################
# paths
table_folder <- file.path("data", "wikidata", "tables")
stat_folder <- file.path("data", "wikidata", "stats")




########################################################################
# load data tables
teams <- read.csv(file.path(table_folder, "teams.csv"))
cat("Raw number of teams:", nrow(teams), "\n")

players <- read.csv(file.path(table_folder, "players.csv"))
cat("Raw number of players:", nrow(players), "\n")

stints <- read.csv(file.path(table_folder, "stints.csv"))
cat("Raw number of stints:", nrow(stints), "\n")




########################################################################
# number of players by start year of career

# init structures
years <- as.integer(players[, "careerStartYears"])
years <- years[!is.na(years)]

# compute stats
player_by_year <- sapply(sort(unique(years)), function(year) length(which(years == year)))

# produce plot
pdf(file.path(stat_folder, "player_by_start-year.pdf"))
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

# filter out stints without a start year
idx <- which(is.na(stints[, "startYear"]))
filt_stints <- stints[-idx, ]

# get start and end years
start_years <- as.integer(filt_stints[, "startYear"])
end_years <- as.integer(filt_stints[, "endYear"])

# if no end year: use the start year
idx <- which(is.na(end_years))
end_years[idx] <- start_years[idx]

# init temporal info
start_year <- min(c(start_years, end_years))
end_year <- max(c(start_years, end_years))

# init nation info
all_nations <- get_merged_nations(players)
# count them to select which nations to display later (cannot show them all)
tt <- sort(table(all_nations), decreasing = TRUE)

# match nation for each stint
idx <- match(filt_stints[, "playerId"], players[, "playerId"])
nations <- all_nations[idx]

# compute overall stats
player_by_year <- sapply(start_year:end_year, function(year) length(which(start_years <= year & end_years >= year)))

# # clean data tables
# source("src/wikidata/load_tables.R")

# compute nation-wise stats
# sort(table(nations))
unique_nations <- head(names(tt), 10)
for(unique_nation in unique_nations) {
  temp <- sapply(start_year:end_year, function(year) length(which(start_years <= year & end_years >= year & nations == unique_nation)))
  player_by_year <- rbind(player_by_year, temp)
}

# set column/row names
colnames(player_by_year) <- start_year:end_year
rownames(player_by_year) <- c("All", unique_nations)

# set colors
cols <- get.palette(values = nrow(player_by_year))
names(cols) <- rownames(player_by_year)

# produce plot
pdf(file.path(stat_folder, "active-player_by_year.pdf"))
plot(
  NULL,
  xlab = "Year", ylab = "Number of players",
  main = "Number of active players by year",
  # log = "y",
  xlim = c(start_year,end_year),
  ylim = c(0, max(player_by_year["All", ]))
)
for (unique_nation in rownames(player_by_year)) {
  lines(
    x = start_year:end_year,
    y = player_by_year[unique_nation, ],
    col = cols[unique_nation]
  )
}
legend(
  x = "topleft",
  fill = cols[rownames(player_by_year)],
  legend = rownames(player_by_year)
)
dev.off()

# same plot, but with stacked areas
library("ggplot2")
library("RColorBrewer")

# adjust the data to please ggplot
player_by_year <- player_by_year[-c(9:11), ]
player_by_year[1, ] <- player_by_year[1, ] - colSums(player_by_year[-1, ])
rownames(player_by_year)[1] <- "Others"
time <- rep(as.integer(colnames(player_by_year)), nrow(player_by_year))  # x Axis
value <- c(t(player_by_year))               # y Axis
group <- rep(rownames(player_by_year), each = ncol(player_by_year))        # group, one shape per group
data <- data.frame(time, value, group)

# stacked area chart
pdf(file.path(stat_folder, "active-player_by_year_stacked-areas.pdf"))
ggplot(data, aes(x = time, y = value, fill = group)) + scale_fill_brewer(palette = "Dark2") + geom_area()
dev.off()
