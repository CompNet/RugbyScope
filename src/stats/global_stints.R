########################################################################
# Generates various plots regarding the stints table, without
# considering time.
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/stats/global_stints.R")
########################################################################
library("dplyr")
library("UpSetR")
library("circlize")
library("corrplot")
#library("pheatmap")
library("viridis")




source("src/common/logging.R")
source("src/common/colors.R")




########################################################################
# start logging
start.rec.log("GlobalStints")




########################################################################
# create output folder
stats_folder <- file.path("data", "stats", "global_stints")
dir.create(stats_folder, showWarnings = FALSE, recursive = TRUE)

# load tables
source("src/stats/load_all_tables.R")




########################################################################
# distribution of start years

start_dates <- stints[, "startYear"]
start_dates <- start_dates[!is.na(start_dates)]

plot_file <- file.path(stats_folder, paste0("start-years", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  hist(start_dates,
    main = NA,
    xlab = "Stint start year",
    col = "red",
    breaks = 30,
    las = 2
  )
dev.off()




########################################################################
# distribution of end years

end_dates <- stints[, "endYear"]
end_dates <- end_dates[!is.na(end_dates)]

plot_file <- file.path(stats_folder, paste0("end-years", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  hist(end_dates,
    main = NA,
    xlab = "Stint end year",
    col = "red",
    breaks = 30,
    las = 2
  )
dev.off()




########################################################################
# distribution of types

# retrieve types
types <- stints[, "type"]

# count values
types_tt <- table(types, useNA = "always")
print(types_tt)

# focus on most frequent values
types_tt0 <- sort(table(types, useNA = "no"), decreasing = TRUE)
top_types <- names(types_tt0)

# remove NAs
types_tt2 <- types_tt[!is.na(names(types_tt))]

# set colors
color_palette <- get.palette(values = length(top_types))
names(color_palette) <- top_types

# generate barplot
plot_file <- file.path(stats_folder, paste0("types", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  barplot(
    height = types_tt2[top_types],
    names.arg = top_types,
    #xlab = "Stint types",
    legend = FALSE,
    #las = 2, log = "y",
    col = color_palette[top_types]
  )
dev.off()




########################################################################
# distribution of data sources
source_names <- c("enWP", "esWP", "frWP", "itWP", "jaWP", "WD")

# get data source info
data_sources <- c()
data_sources_df <- matrix(0, nrow = nrow(stints), ncol = length(source_names), dimnames = list(c(), source_names))
for (t in 1:nrow(stints)) {
  if (t %% 1000 == 0)
    tlog(2, "Processing stint ", t, "/", nrow(stints))

  # get data source list
  lst <- strsplit(stints[t, "dataSources"], split = ";")
  for (source in lst[[1]])
    data_sources_df[t, trimws(source)] <- 1
  stint_data_sources <- trimws(lst[[1]])

  # add to stat list
  data_sources <- c(data_sources, stint_data_sources)
}

# count values
countr_tt <- table(data_sources, useNA = "always")
print(countr_tt)

# focus on most frequent values
countr_tt0 <- sort(table(data_sources, useNA = "no"), decreasing = TRUE)
top_data_sources <- names(countr_tt0)

# remove NAs
countr_tt2 <- countr_tt[!is.na(names(countr_tt))]

# set colors
color_palette <- DATASOURCE_COLORS

# generate barplot
plot_file <- file.path(stats_folder, paste0("data-sources_barplot", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  barplot(
    height = countr_tt2[top_data_sources],
    names.arg = top_data_sources,
    #xlab = "Stint data sources",
    legend = FALSE,
    las = 2,
    col = color_palette[top_data_sources]
  )
dev.off()

# generate up-set diagram
plot_file <- file.path(stats_folder, paste0("data-sources_up-set", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  upset(as.data.frame(data_sources_df), sets = source_names)
dev.off()

# generate chordal diagram
overlap <- t(data_sources_df) %*% data_sources_df
diag(overlap) <- 0
#
plot_file <- file.path(stats_folder, paste0("data-sources_chord-diag", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  chordDiagram(overlap, grid.col = color_palette[source_names])
dev.off()

# generate Jaccard similarity matrix
jacc_sim <- matrix(0, nrow = length(source_names), ncol = length(source_names), dimnames = list(source_names, source_names))
for (i in 1:length(source_names)) {
  for (j in 1:length(source_names)) {
    jacc_sim[i, j] <- sum(data_sources_df[, i] & data_sources_df[, j]) / sum(data_sources_df[, i] | data_sources_df[, j])
  }
}
#
plot_file <- file.path(stats_folder, paste0("data-sources_jaccard-matrix", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  corrplot(jacc_sim,
    is.corr = FALSE, diag = FALSE,
    method = "color",
    addCoef.col = "white",
    col = viridis(100), col.lim = c(0, 1), tl.col = "black"
  )
dev.off()




########################################################################
# distribution of team countries
plot_top <- 12

# get team country info
team_countries <- c()
for (t in 1:nrow(stints)) {
  if (t %% 1000 == 0)
    tlog(2, "Processing stint ", t, "/", nrow(stints))

  team_id <- stints[t, "teamRsId"]
  idx <- which(teams[, "rugbyscopeId"] == team_id)

  # get country list
  stint_countries <- trimws(strsplit(teams[idx, "countries"], split = ";")[[1]])

  # add to stat list
  team_countries <- c(team_countries, stint_countries)
}

# count values
countr_tt <- table(team_countries, useNA = "always")
print(countr_tt)

# focus on most frequent values
countr_tt0 <- sort(table(team_countries, useNA = "no"), decreasing = TRUE)
top_countries <- names(countr_tt0)[1:plot_top]

# remove NAs
countr_tt2 <- countr_tt[!is.na(names(countr_tt))]

# add a new value for category others
countr_tt2 <- c(countr_tt2, "Others" = sum(countr_tt2[!(names(countr_tt2) %in% top_countries)], na.rm = TRUE))
top_countries <- c(top_countries, "Others")

# set colors
color_palette <- c(COUNTRY_COLORS, "Others" = "#919191")

# generate barplot
plot_file <- file.path(stats_folder, paste0("team-countries", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  barplot(
    height = countr_tt2[top_countries],
    names.arg = top_countries,
    #xlab = "Stint team countries",
    legend = FALSE,
    las = 2,
    col = color_palette[top_countries]
  )
dev.off()




########################################################################
# distribution of player countries
plot_top <- 12

# get player country info
player_countries <- c()
for (t in 1:nrow(stints)) {
  if (t %% 1000 == 0)
    tlog(2, "Processing stint ", t, "/", nrow(stints))

  player_id <- stints[t, "playerId"]
  idx <- which(players[, "wikidataId"] == player_id)

  # get country list
  sport_countries <- players[idx, "sportCountries"]
  if (!is.na(sport_countries))
    stint_countries <- trimws(strsplit(sport_countries, split = ";")[[1]])
  else {
    citizenships <- players[idx, "citizenships"]
    stint_countries <- trimws(strsplit(citizenships, split = ";")[[1]])
  }

  # add to stat list
  player_countries <- c(player_countries, stint_countries)
}

# count values
countr_tt <- table(player_countries, useNA = "always")
print(countr_tt)

# focus on most frequent values
countr_tt0 <- sort(table(player_countries, useNA = "no"), decreasing = TRUE)
top_countries <- names(countr_tt0)[1:plot_top]

# remove NAs
countr_tt2 <- countr_tt[!is.na(names(countr_tt))]

# add a new value for category others
countr_tt2 <- c(countr_tt2, "Others" = sum(countr_tt2[!(names(countr_tt2) %in% top_countries)], na.rm = TRUE))
top_countries <- c(top_countries, "Others")

# set colors
color_palette <- c(COUNTRY_COLORS, "Others" = "#919191")

# generate barplot
plot_file <- file.path(stats_folder, paste0("player-countries", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  barplot(
    height = countr_tt2[top_countries],
    names.arg = top_countries,
    #xlab = "Stint player countries",
    legend = FALSE,
    las = 2,
    col = color_palette[top_countries]
  )
dev.off()




########################################################################
# stop logging
end.rec.log()
