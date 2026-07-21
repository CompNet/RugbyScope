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

# retrieve values
start_dates <- stints[, "startYear"]

# earliest year
earliest_year <- min(start_dates, na.rm = TRUE)
idx <- which(start_dates == earliest_year)
earliest_player_names <- stints[idx, "playerName"]
earliest_player_ids <- stints[idx, "playerId"]
tlog("Earliest start year: ", paste0(earliest_year, collapse = ", "))
tlog("Players (", length(idx), "): ", paste0(earliest_player_names, collapse = ", "))
print(players[players[, "wikidataId"] %in% earliest_player_ids, ])

# latest year
latest_year <- max(start_dates, na.rm = TRUE)
idx <- which(start_dates == latest_year)
latest_player_names <- stints[idx, "playerName"]
latest_player_ids <- stints[idx, "playerId"]
tlog("Latest start year: ", paste0(latest_year, collapse = ", "))
tlog("Players (", length(idx), "): ", paste0(latest_player_names, collapse = ", "))
print(players[players[, "wikidataId"] %in% latest_player_ids, ])

# compute distribution
tt <- table(start_dates, useNA = "always")
start_dates <- start_dates[!is.na(start_dates)]

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("start-years0", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("StartYear", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# produce plot
plot_file <- file.path(stats_folder, paste0("start-years", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(start_dates,
    main = NA,
    xlab = "Stint start year",
    col = "red",
    # las = 2,
    breaks = 30
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("start-years", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of end years

# retrieve values
end_dates <- stints[, "endYear"]

# earliest year
earliest_year <- min(end_dates, na.rm = TRUE)
idx <- which(end_dates == earliest_year)
earliest_player_names <- stints[idx, "playerName"]
earliest_player_ids <- stints[idx, "playerId"]
tlog("Earliest end year: ", paste0(earliest_year, collapse = ", "))
tlog("Players (", length(idx), "): ", paste0(earliest_player_names, collapse = ", "))
print(players[players[, "wikidataId"] %in% earliest_player_ids, ])

# latest year
latest_year <- max(end_dates, na.rm = TRUE)
idx <- which(end_dates == latest_year)
latest_player_names <- stints[idx, "playerName"]
latest_player_ids <- stints[idx, "playerId"]
tlog("Latest end year: ", paste0(latest_year, collapse = ", "))
tlog("Players (", length(idx), "): ", paste0(latest_player_names, collapse = ", "))
print(players[players[, "wikidataId"] %in% latest_player_ids, ])

# compute distribution
tt <- table(end_dates, useNA = "always")
end_dates <- end_dates[!is.na(end_dates)]

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("end-years0", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("EndYear", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# produce plot
plot_file <- file.path(stats_folder, paste0("end-years", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(end_dates,
    main = NA,
    xlab = "Stint end year",
    col = "red",
    # las = 2,
    breaks = 30
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("end-years", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of types

# retrieve types
types <- stints[, "type"]

# count values
types_tt <- table(types, useNA = "always")
print(types_tt)
# export values as a csv file
tab_file <- file.path(stats_folder, paste0("types0", ".csv"))
tab <- as.data.frame(types_tt)
colnames(tab) <- c("Type", "Count")
write.csv(as.data.frame(tab), tab_file, row.names = FALSE, fileEncoding = "UTF-8")

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
  par(mgp = c(3.0, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(1.50, 4.00, 0.25, 0.00))  # control margins: B L T R
  heights <- types_tt2[top_types]

  # init plot
  bp <- barplot(
    height = heights,
    #xlab = "Stint type",
    ylab = "Frequency",
    names.arg = FALSE,
    legend = FALSE,
    las = 2,
    col = color_palette[top_types]
  )
  mtext("Stint type", side = 1, line = 0.25)

  # decide bar text pos
  outside_text <- which(heights < 0.5 * max(heights, na.rm = TRUE))
  inside_text <- which(heights >= 0.5 * max(heights, na.rm = TRUE))
  
  # bar names on top
  if (length(outside_text) > 0) {
    text(bp[outside_text],
      heights[outside_text] + 0.025 * max(heights, na.rm = TRUE),
      labels = top_types[outside_text], cex = 2,
      col = "black",
      srt = 90,
      adj = c(0, 0.5),
      xpd = TRUE
    )
  }

  # bar names inside
  if (length(inside_text) > 0) {
    text(bp[inside_text],
      heights[inside_text] - 0.025 * max(heights, na.rm = TRUE),
      labels = top_types[inside_text], cex = 2,
      col = text_color(color_palette[top_types[inside_text]]),
      srt = 90,
      adj = c(1, 0.5),
      xpd = TRUE
    )
  }
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("types", ".csv"))
tab <- cbind("Type" = top_types, "Count" = heights)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of stint duration

# retrieve stint stats
stint_dur <- stints[, "endYear"] - stints[, "startYear"]

# display stats
tlog("Average stint duration: ", mean(stint_dur, na.rm = TRUE), "(sd: ", sd(stint_dur, na.rm = TRUE),")")
longest_stint <- max(stint_dur, na.rm = TRUE)
idx <- which(stint_dur == longest_stint)
longest_player_names <- stints[idx, "playerName"]
longest_player_ids <- stints[idx, "playerId"]
tlog("Longest stint: ", longest_stint)
tlog("Players (", length(idx), "): ", paste0(longest_player_names, collapse = ", "))
print(players[players[, "wikidataId"] %in% longest_player_ids, ])

# export values as a csv file
tt <- table(stint_dur, useNA = "always")
tab_file <- file.path(stats_folder, paste0("durations0", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("Duration", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# plot stint durations
plot_file <- file.path(stats_folder, paste0("durations", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  # excluding a statistic anomaly: a 50 year long stint (far longer than the rest)
  idx <- which(stints[, "playerId"] == "Q131675151")

  hh <- hist(stint_dur[-idx],
    main = NA,
    xlab = "Stint duration",
    col = "red",
    breaks = max(stint_dur, na.rm = TRUE)
    # log = "y"
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("durations", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of points scored and matches played

# retrieve stint stats
matches_played <- stints[, "matchesPlayed"]
points_scored <- stints[, "pointsScored"]

# display stats
tlog("Average number of matches played: ", mean(matches_played, na.rm = TRUE), "(sd: ", sd(matches_played, na.rm = TRUE),")")
max_val <- max(matches_played, na.rm = TRUE)
idx <- which(matches_played == max_val)
max_player_names <- stints[idx, "playerName"]
max_player_ids <- stints[idx, "playerId"]
tlog(2, "Max value: ", max_val)
tlog(2, "Players (", length(idx), "): ", paste0(max_player_names, collapse = ", "))
print(players[players[, "wikidataId"] %in% longest_player_ids, ])
#
tlog("Average number of points scored: ", mean(points_scored, na.rm = TRUE), "(sd: ", sd(points_scored, na.rm = TRUE),")")
max_val <- max(points_scored, na.rm = TRUE)
idx <- which(points_scored == max_val)
max_player_names <- stints[idx, "playerName"]
max_player_ids <- stints[idx, "playerId"]
tlog(2, "Max value: ", max_val)
tlog(2, "Players (", length(idx), "): ", paste0(max_player_names, collapse = ", "))
print(players[players[, "wikidataId"] %in% max_player_ids, ])

# export values as a csv file
tt <- table(stint_dur, useNA = "always")
tab_file <- file.path(stats_folder, paste0("durations0", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("Duration", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# plot stint durations
plot_file <- file.path(stats_folder, paste0("durations", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  # excluding a statistic anomaly: a 50 year long stint (far longer than the rest)
  idx <- which(stints[, "playerId"] == "Q131675151")

  hh <- hist(stint_dur[-idx],
    main = NA,
    xlab = "Stint duration",
    col = "red",
    breaks = max(stint_dur, na.rm = TRUE)
    # log = "y"
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("durations", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of data sources
source_names <- c("enWP", "esWP", "frWP", "itWP", "jaWP", "WD")

# get data source info
data_sources <- c()
data_sources_df <- matrix(0, nrow = nrow(stints), ncol = length(source_names), dimnames = list(c(), source_names))
for (s in 1:nrow(stints)) {
  if (s %% 1000 == 0)
    tlog(2, "Processing stint ", s, "/", nrow(stints))

  # get data source list
  lst <- strsplit(stints[s, "dataSources"], split = ";")
  for (source in lst[[1]])
    data_sources_df[s, trimws(source)] <- 1
  stint_data_sources <- trimws(lst[[1]])

  # add to stat list
  data_sources <- c(data_sources, stint_data_sources)
}
data_source_nbr <- apply(data_sources_df, 1, sum)
tlog("Number of distinct values: ", length(unique(data_sources)))
tlog("Average number of sources by stint: ", mean(data_source_nbr), " (", sd(data_source_nbr),")")

# count values
sources_tt <- table(data_sources, useNA = "always")
print(sources_tt)

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("data-sources0", ".csv"))
tab <- as.data.frame(sources_tt)
colnames(tab) <- c("Source", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# focus on most frequent values
sources_tt0 <- sort(table(data_sources, useNA = "no"), decreasing = TRUE)
top_data_sources <- names(sources_tt0)

# remove NAs
sources_tt2 <- sources_tt[!is.na(names(sources_tt))]

# set colors
color_palette <- DATASOURCE_COLORS

# generate barplot
plot_file <- file.path(stats_folder, paste0("data-sources_barplot", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(3.0, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(1.50, 4.00, 0.25, 0.00))  # control margins: B L T R
  heights <- sources_tt2[top_data_sources]

  # init plot
  bp <- barplot(
    height = heights,
    #xlab = "Stint data source",
    ylab = "Frequency",
    names.arg = FALSE,
    legend = FALSE,
    las = 2,
    col = color_palette[top_data_sources]
  )
  mtext("Stint data source", side = 1, line = 0.25)

  # decide bar text pos
  outside_text <- which(heights < 0.5 * max(heights, na.rm = TRUE))
  inside_text <- which(heights >= 0.5 * max(heights, na.rm = TRUE))
  
  # bar names on top
  if (length(outside_text) > 0) {
    text(bp[outside_text],
      heights[outside_text] + 0.025 * max(heights, na.rm = TRUE),
      labels = top_data_sources[outside_text], cex = 2,
      col = "black",
      srt = 90,
      adj = c(0, 0.5),
      xpd = TRUE
    )
  }

  # bar names inside
  if (length(inside_text) > 0) {
    text(bp[inside_text],
      heights[inside_text] - 0.025 * max(heights, na.rm = TRUE),
      labels = top_data_sources[inside_text], cex = 2,
      col = text_color(color_palette[top_data_sources[inside_text]]),
      srt = 90,
      adj = c(1, 0.5),
      xpd = TRUE
    )
  }
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("data-sources", ".csv"))
tab <- cbind("Source" = top_data_sources, "Count" = heights)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

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

# export similarity as a csv file
tab_file <- file.path(stats_folder, paste0("data-sources_jaccard-matrix", ".csv"))
write.csv(jacc_sim, tab_file, row.names = TRUE, fileEncoding = "UTF-8")




########################################################################
# distribution of team countries
plot_top <- 12

# get team country info
team_countries <- c()
for (s in 1:nrow(stints)) {
  if (s %% 1000 == 0)
    tlog(2, "Processing stint ", s, "/", nrow(stints))

  team_id <- stints[s, "teamRsId"]
  t <- which(teams[, "rugbyscopeId"] == team_id)

  # get country list
  stint_countries <- trimws(strsplit(teams[t, "countries"], split = ";")[[1]])

  # add to stat list
  team_countries <- c(team_countries, stint_countries)
}

# count values
tm_countr_tt <- table(team_countries, useNA = "always")
print(tm_countr_tt)
# export values as a csv file
tab_file <- file.path(stats_folder, paste0("team-countries0", ".csv"))
tab <- as.data.frame(tm_countr_tt)
colnames(tab) <- c("Country", "Count")
write.csv(as.data.frame(tab), tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# focus on most frequent values
tm_countr_tt0 <- sort(table(team_countries, useNA = "no"), decreasing = TRUE)
top_countries <- names(tm_countr_tt0)[1:plot_top]

# remove NAs
tm_countr_tt2 <- tm_countr_tt[!is.na(names(tm_countr_tt))]

# add a new value for category others
tm_countr_tt2 <- c(tm_countr_tt2, "Others" = sum(tm_countr_tt2[!(names(tm_countr_tt2) %in% top_countries)], na.rm = TRUE))
top_countries <- c(top_countries, "Others")

# set colors
color_palette <- c(COUNTRY_COLORS, "Others" = "#919191")

# generate barplot
plot_file <- file.path(stats_folder, paste0("team-countries", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(3.0, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(1.50, 4.00, 0.25, 0.00))  # control margins: B L T R
  heights <- tm_countr_tt2[top_countries]

  # init plot
  bp <- barplot(
    height = heights,
    #xlab = "Stint team country",
    ylab = "Frequency",
    names.arg = FALSE,
    legend = FALSE,
    las = 2,
    col = color_palette[top_countries]
  )
  mtext("Stint team country", side = 1, line = 0.25)

  # decide bar text pos
  outside_text <- which(heights < 0.5 * max(heights, na.rm = TRUE))
  inside_text <- which(heights >= 0.5 * max(heights, na.rm = TRUE))

  # bar names on top
  if (length(outside_text) > 0) {
    text(bp[outside_text],
      heights[outside_text] + 0.025 * max(heights, na.rm = TRUE),
      labels = top_countries[outside_text],
      col = "black",
      srt = 90,
      adj = c(0, 0.5),
      xpd = TRUE
    )
  }

  # bar names inside
  if (length(inside_text) > 0) {
    text(bp[inside_text],
      heights[inside_text] - 0.025 * max(heights, na.rm = TRUE),
      labels = top_countries[inside_text],
      col = text_color(color_palette[top_countries[inside_text]]),
      srt = 90,
      adj = c(1, 0.5),
      xpd = TRUE
    )
  }
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("team-countries", ".csv"))
tab <- cbind("Country" = top_countries, "Count" = heights)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of player countries
plot_top <- 12

# get player country info
player_countries <- c()
for (s in 1:nrow(stints)) {
  if (s %% 1000 == 0)
    tlog(2, "Processing stint ", s, "/", nrow(stints))

  player_id <- stints[s, "playerId"]
  p <- which(players[, "wikidataId"] == player_id)

  # get country list
  sport_countries <- players[p, "sportCountries"]
  if (!is.na(sport_countries))
    stint_countries <- trimws(strsplit(sport_countries, split = ";")[[1]])
  else {
    citizenships <- players[p, "citizenships"]
    stint_countries <- trimws(strsplit(citizenships, split = ";")[[1]])
  }

  # add to stat list
  player_countries <- c(player_countries, stint_countries)
}

# count values
pl_countr_tt <- table(player_countries, useNA = "always")
print(pl_countr_tt)
# export values as a csv file
tab_file <- file.path(stats_folder, paste0("player-countries0", ".csv"))
tab <- as.data.frame(pl_countr_tt)
colnames(tab) <- c("Country", "Count")
write.csv(as.data.frame(tab), tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# focus on most frequent values
pl_countr_tt0 <- sort(table(player_countries, useNA = "no"), decreasing = TRUE)
top_countries <- names(pl_countr_tt0)[1:plot_top]

# remove NAs
pl_countr_tt2 <- pl_countr_tt[!is.na(names(pl_countr_tt))]

# add a new value for category others
pl_countr_tt2 <- c(pl_countr_tt2, "Others" = sum(pl_countr_tt2[!(names(pl_countr_tt2) %in% top_countries)], na.rm = TRUE))
top_countries <- c(top_countries, "Others")

# set colors
color_palette <- c(COUNTRY_COLORS, "Others" = "#919191")

# generate barplot
plot_file <- file.path(stats_folder, paste0("player-countries", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(3.0, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(1.50, 4.00, 0.25, 0.00))  # control margins: B L T R
  heights <- pl_countr_tt2[top_countries]

  # init plot
  bp <- barplot(
    height = heights,
    #xlab = "Stint player country",
    ylab = "Frequency",
    names.arg = FALSE,
    legend = FALSE,
    las = 2,
    col = color_palette[top_countries]
  )
  mtext("Stint player country", side = 1, line = 0.25)

  # decide bar text pos
  outside_text <- which(heights < 0.5 * max(heights, na.rm = TRUE))
  inside_text <- which(heights >= 0.5 * max(heights, na.rm = TRUE))

  # bar names on top
  if (length(outside_text) > 0) {
    text(bp[outside_text],
      heights[outside_text] + 0.025 * max(heights, na.rm = TRUE),
      labels = top_countries[outside_text],
      col = "black",
      srt = 90,
      adj = c(0, 0.5),
      xpd = TRUE
    )
  }

  # bar names inside
  if (length(inside_text) > 0) {
    text(bp[inside_text],
      heights[inside_text] - 0.025 * max(heights, na.rm = TRUE),
      labels = top_countries[inside_text],
      col = text_color(color_palette[top_countries[inside_text]]),
      srt = 90,
      adj = c(1, 0.5),
      xpd = TRUE
    )
  }
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("player-countries", ".csv"))
tab <- cbind("Country" = top_countries, "Count" = heights)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of sources vs. player countries

# retrieve sources
cr_data_sources <- c()
cr_countries <- c()
for (s in 1:nrow(stints)) {
  if (s %% 1000 == 0)
    tlog(2, "Processing stint ", s, "/", nrow(stints))

  player_id <- stints[s, "playerId"]
  p <- which(players[, "wikidataId"] == player_id)

  # get countries
  sport_countries <- players[p, "sportCountries"]
  if (!is.na(sport_countries))
    player_countries <- trimws(strsplit(sport_countries, split = ";")[[1]])
  else {
    citizenships <- players[p, "citizenships"]
    player_countries <- trimws(strsplit(citizenships, split = ";")[[1]])
  }

  # get data source list
  stint_data_sources <- trimws(strsplit(stints[s, "dataSources"], split = ";")[[1]])

  # add to stat lists
  cr_countries <- c(cr_countries, rep(player_countries, each = length(stint_data_sources)))
  cr_data_sources <- c(cr_data_sources, rep(stint_data_sources, length(player_countries)))
}

# count values
cr_tt <- table(cr_data_sources, cr_countries, useNA = "always")
print(cr_tt)
# export values as a csv file
tab_file <- file.path(stats_folder, paste0("data-sources_vs_player-countries_contingency0", "-nbr", ".csv"))
write.csv(cr_tt, tab_file, row.names = TRUE, fileEncoding = "UTF-8")

# replace minority countries
idx <- which(is.na(colnames(cr_tt)) | !(colnames(cr_tt) %in% top_countries))
cr_tt2 <- cbind(cr_tt, "Others" = rowSums(cr_tt[, idx], na.rm = TRUE))

# remove NAs
cr_tt2 <- cr_tt2[!is.na(rownames(cr_tt2)), ]

# generate contingency table
for (i in 1:2) {
  if (i ==  2) {
    for (top_country in top_countries)
      cr_tt2[, top_country] <- 100 * cr_tt2[, top_country] / pl_countr_tt2[top_country]
  }
  plot_file <- file.path(stats_folder, paste0("data-sources_vs_player-countries_contingency", if (i == 1) "-nbr" else "-prop", ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    corrplot(cr_tt2[, top_countries],
      is.corr = FALSE, #diag = FALSE,
      method = "color",
      number.digits = 0,
      addCoef.col = "white", number.cex = if(i == 1) 0.75 else 1,
      col = viridis(100), #col.lim = c(0, 1), 
      tl.col = "black"
    )
  dev.off()

  # export values as a csv file
  tab_file <- file.path(stats_folder, paste0("data-sources_vs_player-countries_contingency", if (i == 1) "-nbr" else "-prop", ".csv"))
  write.csv(cr_tt2[, top_countries], tab_file, row.names = TRUE, fileEncoding = "UTF-8")
}




########################################################################
# distribution of sources vs. team countries

# retrieve sources
cr_data_sources <- c()
cr_countries <- c()
for (s in 1:nrow(stints)) {
  if (s %% 1000 == 0)
    tlog(2, "Processing stint ", s, "/", nrow(stints))

  team_id <- stints[s, "teamRsId"]
  t <- which(teams[, "rugbyscopeId"] == team_id)

  # get countries
  team_countries <- trimws(strsplit(teams[t, "countries"], split = ";")[[1]])

  # get data source list
  stint_data_sources <- trimws(strsplit(stints[s, "dataSources"], split = ";")[[1]])

  # add to stat lists
  cr_countries <- c(cr_countries, rep(team_countries, each = length(stint_data_sources)))
  cr_data_sources <- c(cr_data_sources, rep(stint_data_sources, length(team_countries)))
}

# count values
cr_tt <- table(cr_data_sources, cr_countries, useNA = "always")
print(cr_tt)
# export values as a csv file
tab_file <- file.path(stats_folder, paste0("data-sources_vs_team-countries_contingency0", "-nbr", ".csv"))
write.csv(cr_tt, tab_file, row.names = TRUE, fileEncoding = "UTF-8")

# replace minority countries
idx <- which(is.na(colnames(cr_tt)) | !(colnames(cr_tt) %in% top_countries))
cr_tt2 <- cbind(cr_tt, "Others" = rowSums(cr_tt[, idx], na.rm = TRUE))

# remove NAs
cr_tt2 <- cr_tt2[!is.na(rownames(cr_tt2)), ]

# generate contingency table
for (i in 1:2) {
  if (i ==  2) {
    for (top_country in top_countries)
      cr_tt2[, top_country] <- 100 * cr_tt2[, top_country] / tm_countr_tt2[top_country]
  }
  plot_file <- file.path(stats_folder, paste0("data-sources_vs_team-countries_contingency", if (i == 1) "-nbr" else "-prop", ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    corrplot(cr_tt2[, top_countries],
      is.corr = FALSE, #diag = FALSE,
      method = "color",
      number.digits = 0,
      addCoef.col = "white", number.cex = if(i == 1) 0.75 else 1,
      col = viridis(100), #col.lim = c(0, 1), 
      tl.col = "black"
    )
  dev.off()

  # export values as a csv file
  tab_file <- file.path(stats_folder, paste0("data-sources_vs_team-countries_contingency", if (i == 1) "-nbr" else "-prop", ".csv"))
  write.csv(cr_tt2[, top_countries], tab_file, row.names = TRUE, fileEncoding = "UTF-8")
}




########################################################################
# overall completeness stats
fields <- c("type", "startYear", "endYear", "matchesPlayed", "pointsScored", "dataSources")

pal <- viridis(100)
props <- sapply(fields, function(field) length(which(!is.na(stints[, field])))) * 100 / nrow(stints)
percs <- props * 100

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("completeness_all-fields", ".csv"))
tab <- cbind("Field" = fields, "CompletenessRate" = props)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

colors <- pal[pmax(1, pmin(100, round(perc)))]

# generate barplot
plot_file <- file.path(stats_folder, paste0("completeness_all-fields", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(2.0, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(9.00, 3.00, 0.50, 0.00))  # control margins: B L T R

  # init plot
  bp <- barplot(
    height = percs,
    names.arg = fields,
    #xlab = "Team fields",
    legend = FALSE,
    las = 2,
    col = colors,
  )

  # add bar text
  text(
    x = bp,
    y = percs - 0.1 * max(percs),
    labels = paste0(round(percs), "%", sep = ""),
    pos = 3, col = sapply(percs, function(val) if (val < 75) "white" else "black"),
    cex = 1.5, font = 2
  )
dev.off()




########################################################################
# stop logging
end.rec.log()
