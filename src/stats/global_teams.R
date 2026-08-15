########################################################################
# Generates various plots regarding the teams table, without
# considering time.
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/stats/global_teams.R")
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
start.rec.log("GlobalTeams")




########################################################################
# create output folder
stats_folder <- file.path("data", "stats", "global_teams")
dir.create(stats_folder, showWarnings = FALSE, recursive = TRUE)

# load tables
source("src/stats/load_all_tables.R")




########################################################################
# distribution of inception years

# retrieve values
inception_dates <- teams[, "inceptionDate"] %>% as.Date() %>% format("%Y") %>% as.integer()

# earliest year
earliest_date <- min(teams[, "inceptionDate"], na.rm = TRUE)
idx <- which(teams[, "inceptionDate"] == earliest_date)
earliest_team_names <- teams[idx, "fullName"]
tlog("Earliest inception date: ", paste0(earliest_date, collapse = ", "))
tlog("Teams (", length(idx), "): ", paste0(earliest_team_names, collapse = ", "))
print(teams[idx, ])

# latest year
latest_date <- max(teams[, "inceptionDate"], na.rm = TRUE)
idx <- which(teams[, "inceptionDate"] == latest_date)
latest_team_names <- teams[idx, "fullName"]
tlog("Latest inception date: ", paste0(latest_date, collapse = ", "))
tlog("Teams (", length(idx), "): ", paste0(latest_team_names, collapse = ", "))
print(teams[idx, ])

# compute distribution
tt <- table(inception_dates, useNA = "always")
inception_dates <- inception_dates[!is.na(inception_dates)]

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("inception-years0", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("InceptionYear", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# produce plot
plot_file <- file.path(stats_folder, paste0("inception-years", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(inception_dates,
    main = NA,
    xlab = "Team inception year",
    col = "red",
    # las = 2,
    breaks = 30
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("inception-years", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of termination years

# retrieve values
termination_dates <- teams[, "terminationDate"] %>% as.Date() %>% format("%Y") %>% as.integer()

# earliest year
earliest_date <- min(teams[, "terminationDate"], na.rm = TRUE)
idx <- which(teams[, "terminationDate"] == earliest_date)
earliest_team_names <- teams[idx, "fullName"]
tlog("Earliest termination date: ", paste0(earliest_date, collapse = ", "))
tlog("Teams (", length(idx), "): ", paste0(earliest_team_names, collapse = ", "))
print(teams[idx, ])

# latest year
latest_date <- max(teams[, "terminationDate"], na.rm = TRUE)
idx <- which(teams[, "terminationDate"] == latest_date)
latest_team_names <- teams[idx, "fullName"]
tlog("Latest termination date: ", paste0(latest_date, collapse = ", "))
tlog("Teams (", length(idx), "): ", paste0(latest_team_names, collapse = ", "))
print(teams[idx, ])

# compute distribution
tt <- table(termination_dates, useNA = "always")
termination_dates <- termination_dates[!is.na(termination_dates)]

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("termination-years0", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("TerminationYear", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# produce plot
plot_file <- file.path(stats_folder, paste0("termination-years", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(termination_dates,
    main = NA,
    xlab = "Team termination year",
    col = "red",
    # las = 2,
    breaks = 30
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("termination-years", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of nations
plot_top <- 12

# get nation info
nations <- c()
nation_nbr <- c()
for (t in 1:nrow(teams)) {
  if (t %% 1000 == 0)
    tlog(2, "Processing team ", t, "/", nrow(teams))
  team_id <- teams[t, "rugbyscopeId"]

  # get nation list
  team_nations <- trimws(strsplit(teams[t, "nations"], split = ";")[[1]])

  # add to stat list
  nations <- c(nations, team_nations)
  nation_nbr <- c(nation_nbr, length(team_nations))
}
tlog("Number of distinct team nations: ", length(unique(nations)))
tlog("Average number of nations by team: ", mean(nation_nbr, na.rm = TRUE), " (", sd(nation_nbr, na.rm = TRUE),")")



# count values
nat_tt <- table(nations, useNA = "always")
print(nat_tt)
# export values as a csv file
tab_file <- file.path(stats_folder, paste0("nations0", ".csv"))
tab <- as.data.frame(nat_tt)
colnames(tab) <- c("Nation", "Count")
write.csv(as.data.frame(tab), tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# focus on most frequent values
nat_tt0 <- sort(table(nations, useNA = "no"), decreasing = TRUE)
top_nations <- names(nat_tt0)[1:plot_top]

# remove NAs
nat_tt2 <- nat_tt[!is.na(names(nat_tt))]

# add a new value for category others
nat_tt2 <- c(nat_tt2, "Others" = sum(nat_tt2[!(names(nat_tt2) %in% top_nations)], na.rm = TRUE))
top_nations <- c(top_nations, "Others")

# set colors
color_palette <- c(NATION_COLORS, "Others" = "#919191")

# generate barplot
plot_file <- file.path(stats_folder, paste0("nations", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(3.0, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(1.50, 4.00, 0.25, 0.00))  # control margins: B L T R
  heights <- nat_tt2[top_nations]

  # init plot
  bp <- barplot(
    height = heights,
    #xlab = "Team Nations",
    ylab = "Frequency",
    names.arg = FALSE,
    legend = FALSE,
    las = 2,
    col = color_palette[top_nations]
  )
  mtext("Team nation", side = 1, line = 0.25)

  # decide bar text pos
  outside_text <- which(heights < 0.5 * max(heights, na.rm = TRUE))
  inside_text <- which(heights >= 0.5 * max(heights, na.rm = TRUE))
  # bar names on top
  if (length(outside_text) > 0) {
    text(bp[outside_text],
      heights[outside_text] + 0.025 * max(heights, na.rm = TRUE),
      labels = top_nations[outside_text],
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
      labels = top_nations[inside_text],
      col = text_color(color_palette[top_nations[inside_text]]),
      srt = 90,
      adj = c(1, 0.5),
      xpd = TRUE
    )
  }
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("nations", ".csv"))
tab <- cbind("Nation" = top_nations, "Count" = heights)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of types
plot_top <- 9

# retrieve types
types <- teams[, "type"]

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
top_types <- names(types_tt0)[1:plot_top]

# remove NAs
types_tt2 <- types_tt[!is.na(names(types_tt))]

# set colors
color_palette <- TEAMTYPE_COLORS

# add a new value for category others
if (length(unique(types)) > plot_top) {
  types_tt2 <- c(types_tt2, "Others" = sum(types_tt2[!(names(types_tt2) %in% top_types)], na.rm = TRUE))
  top_types <- c(top_types, "Others")

  # set colors
  color_palette <- c(color_palette, "Others" = "#919191")
}

# generate barplot
plot_file <- file.path(stats_folder, paste0("types", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(1.50, 2.50, 0.00, 0.00))  # control margins: B L T R
  heights <- types_tt2[top_types]

  # init plot
  bp <- barplot(
    height = heights,
    # xlab = "Team type",
    ylab = "Frequency",
    names.arg = FALSE,
    legend = FALSE,
    las = 2, log = "y",
    col = color_palette[top_types]
  )
  mtext("Team type", side = 1, line = 0.25)

  # decide bar text pos
  outside_text <- which(heights < 0.5 * max(heights, na.rm = TRUE))
  inside_text <- which(heights >= 0.5 * max(heights, na.rm = TRUE))

  # bar names on top
  if (length(outside_text) > 0) {
    text(bp[outside_text],
      10^(log(heights[outside_text],10) + 0.025 * max(log(heights,10), na.rm = TRUE)),
      labels = top_types[outside_text],
      col = "black",
      srt = 90,
      adj = c(0, 0.5),
      xpd = TRUE
    )
  }

  # bar names inside
  if (length(inside_text) > 0) {
    text(bp[inside_text],
      10^(log(heights[inside_text], 10) - 0.025 * max(log(heights, 10), na.rm = TRUE)),
      labels = top_types[inside_text],
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
# distribution of stint number and duration, numbers of matches played and points scored

# retrieve stint stats
stint_nbr <- c()
stint_dur <- c()
points_scored_mean <- c()
matches_played_mean <- c()
points_scored_nbr <- c()
matches_played_nbr <- c()
for (t in 1:nrow(teams)) {
  if (t %% 1000 == 0)
    tlog(2, "Processing entry ", t, "/", nrow(teams))
  team_id <- teams[t, "rugbyscopeId"]

  team_stints <- stints[stints[, "teamRsId"] == team_id, ]
  stint_nbr <- c(stint_nbr, nrow(team_stints))
  team_durations <- team_stints[, "endYear"] - team_stints[, "startYear"]
  stint_dur <- c(stint_dur, mean(team_durations, na.rm = TRUE))

  total_points <- sum(team_stints[, "pointsScored"], na.rm = TRUE)
  points_scored_nbr <- c(points_scored_nbr, total_points)
  total_matches <- sum(team_stints[, "matchesPlayed"], na.rm = TRUE)
  matches_played_nbr <- c(matches_played_nbr, total_matches)

  idx <- which(!is.na(team_stints[, "pointsScored"]) & !is.na(team_durations))
  tmp_scored <- sum(team_stints[idx, "pointsScored"], na.rm = TRUE) / sum(team_durations[idx], na.rm = TRUE)
  if (is.nan(tmp_scored) || is.infinite(tmp_scored))
    tmp_scored <- NA
  points_scored_mean <- c(points_scored_mean, tmp_scored)
  #
  idx <- which(!is.na(team_stints[, "matchesPlayed"]) & !is.na(team_durations))
  tmp_played <- sum(team_stints[idx, "matchesPlayed"], na.rm = TRUE) / sum(team_durations[idx], na.rm = TRUE)
  if (is.nan(tmp_played) || is.infinite(tmp_played))
    tmp_played <- NA
  matches_played_mean <- c(matches_played_mean, tmp_played)
}

# display basic stats
tlog("Average stint number by team: ", mean(stint_nbr, na.rm = TRUE), " (sd: ", sd(stint_nbr, na.rm = TRUE),")")
max_val <- max(stint_nbr, na.rm = TRUE)
idx <- which(stint_nbr == max_val)
max_names <- teams[idx, "fullName"]
tlog(2, "Max value: ", max_val)
tlog(2, "Max teams (", length(idx), "): ", paste0(max_names, collapse = ", "))
print(teams[idx, ])
#
tlog("Average mean stint duration by team: ", mean(stint_dur, na.rm = TRUE), " (sd: ", sd(stint_dur, na.rm = TRUE),")")
max_val <- max(stint_dur, na.rm = TRUE)
idx <- which(stint_dur == max_val)
max_names <- teams[idx, "fullName"]
tlog(2, "Max value: ", max_val)
tlog(2, "Max teams (", length(idx), "): ", paste0(max_names, collapse = ", "))
print(teams[idx, ])
#
tlog("Average yearly number of points scored by team: ", mean(points_scored_mean, na.rm = TRUE), " (sd: ", sd(points_scored_mean, na.rm = TRUE),")")
max_val <- max(points_scored_mean, na.rm = TRUE)
idx <- which(points_scored_mean == max_val)
max_names <- teams[idx, "fullName"]
tlog(2, "Max value: ", max_val)
tlog(2, "Max teams (", length(idx), "): ", paste0(max_names, collapse = ", "))
print(teams[idx, ])
# # this actually does not make much sense, since the same match can be counted several (many) times
# tlog("Average yearly number of matches played by team: ", mean(matches_played_mean, na.rm = TRUE), " (sd: ", sd(matches_played_mean, na.rm = TRUE),")")
# max_val <- max(matches_played_mean, na.rm = TRUE)
# idx <- which(matches_played_mean == max_val)
# max_names <- teams[idx, "fullName"]
# tlog(2, "Max value: ", max_val)
# tlog(2, "Max teams (", length(idx), "): ", paste0(max_names, collapse = ", "))
# print(teams[idx, ])
#
tlog("Average overall number of points scored by team: ", mean(points_scored_nbr, na.rm = TRUE), " (sd: ", sd(points_scored_nbr, na.rm = TRUE),")")
max_val <- max(points_scored_nbr, na.rm = TRUE)
idx <- which(points_scored_nbr == max_val)
max_names <- teams[idx, "fullName"]
tlog(2, "Max value: ", max_val)
tlog(2, "Max teams (", length(idx), "): ", paste0(max_names, collapse = ", "))
print(teams[idx, ])
# # same, this does not make much sense
# tlog("Average overall number of matches played by team: ", mean(matches_played_nbr, na.rm = TRUE), " (sd: ", sd(matches_played_nbr, na.rm = TRUE),")")
# max_val <- max(matches_played_nbr, na.rm = TRUE)
# idx <- which(matches_played_nbr == max_val)
# max_names <- teams[idx, "fullName"]
# tlog(2, "Max value: ", max_val)
# tlog(2, "Max teams (", length(idx), "): ", paste0(max_names, collapse = ", "))
# print(teams[idx, ])

# export stint numbers as a csv file
tt <- table(stint_nbr, useNA = "always")
tab_file <- file.path(stats_folder, paste0("stint-numbers0", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("StintNumber", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")
# export career points as a csv file
tt <- table(points_scored_nbr, useNA = "always")
tab_file <- file.path(stats_folder, paste0("points-scored-overall0", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("PointsScored", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")
# # export career matches as a csv file
# tt <- table(matches_played_nbr, useNA = "always")
# tab_file <- file.path(stats_folder, paste0("matches-played-overall0", ".csv"))
# tab <- as.data.frame(tt)
# colnames(tab) <- c("MatchesPlayed", "Count")
# write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")


# plot stint numbers
plot_file <- file.path(stats_folder, paste0("stint-numbers", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(stint_nbr,
    main = NA,
    xlab = "Number of stints by team",
    # breaks = 30,
    # log = "y",
    col = "red"
  )

  # #### log version -- start

  # hh <- hist(stint_nbr, plot = FALSE)
  # # choose a baseline for the bars (can't be 0 on log scale)
  # ybottom <- min(hh$counts[hh$counts > 0])  # or just use 1
  # # set up an empty plot with correct axes/limits
  # plot(
  #   x = hh$breaks,
  #   y = c(hh$counts, NA),
  #   type = "n",
  #   xlab = "Number of stints by team",
  #   ylab = "Frequency",
  #   ylim = c(ybottom, max(hh$counts)),
  #   log = "y"
  # )

  # # draw the bars manually
  # rect(
  #   xleft = hh$breaks[-length(hh$breaks)],
  #   xright = hh$breaks[-1],
  #   ybottom = ybottom,
  #   ytop = ifelse(hh$counts == 0, ybottom, hh$counts),
  #   col = "red"
  # )

  # #### log version -- end
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("stint-numbers", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")


# plot stint durations
plot_file <- file.path(stats_folder, paste0("stint-durations", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  # excluding a statistic anomaly: a 50 year long stint (far longer than the rest)
  idx <- which(players[, "wikidataId"] == "Q131675151")

  hh <- hist(stint_dur[-idx],
    main = NA,
    xlab = "Average mean stint duration by team",
    # log = "y",
    col = "red"
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("stint-durations", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")


# plot yearly points scored
plot_file <- file.path(stats_folder, paste0("points-scored-yearly", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(points_scored_mean,
    main = NA,
    xlab = "Yearly points scored by team",
    col = "red",
    # log = "y"
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("points-scored-yearly", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")


# # plot yearly matches played
# plot_file <- file.path(stats_folder, paste0("matches-played-yearly", ".pdf"))
# tlog("Producing plot file: ", plot_file)
# pdf(plot_file, width = 7, height = 7)
#   par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
#   par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R
#
#   hh <- hist(matches_played_mean,
#     main = NA,
#     xlab = "Yearly matches played by team",
#     col = "red",
#     # log = "y"
#   )
# dev.off()
#
# # export values as a csv file
# tab_file <- file.path(stats_folder, paste0("matches-played-yearly", ".csv"))
# nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
# tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
# write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")


# plot career points scored
plot_file <- file.path(stats_folder, paste0("points-scored-overall", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(points_scored_nbr,
    main = NA,
    xlab = "Overall points scored by team",
    col = "red",
    # log = "y"
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("points-scored-overall", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")


# # plot career matches played
# plot_file <- file.path(stats_folder, paste0("matches-played-overall", ".pdf"))
# tlog("Producing plot file: ", plot_file)
# pdf(plot_file, width = 7, height = 7)
#   par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
#   par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R
#
#   hh <- hist(matches_played_nbr,
#     main = NA,
#     xlab = "Overall matches played by team",
#     col = "red",
#     # log = "y"
#   )
# dev.off()
#
# # export values as a csv file
# tab_file <- file.path(stats_folder, paste0("matches-played-overall", ".csv"))
# nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
# tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
# write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of data sources
source_names <- c("DBPD", "enWP", "esWP", "frWP", "itWP", "jaWP", "WD")
map <- c("dbpediaId" = "DBPD", "wikipediaEn" = "enWP", "wikipediaEs" = "esWP", "wikipediaFr" = "frWP", "wikipediaIt" = "itWP", "wikipediaJa" = "jaWP", "wikidataId" = "WD")

# retrieve sources
data_sources <- c()
data_sources_df <- matrix(0, nrow = nrow(teams), ncol = length(source_names), dimnames = list(c(), source_names))
for (t in 1:nrow(teams)) {
  if (t %% 1000 == 0)
    tlog(2, "Processing entry ", t, "/", nrow(teams))
  team_id <- teams[t, "rugbyscopeId"]

  team_data_sources <- c()
  for (col in names(map)) {
    if (!is.na(teams[t, col])) {
      team_data_sources <- c(team_data_sources, map[col])
      data_sources_df[t, map[col]] <- 1
    }
  }

  # add to stat list
  data_sources <- c(data_sources, team_data_sources)
}
data_source_nbr <- apply(data_sources_df, 1, sum)
tlog("Number of distinct values: ", length(unique(data_sources)))
tlog("Average number of sources by team: ", mean(data_source_nbr), " (", sd(data_source_nbr),")")

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
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(1.50, 2.50, 0.00, 0.00))  # control margins: B L T R
  heights <- sources_tt2[top_data_sources]

  # init plot
  bp <- barplot(
    height = heights,
    # xlab = "Team data source",
    ylab = "Frequency",
    names.arg = FALSE,
    legend = FALSE,
    #las = 2,
    col = color_palette[top_data_sources]
  )
  mtext("Team data source", side = 1, line = 0.25)

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



# produce plot
plot_file <- file.path(stats_folder, paste0("data-source-nbr", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(data_source_nbr,
    main = NA,
    xlab = "Number of player data sources",
    col = "red", breaks = 7
  )
dev.off()

# export counts as a csv file
tt <- table(data_source_nbr)
tab_file <- file.path(stats_folder, paste0("data-source-nbr", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("SourceNumber", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of sources vs. nations

# retrieve sources
cr_data_sources <- c()
cr_nations <- c()
for (t in 1:nrow(teams)) {
  if (t %% 1000 == 0)
    tlog(2, "Processing entry ", t, "/", nrow(teams))
  team_id <- teams[t, "rugbyscopeId"]

  # get nations
  team_nations <- trimws(strsplit(teams[t, "nations"], split = ";")[[1]])

  # get data sources
  team_data_sources <- c()
  for (col in names(map)) {
    if (!is.na(teams[t, col])) {
      team_data_sources <- c(team_data_sources, map[col])
    }
  }

  # add to stat lists
  cr_nations <- c(cr_nations, rep(team_nations, each = length(team_data_sources)))
  cr_data_sources <- c(cr_data_sources, rep(team_data_sources, length(team_nations)))
}

# count values
cr_tt <- table(cr_data_sources, cr_nations, useNA = "always")
print(cr_tt)
# export values as a csv file
tab_file <- file.path(stats_folder, paste0("data-sources_vs_nations_contingency0", "-nbr", ".csv"))
write.csv(cr_tt, tab_file, row.names = TRUE, fileEncoding = "UTF-8")

# replace minority nations
idx <- which(is.na(colnames(cr_tt)) | !(colnames(cr_tt) %in% top_nations))
cr_tt2 <- cbind(cr_tt, "Others" = rowSums(cr_tt[, idx], na.rm = TRUE))

# remove NAs
cr_tt2 <- cr_tt2[!is.na(rownames(cr_tt2)), ]

# generate contingency table
for (i in 1:2) {
  if (i ==  2) {
    for (top_nation in top_nations)
      cr_tt2[, top_nation] <- 100 * cr_tt2[, top_nation] / nat_tt2[top_nation]
  }
  plot_file <- file.path(stats_folder, paste0("data-sources_vs_nations_contingency", if (i == 1) "-nbr" else "-prop", ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    corrplot(cr_tt2[, top_nations],
      is.corr = FALSE, #diag = FALSE,
      method = "color",
      number.digits = 0,
      addCoef.col = "white",
      col = viridis(100), #col.lim = c(0, 1), 
      tl.col = "black"
    )
  dev.off()

  # export values as a csv file
  tab_file <- file.path(stats_folder, paste0("data-sources_vs_nations_contingency", if (i == 1) "-nbr" else "-prop", ".csv"))
  write.csv(cr_tt2[, top_nations], tab_file, row.names = TRUE, fileEncoding = "UTF-8")
}




########################################################################
# overall completeness stats
field_groups <- list(
  "perso-fields" = c("type", "inceptionDate", "terminationDate", "nations"),
  "rugby-fields" = c("affiliations", "competitions", "tier", "homeVenueNames", "homeVenueCapacities", "locations"),
  "id-fields" = c("wikidataId", "allRugbyId", "googleKnowlId", "dbpediaId"),
  "wp-fields" = c("wikipediaEn", "wikipediaFr", "wikipediaIt", "wikipediaEs", "wikipediaJa")
)

pal <- viridis(100)
for (g in 1:(length(field_groups) + 1)) {
  if (g > length(field_groups)) {
    fields <- unlist(field_groups)
    group_name <- "all-fields"
  } else {
    fields <- field_groups[[g]]
    group_name <- names(field_groups)[g]
  }
  props <- sapply(fields, function(field) length(which(!is.na(teams[, field])))) * 100 / nrow(teams)
  percs <- props * 100

  # export values as a csv file
  if (g > length(field_groups)) {
    tab_file <- file.path(stats_folder, paste0("completeness_", group_name, ".csv"))
    tab <- cbind("Field" = fields, "CompletenessRate" = props)
    write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")
  }

  colors <- pal[pmax(1, pmin(100, round(percs)))]

  # generate barplot
  plot_file <- file.path(stats_folder, paste0("completeness_", group_name, ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    par(mgp = c(2.0, 0.5, 0))             # reduce space between axis title / axis values and axis line
    par(mar = c(9.00, 3.00, 0.50, 0.00))  # control margins: B L T R

    # init barplot
    bp <- barplot(
      height = percs,
      names.arg = fields,
      #xlab = "Team fields",
      legend = FALSE,
      las = 2,
      col = colors,
    )

    # add bar values
    text(
      x = if (g > length(field_groups)) bp + 0.20 else bp,
      y = if (g > length(field_groups)) percs - 0.06 * max(percs) else percs - 0.1 * max(percs),
      labels = paste0(round(percs), "%", sep = ""),
      pos = 3, col = sapply(percs, function(val) if (val < 75) "white" else "black"),
      cex = if (g > length(field_groups)) 0.75 else 1.5, font = 2,
      srt = if (g > length(field_groups)) 90 else 0
    )
  dev.off()
}




########################################################################
# stop logging
end.rec.log()
