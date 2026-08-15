########################################################################
# Generates various plots regarding the players table, without
# considering time.
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/stats/global_players.R")
########################################################################
library("dplyr")
library("UpSetR")
library("circlize")
library("corrplot")
#library("pheatmap")
library("viridis")



source("src/common/logging.R")
source("src/common/colors.R")
source("src/common/norm_positions.R")




########################################################################
# start logging
start.rec.log("GlobalPlayers")




########################################################################
# create output folder
stats_folder <- file.path("data", "stats", "global_players")
dir.create(stats_folder, showWarnings = FALSE, recursive = TRUE)

# load tables
source("src/stats/load_all_tables.R")




########################################################################
# distribution of birthdates

# retrieve values
birth_year <- players[, "birthDate"] %>% as.Date() %>% format("%Y") %>% as.integer()

# earliest year
earliest_date <- min(players[, "birthDate"], na.rm = TRUE)
idx <- which(players[, "birthDate"] == earliest_date)
earliest_player_names <- players[idx, "fullName"]
tlog("Earliest birth date: ", paste0(earliest_date, collapse = ", "))
tlog("Players (", length(idx), "): ", paste0(earliest_player_names, collapse = ", "))
print(players[idx, ])

# latest year
latest_date <- max(players[, "birthDate"], na.rm = TRUE)
idx <- which(players[, "birthDate"] == latest_date)
latest_player_names <- players[idx, "fullName"]
tlog("Latest birth date: ", paste0(latest_date, collapse = ", "))
tlog("Players (", length(idx), "): ", paste0(latest_player_names, collapse = ", "))
print(players[idx, ])

# compute distribution
tt <- table(birth_year, useNA = "always")
birth_year <- birth_year[!is.na(birth_year)]

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("birthyears0", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("BirthYear", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# produce plot
plot_file <- file.path(stats_folder, paste0("birthyears", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(birth_year,
    main = NA,
    xlab = "Player year of birth",
    col = "red"
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("birthyears", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of deathdates

# retrieve values
death_year <- players[, "deathDate"] %>% as.Date() %>% format("%Y") %>% as.integer()

# earliest year
earliest_date <- min(players[, "deathDate"], na.rm = TRUE)
idx <- which(players[, "deathDate"] == earliest_date)
earliest_player_names <- players[idx, "fullName"]
tlog("Earliest death date: ", paste0(earliest_date, collapse = ", "))
tlog("Players (", length(idx), "): ", paste0(earliest_player_names, collapse = ", "))
print(players[idx, ])

# latest year
latest_date <- max(players[, "deathDate"], na.rm = TRUE)
idx <- which(players[, "deathDate"] == latest_date)
latest_player_names <- players[idx, "fullName"]
tlog("Latest death date: ", paste0(latest_date, collapse = ", "))
tlog("Players (", length(idx), "): ", paste0(latest_player_names, collapse = ", "))
print(players[idx, ])

# compute distribution
tt <- table(death_year, useNA = "always")
death_year <- death_year[!is.na(death_year)]

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("deathyears", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("DeathYear", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# produce plot
plot_file <- file.path(stats_folder, paste0("deathyears", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(death_year,
    main = NA,
    xlab = "Player year of death",
    col = "red"
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("deathyears", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of career start years

# retrieve values
start_year <- players[, "careerStartYear"]

# earliest year
earliest_year <- min(start_year, na.rm = TRUE)
idx <- which(players[, "careerStartYear"] == earliest_year)
earliest_player_names <- players[idx, "fullName"]
tlog("Earliest career start year: ", paste0(earliest_year, collapse = ", "))
tlog("Players (", length(idx), "): ", paste0(earliest_player_names, collapse = ", "))
print(players[idx, ])

# latest year
latest_year <- max(start_year, na.rm = TRUE)
idx <- which(players[, "careerStartYear"] == latest_year)
latest_player_names <- players[idx, "fullName"]
tlog("Latest career start year: ", paste0(latest_year, collapse = ", "))
tlog("Players (", length(idx), "): ", paste0(latest_player_names, collapse = ", "))
print(players[idx, ])

# compute distribution
tt <- table(start_year, useNA = "always")
start_year <- start_year[!is.na(start_year)]

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("career-start-years0", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("CareerStartYear", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# produce plot
plot_file <- file.path(stats_folder, paste0("career-start-years", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(start_year,
    main = NA,
    xlab = "Player year of career start",
    col = "red"
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("career-start-year", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of career end years

# retrieve values
end_year <- players[, "careerEndYear"]

# earliest year
earliest_year <- min(end_year, na.rm = TRUE)
idx <- which(players[, "careerEndYear"] == earliest_year)
earliest_player_names <- players[idx, "fullName"]
tlog("Earliest career end year: ", paste0(earliest_year, collapse = ", "))
tlog("Players (", length(idx), "): ", paste0(earliest_player_names, collapse = ", "))
print(players[idx, ])

# latest year
latest_year <- max(end_year, na.rm = TRUE)
idx <- which(players[, "careerEndYear"] == latest_year)
latest_player_names <- players[idx, "fullName"]
tlog("Latest career end: ", paste0(latest_year, collapse = ", "))
tlog("Players (", length(idx), "): ", paste0(latest_player_names, collapse = ", "))
print(players[idx, ])

# compute distribution
tt <- table(end_year, useNA = "always")
end_year <- end_year[!is.na(end_year)]

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("career-end-years0", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("CareerEndYear", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# produce plot
plot_file <- file.path(stats_folder, paste0("career-end-years", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(end_year,
    main = NA,
    xlab = "Player year of career end",
    col = "red"
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("career-end-years", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of nations
plot_top <- 12

# get nation info
nations <- c()
nation_nbr <- c()
for (p in 1:nrow(players)) {
  if (p %% 1000 == 0)
    tlog(2, "Processing player ", p, "/", nrow(players))
  player_id <- players[p, "wikidataId"]

  # get nation list
  sport_nations <- players[p, "sportNations"]
  if (!is.na(sport_nations))
    player_nations <- trimws(strsplit(sport_nations, split = ";")[[1]])
  else {
    citizenships <- players[p, "citizenships"]
    player_nations <- trimws(strsplit(citizenships, split = ";")[[1]])
  }

  # add to stat list
  nations <- c(nations, player_nations)
  nation_nbr <- c(nation_nbr, length(player_nations))
}
tlog("Number of distinct player nations: ", length(unique(nations)))
tlog("Average number of nations by player: ", mean(nation_nbr, na.rm = TRUE), " (", sd(nation_nbr, na.rm = TRUE),")")

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
    #xlab = "Player Nations",
    ylab = "Frequency",
    names.arg = FALSE,
    legend = FALSE,
    las = 2,
    col = color_palette[top_nations]
  )
  mtext("Player nation", side = 1, line = 0.25)

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
# distribution of positions
plot_top <- 12

# retrieve positions
positions <- c()
pos_heights <- c()
pos_weights <- c()
pos_counts <- c()
for (p in 1:nrow(players)) {
  if (p %% 1000 == 0)
    tlog(2, "Processing entry ", p, "/", nrow(players))
  player_id <- players[p, "wikidataId"]

  # get position lists
  pos <- players[p, "positions"]
  if (!is.na(pos)) {
    player_positions <- trimws(strsplit(pos, split = ";")[[1]])

    # add to stat list
    positions <- c(positions, player_positions)
    pos_counts <- c(pos_counts, length(player_positions))
    pos_heights <- c(pos_heights, rep(players[p, "height"], length(player_positions)))
    pos_weights <- c(pos_weights, rep(players[p, "weight"], length(player_positions)))
  }
}
tlog("Number of distinct player nations: ", length(unique(positions)))
tlog("Average number of position by player: ", mean(pos_counts), " (", sd(pos_counts),")")

# loop over aggregation levels
for (plot_agg in 1:4) {
  # aggregate positions
  tmp <- aggregate_positions(positions = positions, granularity = plot_agg)
  positions2 <- tmp$positions
  top_positions <- tmp$top_positions

  # count values
  pos_tt <- table(positions2, useNA = "always")
  print(pos_tt)

  # export values as a csv file
  tab_file <- file.path(stats_folder, paste0("positions0_agg=", plot_agg, ".csv"))
  tab <- as.data.frame(pos_tt)
  colnames(tab) <- c("Position", "Count")
  write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

  # # focus on most frequent values
  # pos_tt0 <- sort(table(positions, useNA = "no"), decreasing = TRUE)
  # top_positions <- names(pos_tt0)[1:min(plot_top, length(pos_tt0))]

  # remove NAs
  pos_tt2 <- pos_tt[!is.na(names(pos_tt))]

  # add a new value for category others
  pos_tt2 <- c(pos_tt2, "Others" = sum(pos_tt2[!(names(pos_tt2) %in% top_positions)], na.rm = TRUE))
  top_positions <- c(top_positions, "Others")

  # set colors
  color_palette <- c(POSITION_COLORS, "Others" = "#919191")

  # generate barplot
  plot_file <- file.path(stats_folder, paste0("positions_agg=", plot_agg, ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
    par(mar = c(1.50, 2.50, 0.00, 0.00))  # control margins: B L T R
    
    heights <- pos_tt2[top_positions]
    # export values as a csv file
    tab_file <- file.path(stats_folder, paste0("positions_agg=", plot_agg, ".csv"))
    tab <- cbind("Position" = top_positions, "Count" = heights)
    write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

    # init plot
    bp <- barplot(
      height = heights,
      # xlab = "Player positions",
      ylab = "Frequency",
      names.arg = FALSE,
      legend = FALSE,
      #las = 2,
      col = color_palette[top_positions]
    )
    mtext("Player position", side = 1, line = 0.25)

    # decide bar text pos
    outside_text <- which(heights < 0.5 * max(heights, na.rm = TRUE))
    inside_text <- which(heights >= 0.5 * max(heights, na.rm = TRUE))

    # bar names on top
    if (length(outside_text) > 0) {
      text(bp[outside_text],
        heights[outside_text] + 0.025 * max(heights, na.rm = TRUE),
        labels = top_positions[outside_text],
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
        labels = top_positions[inside_text],
        col = text_color(color_palette[top_positions[inside_text]]),
        srt = 90,
        adj = c(1, 0.5),
        xpd = TRUE
      )
    }
  dev.off()
}




########################################################################
# distribution of heights

# retrieve values
heights <- players[, "height"]

# shortest player
shortest_height <- min(heights, na.rm = TRUE)
idx <- which(heights == shortest_height)
shortest_player_names <- players[idx, "fullName"]
tlog("Shortest height: ", paste0(shortest_height, collapse = ", "))
tlog("Players (", length(idx), "): ", paste0(shortest_player_names, collapse = ", "))
print(players[idx, ])

# tallest player
tallest_height <- max(heights, na.rm = TRUE)
idx <- which(heights == tallest_height)
tallest_player_names <- players[idx, "fullName"]
tlog("Tallest height: ", paste0(tallest_height, collapse = ", "))
tlog("Players (", length(idx), "): ", paste0(tallest_player_names, collapse = ", "))
print(players[idx, ])

# remove empty values
heights <- heights[!is.na(heights)]

plot_file <- file.path(stats_folder, paste0("heights", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(heights,
    main = NA,
    xlab = "Player height (cm)",
    col = "red",
    breaks = 30
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("heights", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")



# heights vs. position
for (plot_agg in 1:4) {
  # aggregate positions
  tmp <- aggregate_positions(positions = positions, granularity = plot_agg)
  positions2 <- tmp$positions
  top_positions <- tmp$top_positions

  # add category others
  positions2[!(positions2 %in% top_positions)] <- "Others"
  top_positions <- c(top_positions, "Others")

  # compute density by position
  densities <- list()
  xlim <- range(pos_heights, na.rm = TRUE)
  ylim <- c(1, -1)
  for (position in top_positions) {
    idx <- which(!is.na(pos_heights) & !is.na(positions2) & positions2 == position)
    if (length(idx) > 2) {
      densities[[position]] <- density(pos_heights[idx], na.rm = TRUE)
      ylim[1] <- min(c(ylim[1], densities[[position]]$y))
      ylim[2] <- max(c(ylim[2], densities[[position]]$y))
    }
  }

  # set colors
  color_palette <- c(POSITION_COLORS, "Others" = "#919191")

  # generate density plot
  plot_file <- file.path(stats_folder, paste0("height_vs_position_agg=", plot_agg, ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
    par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

    # init empty plot
    plot(NULL,
      xlim = xlim, ylim = ylim,
      xlab = "Player height (cm)",
      ylab = "Density"
    )
    # add series
    for (position in top_positions) {
      if (!all(is.na(densities[[position]]))) {
        lines(densities[[position]],
          col = color_palette[position],
          lwd = 2
        )
      }
    }
    # add legend
    legend(
      "topleft",
      legend = top_positions,
      # cex = 0.8,
      fill = color_palette[top_positions],
      bg = "#FFFFFFBB"
    )
  dev.off()
}




########################################################################
# distribution of weights

weights <- players[, "weight"]
weights <- weights[!is.na(weights)]

plot_file <- file.path(stats_folder, paste0("weights", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(weights,
    main = NA,
    xlab = "Player weight (kg)",
    col = "red",
    breaks = 30
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("weights", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")



# weights vs. position
for (plot_agg in 1:4) {
  # aggregate positions
  tmp <- aggregate_positions(positions = positions, granularity = plot_agg)
  positions2 <- tmp$positions
  top_positions <- tmp$top_positions

  # add category others
  positions2[!(positions2 %in% top_positions)] <- "Others"
  top_positions <- c(top_positions, "Others")

  # compute density by position
  densities <- list()
  xlim <- range(pos_weights, na.rm = TRUE)
  ylim <- c(1, -1)
  for (position in top_positions) {
    idx <- which(!is.na(pos_weights) & !is.na(positions2) & positions2 == position)
    if (length(idx) > 2) {
      densities[[position]] <- density(pos_weights[idx], na.rm = TRUE)
      ylim[1] <- min(c(ylim[1], densities[[position]]$y))
      ylim[2] <- max(c(ylim[2], densities[[position]]$y))
    }
  }

  # set colors
  color_palette <- c(POSITION_COLORS, "Others" = "#919191")

  # generate density plot
  plot_file <- file.path(stats_folder, paste0("weight_vs_position_agg=", plot_agg, ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
    par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R
    # init empty plot
    plot(NULL,
      xlim = xlim, ylim = ylim,
      xlab = "Player weight (kg)",
      ylab = "Density"
    )
    # add series
    for (position in top_positions) {
      if (!all(is.na(densities[[position]]))) {
        lines(densities[[position]],
          col = color_palette[position],
          lwd = 2
        )
      }
    }
    # add legend
    legend(
      "topleft",
      legend = top_positions,
      # cex = 0.8,
      fill = color_palette[top_positions],
      bg = "#FFFFFFBB"
    )
  dev.off()
}




########################################################################
# distribution of stint number and duration, numbers of matches played and points scored

# retrieve stint stats
stint_nbr <- c()
stint_dur <- c()
points_scored_mean <- c()
matches_played_mean <- c()
points_scored_nbr <- c()
matches_played_nbr <- c()
for (p in 1:nrow(players)) {
  if (p %% 1000 == 0)
    tlog(2, "Processing entry ", p, "/", nrow(players))
  player_id <- players[p, "wikidataId"]

  player_stints <- stints[stints[, "playerId"] == player_id, ]
  stint_nbr <- c(stint_nbr, nrow(player_stints))
  player_durations <- player_stints[, "endYear"] - player_stints[, "startYear"]
  stint_dur <- c(stint_dur, mean(player_durations, na.rm = TRUE))

  total_points <- sum(player_stints[, "pointsScored"], na.rm = TRUE)
  points_scored_nbr <- c(points_scored_nbr, total_points)
  total_matches <- sum(player_stints[, "matchesPlayed"], na.rm = TRUE)
  matches_played_nbr <- c(matches_played_nbr, total_matches)

  idx <- which(!is.na(player_stints[, "pointsScored"]) & !is.na(player_durations))
  tmp_scored <- sum(player_stints[idx, "pointsScored"], na.rm = TRUE) / sum(player_durations[idx], na.rm = TRUE)
  if (is.nan(tmp_scored) || is.infinite(tmp_scored))
    tmp_scored <- NA
  points_scored_mean <- c(points_scored_mean, tmp_scored)
  #
  idx <- which(!is.na(player_stints[, "matchesPlayed"]) & !is.na(player_durations))
  tmp_played <- sum(player_stints[idx, "matchesPlayed"], na.rm = TRUE) / sum(player_durations[idx], na.rm = TRUE)
  if (is.nan(tmp_played) || is.infinite(tmp_played))
    tmp_played <- NA
  matches_played_mean <- c(matches_played_mean, tmp_played)
}

# display basic stats
tlog("Average stint number by player: ", mean(stint_nbr, na.rm = TRUE), " (sd: ", sd(stint_nbr, na.rm = TRUE),")")
max_val <- max(stint_nbr, na.rm = TRUE)
idx <- which(stint_nbr == max_val)
max_names <- players[idx, "fullName"]
tlog(2, "Max value: ", max_val)
tlog(2, "Max players (", length(idx), "): ", paste0(max_names, collapse = ", "))
print(players[idx, ])
#
tlog("Average mean stint duration by player: ", mean(stint_dur, na.rm = TRUE), " (sd: ", sd(stint_dur, na.rm = TRUE),")")
max_val <- max(stint_dur, na.rm = TRUE)
idx <- which(stint_dur == max_val)
max_names <- players[idx, "fullName"]
tlog(2, "Max value: ", max_val)
tlog(2, "Max players (", length(idx), "): ", paste0(max_names, collapse = ", "))
print(players[idx, ])
#
tlog("Average yearly number of points scored by player: ", mean(points_scored_mean, na.rm = TRUE), " (sd: ", sd(points_scored_mean, na.rm = TRUE),")")
max_val <- max(points_scored_mean, na.rm = TRUE)
idx <- which(points_scored_mean == max_val)
max_names <- players[idx, "fullName"]
tlog(2, "Max value: ", max_val)
tlog(2, "Max players (", length(idx), "): ", paste0(max_names, collapse = ", "))
print(players[idx, ])
#
tlog("Average yearly number of matches played by player: ", mean(matches_played_mean, na.rm = TRUE), " (sd: ", sd(matches_played_mean, na.rm = TRUE),")")
max_val <- max(matches_played_mean, na.rm = TRUE)
idx <- which(matches_played_mean == max_val)
max_names <- players[idx, "fullName"]
tlog(2, "Max value: ", max_val)
tlog(2, "Max players (", length(idx), "): ", paste0(max_names, collapse = ", "))
print(players[idx, ])
#
tlog("Average career number of points scored by player: ", mean(points_scored_nbr, na.rm = TRUE), " (sd: ", sd(points_scored_nbr, na.rm = TRUE),")")
max_val <- max(points_scored_nbr, na.rm = TRUE)
idx <- which(points_scored_nbr == max_val)
max_names <- players[idx, "fullName"]
tlog(2, "Max value: ", max_val)
tlog(2, "Max players (", length(idx), "): ", paste0(max_names, collapse = ", "))
print(players[idx, ])
#
tlog("Average career number of matches played by player: ", mean(matches_played_nbr, na.rm = TRUE), " (sd: ", sd(matches_played_nbr, na.rm = TRUE),")")
max_val <- max(matches_played_nbr, na.rm = TRUE)
idx <- which(matches_played_nbr == max_val)
max_names <- players[idx, "fullName"]
tlog(2, "Max value: ", max_val)
tlog(2, "Max players (", length(idx), "): ", paste0(max_names, collapse = ", "))
print(players[idx, ])

# export stint numbers as a csv file
tt <- table(stint_nbr, useNA = "always")
tab_file <- file.path(stats_folder, paste0("stint-numbers0", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("StintNumber", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")
# export career points as a csv file
tt <- table(points_scored_nbr, useNA = "always")
tab_file <- file.path(stats_folder, paste0("points-scored-career0", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("PointsScored", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")
# export career matches as a csv file
tt <- table(matches_played_nbr, useNA = "always")
tab_file <- file.path(stats_folder, paste0("matches-played-career0", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("MatchesPlayed", "Count")
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")


# plot stint numbers
plot_file <- file.path(stats_folder, paste0("stint-numbers", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(stint_nbr,
    main = NA,
    xlab = "Number of stints by player",
    col = "red"
  )
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
    xlab = "Average mean stint duration by player",
    col = "red",
    # log = "y"
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
    xlab = "Yearly points scored by player",
    col = "red",
    # log = "y"
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("points-scored-yearly", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")


# plot yearly matches played
plot_file <- file.path(stats_folder, paste0("matches-played-yearly", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(matches_played_mean,
    main = NA,
    xlab = "Yearly matches played by player",
    col = "red",
    # log = "y"
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("matches-played-yearly", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")


# plot career points scored
plot_file <- file.path(stats_folder, paste0("points-scored-career", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(points_scored_nbr,
    main = NA,
    xlab = "Career points scored by player",
    col = "red",
    # log = "y"
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("points-scored-career", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")


# plot career matches played
plot_file <- file.path(stats_folder, paste0("matches-played-career", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hh <- hist(matches_played_nbr,
    main = NA,
    xlab = "Career matches played by player",
    col = "red",
    # log = "y"
  )
dev.off()

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("matches-played-career", ".csv"))
nms <- apply(cbind(hh$breaks[1:(length(hh$breaks)-1)], hh$breaks[2:length(hh$breaks)]), 1, function(row) paste0("[", row[1], ", ", row[2], "["))
tab <- cbind("Intervals" = nms, "Counts" = hh$counts, "Density" = hh$density)
write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# distribution of data sources
source_names <- c("DBPD", "enWP", "esWP", "frWP", "itWP", "jaWP", "WD")
map <- c("dbpediaId" = "DBPD", "wikipediaEn" = "enWP", "wikipediaEs" = "esWP", "wikipediaFr" = "frWP", "wikipediaIt" = "itWP", "wikipediaJa" = "jaWP", "wikidataId" = "WD")

# retrieve sources
data_sources <- c()
data_sources_df <- matrix(0, nrow = nrow(players), ncol = length(source_names), dimnames = list(c(), source_names))
for (p in 1:nrow(players)) {
  if (p %% 1000 == 0)
    tlog(2, "Processing entry ", p, "/", nrow(players))
  player_id <- players[p, "wikidataId"]

  player_data_sources <- c()
  for (col in names(map)) {
    if (!is.na(players[p, col])) {
      player_data_sources <- c(player_data_sources, map[col])
      data_sources_df[p, map[col]] <- 1
    }
  }

  # add to stat list
  data_sources <- c(data_sources, player_data_sources)
}
data_source_nbr <- apply(data_sources_df, 1, sum)
tlog("Number of distinct values: ", length(unique(data_sources)))
tlog("Average number of sources by player: ", mean(data_source_nbr), " (", sd(data_source_nbr),")")

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
    # xlab = "Player data source",
    ylab = "Frequency",
    names.arg = FALSE,
    legend = FALSE,
    #las = 2,
    col = color_palette[top_data_sources]
  )
  mtext("Player data source", side = 1, line = 0.25)

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
for (p in 1:nrow(players)) {
  if (p %% 1000 == 0)
    tlog(2, "Processing entry ", p, "/", nrow(players))
  player_id <- players[p, "wikidataId"]

  # get nations
  sport_nations <- players[p, "sportNations"]
  if (!is.na(sport_nations))
    player_nations <- trimws(strsplit(sport_nations, split = ";")[[1]])
  else {
    citizenships <- players[p, "citizenships"]
    player_nations <- trimws(strsplit(citizenships, split = ";")[[1]])
  }

  # get data sources
  player_data_sources <- c()
  for (col in names(map)) {
    if (!is.na(players[p, col])) {
      player_data_sources <- c(player_data_sources, map[col])
    }
  }

  # add to stat lists
  cr_nations <- c(cr_nations, rep(player_nations, each = length(player_data_sources)))
  cr_data_sources <- c(cr_data_sources, rep(player_data_sources, length(player_nations)))
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
nations <- sapply(1:nrow(players), function(p) if (is.na(players[p, "sportNations"])) players[p, "citizenships"] else players[p, "sportNations"])
players <- cbind(players, "nations" = nations)

field_groups <- list(
  "perso-fields" = c("birthDate", "birthPlace", "deathDate", "deathPlace", "nations"),
  "rugby-fields" = c("positions", "careerStartYear", "careerEndYear", "weight", "height"),
  "id-fields" = c("espnScrumId", "allRugbyId", "googleKnowlId", "itsRugbyId", "rugbyDatabaseId", "dbpediaId"),
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
  props <- sapply(fields, function(field) length(which(!is.na(players[, field])))) / nrow(players)
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
      #xlab = "Player fields",
      ylab = "Completeness (%)",
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
