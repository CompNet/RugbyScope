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

# compute stats
birth_year <- players[, "birthDate"] %>% as.Date() %>% format("%Y") %>% as.integer()
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

# compute stats
death_year <- players[, "deathDate"] %>% as.Date() %>% format("%Y") %>% as.integer()
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
# distribution of countries
plot_top <- 12

# get country info
countries <- c()
country_nbr <- c()
for (p in 1:nrow(players)) {
  if (p %% 1000 == 0)
    tlog(2, "Processing player ", p, "/", nrow(players))
  player_id <- players[p, "wikidataId"]

  # get country list
  sport_countries <- players[p, "sportCountries"]
  if (!is.na(sport_countries))
    player_countries <- trimws(strsplit(sport_countries, split = ";")[[1]])
  else {
    citizenships <- players[p, "citizenships"]
    player_countries <- trimws(strsplit(citizenships, split = ";")[[1]])
  }

  # add to stat list
  countries <- c(countries, player_countries)
  country_nbr <- c(country_nbr, length(player_countries))
}
tlog("Number of distinct player countries: ", length(unique(countries)))
tlog("Average number of countries by team: ", mean(country_nbr, na.rm = TRUE), " (", sd(country_nbr, na.rm = TRUE),")")

# count values
countr_tt <- table(countries, useNA = "always")
print(countr_tt)
# export values as a csv file
tab_file <- file.path(stats_folder, paste0("countries0", ".csv"))
tab <- as.data.frame(countr_tt)
colnames(tab) <- c("Country", "Count")
write.csv(as.data.frame(tab), tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# focus on most frequent values
countr_tt0 <- sort(table(countries, useNA = "no"), decreasing = TRUE)
top_countries <- names(countr_tt0)[1:plot_top]

# remove NAs
countr_tt2 <- countr_tt[!is.na(names(countr_tt))]

# add a new value for category others
countr_tt2 <- c(countr_tt2, "Others" = sum(countr_tt2[!(names(countr_tt2) %in% top_countries)], na.rm = TRUE))
top_countries <- c(top_countries, "Others")

# set colors
color_palette <- c(COUNTRY_COLORS, "Others" = "#919191")

# generate barplot
plot_file <- file.path(stats_folder, paste0("countries", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(3.0, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(1.50, 4.00, 0.25, 0.00))  # control margins: B L T R
  heights <- countr_tt2[top_countries]

  # init plot
  bp <- barplot(
    height = heights,
    #xlab = "Player Countries",
    ylab = "Frequency",
    names.arg = FALSE,
    legend = FALSE,
    las = 2,
    col = color_palette[top_countries]
  )
  mtext("Player country", side = 1, line = 0.25)

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
tab_file <- file.path(stats_folder, paste0("countries", ".csv"))
tab <- cbind("Country" = top_countries, "Count" = heights)
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
tlog("Number of distinct player countries: ", length(unique(positions)))
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

heights <- players[, "height"]
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
# distribution of stint number and duration

# retrieve stint stats
stint_nbr <- c()
stint_dur <- c()
for (p in 1:nrow(players)) {
  if (p %% 1000 == 0)
    tlog(2, "Processing entry ", p, "/", nrow(players))
  player_id <- players[p, "wikidataId"]

  player_stints <- stints[stints[, "playerId"] == player_id, ]
  stint_nbr <- c(stint_nbr, nrow(player_stints))
  stint_dur <- c(stint_dur, mean(player_stints[, "endYear"] - player_stints[, "startYear"], na.rm = TRUE))
}
tt <- table(stint_nbr, useNA = "always")
tlog("Average stint number by player: ", mean(stint_nbr, na.rm = TRUE), "(sd: ", sd(stint_nbr, na.rm = TRUE),")")
tlog("Average mean stint duration by player: ", mean(stint_dur, na.rm = TRUE), "(sd: ", sd(stint_dur, na.rm = TRUE),")")

# export values as a csv file
tab_file <- file.path(stats_folder, paste0("stint-numbers0", ".csv"))
tab <- as.data.frame(tt)
colnames(tab) <- c("StintNumber", "Count")
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




########################################################################
# distribution of sources vs. countries

# retrieve sources
cr_data_sources <- c()
cr_countries <- c()
for (p in 1:nrow(players)) {
  if (p %% 1000 == 0)
    tlog(2, "Processing entry ", p, "/", nrow(players))
  player_id <- players[p, "wikidataId"]

  # get countries
  sport_countries <- players[p, "sportCountries"]
  if (!is.na(sport_countries))
    player_countries <- trimws(strsplit(sport_countries, split = ";")[[1]])
  else {
    citizenships <- players[p, "citizenships"]
    player_countries <- trimws(strsplit(citizenships, split = ";")[[1]])
  }

  # get data sources
  player_data_sources <- c()
  for (col in names(map)) {
    if (!is.na(players[p, col])) {
      player_data_sources <- c(player_data_sources, map[col])
    }
  }

  # add to stat lists
  cr_countries <- c(cr_countries, rep(player_countries, each = length(player_data_sources)))
  cr_data_sources <- c(cr_data_sources, rep(player_data_sources, length(player_countries)))
}

# count values
cr_tt <- table(cr_data_sources, cr_countries, useNA = "always")
print(cr_tt)
# export values as a csv file
tab_file <- file.path(stats_folder, paste0("data-sources_vs_countries_contingency0", "-nbr", ".csv"))
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
      cr_tt2[, top_country] <- 100 * cr_tt2[, top_country] / countr_tt2[top_country]
  }
  plot_file <- file.path(stats_folder, paste0("data-sources_vs_countries_contingency", if (i == 1) "-nbr" else "-prop", ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    corrplot(cr_tt2[, top_countries],
      is.corr = FALSE, #diag = FALSE,
      method = "color",
      number.digits = 0,
      addCoef.col = "white",
      col = viridis(100), #col.lim = c(0, 1), 
      tl.col = "black"
    )
  dev.off()

  # export values as a csv file
  tab_file <- file.path(stats_folder, paste0("data-sources_vs_countries_contingency", if (i == 1) "-nbr" else "-prop", ".csv"))
  write.csv(cr_tt2[, top_countries], tab_file, row.names = TRUE, fileEncoding = "UTF-8")
}




########################################################################
# overall completeness stats
countries <- sapply(1:nrow(players), function(p) if (is.na(players[p, "sportCountries"])) players[p, "citizenships"] else players[p, "sportCountries"])
players <- cbind(players, "countries" = countries)

field_groups <- list(
  "perso-fields" = c("birthDate", "birthPlace", "deathDate", "deathPlace", "countries"),
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
  vals <- sapply(fields, function(field) length(which(!is.na(players[, field])))) * 100 / nrow(players)

  # export values as a csv file
  if (g > length(field_groups)) {
    tab_file <- file.path(stats_folder, paste0("completeness_", group_name, ".csv"))
    tab <- cbind("Field" = fields, "CompletenessRate" = vals)
    write.csv(tab, tab_file, row.names = FALSE, fileEncoding = "UTF-8")
  }

  colors <- pal[pmax(1, pmin(100, round(vals)))]

  # generate barplot
  plot_file <- file.path(stats_folder, paste0("completeness_", group_name, ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    par(mgp = c(2.0, 0.5, 0))             # reduce space between axis title / axis values and axis line
    par(mar = c(9.00, 3.00, 0.50, 0.00))  # control margins: B L T R

    # init barplot
    bp <- barplot(
      height = vals,
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
      y = if (g > length(field_groups)) vals - 0.06 * max(vals) else vals - 0.1 * max(vals),
      labels = paste0(round(vals), "%", sep = ""),
      pos = 3, col = sapply(vals, function(val) if (val < 75) "white" else "black"),
      cex = if (g > length(field_groups)) 0.75 else 1.5, font = 2,
      srt = if (g > length(field_groups)) 90 else 0
    )
  dev.off()
}




########################################################################
# stop logging
end.rec.log()
