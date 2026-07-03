########################################################################
# Generates various plots regarding the evolution of the players' size,
# in terms of both weight and height. The size is also shown depending
# on the position.
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/stats/evol_player-size.R")
########################################################################
source("src/common/logging.R")
source("src/common/colors.R")
source("src/common/norm_positions.R")




########################################################################
# start logging
start.rec.log("EvolPlayerSize")




########################################################################
# parameters

mode <- "active-years"  # "start-year" or "active-years"
mode_labels <- c("start-year" = "career start year", "active-years" = "active years")
mode_xlabels <- c("start-year" = "Career start year", "active-years" = "Active year")



########################################################################
# create output folder
stats_folder <- file.path("data", "stats", paste0("evol_player-size_vs_", mode))
dir.create(stats_folder, showWarnings = FALSE, recursive = TRUE)

# load tables
source("src/stats/load_all_tables.R")




########################################################################
# identify start years

ref_years <- c()
ref_players <- c()
if (mode == "start-year") {
  for (p in 1:nrow(players)) {
    player_id <- players[p, "wikidataId"]
    if (p %% 1000 == 0)
      tlog(2, "Processing player ", player_id, "(", p, "/", nrow(players), ")")

    career_start <- players[p, "careerStartYear"]

    player_stints <- stints[stints[, "playerId"] == player_id, ]
    first_stint <- suppressWarnings(min(c(player_stints[, "startYear"], player_stints[, "endYear"]), na.rm = TRUE))

    if (is.na(first_stint) || is.infinite(first_stint))
      ref_years <- c(ref_years, career_start)
    else
      ref_years <- c(ref_years, first_stint)
    ref_players <- c(ref_players, player_id)
  }
# identify active years
} else if (mode == "active-years") {
  for (p in 1:nrow(players)) {
    player_id <- players[p, "wikidataId"]
    if (p %% 1000 == 0)
      tlog(2, "Processing player ", player_id, "(", p, "/", nrow(players), ")")

    player_stints <- stints[stints[, "playerId"] == player_id, ]
    active_years <- c()
    if (nrow(player_stints) > 0) {
      for (s in 1:nrow(player_stints)) {
        stint_start <- player_stints[s, "startYear"]
        stint_end <- player_stints[s, "endYear"]
        if (is.na(stint_start)) {
          if (!is.na(stint_end))
            active_years <- c(active_years, stint_end)
        } else {
          if (is.na(stint_end))
            active_years <- c(active_years, stint_start)
          else
            active_years <- c(active_years, stint_start:stint_end)
        }
      }
      active_years <- sort(unique(active_years))
    }

    if (length(active_years) > 0) {
      ref_years <- c(ref_years, active_years)
      ref_players <- c(ref_players, rep(player_id, length(active_years)))
    }
  }
} else {
  stop("Unknown mode: ", mode)
}




########################################################################
# weight and height of player by year

# get player height and weight
heights <- c()
weights <- c()
size_years <- c()
size_players <- c()
for (i in 1:length(ref_players)) {
  if (i %% 1000 == 0)
    tlog(2, "Processing entry ", i, "/", length(ref_players))
  player_id <- ref_players[i]
  p <- which(players[, "wikidataId"] == player_id)

  # get size
  height <- players[p, "height"]
  weight <- players[p, "weight"]

  # add to stat lists
  heights <- c(heights, height)
  weights <- c(weights, weight)
  size_years <- c(size_years, ref_years[p])
  size_players <- c(size_players, player_id)
}



# compute stats by year
av_heights <- c()
av_weights <- c()
sd_heights <- c()
sd_weights <- c()
un_years <- sort(unique(ref_years))
for (year in un_years) {
  # heights
  idx <- which(size_years == year & !is.na(heights))
  if (length(idx) > 0) {
    av_heights <- c(av_heights, mean(heights[idx]))
    sd_heights <- c(av_heights, sd(heights[idx]))
  } else {
    av_heights <- c(av_heights, NA)
    sd_heights <- c(av_heights, NA)
  }

  # weights
  idx <- which(size_years == year & !is.na(weights))
  if (length(idx) > 0) {
    av_weights <- c(av_weights, mean(weights[idx]))
    sd_weights <- c(sd_weights, sd(weights[idx]))
  } else {
    av_weights <- c(av_weights, NA)
    sd_weights <- c(av_weights, NA)
  }
}



# put in lists
vals <- list(height = heights, weight = weights)
av_vals <- list(height = av_heights, weight = av_weights)
sd_vals <- list(height = sd_heights, weight = sd_weights)
size_labels <- c(height = "Player average height (cm)", weight = "Player average weight (kg)")

# produce plot files
for (size_name in c("height", "weight")) {
  plot_file <- file.path(stats_folder, paste0(size_name, "_all", ".pdf"))
  tlog("Producing plot file: ", plot_file)

  pdf(plot_file, width = 14, height = 7)
    plot(
      x = size_years, y = vals[[size_name]],
      xlab = mode_xlabels[mode], ylab = size_labels[size_name],
      log = if (plot_log) "y" else "",
      col = "#ff00000b", pch = 19,
      main = paste0("Average player ", size_name," as a function of ", mode_labels[mode])
    )
    lines(
      x = un_years, y = av_vals[[size_name]],
      col = "black",
      lwd = 2
    )
  dev.off()
}




########################################################################
# weight and height of player by year and position

# note: multiple positions are possible, so the same player can be counted several times

# get country info
positions <- c()
position_years <- c()
position_heights <- c()
position_weights <- c()
for (i in 1:length(size_players)) {
  if (i %% 1000 == 0)
    tlog(2, "Processing entry ", i, "/", length(size_players))
  player_id <- size_players[i]
  p <- which(players[, "wikidataId"] == player_id)

  # get position lists
  pos <- players[p, "positions"]
  if (!is.na(pos)) {
    player_positions <- trimws(strsplit(pos, split = ";")[[1]])

    # add to stat list
    for (player_position in player_positions) {
      positions <- c(positions, player_position)
      position_years <- c(position_years, ref_years[p])
      position_heights <- c(position_heights, heights[p])
      position_weights <- c(position_weights, heights[p])
    }
  } else {
    positions <- c(positions, NA)
    position_years <- c(position_years, ref_years[p])
    position_heights <- c(position_heights, heights[p])
    position_weights <- c(position_weights, heights[p])
  }
}



# produce plot files
for (plot_log in c(FALSE, TRUE)) {
  for (plot_smoothed in c(FALSE, TRUE)) {
    for (plot_agg in 1:4) {
      # aggregate positions depending on current granularity
      tmp <- aggregate_positions(positions = positions, granularity = plot_agg)
      positions2 <- tmp$positions
      top_positions <- tmp$top_positions

      # add category others
      positions2[!(positions2 %in% top_positions)] <- "Others"
      top_positions <- c(top_positions, "Others")

      # compute stats by year and position
      av_heights <- matrix(nrow = length(top_positions), ncol = length(un_years), dimnames = list(top_positions, un_years))
      av_weights <- matrix(nrow = length(top_positions), ncol = length(un_years), dimnames = list(top_positions, un_years))
      sd_heights <- matrix(nrow = length(top_positions), ncol = length(un_years), dimnames = list(top_positions, un_years))
      sd_weights <- matrix(nrow = length(top_positions), ncol = length(un_years), dimnames = list(top_positions, un_years))
      for (year in un_years) {
        for (position in top_positions) {
          # heights
          idx <- which(position_years == year & !is.na(position_heights) & !is.na(positions2) & positions2 == position)
          if (length(idx) > 0) {
            av_heights[position, year] <- mean(position_heights[idx])
            sd_heights[position, year] <- sd(position_heights[idx])
          }

          # weights
          idx <- which(position_years == year & !is.na(position_weights) & !is.na(positions2) & positions2 == position)
          if (length(idx) > 0) {
            av_weights[position, year] <- mean(position_weights[idx])
            sd_weights[position, year] <- sd(position_weights[idx])
          }
        }
      }

      # replace zeros by NA to avoid log(0) in the plot
      if (plot_log) {
        av_heights[av_heights == 0] <- NA
        sd_heights[av_heights == 0] <- NA
        av_weights[av_weights == 0] <- NA
        sd_weights[av_weights == 0] <- NA
      }

      # put values in lists
      vals <- list(height = position_heights, weight = position_weights)
      av_vals <- list(height = av_heights, weight = av_weights)
      sd_vals <- list(height = sd_heights, weight = sd_weights)

      # set colors
      color_palette <- c(POSITION_COLORS, "Others" = "#919191")

      for (size_name in c("height", "weight")) {
        plot_file <- file.path(stats_folder, paste0(size_name, "_positions_agg=", plot_agg, "_smoothed=", plot_smoothed, "_ylog=", plot_log, ".pdf"))
        tlog("Producing plot file: ", plot_file)

        pdf(plot_file, width = 14, height = 7)
          # init plot
          plot(
            xlab = mode_xlabels[mode], ylab = size_labels[size_name],
            log = if (plot_log) "y" else "",
            main = paste0("Average player ", size_name," as a function of ", mode_labels[mode]),
            xlim = c(min(as.integer(un_years)), max(as.integer(colnames(un_years)))),
            ylim = c(min(vals[size_name], na.rm = TRUE), max(vals[size_name], na.rm = TRUE))
          )
          # add series
          for (position in top_positions) {
            # plot points
            idx <- which(positions == position)
            x <- as.integer(position_years[idx])
            y <- as.integer(vals[[size_name]][idx])
            plot(
              x = x, y = y, pch = 19,
              col = make.color.transparent(color = color_palette[position], transparency = 80)
            )
            # plot average
            x <- as.integer(un_years)
            y <- as.integer(av_vals[[size_name]][position, ])
            if (plot_smoothed)
              fit <- loess(y ~ x, na.action = na.exclude, span = 0.1)
            lines(
              x = x,
              y = if (plot_smoothed) predict(fit) else y,
              col = color_palette[position],
              lwd = 2
            )
          }
          # add legend
          legend(
            "topleft",
            legend = top_positions,
            # cex = 0.8,
            fill = color_palette[top_positions]
          )
        dev.off()
      }
    }
  }
}




########################################################################
# stop logging
end.rec.log()
