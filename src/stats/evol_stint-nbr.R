########################################################################
# Generates various plots regarding the evolution of the number of stints
# that were founded in a given year, or that were active at a given year:
# - overall
# - by nation
# - by type
# - by source
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/stats/evol_stint-nbr.R")
########################################################################
source("src/common/logging.R")
source("src/common/colors.R")




########################################################################
# start logging
start.rec.log("EvolStintNbr")




########################################################################
# parameters

mode <- "active-years"  # "start-year" or "active-years"
mode_labels <- c("start-year" = "career start year", "active-years" = "active year")
mode_xlabels <- c("start-year" = "Career start year", "active-years" = "Active year")




########################################################################
# create output folder
stats_folder <- file.path("data", "stats", paste0("evol_stint-nbr_vs_", mode))
dir.create(stats_folder, showWarnings = FALSE, recursive = TRUE)

# load tables
source("src/stats/load_all_tables.R")




########################################################################
# total number of stints by start year

# identify start years
ref_years <- c()
ref_stints <- c()
if (mode == "start-year") {
  for (s in 1:nrow(stints)) {
    if (s %% 1000 == 0)
      tlog(2, "Processing stint ", s, "/", nrow(stints))

    # get stint year
    start_year <- stints[s, "startYear"]
    end_year <- stints[s, "endYear"]
    if (is.na(start_year))
      year <- end_year
    else if (is.na(end_year))
      year <- start_year
    else
      year <- min(start_year, end_year)

    # add to list
    ref_years <- c(ref_years, year)
    ref_stints <- c(ref_stints, s)
  }
# identify active years
} else if (mode == "active-years") {
  for (s in 1:nrow(stints)) {
    if (s %% 1000 == 0)
      tlog(2, "Processing stint ", s, "/", nrow(stints))

    active_years <- c()
    start_year <-stints[s, "startYear"]
    end_year <- stints[s, "endYear"]
    if (is.na(start_year)) {
      if (!is.na(end_year))
        active_years <- c(active_years, end_year)
    } else {
      if (is.na(end_year))
        active_years <- c(active_years, start_year)
      else
        active_years <- c(active_years, start_year:end_year)
    }
    active_years <- sort(unique(active_years))

    if (length(active_years) > 0) {
      ref_years <- c(ref_years, active_years)
      ref_stints <- c(ref_stints, rep(s, length(active_years)))
    }
  }
} else {
  stop("Unknown mode: ", mode)
}
all_tt <- table(ref_years, useNA = "always")
print(all_tt)



# produce plot file
for (plot_log in c(FALSE, TRUE)) {
  for (plot_smoothed in c(FALSE, TRUE)) {
    plot_file <- file.path(stats_folder, paste0("all_smoothed=", plot_smoothed, "_ylog=", plot_log, ".pdf"))
    tlog("Producing plot file: ", plot_file)

    all_tt2 <- all_tt[!is.na(names(all_tt))]

    pdf(plot_file, width = 14, height = 7)
      par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
      par(mar = c(3.00, 2.75, 1.75, 0.10))  # control margins: B L T R
      x <- as.integer(names(all_tt2))
      y <- as.integer(all_tt2)
      if (plot_smoothed)
        fit <- loess(y ~ x, na.action = na.exclude, span = 0.05)
      # init plot
      plot(NULL,
        xlim = range(x, na.rm = TRUE),
        ylim = range(y, na.rm = TRUE),
        xlab = mode_xlabels[mode], ylab = "Number of stints",
        #main = paste0("Number of stints as a function of ", mode_labels[mode]),
        log = if (plot_log) "y" else "",
        type = "l", col = "red", lwd = 2
      )
      # add vertical lines
      abline(v = "1871", col = "black")
      abline(v = "1895", col = "black")
      u3 <- if (plot_log) 10^par("usr")[3] else par("usr")[3]
      u4 <- if (plot_log) 10^par("usr")[4] else par("usr")[4]
      rect(1914, u3, 1918, u4, border = NA, col = "#EEEEEE")
      # abline(v = "1914", col = "black")
      # abline(v = "1918", col = "black")
      rect(1939, u3, 1945, u4, border = NA, col = "#EEEEEE")
      # abline(v = "1939", col = "black")
      # abline(v = "1945", col = "black")
      abline(v = "1995", col = "black")
      for (year in seq(1987, 2023, by = 4))
        abline(v = year, col = "black", lty = 3)
      axis(3, at = c(1871, 1895, 1916, 1942, 1995), labels = c("RFU", "Schism", "WW1", "WW2", "Professionalism"))
      # add lines
      lines(
        x = x,
        y = if (plot_smoothed) predict(fit) else y,
        type = "l", col = "red", lwd = 2
      )
    dev.off()
  }
}




########################################################################
# number of stints by year by player nation

# note: some players are related to multiple nations, so the same stint can be counted several times

# get nation info
player_nations <- c()
player_nation_years <- c()
for (i in 1:length(ref_stints)) {
  if (i %% 1000 == 0)
    tlog(2, "Processing entry ", i, "/", length(ref_stints))

  # retrieve player id
  player_id <- stints[ref_stints[i], "playerId"]
  p <- which(players[, "wikidataId"] == player_id)

  # get player nation list
  sport_nations <- players[p, "sportNations"]
  if (!is.na(sport_nations))
    tmp_nations <- trimws(strsplit(sport_nations, split = ";")[[1]])
  else {
    citizenships <- players[p, "citizenships"]
    tmp_nations <- trimws(strsplit(citizenships, split = ";")[[1]])
  }

  # add to lists
  player_nations <- c(player_nations, tmp_nations)
  player_nation_years <- c(player_nation_years, rep(ref_years[i], length(tmp_nations)))
}
nat_tt <- table(player_nations, player_nation_years, useNA = "always")
print(nat_tt)



# produce plot files
for (plot_log in c(FALSE, TRUE)) {
  for (plot_smoothed in c(FALSE, TRUE)) {
    for (plot_top in c(5, 8)) {
      plot_file <- file.path(stats_folder, paste0("player-nations_top=", plot_top, "_smoothed=", plot_smoothed, "_ylog=", plot_log, ".pdf"))
      tlog("Producing plot file: ", plot_file)

      # overal values
      nat_tt0 <- sort(table(player_nations, useNA = "no"), decreasing = TRUE)
      top_nations <- names(nat_tt0)[1:plot_top]

      # set colors
      color_palette <- c(NATION_COLORS, "Others" = "#919191")

      nat_tt2 <- nat_tt[, !is.na(colnames(nat_tt))]
      # add a new line for the rest of the nations
      nat_tt2 <- rbind(nat_tt2, "Others" = colSums(nat_tt2[!(rownames(nat_tt2) %in% top_nations), , drop = FALSE], na.rm = TRUE))
      top_nations <- c(top_nations, "Others")
      # replace zeros by NA to avoid log(0) in the plot
      if (plot_log)
        nat_tt2[nat_tt2 == 0] <- 1

      pdf(plot_file, width = 14, height = 7)
        par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
        par(mar = c(3.00, 2.75, 1.75, 0.10))  # control margins: B L T R
        # init plot
        plot(NULL,
          xlab = mode_xlabels[mode], ylab = "Number of stints",
          log = if (plot_log) "y" else "",
          #main = paste0("Number of stints as a function of ", mode_labels[mode]),
          xlim = c(min(as.integer(colnames(nat_tt2))), max(as.integer(colnames(nat_tt2)))),
          ylim = c(min(nat_tt2, na.rm = TRUE), max(nat_tt2, na.rm = TRUE))
        )
         # add vertical lines
        abline(v = "1871", col = "black")
        abline(v = "1895", col = "black")
        u3 <- if (plot_log) 10^par("usr")[3] else par("usr")[3]
        u4 <- if (plot_log) 10^par("usr")[4] else par("usr")[4]
        rect(1914, u3, 1918, u4, border = NA, col = "#EEEEEE")
        # abline(v = "1914", col = "black")
        # abline(v = "1918", col = "black")
        rect(1939, u3, 1945, u4, border = NA, col = "#EEEEEE")
        # abline(v = "1939", col = "black")
        # abline(v = "1945", col = "black")
        abline(v = "1995", col = "black")
        for (y in seq(1987, 2023, by = 4))
          abline(v = y, col = "black", lty = 3)
        axis(3, at = c(1871, 1895, 1916, 1942, 1995), labels = c("RFU", "Schism", "WW1", "WW2", "Professionalism"))
        # add series
        for (nation in top_nations) {
          x <- as.integer(colnames(nat_tt2))
          y <- as.integer(nat_tt2[nation, ])
          if (plot_smoothed)
              fit <- loess(y ~ x, na.action = na.exclude, span = 0.1)
          lines(
            x = x,
            y = if (plot_smoothed) predict(fit) else y,
            col = color_palette[nation],
            lwd = 2
          )
        }
        # add legend
        legend(
          "topleft",
          legend = top_nations,
          # cex = 0.8,
          fill = color_palette[top_nations],
          bg = "#FFFFFFBB"
        )
      dev.off()
    }
  }
}




########################################################################
# number of stints by year by team nation

# note: some teams are related to multiple nations, so the same stint can be counted several times

# get nation info
team_nations <- c()
team_nation_years <- c()
for (i in 1:length(ref_stints)) {
  if (i %% 1000 == 0)
    tlog(2, "Processing entry ", i, "/", length(ref_stints))

  # retrieve team id
  team_id <- stints[ref_stints[i], "teamRsId"]
  t <- which(teams[, "rugbyscopeId"] == team_id)

  # get nation list
  sport_nations <- teams[t, "nations"]
  if (!is.na(sport_nations)) {
    tmp_nations <- trimws(strsplit(sport_nations, split = ";")[[1]])
  
    # add to lists
    team_nations <- c(team_nations, tmp_nations)
    team_nation_years <- c(team_nation_years, rep(ref_years[i], length(tmp_nations)))
  }
}
nat_tt <- table(team_nations, team_nation_years, useNA = "always")
print(nat_tt)



# produce plot files
for (plot_log in c(FALSE, TRUE)) {
  for (plot_smoothed in c(FALSE, TRUE)) {
    for (plot_top in c(5, 8)) {
      plot_file <- file.path(stats_folder, paste0("team-nations_top=", plot_top, "_smoothed=", plot_smoothed, "_ylog=", plot_log, ".pdf"))
      tlog("Producing plot file: ", plot_file)

      # overal values
      nat_tt0 <- sort(table(team_nations, useNA = "no"), decreasing = TRUE)
      top_nations <- names(nat_tt0)[1:plot_top]

      # set colors
      color_palette <- c(NATION_COLORS, "Others" = "#919191")

      nat_tt2 <- nat_tt[, !is.na(colnames(nat_tt))]
      # add a new line for the rest of the nations
      nat_tt2 <- rbind(nat_tt2, "Others" = colSums(nat_tt2[!(rownames(nat_tt2) %in% top_nations), , drop = FALSE], na.rm = TRUE))
      top_nations <- c(top_nations, "Others")
      # replace zeros by NA to avoid log(0) in the plot
      if (plot_log)
        nat_tt2[nat_tt2 == 0] <- 1

      pdf(plot_file, width = 14, height = 7)
        par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
        par(mar = c(3.00, 2.75, 1.75, 0.10))  # control margins: B L T R
        # init plot
        plot(NULL,
          xlab = mode_xlabels[mode], ylab = "Number of stints",
          log = if (plot_log) "y" else "",
          #main = paste0("Number of stints as a function of ", mode_labels[mode]),
          xlim = c(min(as.integer(colnames(nat_tt2))), max(as.integer(colnames(nat_tt2)))),
          ylim = c(min(nat_tt2, na.rm = TRUE), max(nat_tt2, na.rm = TRUE))
        )
        # add vertical lines
        abline(v = "1871", col = "black")
        abline(v = "1895", col = "black")
        u3 <- if (plot_log) 10^par("usr")[3] else par("usr")[3]
        u4 <- if (plot_log) 10^par("usr")[4] else par("usr")[4]
        rect(1914, u3, 1918, u4, border = NA, col = "#EEEEEE")
        # abline(v = "1914", col = "black")
        # abline(v = "1918", col = "black")
        rect(1939, u3, 1945, u4, border = NA, col = "#EEEEEE")
        # abline(v = "1939", col = "black")
        # abline(v = "1945", col = "black")
        abline(v = "1995", col = "black")
        for (y in seq(1987, 2023, by = 4))
          abline(v = y, col = "black", lty = 3)
        axis(3, at = c(1871, 1895, 1916, 1942, 1995), labels = c("RFU", "Schism", "WW1", "WW2", "Professionalism"))
        # add series
        for (nation in top_nations) {
          x <- as.integer(colnames(nat_tt2))
          y <- as.integer(nat_tt2[nation, ])
          if (plot_smoothed)
              fit <- loess(y ~ x, na.action = na.exclude, span = 0.1)
          lines(
            x = x,
            y = if (plot_smoothed) predict(fit) else y,
            col = color_palette[nation],
            lwd = 2
          )
        }
        # add legend
        legend(
          "topleft",
          legend = top_nations,
          # cex = 0.8,
          fill = color_palette[top_nations],
          bg = "#FFFFFFBB"
        )
      dev.off()
    }
  }
}




########################################################################
# number of stints by year by type

# get type info
types <- c()
for (i in 1:length(ref_stints)) {
  if (i %% 1000 == 0)
    tlog(2, "Processing entry ", i, "/", length(ref_stints))
  type <- stints[ref_stints[i], "type"]

  # add type to stat list
  types <- c(types, type)
}
pos_tt <- table(types, ref_years, useNA = "always")
print(pos_tt)



# produce plot files
for (plot_log in c(FALSE, TRUE)) {
  for (plot_smoothed in c(FALSE, TRUE)) {
    plot_file <- file.path(stats_folder, paste0("types_smoothed=", plot_smoothed, "_ylog=", plot_log, ".pdf"))
    tlog("Producing plot file: ", plot_file)

    # overal values
    pos_tt0 <- sort(table(types, useNA = "no"), decreasing = TRUE)
    top_types <- names(pos_tt0)

    # set colors
    color_palette <- get.palette(values = length(top_types))
    names(color_palette) <- top_types

    # compute freq table
    pos_tt <- table(types, ref_years, useNA = "always")
    pos_tt <- pos_tt[, !is.na(colnames(pos_tt))]

    # replace zeros by NA to avoid log(0) in the plot
    if (plot_log)
      pos_tt[pos_tt == 0] <- NA

    pdf(plot_file, width = 14, height = 7)
      par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
      par(mar = c(3.00, 2.75, 1.75, 0.10))  # control margins: B L T R
      # init plot
      plot(NULL,
        xlab = mode_xlabels[mode], ylab = "Number of stints",
        log = if (plot_log) "y" else "",
        #main = paste0("Number of stints as a function of ", mode_labels[mode]),
        xlim = c(min(as.integer(colnames(pos_tt))), max(as.integer(colnames(pos_tt)))),
        ylim = c(min(pos_tt, na.rm = TRUE), max(pos_tt, na.rm = TRUE))
      )
      # add vertical lines
      abline(v = "1871", col = "black")
      abline(v = "1895", col = "black")
      u3 <- if (plot_log) 10^par("usr")[3] else par("usr")[3]
      u4 <- if (plot_log) 10^par("usr")[4] else par("usr")[4]
      rect(1914, u3, 1918, u4, border = NA, col = "#EEEEEE")
      # abline(v = "1914", col = "black")
      # abline(v = "1918", col = "black")
      rect(1939, u3, 1945, u4, border = NA, col = "#EEEEEE")
      # abline(v = "1939", col = "black")
      # abline(v = "1945", col = "black")
      abline(v = "1995", col = "black")
      for (y in seq(1987, 2023, by = 4))
        abline(v = y, col = "black", lty = 3)
      axis(3, at = c(1871, 1895, 1916, 1942, 1995), labels = c("RFU", "Schism", "WW1", "WW2", "Professionalism"))
      # add series
      for (type in top_types) {
        x <- as.integer(colnames(pos_tt))
        y <- as.integer(pos_tt[type, ])
        if (plot_smoothed)
            fit <- loess(y ~ x, na.action = na.exclude, span = 0.1)
        lines(
          x = x,
          y = if (plot_smoothed) predict(fit) else y,
          col = color_palette[type],
          lwd = 2
        )
      }
      # add legend
      legend(
        "topleft",
        legend = top_types,
        # cex = 0.8,
        fill = color_palette[top_types],
        bg = "#FFFFFFBB"
      )
    dev.off()
  }
}




########################################################################
# number of stints by year by source
source_names <- c("enWP", "esWP", "frWP", "itWP", "jaWP", "WD")

# note: some stints come from multiple sources, so the same stint can be counted several times

# get source info
sources <- c()
source_years <-c()
for (i in 1:length(ref_stints)) {
  if (i %% 1000 == 0)
    tlog(2, "Processing entry ", i, "/", length(ref_stints))

  stint_sources <- stints[ref_stints[i], "dataSources"]
  lst <- strsplit(stint_sources, split = ";")
  stint_data_sources <- trimws(lst[[1]])

  # add sources to stat list
  sources <- c(sources, stint_data_sources)
  source_years <- c(source_years, rep(ref_years[i], length(stint_data_sources)))
}
pos_tt <- table(sources, source_years, useNA = "always")
print(pos_tt)



# produce plot files
for (plot_log in c(FALSE, TRUE)) {
  for (plot_smoothed in c(FALSE, TRUE)) {
    plot_file <- file.path(stats_folder, paste0("data-sources_smoothed=", plot_smoothed, "_ylog=", plot_log, ".pdf"))
    tlog("Producing plot file: ", plot_file)

    # overal values
    pos_tt0 <- sort(table(sources, useNA = "no"), decreasing = TRUE)
    top_sources <- names(pos_tt0)

    # set colors
    color_palette <- DATASOURCE_COLORS

    # compute freq table
    pos_tt <- table(sources, source_years, useNA = "always")
    pos_tt <- pos_tt[, !is.na(colnames(pos_tt))]

    # replace zeros by NA to avoid log(0) in the plot
    if (plot_log)
      pos_tt[pos_tt == 0] <- NA

    pdf(plot_file, width = 14, height = 7)
      par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
      par(mar = c(3.00, 2.75, 1.75, 0.10))  # control margins: B L T R
      # init plot
      plot(NULL,
        xlab = mode_xlabels[mode], ylab = "Number of stints",
        log = if (plot_log) "y" else "",
        #main = paste0("Number of stints as a function of ", mode_labels[mode]),
        xlim = c(min(as.integer(colnames(pos_tt))), max(as.integer(colnames(pos_tt)))),
        ylim = c(min(pos_tt, na.rm = TRUE), max(pos_tt, na.rm = TRUE))
      )
      # add vertical lines
      abline(v = "1871", col = "black")
      abline(v = "1895", col = "black")
      u3 <- if (plot_log) 10^par("usr")[3] else par("usr")[3]
      u4 <- if (plot_log) 10^par("usr")[4] else par("usr")[4]
      rect(1914, u3, 1918, u4, border = NA, col = "#EEEEEE")
      # abline(v = "1914", col = "black")
      # abline(v = "1918", col = "black")
      rect(1939, u3, 1945, u4, border = NA, col = "#EEEEEE")
      # abline(v = "1939", col = "black")
      # abline(v = "1945", col = "black")
      abline(v = "1995", col = "black")
      for (y in seq(1987, 2023, by = 4))
        abline(v = y, col = "black", lty = 3)
      axis(3, at = c(1871, 1895, 1916, 1942, 1995), labels = c("RFU", "Schism", "WW1", "WW2", "Professionalism"))
      # add series
      for (source in top_sources) {
        x <- as.integer(colnames(pos_tt))
        y <- as.integer(pos_tt[source, ])
        if (plot_smoothed)
            fit <- loess(y ~ x, na.action = na.exclude, span = 0.1)
        lines(
          x = x,
          y = if (plot_smoothed) predict(fit) else y,
          col = color_palette[source],
          lwd = 2
        )
      }
      # add legend
      legend(
        "topleft",
        legend = top_sources,
        # cex = 0.8,
        fill = color_palette[top_sources],
        bg = "#FFFFFFBB"
      )
    dev.off()
  }
}




########################################################################
# stop logging
end.rec.log()
