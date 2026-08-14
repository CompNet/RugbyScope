########################################################################
# Generates plots combining the evolution of the numbers of players,
# teams, and stints, to be placed directly in the paper.
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/stats/evol_all-nbr.R")
########################################################################
library("dplyr")

source("src/common/logging.R")
source("src/common/colors.R")




########################################################################
# parameters

mode <- "active-years"  # "start-year" or "active-years"
mode_labels <- c("start-year" = "career start year", "active-years" = "active year")
mode_xlabels <- c("start-year" = "Career start year", "active-years" = "Active year")




########################################################################
# start logging
start.rec.log(paste0("EvolAllNbr-", mode))
tlog("mode: ", mode)




########################################################################
# create output folder
stats_folder <- file.path("data", "stats", paste0("evol_all-nbr_vs_", mode))
dir.create(stats_folder, showWarnings = FALSE, recursive = TRUE)

# load tables
source("src/stats/load_all_tables.R")




########################################################################
# compute number of players by year

# identify start years
ref_years <- c()
ref_players <- c()
if (mode == "start-year") {
  for (p in 1:nrow(players)) {
    player_id <- players[p, "wikidataId"]
    if (p %% 1000 == 0)
      tlog(2, "Processing player ", player_id, " (", p, "/", nrow(players), ")")

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
      tlog(2, "Processing player ", player_id, " (", p, "/", nrow(players), ")")

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
player_tt <- table(ref_years, useNA = "always")




########################################################################
# compute number of stints by year

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
stint_tt <- table(ref_years, useNA = "always")




########################################################################
# compute number of teams by year

# identify start years
ref_years <- c()
ref_teams <- c()
if (mode == "start-year") {
  for (t in 1:nrow(teams)) {
    team_id <- teams[t, "rugbyscopeId"]
    if (t %% 1000 == 0)
      tlog(2, "Processing team ", team_id, " (", t, "/", nrow(teams), ")")

    inception_year <- teams[t, "inceptionDate"] %>% as.Date() %>% format("%Y") %>% as.integer()

    team_stints <- stints[stints[, "teamRsId"] == team_id, ]
    first_stint <- suppressWarnings(min(c(team_stints[, "startYear"], team_stints[, "endYear"]), na.rm = TRUE))

    if (is.na(first_stint) || is.infinite(first_stint))
      ref_years <- c(ref_years, inception_year)
    else
      ref_years <- c(ref_years, first_stint)
    ref_teams <- c(ref_teams, team_id)
  }

# identify active years
} else if (mode == "active-years") {
  for (t in 1:nrow(teams)) {
    team_id <- teams[t, "rugbyscopeId"]
    if (t %% 1000 == 0)
      tlog(2, "Processing team ", team_id, " (", t, "/", nrow(teams), ")")

    team_stints <- stints[stints[, "teamRsId"] == team_id, ]
    active_years <- c()
    if (nrow(team_stints) > 0) {
      for (s in 1:nrow(team_stints)) {
        stint_start <- team_stints[s, "startYear"]
        stint_end <- team_stints[s, "endYear"]
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
      ref_teams <- c(ref_teams, rep(team_id, length(active_years)))
    }
  }
} else {
  stop("Unknown mode: ", mode)
}
team_tt <- table(ref_years, useNA = "always")




########################################################################
# produce plot files
for (plot_log in c(FALSE, TRUE)) {
  for (plot_smoothed in c(FALSE, TRUE)) {
    plot_file <- file.path(stats_folder, paste0("all_smoothed=", plot_smoothed, "_ylog=", plot_log, ".pdf"))
    tlog("Producing plot file: ", plot_file)

    player_tt2 <- player_tt[!is.na(names(player_tt))]
    stint_tt2 <- stint_tt[!is.na(names(stint_tt))]
    team_tt2 <- team_tt[!is.na(names(team_tt))]

    pdf(plot_file, width = 14, height = 4)
      par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
      par(mar = c(3.00, 2.75, 1.75, 0.10))  # control margins: B L T R

      # prepare values
      player_x <- as.integer(names(player_tt2))
      player_y <- as.integer(player_tt2)
      stint_x <- as.integer(names(stint_tt2))
      stint_y <- as.integer(stint_tt2)
      team_x <- as.integer(names(team_tt2))
      team_y <- as.integer(team_tt2)

      # possibly apply smoothing
      if (plot_smoothed) {
        player_fit <- loess(player_y ~ player_x, na.action = na.exclude, span = 0.05)
        stint_fit <- loess(stint_y ~ stint_x, na.action = na.exclude, span = 0.05)
        team_fit <- loess(team_y ~ team_x, na.action = na.exclude, span = 0.05)
      }

      # init plot
      plot(NULL,
        xlim = range(c(player_x, stint_x, team_x), na.rm = TRUE),
        ylim = range(c(player_y, stint_y, team_y), na.rm = TRUE),
        xlab = mode_xlabels[mode], ylab = "Number of entities",
        #main = paste0("Number of entities as a function of ", mode_labels[mode]),
        log = if (plot_log) "y" else "",
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

      # add series
      lines(
        x = player_x,
        y = if (plot_smoothed) predict(player_fit) else player_y,
        type = "l", col = "#952d2d", lwd = 2
      )
      lines(
        x = stint_x,
        y = if (plot_smoothed) predict(stint_fit) else stint_y,
        type = "l", col = "#32329c", lwd = 2
      )
      lines(
        x = team_x,
        y = if (plot_smoothed) predict(team_fit) else team_y,
        type = "l", col = "#229b22", lwd = 2
      )

      # add legend
      legend(
        "topleft",
        legend = c("Players", "Teams", "Stints"),
        # cex = 0.8,
        fill = c("#952d2d", "#229b22", "#32329c"),
        bg = "#FFFFFFBB"
      )
    dev.off()
  }
}




########################################################################
# stop logging
end.rec.log()
